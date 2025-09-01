## dataset from https://www.ons.gov.uk/datasets/create
## See data/age_ethnicity_householdsize.txt files for details on how the 
## file was generated
clean_hh_size <- function(age_groups, region = "England"){
  ## Import the dataset
  if(region == "England"){
    age_eth_hh_ref <- import("data/age_ethnicity_householdsize.csv")
  } else if(region == "London"){
    age_eth_hh_ref <- import("data/age_ethnicity_householdsize_london.csv")
  } else if(region %in% c("Birmingham", "Leicester", "Liverpool", 
                          "Manchester", "York")){
    age_eth_hh_ref <- import("data/age_ethnicity_household_size_la.csv")
  } else 
    stop("region must be England, London, Birmingham, Leicester, Liverpool,
         Manchester, or York")
  
  ## rename the columns
  colnames(age_eth_hh_ref) <- c(
    "code", "region", "age", "age_full", "ethnic_code", "ethnic_group", 
    "hh_size_code", "hh_size", "n")
  
  ## Use age_full to create age_min and age_max, the boundaries of the age groups
  age_eth_hh_ref <- age_eth_hh_ref |> 
    filter(region == region) |> 
    mutate(
      age_min = case_when(
        ## If age_full is XXX and under => set age_min to 0
        grepl("and under", age_full) ~ 0,
        ## If age_full is "XXX and over" => set age_min to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and over", age_full) ~ 
          gsub("[a-z]", "", tolower(age_full)) |> as.numeric(),
        ## If age_full is "Aged XXX to YYY years" => set age_min to XXX, so 
        ## remove "Aged " and select everything before the space
        grepl(" to ", age_full) ~ gsub("Aged ", "", age_full) |> 
          gsub(pattern = "[ ].*", replacement = "") |> as.numeric(),
        ## Otherwise, then age full follows the format "Aged XXX", and age_min
        ## is XXX
        .default = gsub("[^0-9.-]", "", age_full) |> as.numeric()),
      age_max = case_when(
        ## If age_full is XXX and over => set age_max to 93
        grepl("and over", age_full) ~ 93,
        ## If age_full is XXX and under => set age_max to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and under", age_full) ~ 
          gsub("[a-z]", "", tolower(age_full)) |> as.numeric(),
        ## If age_full is "Aged XXX to YYY years" => set age_max to YYY, so 
        ## remove " years" and select everything after "to "
        grepl(" to ", age_full) ~ gsub(" years", "", age_full) |> 
          gsub(pattern = ".*to ", replacement = "") |> as.numeric(),
        ## Otherwise, then age full follows the format "Aged XXX", and age_max
        ## is XXX
        .default = as.numeric(gsub("[^0-9.-]", "", age_full)))
    )
  
  ## Mach age_min to age_groups, the age groups in the model
  age_match <- character()

  for(i in seq_along(age_eth_hh_ref$age_min)){
    ## For each value of age_min in age_eth_hh_ref, set age_match to the highest
    ## value of age_groups with the lower bound below than age_min[i]
    age_match[i] <- 
      age_groups[
        (as.numeric(gsub("[-].*", "", age_groups)) <= age_eth_hh_ref$age_min[i]) |> 
          which() |> max()]
  }
  
  ## Add age_match to age_eth_hh_ref
  age_eth_hh_ref$age_group <- age_match
  
  age_eth_hh <- 
    age_eth_hh_ref |>  
    ## change hh_size to a numeric value
    mutate(
      hh_size = as.numeric(gsub("[^0-9.-]", "", hh_size))) |> 
    ## remove lines where household = 0
    filter(hh_size_code > 0) |>
    ## Rename hh_size to match the model coefficients
    mutate(hh_size = case_when(
      hh_size == 1 ~ "alone", 
      hh_size == 2 ~ "Two", 
      hh_size == 3 ~ "Three", 
      hh_size == 4 ~ "Four", 
      hh_size > 4 ~ "More than 4", 
      .default = as.character(hh_size))) |> 
    ## Sum n over the new values of hh_size
    group_by(hh_size, age_group, ethnic_group) |> 
    summarise(n = sum(n)) |> 
    ## Compute the distribution of household size by age and ethnic group 
    group_by(age_group, ethnic_group) |> 
    mutate(tot = sum(n), prop = n /sum(n))


  return(age_eth_hh)
}

## household income by ethnicity, 
## from https://www.ethnicity-facts-figures.service.gov.uk/work-pay-and-benefits/pay-and-income/household-income/latest/#by-ethnicity
## (download the data here: https://www.ethnicity-facts-figures.service.gov.uk/work-pay-and-benefits/pay-and-income/household-income/latest/downloads/household-income-2021.csv)
clean_income <- function(){
  ## The data is only available at a national level, so we assume the distribution
  ## by ethnicity does not change by area
  eth_income_ref <- import("data/income_by_ethnicity.csv")
  
  eth_income <-
    eth_income_ref |> 
    ## Only keep the entries corresponding to the latest dates
    filter(substr(Time, 1, 4) == max(as.numeric(substr(Time, 1, 4)))) |> 
    ## Compute the number of households by multiplying value by the denominator
    mutate(n = as.numeric(Value) * Denominator / 100,
           ## re-format income, by removing all lowercase letters (and only keep 
           ## numbers and GBP), so income looks like GBPXXXGBPYYY
           income = gsub("[a-zL, ]", "", `Income bracket`),
           ## Remove the first three characters, so income looks like XXXGBPYYY
           income = sub("...", "", income),
           ## The lower income boundary is what comes before "GBP" in income
           ## Multiplied by 50 to move from weekly values to annual values
           min = as.numeric(gsub("GBP.*", "", income)) * 50,
           ## The upper income boundary is what comes after "GBP" in income
           ## Multiplied by 50 to move from weekly values to annual values
           max = as.numeric(gsub(".*GBP", "", income)) * 50) |> 
    ## rename income_group to match the coefficients from the model
    mutate(income_group = case_when(
      min < 20000 ~ "p_income_Lessthan20000",
      min < 40000 ~ "p_income_20000_39999",
      min < 60000 ~ "p_income_40000_59999",
      min < 100000 ~ "p_income_60000_100000",
      min >= 100000 ~ "p_income_Over100000"
    )) |> 
    group_by(income_group, `Ethnicity of household reference person`, Denominator) |> 
    summarise(n = sum(n)) |> 
    ## Compute the proportion
    mutate(prop = n / Denominator) |> 
    group_by()
  colnames(eth_income) <- c("income", "ethnicity", "denominator", "n", "prop")
  
  eth_income <- eth_income |> select(income, ethnicity, prop)
  
  return(eth_income)  
}

## dataset from https://www.ons.gov.uk/datasets/create
## See data/age_ethnicity_economic.txt files for details on how the 
## file was generated
clean_employ <- function(age_groups, region = "England"){
  ## Employ employment level at a national level (will be used to compute the 
  ## gender distribution if region is not "England")
  national_level <- import("data/age_ethnicity_economic.csv")
  
  ## rename the columns
  colnames(national_level) <- c(
    "code", "region", "age", "age_full", "sex_code", "sex", "ethnic_code",
    "ethnic_group", "econ_code", "econ", "n")
  
  ## Import the dataset
  if(region == "England"){
    age_eth_employ_ref <- national_level
  } else if(region == "London"){
    age_eth_employ_ref <- rbind.data.frame(
      cbind.data.frame(import("data/age_ethnicity_economic_london.csv"),
                       "Sex (2 categories)" = 1,  
                       "Code Sex (2 categories)" = "Female"),
      cbind.data.frame(import("data/age_ethnicity_economic_london.csv"),
                       "Sex (2 categories)" = 2,
                       "Code Sex (2 categories)" = "Male")
    )
    colnames(age_eth_employ_ref) <- c(
      "code", "region", "age", "age_full", "ethnic_code", "ethnic_group", 
      "econ_code", "econ", "n", "sex_code", "sex")

    ## We could not import the gender distribution in the raw data (too many 
    ## variables at a local level), so we use the gender distribution from the 
    ## national data to infer the local gender distribution
    age_eth_employ_ref <- 
      age_eth_employ_ref |> 
      left_join(national_level |> 
                  group_by(age, age_full, ethnic_group, econ) |> 
                  mutate(prop = n / sum(n)) |> 
                  select(-region, -code, -n)) |> 
      mutate(prop = case_when(is.na(prop) ~ 0, .default = prop),
             n = round(prop * n)) |> 
      select(-prop)
    
  } else if(region %in% c("Birmingham", "Leicester", "Liverpool", "Manchester",
                          "York")){
    age_eth_employ_ref <- 
      rbind.data.frame(
        cbind.data.frame(import("data/age_ethnicity_economic_la.csv"),
                         "Sex (2 categories)" = 1,  
                         "Code Sex (2 categories)" = "Female"),
        cbind.data.frame(import("data/age_ethnicity_economic_la.csv"),
                         "Sex (2 categories)" = 2,
                         "Code Sex (2 categories)" = "Male")
      )
    colnames(age_eth_employ_ref) <- c(
      "code", "region", "age", "age_full", "ethnic_code", "ethnic_group", 
      "econ_code", "econ", "n", "sex_code", "sex")

    ## We could not import the gender distribution in the raw data (too many 
    ## variables at a local level), so we use the gender distribution from the 
    ## national data to infer the local gender distribution
    age_eth_employ_ref <- 
      filter(region == region) |> 
      left_join(national_level |> 
                  group_by(age, age_full, ethnic_group, econ) |> 
                  mutate(prop = n / sum(n)) |> 
                  select(-region, -code, -n)) |> 
      mutate(prop = case_when(is.na(prop) ~ 0, .default = prop),
             n = round(prop * n)) |> 
      select(-prop)
    
  } else 
    stop("region must be England, London, Birmingham, Leicester, Liverpool,
         Manchester, or York")
  
  ## Use age_full to create age_min and age_max, the boundaries of the age groups
  age_eth_employ <- 
    age_eth_employ_ref |> 
    mutate(
      age_min = case_when(
        ## If age_full is XXX and under => set age_min to 0
        grepl("and under", age_full) ~ 0,
        ## If age_full is "XXX and over" => set age_min to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and over", age_full) ~ 
          gsub("[a-z]", "", tolower(age_full)) |> as.numeric(),
        ## If age_full is "Aged XXX to YYY years" => set age_min to XXX, so 
        ## remove "Aged " and select everything before the space
        grepl(" to ", age_full) ~ gsub("Aged ", "", age_full) |> 
          gsub(pattern = "[ ].*", replacement = "") |> as.numeric(),
        ## Otherwise, then age full follows the format "Aged XXX", and age_min
        ## is XXX
        .default = gsub("[^0-9.-]", "", age_full) |> as.numeric()),
      age_max = case_when(
        ## If age_full is XXX and over => set age_max to 93
        grepl("and over", age_full) ~ 93,
        ## If age_full is XXX and under => set age_max to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and under", age_full) ~ 
          gsub("[a-z]", "", tolower(age_full)) |> as.numeric(),
        ## If age_full is "Aged XXX to YYY years" => set age_max to YYY, so 
        ## remove " years" and select everything after "to "
        grepl(" to ", age_full) ~ gsub(" years", "", age_full) |> 
          gsub(pattern = ".*to ", replacement = "") |> as.numeric(),
        ## Otherwise, then age full follows the format "Aged XXX", and age_max
        ## is XXX
        .default = as.numeric(gsub("[^0-9.-]", "", age_full)))
    ) |> 
    ## Below 18, all individuals are considered as children
    filter(age_min >= 18)
  
  ## Mach age_min to age_groups, the age groups in the model
  age_match <- character()
  age_group_adult <- age_groups[as.numeric(gsub(".*[-]", "", age_groups)) >= 18]

  for(i in seq_along(age_eth_employ$age_min)){
    ## For each value of age_min in age_eth_hh_ref, set age_match to the highest
    ## value of age_groups with the lower bound below than age_min[i]
    age_match[i] <- 
      age_group_adult[
        (substr(age_group_adult, 1, 2) <= as.numeric(age_eth_employ$age_min[i])) |> 
          which() |> max()
      ]
  }
  
  ## Add age_match to age_eth_hh_ref
  age_eth_employ$age_group <- age_match
  
  age_eth_employ <- 
    age_eth_employ |> 
    ## change econ to match the levels in the model
    mutate(econ = case_when(
      grepl("In employment", econ) ~ "employed",
      grepl("Unemployed", econ) ~ "unemployed",
      .default = gsub("Economically inactive: ", "", econ)
    )) |> 
    ## Sum n over the new values of econ
    group_by(econ, age_group, sex, ethnic_group) |> 
    summarise(n = sum(n)) |> 
    ## Compute the distribution of econ by age, gender, and ethnic group 
    group_by(age_group, sex, ethnic_group) |> 
    mutate(tot = sum(n), prop = n /sum(n))
  
  age_eth_employ$prop[is.nan(age_eth_employ$prop)] <- 0
  
  return(age_eth_employ)
}

