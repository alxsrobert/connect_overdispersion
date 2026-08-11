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
    age_eth_hh_ref <- import("data/age_ethnicity_householdsize_la.csv")
  } else 
    stop("region must be England, London, Birmingham, Leicester, Liverpool,
         Manchester, or York")
  
  ## rename the columns
  colnames(age_eth_hh_ref) <- c(
    "code", "area", "age", "age_full", "ethnic_code", "ethnic_group", 
    "hh_size_code", "hh_size", "n")
  
  ## Use age_full to create age_min and age_max, the boundaries of the age groups
  age_eth_hh_ref <- age_eth_hh_ref |> 
    filter(area == region) |> 
    mutate(
      age_min = case_when(
        ## If age_full is XXX and under => set age_min to 0
        grepl("and under", age_full) ~ "0",
        ## If age_full is "XXX and over" => set age_min to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and over", age_full) ~ gsub("[a-z]", "", tolower(age_full)),
        ## If age_full is "Aged XXX to YYY years" => set age_min to XXX, so 
        ## remove "Aged " and select everything before the space
        grepl(" to ", age_full) ~ gsub("Aged ", "", age_full) |> 
          gsub(pattern = "[ ].*", replacement = ""),
        ## Otherwise, then age full follows the format "Aged XXX", and age_min
        ## is XXX
        .default = gsub("[^0-9.-]", "", age_full)),
      age_max = case_when(
        ## If age_full is XXX and over => set age_max to 93
        grepl("and over", age_full) ~ "93",
        ## If age_full is XXX and under => set age_max to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and under", age_full) ~ gsub("[a-z]", "", tolower(age_full)),
        ## If age_full is "Aged XXX to YYY years" => set age_max to YYY, so 
        ## remove " years" and select everything after "to "
        grepl(" to ", age_full) ~ gsub(" years", "", age_full) |> 
          gsub(pattern = ".*to ", replacement = ""),
        ## Otherwise, then age full follows the format "Aged XXX", and age_max
        ## is XXX
        .default = gsub("[^0-9.-]", "", age_full))
    ) |> 
    mutate(age_min = as.numeric(age_min), 
           age_max = as.numeric(age_max))
  
  ## Mach age_min to age_groups, the age groups in the model
  age_match <- character()

  for(i in seq_along(age_eth_hh_ref$age_min)){
    ## For each value of age_min in age_eth_hh_ref, set age_match to the highest
    ## value of age_groups with the lower bound below than age_min[i]
    if(all((as.numeric(gsub("[-].*", "", age_groups)) > age_eth_hh_ref$age_min[i]))){
      age_match[i] <- NA
    } else {
      age_match[i] <- 
        age_groups[
          (as.numeric(gsub("[-].*", "", age_groups)) <= age_eth_hh_ref$age_min[i]) |> 
            which() |> max()]
    }
  }
  
  ## Add age_match to age_eth_hh_ref
  age_eth_hh_ref$age_group <- age_match
  
  age_eth_hh <- 
    age_eth_hh_ref |>  
    filter(!is.na(age_group)) |> 
    ## change hh_size to a numeric value
    mutate(
      hh_size = as.numeric(gsub("[^0-9.-]", "", hh_size))) |> 
    ## remove lines where household = 0
    filter(hh_size_code > 0) |>
    ## Rename hh_size to match the model coefficients
    mutate(hh_size = case_when(
      hh_size == 1 ~ "Alone", 
      hh_size == 2 ~ "Two", 
      hh_size == 3 ~ "Three", 
      hh_size == 4 ~ "Four", 
      hh_size > 4 ~ "More than 4", 
      .default = as.character(hh_size))) |> 
    ## Sum n over the new values of hh_size
    group_by(hh_size, age_group, ethnic_group) |> 
    summarise(n = sum(n), .groups = "drop") |> 
    ## Compute the distribution of household size by age and ethnic group 
    group_by(age_group, ethnic_group) |> 
    mutate(tot = sum(n), prop = n /sum(n))


  return(age_eth_hh)
}

## dataset from https://www.ons.gov.uk/datasets/create
## See data/age_ethnicity_hiqual files for details on how the 
## file was generated
clean_hiqual <- function(age_groups, region = "England"){
  ## Import the dataset
  if(region == "England"){
    age_eth_hiqual_ref <- import("data/age_ethnicity_hiqual.csv")
  } else if(region == "London"){
    age_eth_hiqual_ref <- import("data/age_ethnicity_hiqual_london.csv")
  } else if(region %in% c("Birmingham", "Leicester", "Liverpool", 
                          "Manchester", "York")){
    age_eth_hiqual_ref <- import("data/age_ethnicity_hiqual_la.csv")
  } else 
    stop("region must be England, London, Birmingham, Leicester, Liverpool,
         Manchester, or York")
  
  ## rename the columns
  colnames(age_eth_hiqual_ref) <- c(
    "code", "area", "age", "age_full", "ethnic_code", "ethnic_group", 
    "hiqual_code", "hiqual", "sex_code", "sex", "n")
  
  ## Use age_full to create age_min and age_max, the boundaries of the age groups
  age_eth_hiqual_ref <- age_eth_hiqual_ref |> 
    filter(area == region) |> 
    mutate(
      age_min = case_when(
        ## If age_full is XXX and under => set age_min to 0
        grepl("and under", age_full) ~ "0",
        ## If age_full is "XXX and over" => set age_min to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and over", age_full) ~ gsub("[a-z]", "", tolower(age_full)),
        ## If age_full is "Aged XXX to YYY years" => set age_min to XXX, so 
        ## remove "Aged " and select everything before the space
        grepl(" to ", age_full) ~ gsub("Aged ", "", age_full) |> 
          gsub(pattern = "[ ].*", replacement = ""),
        ## Otherwise, then age full follows the format "Aged XXX", and age_min
        ## is XXX
        .default = gsub("[^0-9.-]", "", age_full)),
      age_max = case_when(
        ## If age_full is XXX and over => set age_max to 93
        grepl("and over", age_full) ~ "93",
        ## If age_full is XXX and under => set age_max to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and under", age_full) ~ gsub("[a-z]", "", tolower(age_full)),
        ## If age_full is "Aged XXX to YYY years" => set age_max to YYY, so 
        ## remove " years" and select everything after "to "
        grepl(" to ", age_full) ~ gsub(" years", "", age_full) |> 
          gsub(pattern = ".*to ", replacement = ""),
        ## Otherwise, then age full follows the format "Aged XXX", and age_max
        ## is XXX
        .default = gsub("[^0-9.-]", "", age_full))
    ) |> 
    mutate(age_min = as.numeric(age_min), 
           age_max = as.numeric(age_max)) |>
    ## Below 18, all individuals are considered as children
    filter(age_min >= 18)
  
  ## Mach age_min to age_groups, the age groups in the model
  age_match <- character()
  
  for(i in seq_along(age_eth_hiqual_ref$age_min)){
    ## For each value of age_min in age_eth_hiqual_ref, set age_match to the highest
    ## value of age_groups with the lower bound below than age_min[i]
    if(all((as.numeric(gsub("[-].*", "", age_groups)) > age_eth_hiqual_ref$age_min[i]))){
      age_match[i] <- NA
    } else {
      age_match[i] <- 
        age_groups[
          (as.numeric(gsub("[-].*", "", age_groups)) <= age_eth_hiqual_ref$age_min[i]) |> 
            which() |> max()]
    }
  }
  
  ## Add age_match to age_eth_hiqual_ref
  age_eth_hiqual_ref$age_group <- age_match
  
  age_eth_hiqual <- 
    age_eth_hiqual_ref |>  
    filter(!is.na(age_group)) |> 
    ## change hh_size to a numeric value
    mutate(
      hiqual_code = as.numeric(gsub("[^0-9.-]", "", hiqual_code))) |> 
    filter(hiqual_code >= 0) |> 
    ## Rename hh_size to match the model coefficients
    mutate(hiqual = case_when(
      hiqual_code == 0 ~ "Noquali", 
      hiqual_code == 1 ~ "Level1", 
      hiqual_code == 2 ~ "Level2", 
      hiqual_code == 3 ~ "Level3", 
      hiqual_code == 4 ~ "Level4", 
      hiqual_code == 5 ~ "Apprenti",
      .default = NA)) |> 
    ## Sum n over the new values of hh_size
    group_by(hiqual, sex, age_group, ethnic_group) |> 
    summarise(n = sum(n), .groups = "drop") |> 
    ## Compute the distribution of household size by age and ethnic group 
    group_by(sex, age_group, ethnic_group) |> 
    mutate(tot = sum(n), prop = n /sum(n))
  
  age_eth_hiqual$prop[is.nan(age_eth_hiqual$prop)] <- 0
  
  return(age_eth_hiqual)
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
    "code", "area", "age", "age_full", "sex_code", "sex", "ethnic_code",
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
      "code", "area", "age", "age_full", "ethnic_code", "ethnic_group", 
      "econ_code", "econ", "n", "sex_code", "sex")

    ## We could not import the gender distribution in the raw data (too many 
    ## variables at a local level), so we use the gender distribution from the 
    ## national data to infer the local gender distribution
    age_eth_employ_ref <- 
      age_eth_employ_ref |> 
      left_join(national_level |> 
                  group_by(age, age_full, ethnic_group, econ) |> 
                  mutate(prop = n / sum(n)) |> 
                  select(-area, -code, -n),
                by = c("age", "age_full", "ethnic_code", "ethnic_group",
                       "econ_code", "econ", "sex_code", "sex")) |> 
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
      "code", "area", "age", "age_full", "ethnic_code", "ethnic_group", 
      "econ_code", "econ", "n", "sex_code", "sex")

    ## We could not import the gender distribution in the raw data (too many 
    ## variables at a local level), so we use the gender distribution from the 
    ## national data to infer the local gender distribution
    age_eth_employ_ref <- age_eth_employ_ref |> 
      filter(area == region) |> 
      left_join(national_level |> 
                  group_by(age, age_full, ethnic_group, econ) |> 
                  mutate(prop = n / sum(n)) |> 
                  select(-area, -code, -n),
                by = c("age", "age_full", "ethnic_code", "ethnic_group",
                       "econ_code", "econ", "sex_code", "sex")
                ) |>
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
        grepl("and under", age_full) ~ "0",
        ## If age_full is "XXX and over" => set age_min to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and over", age_full) ~ gsub("[a-z]", "", tolower(age_full)),
        ## If age_full is "Aged XXX to YYY years" => set age_min to XXX, so 
        ## remove "Aged " and select everything before the space
        grepl(" to ", age_full) ~ gsub("Aged ", "", age_full) |> 
          gsub(pattern = "[ ].*", replacement = ""),
        ## Otherwise, then age full follows the format "Aged XXX", and age_min
        ## is XXX
        .default = gsub("[^0-9.-]", "", age_full)),
      age_max = case_when(
        ## If age_full is XXX and over => set age_max to 93
        grepl("and over", age_full) ~ "93",
        ## If age_full is XXX and under => set age_max to XXX (i.e. remove all
        ## non numeric characters)
        grepl("and under", age_full) ~ gsub("[a-z]", "", tolower(age_full)),
        ## If age_full is "Aged XXX to YYY years" => set age_max to YYY, so 
        ## remove " years" and select everything after "to "
        grepl(" to ", age_full) ~ gsub(" years", "", age_full) |> 
          gsub(pattern = ".*to ", replacement = ""),
        ## Otherwise, then age full follows the format "Aged XXX", and age_max
        ## is XXX
        .default = gsub("[^0-9.-]", "", age_full))
    ) |> 
    mutate(age_min = as.numeric(age_min), 
           age_max = as.numeric(age_max)) |>
    ## Below 18, all individuals are considered as children
    filter(age_min >= 18)
  
  ## Match age_min to age_groups, the age groups in the model
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
    summarise(n = sum(n), .groups = "drop") |> 
    ## Compute the distribution of econ by age, gender, and ethnic group 
    group_by(age_group, sex, ethnic_group) |> 
    mutate(tot = sum(n), prop = n /sum(n))
  
  age_eth_employ$prop[is.nan(age_eth_employ$prop)] <- 0
  
  return(age_eth_employ)
}

## Import population data: use household size data and aggregate by age
clean_age_eth <- function(age_groups, region = "England"){
  ## Import household data, rename ethnic groups
  household_data <- clean_hh_size(age_groups, region) |> 
    filter(ethnic_group != "Does not apply") |> 
    mutate(ethnicity = case_when(
      grepl("Asian", ethnic_group) ~ "Asian",
      grepl("Black", ethnic_group) ~ "Black",
      grepl("Mixed", ethnic_group) ~ "Mixed",
      grepl("White", ethnic_group) ~ "White",
      .default = "Other"
    ))
  
  ## Merge all levels of household and compute age distribution by ethnicity
  ## Add level with ethnicity = "All" corresponding to the age distribution 
  ## across ethnicities
  pop_data_ethnicity <- 
    rbind.data.frame(
      household_data |> mutate(ethnicity = "All"),
      household_data 
    ) |> 
    group_by(age_group  = factor(age_group, levels = age_groups), ethnicity) |> 
    summarise(nb = sum(n), .groups = "drop") |> 
    group_by(ethnicity) |> 
    mutate(prop = nb / sum(nb))


  return(pop_data_ethnicity)
}
