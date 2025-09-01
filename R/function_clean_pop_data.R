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
