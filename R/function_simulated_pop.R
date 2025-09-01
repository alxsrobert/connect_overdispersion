#' Create synthetic population
#'
#' @param list_with_inc List of regression outputs (saved in script_regression.R)
#' @param tot_size Overall size of the population
#' @param n_draws Number of draws from the regression results used to simulate the number of contacts
#' @param which_model Which model from list_with_inc should be used to simulate
#' the number of contacts
#' @param anon logical: whether to use anonymised data
#' @param seed 
#' @param which_type What type of population is used to generate the feature of 
#' each individual ("at baseline": only one level of income, gender, age, 
#' household size, and employment) is used; "population": the age, gender, 
#' household size, income, and employment distribution in the simulated 
#' population corresponds to the overall distribution in the UK by age;
#' "ethnicity-stratified\n population": the distribution of age, gender, 
#' household size, income, and employment in the simulated population 
#' corresponds to the distribution *by ethnicity* and age in the UK.
#'
#' @return Data frame containing the age group, gender, household size, income, 
#' employment status, and ethnicity for each individual.
#' @export
#'
#' @examples
create_pop <- function(
    list_with_inc, tot_size, n_draws, which_model, anon, seed, which_type){
  ### First, we want to import the distribution of household size, income, 
  ### employment status, age group and ethnicity in the population.
  ## We use the age groups from the regression model, and order them in 
  ## increasing order.
  all_age_groups <- list_with_inc[[which_model]]$data$p_age_group |> 
    unique() |> as.character()
  age_group_order <- order(as.numeric(gsub(".*[-+]", "", all_age_groups)))
  age_group_level <- unique(all_age_groups)[age_group_order]
  
  vec_ethnicity_rural <- c("Asian_Urban", "Black_Urban", "Mixed_Urban",
                           "White_Urban", "White_Rural")
  n_level <- length(vec_ethnicity_rural)  
  
  # Create population according to the type
  if(which_type == "at baseline"){
    ## type = baseline: Using one level for each variable, only ethnicity changes
    pop_size <- round(pop_size / n_level) * n_level
    df_indiv <- tibble(
      age = factor("18-24", levels = age_group_level),
      ethnicity_rural = rep(vec_ethnicity_rural, pop_size / n_level),
      day_week = "weekday",
      p_gender = "Female",
      cat_household_members = "Three",
      employ = "employed",
      income = "p_income_20000_39999"
    ) |> mutate(ethnicity = gsub("[_].*", "", ethnicity_rural))
    
    
  } else if(which_type == "population"){
    ## type = population: Using the distribution of each variable in the whole 
    ## population. In df_indiv, only ethnicity changes
    
    ## Proportion of household size in England, stratified by age and ethnic group
    ## Group all ethnicities, then compute proportion
    dt_hh_size <- clean_hh_size(age_group_level) |> 
      group_by(age_group, hh_size) |>
      summarise(n = sum(n), .groups = "drop") |>
      group_by(age_group) |>
      mutate(tot = sum(n), prop = n /sum(n), ethnic_group = "All")
    ## Proportion of household income in England, stratified by ethnic group
    dt_income <- clean_income() |> filter(ethnicity == "All")
    ## Proportion of employment status in England, stratified by age, gender, and
    ## ethnic group
    ## Group all ethnicities, then compute proportion
    dt_employ <- clean_employ(age_group_level) |>
      group_by(age_group, econ, sex) |>
      summarise(n = sum(n), .groups = "drop") |>
      group_by(age_group, sex) |>
      mutate(tot = sum(n), prop = n /sum(n), ethnic_group = "All")
    ## Distribution of ethnicity by age group in England
    dt_age_eth <- clean_age_eth(age_group_level) |> filter(ethnicity == "all")
    
    ## Divide and multiply by n_level, to make sure the number of replicates
    ## in rep(vec_ethnicity_rural, pop_size / n_level) is round
    pop_size <- round(pop_size / n_level) * n_level
    df_indiv <- tibble(
      # Sample age from dt_age_eth
      age = sample(dt_age_eth$age_group, size = pop_size, replace = TRUE, 
                   prob = dt_age_eth$prop),
      ethnicity_rural = rep(vec_ethnicity_rural, pop_size / n_level),
      day_week = "weekday",
      # Equal distribution of gender
      p_gender = sample(c("Male", "Female"), pop_size, replace = TRUE)
    ) |> 
      # Add an "ethnicity" column taking the first half of ethnicity_rural
      mutate(ethnicity = gsub("[_].*", "", ethnicity_rural)) |> 
      # For each row, use dt_hh_size to draw the household size given the age of 
      # each individual
      rowwise() |> 
      mutate(cat_household_members = sample(
        dt_hh_size$hh_size[dt_hh_size$age_group == age], 
        replace = TRUE, size = 1, 
        dt_hh_size$prop[dt_hh_size$age_group == age])) |> 
      # For each row, use dt_income to draw the household income given the age of 
      # each individual
      mutate(income = sample(
        dt_income$income, 
        replace = TRUE, size = 1, 
        dt_income$prop)) |>
      # For each row, use dt_employ to draw the employment status given the age 
      # and gender of each individual. In age is below 18, employ is set to "child"
      mutate(employ = ifelse(
        as.numeric(gsub(".*[-]", "", age)) >= 18, yes = 
          sample(
            dt_employ$econ[dt_employ$sex  == p_gender & 
                             dt_employ$age_group == age], 
            replace = TRUE, size = 1, 
            dt_employ$prop[dt_employ$sex  == p_gender & 
                             dt_employ$age_group == age]),
        no = "child"),
        employ = gsub("[ _]", "", employ),
        employ = gsub("-", "_", employ))|> 
      ungroup()
    
  } else if(which_type == "ethnicity-stratified\n population"){
    ## type = ethnicity-stratified population: Using the distribution of each 
    ## variable in the population for each ethnicity.
    df_indiv <- tibble()
    pop_size_i <- round(pop_size / 5)
    for(i in vec_ethnicity_rural){
      # Extract the ethnicity level for i
      ethnic_i <- gsub("[_].*", "", i)
      
      ## Proportion of household size in England, stratified by age and ethnic group
      dt_hh_size_i <- clean_hh_size(age_group_level) |> 
        filter(grepl(ethnic_i, ethnic_group))
      ## Proportion of household income in England, stratified by ethnic group
      dt_income_i <- clean_income() |> filter(ethnicity == ethnic_i)
      ## Proportion of employment status in England, stratified by age, gender, and
      ## ethnic group
      dt_employ_i <- clean_employ(age_group_level) |> 
        filter(grepl(ethnic_i, ethnic_group))
      ## Distribution of ethnicity by age group in England
      dt_age_eth_i <- clean_age_eth(age_group_level) |> 
        filter(ethnicity == tolower(ethnic_i))

      df_indiv_i <- tibble(
        # Sample age from dt_age_eth_i
        age = sample(dt_age_eth_i$age_group, 
                     size = pop_size_i, replace = TRUE, 
                     prob = dt_age_eth_i$prop),
        ethnicity_rural = i,
        ethnicity = ethnic_i,
        day_week = "weekday",
        # Equal distribution of gender
        p_gender = sample(c("Male", "Female"), pop_size_i, replace = TRUE)
      ) |> 
        rowwise() |> 
        # For each row, use dt_hh_size to draw the household size given the age
        # of each individual
        mutate(cat_household_members = sample(
          dt_hh_size_i$hh_size[dt_hh_size_i$age_group == age], 
          replace = TRUE, size = 1, 
          dt_hh_size_i$prop[dt_hh_size_i$age_group == age])) |> 
        # For each row, use dt_income to draw the household income given the age
        # of each individual
        mutate(income = sample(
          dt_income_i$income, 
          replace = TRUE, size = 1, 
          dt_income_i$prop)) |>
        # For each row, use dt_employ to draw the employment status given the age 
        # and gender of each individual. In age is below 18, employ is set to "child"
        mutate(employ = ifelse(
          as.numeric(gsub(".*[-]", "", age)) >= 18, yes = 
            sample(
              dt_employ_i$econ[dt_employ_i$sex  == p_gender & 
                                 dt_employ_i$age_group == age], 
              replace = TRUE, size = 1, 
              dt_employ_i$prop[dt_employ_i$sex  == p_gender & 
                                 dt_employ_i$age_group == age]),
          no = "child"),
          employ = gsub("[ _]", "", employ),
          employ = gsub("-", "_", employ))|> 
        ungroup()
      
      df_indiv <- rbind.data.frame(df_indiv, df_indiv_i)
    }
  }
  
  ## Move to wider format to match the regression inputs
  df_indiv <- 
    df_indiv |> 
    mutate(flag = 1, 
           id_indiv = seq_len(nrow(df_indiv))
    ) |> 
    # Widen according to income
    pivot_wider(names_from = income, values_from = flag, values_fill = 0
    ) |> 
    # Widen according to employ
    mutate(flag = 1) |> 
    pivot_wider(names_from = employ, values_from = flag, values_fill = 0, 
                names_prefix = "employ_") |> 
    # Remove child levels, and reference levels for income and employ
    select(-contains("child")) |> 
    select(-p_income_20000_39999) |> 
    select(-employ_employed) |> 
    # Set 18-24 as the reference of p_age_group, and add individual id
    mutate(p_age_group = relevel(age, ref = "18-24"),
           id_indiv = row_number())
  
  return(df_indiv)
}
