#' Simulate a synthetic population and group each level of age and ethnicity into
#' transmitter groups.
#'
#' @param scenario_contact_group Method used to generate the synthetic population and 
#' the number of contact per individual. Can take one of the following values: 
#' "same_mean": The coefficients of the regression analysis associated with the 
#' impact of ethnicity on the mean number of contacts are set to 1, "same_od": 
#' The coefficients of the regression analysis associated with the impact of 
#' ethnicity on the dispersion are set to 1, "same_pop": The characteristics of
#' the synthetic population are representative of the whole population 
#' (instead of being ethnicity-specific), and "reference". The default value 
#' is "reference". 
#' @param n_group Number of transmitter groups (by default 3: low / medium / high)
#' @param n_draws Number of draws from the regression results used to simulate the number of contacts
#' @param file_in Path to the regression output file.
#' @param which_model Character, defines which model from the list of regression 
#' outputs should be used.
#' @param vec_ethnicity_rural Vector indicating the levels of ethnicity
#' in the synthetic population
#' @param region Region in which the synthetic population is generated. It impacts 
#' the distribution of ethnicity, age by ethnicity, household size, and employment
#' status. Must be one of England, London, Manchester, Birmingham, Leicester, 
#' Liverpool, or York. The default value is "England".
#' @param tot_pop_size Overall size of the synthetic population used to simulate
#' the number of contacts per individual, and compute the transmitter groups.
#' @param each Alternative to pop_size_contact: define the size of the synthetic 
#' population used to simulate the number of contacts per individual by specifying the 
#' Population size at each level of age and ethnicity (e.g. each = 10000 draws 
#' 1000 individuals for each level of age and ethnicity, leading to an overall 
#' synthetic population of 55,000 inhabitants with 5 ethnicities and 11 age groups).
#' @param seed integer: seed of the run
#'
#' @return List containing two numeric matrices: 
#' - prop: the proportion of inhabitants of a given age group and ethnicity
#'         that belongs to each transmitter group.
#' - coef: the average number of contacts in each transmitter group of each
#'         level of age group and ethnicity.
create_contact_group <- function(
    scenario_contact_group, n_group, n_draws, file_in, which_model, 
    vec_ethnicity_rural, region, tot_pop_size = NULL, each = NULL, seed = NULL){
  # CHECK scenario_contact_group is among the possible values
  if(!scenario_contact_group %in% c("same_mean", "same_od", "same_pop", "reference"))
    stop("scenario_contact_group must be one of `same_mean`, `same_od`, `same_pop,` or 
         `reference`")
  
  # Import the regression model
  model <- readRDS(file_in)[[which_model]]
  
  
  if(scenario_contact_group == "same_mean") {
    ## If scenario_contact_group is same_mean, change the values of coefficients
    ## associated with ethnicity
    
    # Extract parameters associated with ethnicity (mean and OD)
    pars_ethnicity <- grep("ethnicity", names(model$fit@sim$samples[[1]]))
    
    # Set values to 0
    for(i in seq_along(model$fit@sim$samples)){
      model$fit@sim$samples[[i]][pars_ethnicity] <- 
        lapply(model$fit@sim$samples[[i]][pars_ethnicity],
               function(X) return(X * 0))
    }
  } else if(scenario_contact_group == "same_od") {
    ## If scenario_contact_group is same_mean, change the values of coefficients
    ## associated with ethnicity and shape
    # Extract parameters associated with ethnicity (OD only)
    pars_ethnicity <- 
      grep("shape_ethnicity", names(model$fit@sim$samples[[1]]))
    
    # Set values to 0
    for(i in seq_along(model$fit@sim$samples)){
      model$fit@sim$samples[[i]][pars_ethnicity] <- 
        lapply(model$fit@sim$samples[[i]][pars_ethnicity],
               function(X) return(X * 0))
    }
  }
  
  ## Generate a synthetic population, with the number of contact per individual
  ## drawn using the regression coefficients.
  # If scenario_contact_group == "same_pop", the demographics characteristics of
  # the population follow the country-wide distribution instead of the ethnicity
  # specific distribution.
  simulated_population <- 
    create_contact_in_pop(
      model = model, tot_pop_size = tot_pop_size, n_draws = n_draws, region = region,
      vec_ethnicity_rural = vec_ethnicity_rural, each = each,
      which_type = if(scenario_contact_group == "same_pop") "population" else "ethnicity-stratified\n population",
      seed = seed) |> 
    select(ethnicity_rural, p_age_group, contact)
  
  ## Create the transmitter groups, drawing the proportion of the population and 
  ## the number of contacts in each group.
  # The transmitter groups are specific to each level of age group and ethnicity
  # Extract the number of age groups and ethnicities
  tot_age <- length(unique(simulated_population$p_age_group))
  tot_eth <- length(unique(simulated_population$ethnicity_rural))
  # Initialise empty matrices for the proportion of the population and the mean
  # number of contacts
  coef <- matrix(ncol = n_group, nrow = tot_age * tot_eth)
  prop <- matrix(ncol = n_group, nrow = tot_age * tot_eth)
  for(i in seq_len(tot_eth)){
    for(j in seq_len(tot_age)){
      # Extract the individuals of the population of ethnicity i and age j
      pop_ij <- simulated_population |> 
        filter(ethnicity_rural == unique(simulated_population$ethnicity_rural)[i], 
               p_age_group == unique(simulated_population$p_age_group)[j])
      
      # Fit the distribution of contact to n_group Poisson distributions
      flexfit_ij <- flexmix(contact ~ 1, data = pop_ij, k = n_group, 
                            model = FLXglm(family = "poisson")
      )
      
      # Extract the proportion of the population in each group
      size_fit <- flexfit_ij@prior
      # Extract the mean number of contacts in each group
      coef_fit <- parameters(flexfit_ij)
      
      # Set the coefficients in increasing orders
      rank_ij <- order(coef_fit[1:n_group])
      # Add the proportion and mean to the prop and coef matrices
      prop[tot_age * (i-1) + j,] <- size_fit[rank_ij]
      coef[tot_age * (i-1) + j,] <- exp(coef_fit[1:n_group][rank_ij])
    }
  }
  
  prop[is.na(prop)] <- 0
  coef[is.na(coef)] <- 0    
  
  rownames(prop) <- rownames(coef) <- 
    paste0(rep(unique(simulated_population$p_age_group), tot_eth),
           rep(unique(simulated_population$ethnicity_rural), each = tot_age))
  
  return(list(prop = prop, coef = coef))
}

