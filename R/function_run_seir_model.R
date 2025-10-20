#' Set up and run a set of simulations using the SEIR transmission model
#'
#' @param n_particles Number of simulations run with the SEIR model.
#' @param model dust compartmental model.
#' @param t Time.
#' @param seed integer: seed of the run
#' @param r0 Basic reproduction number.
#' @param beta Transmissibility rate.
#' @param gamma Duration of infectious period.
#' @param delta Duration of latent period.
#' @param region Region in which the synthetic population and SEIR simulations are 
#' generated. It impacts the number of inhabitants in the population, the 
#' distribution of ethnicity, age by ethnicity, household size, and employment 
#' status. Must be one of England, London, Manchester, Birmingham, Leicester, 
#' Liverpool, or York. The default value is "England".
#' @param pop_size Size of the population in the SEIR model. If set to NULL (default),
#' the population size is the number of inhabitants in the region.
#' @param list_prop_coef list (output from create_contact_group). If set to NULL 
#' (default), list_prop_coef is computed in run_outbreaks. If list_prop_coef is 
#' provided, scenario_contact_group, tot_pop_size, anonymised, each, n_group, k, 
#' which_model, n_draws are not used.
#' @param scenario_contact_group Method used to generate the synthetic population and 
#' the number of contact per individual. Can take one of the following values: 
#' "same_mean": The coefficients of the regression analysis associated with the 
#' impact of ethnicity on the mean number of contacts are set to 1, "same_od": 
#' The coefficients of the regression analysis associated with the impact of 
#' ethnicity on the dispersion are set to 1, "same_pop": The characteristics of
#' the synthetic population are representative of the whole population 
#' (instead of being ethnicity-specific), "same_all": combines same_mean and 
#' same_pop and "reference". The default value is "reference". 
#' @param pop_size_contact Overall size of the synthetic population used to simulate
#' the number of contacts per individual, and compute the transmitter groups.
#' @param each Alternative to pop_size_contact: define the size of the synthetic 
#' population used to simulate the number of contacts per individual by specifying the 
#' population size at each level of age and ethnicity (e.g. each = 1000 draws 
#' 1000 individuals for each level of age and ethnicity, leading to an overall 
#' synthetic population of 55,000 inhabitants with 5 ethnicities and 11 age groups).
#' @param n_group Number of transmitter groups (by default 3: low / medium / high)
#' @param k clustering factor for high-contact individuals.
#' @param which_model Character, defines which model from the list of regression 
#' outputs should be used.
#' @param n_draws Number of draws from the regression outputs used to simulate 
#' the number of contacts
#' @param same_age_distribution Binary, if TRUE: use the same age distribution
#' for all ethnicities
#' @param all_same Binary, if TRUE: The per capita transmission matrix stratified
#' by age, ethnicity, and transmitter group is constant (i.e. homogeneous mixing
#' between groups).
#' @param all_eth Binary, if TRUE: The per capita transmission matrix stratified
#' by ethnicity is constant (i.e. homogeneous mixing between ethnicities).
#' @param anonymised If set to TRUE, use the regression output from the anonymised data.
#' @param return_only_r0 If set to TRUE, changes the function to return r0 after 
#' computing it from beta, the per capita matrix, and gamma.
#'
#' @return A list of named arrays containing the number of individuals in each
#' state, simulation, strata, and at each time step (output from function_run_simulations).
run_outbreaks <- function(
    same_age_distribution = FALSE, scenario_contact_group = "reference", all_same = FALSE,
    all_eth = FALSE, n_group = 3, region = "England", k = 1, pop_size_contact = NULL, 
    r0 = NULL, beta = NULL, gamma = 5, delta = 3, n_particles = 200, t = seq(0, 700), 
    model = seir_stoch_strat, pop_size = NULL, seed = NULL, anonymised = FALSE, 
    n_draws = 5, list_prop_coef = NULL, which_model = "full_od_cathh", each = NULL, 
    return_only_r0 = FALSE){
  ## If there is a seed, set the seed
  if(!is.null(seed)) set.seed(seed)
  
  
  if(is.null(list_prop_coef)){
    if(is.null(pop_size_contact) & is.null(each)) {
      stop("both pop_size_contact and each are null, specify one of the two")
    } else if(!is.null(pop_size_contact) & !is.null(each)) {
      warning("values are assigned to both pop_size_contact and each, the program is using each and ignoring tot_pop_size")
    }
    
    ## Compute the contact groups for each age group and ethnicity
    ## list_prop_coef contains two elements:
    ## - prop: the proportion of inhabitants of a given age group and ethnicity
    ##         that belongs to each transmitter group
    ## - coef: the average number of contacts in each transmitter group of each
    ##         level of age group and ethnicity
    file_in <- paste0("results/regression_output", if(anonymised) "_anoun", ".rds")
    list_prop_coef <- create_contact_group(
      scenario_contact_group = scenario_contact_group, n_group = n_group, 
      n_draws = n_draws, file_in = file_in, which_model = which_model, 
      region = region, seed = NULL, tot_pop_size = pop_size_contact, each = each,
      vec_ethnicity_rural = c("Asian_Urban", "Black_Urban", "Mixed_Urban", 
                              "Other_Urban", "White_Urban"))
  } else if(n_group != ncol(list_prop_coef$prop)) {
    n_group <- ncol(list_prop_coef$prop)
    warning(paste0("n_group is set to ", n_group, 
                   " to match the number of groups in list_prop_coef"))
  }
  
  ## Import age- and ethnicity-stratified per capita contact data from RECONNECT
  contact_age <- readRDS("results/dt_contact_age.RDS")
  contact_eth <- readRDS("results/dt_contact_eth.RDS")
  
  ## Turn contact_age and contact_eth into square matrices
  contact_age <- contact_age[order(contact_age$p_var, contact_age$c_var),]
  contact_eth <- contact_eth[order(contact_eth$p_var, contact_eth$c_var),]
  matrix_age <- 
    matrix(contact_age$mean_mu, 
           nrow = length(unique(unique(contact_age$p_var))),
           ncol = length(unique(unique(contact_age$p_var))),
           dimnames = list(unique(contact_age$p_var),
                           unique(contact_age$c_var)))
  matrix_eth <- 
    matrix(contact_eth$mean_mu, 
           nrow = length(unique(unique(contact_eth$p_var))),
           ncol = length(unique(unique(contact_eth$p_var))),
           dimnames = list(tolower(unique(contact_eth$p_var)),
                           tolower(unique(contact_eth$c_var))))
  # Rename last column / row of matrix_age
  rownames(matrix_age)[rownames(matrix_age) == "70+"] <- 
    colnames(matrix_age)[rownames(matrix_age) == "70+"] <- "70-93"
  
  ## Extract all age groups, and re-arrange them to be in ascending order
  all_age_groups <- rownames(matrix_age)
  ## Remove NAs (if any)
  all_age_groups <- 
    all_age_groups[!is.na(as.numeric(gsub(".*[-+]", "", all_age_groups)))]
  ## Order first half of all_age_groups
  age_group_order <- order(as.numeric(gsub(".*[-+]", "", all_age_groups)))
  ## Set all_age_groups in increasing order
  age_group_level <- unique(all_age_groups)[age_group_order]
  age_group_level <- paste0(
    c(0, as.numeric(gsub(".*[-]", "", rev(rev(age_group_level)[-1]))) + 1),
    "-", gsub(".*[-]", "", age_group_level)
  )
  
  ## Re-arrange matrix_age to be in increasing order of age
  matrix_age <- matrix_age[age_group_level, age_group_level]
  
  ## Extract the number of inhabitants per age group and ethnicity for the region
  ## of interest
  pop_data_ethnicity <- clean_age_eth(age_group_level, region) |> 
    select(age_group, ethnicity, nb) |> 
    filter(ethnicity != "All") |> 
    mutate(ethnicity = paste0("nb_", ethnicity)) |> 
    pivot_wider(names_from = ethnicity, values_from = nb)
  
  ## If same_age_distribution is TRUE, change the age distribution in pop_data_ethnicity
  ## so the age distribution is the same across all ethnicities
  if(same_age_distribution){
    prop_age <- rowSums(pop_data_ethnicity[,-1]) / sum(pop_data_ethnicity[,-1])
    nb_tot <- colSums(pop_data_ethnicity[,-1])
    pop_data_ethnicity[, -1] <- round(outer(prop_age, nb_tot))
  }
  
  ## If pop_size is set, change pop_data_ethnicity so the total number of inhabitants
  ## is equal to pop_size.
  if(!is.null(pop_size)){
    pop_data_ethnicity[, -1] <- round(pop_data_ethnicity[-1] * pop_size / sum(pop_data_ethnicity[, -1]))
  }
  pop_size <- sum(pop_data_ethnicity[, -1])
  
  ## Project the per capita matrices (by age and ethnicity) into the current population
  matrix_age <- matrix_age * 67596281 / pop_size
  matrix_eth <- matrix_eth * 67596281 / pop_size
  
  ## If all_eth is TRUE, then set matrix_eth to be constant (homogeneous mixing)
  ## between ethnicities
  if(all_eth){
    matrix_eth[, ] <- mean(matrix_eth)
  }
  
  ## Change pop_data_ethnicity to be a numeric matrix
  pop_data_ethnicity <- 
    matrix(unlist(pop_data_ethnicity |> select(!age_group)), 
           nrow = nrow(pop_data_ethnicity), ncol = ncol(pop_data_ethnicity) - 1,
           dimnames = list(pop_data_ethnicity$age_group, 
                           colnames(pop_data_ethnicity |> select(-age_group)))
    )
  
  # reorganise matrix_eth
  matrix_eth <- matrix_eth[
    tolower(gsub("nb_", "", colnames(pop_data_ethnicity))),
    tolower(gsub("nb_", "", colnames(pop_data_ethnicity)))]
  
  # Extract the proportion of population and mean number of contacts from list_prop_coef
  prop <- list_prop_coef$prop
  coef <- list_prop_coef$coef
  
  # Only select urban coeffients, and rename the rows
  prop <- prop[grepl("Urban", rownames(prop)),]
  coef <- coef[grepl("Urban", rownames(prop)),]
  rownames(prop) <- tolower(gsub("_Urban", "", rownames(prop)))
  rownames(coef) <- tolower(gsub("_Urban", "", rownames(coef)))
  
  # re-organise rows of coef and prop
  prop <- prop[paste0(rep(rownames(matrix_age), each = nrow(matrix_eth)),
                      rep(rownames(matrix_eth), nrow(matrix_age))),]
  coef <- coef[paste0(rep(rownames(matrix_age), each = nrow(matrix_eth)),
                      rep(rownames(matrix_eth), nrow(matrix_age))),]
  
  ## Run the SEIR model
  y_test <- function_run_simulations(
    n_pop_age_eth_mat = pop_data_ethnicity, mat_age = matrix_age, 
    mat_eth = matrix_eth, n_contact_age_eth_group = coef, 
    prop_indiv_age_eth_group = prop, t = t, gamma = gamma, delta = delta, 
    all_same = all_same, n_particles = n_particles, model = model, 
    r0 = r0, beta = beta, k = k, return_only_r0 = return_only_r0)
  return(y_test)
}

#' Run a set of simulations using the SEIR transmission model
#'
#' @param n_pop_age_eth_mat 2-d numeric matrix showing the number of inhabitants per 
#' age group (row) and ethnicity (column).
#' @param mat_age Per capita transmission matrix by age.
#' @param mat_eth Per capita transmission matrix by ethnicity.
#' @param model 
#' @param n_contact_age_eth_group 2-d numeric matrix: Average number of contacts  
#' by age-ethnicity (row) and transmitter group (column).
#' @param prop_indiv_age_eth_group 2-d numeric matrix: Proportion of the population
#' in each level of age-ethnicity (row) by transmitter group (column) (i.e. 
#' all(rowSums(prop_indiv_age_eth_group = 1))).
#' @param t Time
#' @param gamma Duration of infectious period.
#' @param delta Duration of latent period.
#' @param all_same binary: if TRUE, return an homogeneous mixing per capita 
#' matrix (i.e. all cells have the same value).
#' @param n_particles Number of simulations run with the SEIR model.
#' @param beta Transmissibility rate.
#' @param r0 Basic reproduction number
#' @param k clustering factor for high-contact individuals.
#' @param return_only_r0 If set to TRUE, changes the function to return r0 after 
#' computing it from beta, the per capita matrix, and gamma.
#'
#' @return A list of named arrays containing the number of individuals in each
#' state, simulation, strata, and at each time step.
function_run_simulations <- function(
    n_pop_age_eth_mat, mat_age, mat_eth, model, n_contact_age_eth_group, 
    prop_indiv_age_eth_group, t, gamma, delta, all_same, n_particles,
    beta = NULL, r0 = NULL, k = 1, return_only_r0 = FALSE){
  if(return_only_r0 & !is.null(r0)){
    return_only_r0 <- FALSE
    warning("setting return_only_r0 = FALSE as r0 is set in the arguments, provide beta instead and set r0 to NULL")
  } else if (return_only_r0 & is.null(beta)){
    stop("If return_only_r0 is set to TRUE, beta has to be provided, and the function will compute and return r0")
  }
  
  ## Compute the per capita contact matrix between each strata (i.e. individual
  ## combination of age group, ethnicity, and transmitter group)
  per_cap_matrix <- 
    get_per_capita(n_pop_age_eth_mat = n_pop_age_eth_mat, mat_age_per_cap = mat_age, 
                   mat_eth_per_cap = mat_eth, all_same = all_same,
                   n_contact_age_eth_group = n_contact_age_eth_group, 
                   prop_indiv_age_eth_group = prop_indiv_age_eth_group,
                   k = k
    )
  
  # Extract the number of transmitter groups for each age group / ethnicity
  n_group <- ncol(n_contact_age_eth_group)
  
  ## Compute the number of inhabitants in each column of the per capita matrix:
  ## Multiply the proportion of inhabitant in each transmitter group by 
  ## the number of inhabitants in each level of {age group, ethnicity}
  n_pop_to <- matrix(round(c(
    prop_indiv_age_eth_group * matrix(t(n_pop_age_eth_mat), 
                                      nrow = nrow(mat_age) * ncol(mat_eth),
                                      ncol = n_group))), 
    nrow = prod(dim(prop_indiv_age_eth_group)), 
    ncol = prod(dim(prop_indiv_age_eth_group)), 
    byrow = TRUE)
  ## Set column names (unique combination of ethnicity, age group, and transmitter group)
  colnames(n_pop_to) <- paste0(
    rep(paste0(rep(paste0("eth", seq_len(ncol(n_pop_age_eth_mat))), 
                   nrow(n_pop_age_eth_mat)),
               rep(paste0("age", seq_len(nrow(n_pop_age_eth_mat))), 
                   each = ncol(n_pop_age_eth_mat))),
        n_group),
    rep(paste0("group", seq_len(n_group)), each = prod(dim(n_pop_age_eth_mat))))
  
  ## Generate initial number of exposed individuals (distribute ~ 30 infections
  ## to reduce risk of early extinction)
  prob <- rep(30/sum(n_pop_to[1,]), ncol(n_pop_to))
  E0_vec <- rbinom(n = ncol(n_pop_to), size = n_pop_to[1,], prob = prob)
  
  
  ## If there are any empty groups (i.e. level of populations with 0 inhabitants),
  ## they are removed from n_pop_to and E0_vec, to avoid errors when running the
  ## model
  if(any(n_pop_to[1,] == 0)){
    empty_groups <- which(n_pop_to[1,] == 0)
    n_pop_to <- n_pop_to[-empty_groups, -empty_groups]
    E0_vec <- E0_vec[-empty_groups]
  }
  
  if(is.null(r0) & is.null(beta)){
    stop("both r0 and beta are null, please specify one of the two")
  } else if(!is.null(r0) & !is.null(beta)){
    warning("values are assigned to both r0 and beta, the program is using beta and ignoring r0")
    r0 <- Re(eigen(beta * n_pop_to * per_cap_matrix * gamma)$values[1])
    message(paste0("r0 = ", round(r0, 3)))
  } else if(is.null(beta)){
    ### Compute beta from the next generation matrix: the next generation matrix
    ### is computed from the per capita matrix, the duration of the infectious 
    ### period, and the number of inhabitants in the infectee group
    beta <- r0 / Re(eigen(n_pop_to * per_cap_matrix * gamma)$values[1])
  } else if(return_only_r0) { 
    ### Compute R0 from the next generation matrix: the next generation matrix
    ### is computed from the per capita matrix, the duration of the infectious 
    ### period, and the number of inhabitants in the infectee group
    r0 <- Re(eigen(beta * n_pop_to * per_cap_matrix * gamma)$values[1])
    return(r0)
  }
  
  dt_i <- 1/10
  ## Define the parameters of the SEIR model
  sys <- dust2::dust_system_create(
    model, pars = list(N = n_pop_to[1,],
                       E0 = E0_vec,
                       beta0 = beta, 
                       gamma = 1/gamma, 
                       delta = 1/delta,
                       per_cap_matrix = per_cap_matrix,
                       n_tot = nrow(per_cap_matrix)
    )
    , n_particles = n_particles, dt = dt_i
  )
  
  ## Run the seir model
  dust2::dust_system_set_state_initial(sys)
  y <- dust2::dust_system_simulate(sys, t)
  ## Extract trajectories
  y <- dust2::dust_unpack_state(sys, y)
  
  ## Set names of each strata in y
  y <- lapply(y, function(X){
    rownames(X) <- rownames(per_cap_matrix)
    return(X)
  })
  
  return(y)
}

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
#' (instead of being ethnicity-specific), "same_all": combines same_mean and 
#' same_pop and "reference". The default value is "reference". 
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
#' Population size at each level of age and ethnicity (e.g. each = 1000 draws 
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
  if(!scenario_contact_group %in% c("same_mean", "same_od", "same_pop", "same_all", "reference"))
    stop("scenario_contact_group must be one of `same_mean`, `same_od`, `same_pop`, 
    `same_all` or `reference`")
  
  # Import the regression model
  model <- readRDS(file_in)[[which_model]]
  
  
  if(scenario_contact_group %in% c("same_mean", "same_all")) {
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
    ## If scenario_contact_group is same_od, change the values of the shape coefficients
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
      which_type = if(scenario_contact_group %in% c("same_pop", "same_all")) 
        "population" else "ethnicity-stratified\n population",
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

#' Generate per capita matrix stratified by age, ethnic group, and transmitter group
#'
#' @param n_pop_age_eth_mat 2-d numeric matrix showing the number of inhabitants per 
#' age group (row) and ethnicity (column).
#' @param mat_age_per_cap Per capita transmission matrix by age.
#' @param mat_eth_per_cap Per capita transmission matrix by ethnicity.
#' @param all_same binary: if TRUE, return an homogeneous mixing per capita 
#' matrix (i.e. all cells have the same value).
#' @param n_contact_age_eth_group 2-d numeric matrix: Average number of contacts  
#' by age-ethnicity (row) and transmitter group (column).
#' @param prop_indiv_age_eth_group 2-d numeric matrix: Proportion of the population
#' in each level of age-ethnicity (row) by transmitter group (column) (i.e. 
#' all(rowSums(prop_indiv_age_eth_group = 1))).
#' @param k clustering factor for high-contact individuals.
#'
#' @return Per capita matrix (numeric matrix)
get_per_capita <- function(n_pop_age_eth_mat, mat_age_per_cap, mat_eth_per_cap, 
                           all_same, n_contact_age_eth_group, 
                           prop_indiv_age_eth_group, k){
  ## Total population size
  tot_pop <- sum(n_pop_age_eth_mat)
  ## number of age groups
  n_age <- nrow(mat_age_per_cap)
  ## number of ethnicities
  n_eth <- nrow(mat_eth_per_cap)
  ## number of transmitter groups
  n_group <- ncol(prop_indiv_age_eth_group)
  
  ## number of inhabitants per age group
  n_pop_age_from <- 
    matrix(rowSums(n_pop_age_eth_mat), nrow = n_age, ncol = n_age)
  ## number of inhabitants per ethnicity
  n_pop_eth_from <- 
    matrix(colSums(n_pop_age_eth_mat), nrow = n_eth, ncol = n_eth)
  
  ## Standardise age transmission matrix
  mat_age_standardised <- mat_age_per_cap * t(n_pop_age_from)

  ### Compute distribution of contacts stratified by both age and ethnicity
  ## First, create the transmission matrix stratified by both age and ethnicity
  mat_age_eth <- mat_prop_age_eth <-  
    matrix(nrow = n_age * n_eth, ncol = n_age * n_eth)
  for(i in seq_len(nrow(mat_age_eth))){
    for(j in seq_len(ncol(mat_age_eth))){
      ## Number of contacts between (age_i;eth_i) and (age_j;eth_j)
      eth_i <- rep(seq_len(n_eth), n_age)[i]
      age_i <- rep(seq_len(n_age), each = n_eth)[i]
      eth_j <- rep(seq_len(n_eth), n_age)[j]
      age_j <- rep(seq_len(n_age), each = n_eth)[j]
      
      # Distribution of ethnicity in participant's age group
      n_pop_from <- n_pop_age_eth_mat[age_i,]
      # Distribution of ethnicity in contact's age group
      n_pop_to <- n_pop_age_eth_mat[age_j,]
      
      ## mat_age_standardised[age_i, age_j] is the average number of contacts 
      ## towards individuals of age_j per individual of age_i
      ## mat_age_standardised[age_i, age_j] * sum(n_pop_from) is the total
      ## number of contacts from individuals of age_i towards individuals of age_j
      n_contact_tot <- mat_age_standardised[age_i, age_j] * sum(n_pop_from)
      ## Now we have to distribute these contacts between participant and 
      ## contact ethnicities 
      
      # mat_eth_per_cap is the per capita number of contact between ethnicities
      # mat_eth_per_cap * n_pop_to is the average number of contacts for a participant
      # mat_eth_per_cap * n_pop_to * n_pop_from is the total number of contacts
      # between ethnicities
      contact_per_eth <-
        mat_eth_per_cap *
        t(matrix(n_pop_to, n_eth, n_eth)) *
        matrix(n_pop_from, n_eth, n_eth)
      # contact_per_eth / sum(contact_per_eth) is the distribution of contacts
      # in the population. Therefore, prop_contact["Asian", "Asian"] = .1 would 
      # mean that 10% of all contacts in this population are between individuals of 
      # Asian ethnicity
      prop_contact <- contact_per_eth[eth_i, eth_j] / sum(contact_per_eth)
      
      # n_contact_tot * prop_contact[] is the total number of contacts between 
      # age_i and age_j according to RECONNECT distributed among all ethnicities
      dist_contact <- n_contact_tot * prop_contact
      
      # dist_contact is divided by the number of inhabitants in eth_i to get
      # the number of contact per individual.
      mat_age_eth[i, j] <- dist_contact / n_pop_from[eth_i]
      
    }
  }
  
  # Standardise the resulting contact matrix
  mat_age_eth_standard <- 
    .5 * (mat_age_eth * c(t(n_pop_age_eth_mat)) + 
            t(mat_age_eth * c(t(n_pop_age_eth_mat)))) / 
    c(t(n_pop_age_eth_mat))
  
  ## Then, compute the distribution of contacts for each strata
  # ie: X11% of all contacts from "age1eth1" go to "age1eth1",
  #     X12% of all contacts from "age1eth1" go to "age1eth2" etc...
  for(i in seq_len(nrow(mat_age_eth_standard))){
    for(j in seq_len(ncol(mat_age_eth_standard))){
      mat_prop_age_eth[i, ] <- mat_age_eth_standard[i,] / sum(mat_age_eth_standard[i,])
    }
  }
  
  ## Set column and row names using age group and ethnicity
  colnames(mat_prop_age_eth) <- rownames(mat_prop_age_eth) <- 
    paste0("eth", rep(seq_len(n_eth), n_age), 
           "age", rep(seq_len(n_age), each = n_eth))
  
  
  # CHECK: all row sums of prop_indiv_age_eth_group should be 1
  if(!all(abs(rowSums(prop_indiv_age_eth_group) - 1) < .001)) 
    stop("all row sum of prop_indiv_age_eth_group should be 1")
  
  ## Compute the transmission matrix stratified by age group / ethnicity / transmitter group
  ## by combining: 
  ## 1- the distribution of contacts between age group & ethnicity (mat_prop_age_eth),
  ## 2- the total number of contacts at each level of age group, ethnicity and 
  ##    transmitter group (n_contact_age_eth_group),
  ## 3- the proportion of individuals of an age group and ethnicity that belong 
  ##    to each transmitter group (prop_indiv_age_eth_group)
  mat_contact_age_eth_group <-
    matrix(nrow = n_age * n_eth * n_group, ncol = n_age * n_eth * n_group)

  for(i in seq_len(nrow(mat_contact_age_eth_group))){
    # Extract the participant's age and ethnicity
    ageeth_i <- rep(seq_len(n_eth * n_age), n_group)[i]
    # Extract the participant's transmitter group
    group_i <- rep(seq_len(n_group), each = n_eth * n_age)[i]
    
    ## Extract the distribution of contacts across age and ethnicity given the 
    ## participants' age and ethnicity
    prop_age_eth_i <- mat_prop_age_eth[ageeth_i, ]
    
    ## Integrate different clustering in high-contact individuals
    if(group_i == 3){
      ethnicity_i <- substr(rownames(mat_prop_age_eth)[ageeth_i], 1, 4)
      prop_age_eth_i[grep(ethnicity_i, names(prop_age_eth_i))] <- 
        prop_age_eth_i[grep(ethnicity_i, names(prop_age_eth_i))] * k
      prop_age_eth_i <- prop_age_eth_i/sum(prop_age_eth_i)
    }

    for(j in seq_len(ncol(mat_contact_age_eth_group))){
      # Extract the contact's age and ethnicity
      ageeth_j <- rep(seq_len(n_eth * n_age), n_group)[j]
      # Extract the contact's transmitter group
      group_j <- rep(seq_len(n_group), each = n_eth * n_age)[j]
    
      # The average number of contacts from {ageeth_i, group_i} towards {ageeth_j}
      # is computed by multiplying the number of contacts from {ageeth_i, group_i}
      # by the distribution of contacts from {ageeth_i, group_i}.
      tot_contact <- 
        n_contact_age_eth_group[ageeth_i, group_i] * 
        prop_age_eth_i[ageeth_j]
      
      ## We now need to distribute these contacts in the contact's transmitter 
      ## group:
      # Distribution of contacts in {age_eth_j} among transmitter group = 
      #   distribution of number of contacts from ageeth_j among transmitter groups * 
      #   proportion of ageeth_j that belongs to group_j
      dist_contact <- 
        n_contact_age_eth_group[ageeth_j, ] / sum(n_contact_age_eth_group[ageeth_j, ]) *
        prop_indiv_age_eth_group[ageeth_j, ]
      ## Normalise dist_contact
      dist_contact <- dist_contact[group_j] / sum(dist_contact)
      ## Multiply the number of contact from {ageeth_i, group_i} to {ageeth_j} by
      ## the proportion of contacts in {ageeth_j} that are directed at {group_j}
      mat_contact_age_eth_group[i,j] <- tot_contact * dist_contact
    }
  }
  
  ## Set column and row names of mat_contact_age_eth_group using age group, 
  ## ethnicity, and transmitter group
  colnames(mat_contact_age_eth_group) <- rownames(mat_contact_age_eth_group) <- 
    paste0(rep(rownames(mat_prop_age_eth), n_group), 
           "group", rep(seq_len(n_group), each = n_eth * n_age))
  
  ## CHECK: The number of contacts for each strata of mat_contact_age_eth_group 
  ## should be equal to n_contact_age_eth_group for this strata (the previous 
  ## section just "distributed" these contacts among all age groups, ethnicities 
  ## and transmitter groups)
  if(any(abs(rowSums(mat_contact_age_eth_group) - c(n_contact_age_eth_group)) > .01)) stop()
  
  ## Compute the number of individuals in each strata of age group / ethnicity 
  ## / transmitter group
  n_indiv_age_eth_group <- 
    round(c(prop_indiv_age_eth_group * c(t(n_pop_age_eth_mat))))
  
  ## if all_same is true, then the model is homogeneous: all strata have the 
  ## same number of contact and contact pattern, taken from mean_contact
  if(all_same) {
    ## Compute the mean number of contacts per individual in mat_contact_age_eth_group
    mean_contact <- sum(rowSums(mat_contact_age_eth_group) * n_indiv_age_eth_group / tot_pop)
    
    mat_contact_age_eth_group <- 
      outer(rep(mean_contact, n_eth * n_age * n_group), n_indiv_age_eth_group / tot_pop)
  }
  
  
  ## Standardise mat_contact_age_eth_group
  mat_contact_age_eth_group_standard <- 
    .5 * (mat_contact_age_eth_group * n_indiv_age_eth_group + 
            t(mat_contact_age_eth_group * n_indiv_age_eth_group)) / 
    n_indiv_age_eth_group
  
  
  ## Compute the per capita matrix
  mat_contact_age_eth_group_per_cap <- 
    t(t(mat_contact_age_eth_group_standard) / n_indiv_age_eth_group)
  
  ## Remove empty groups (i.e. transmitter groups with no individual)
  empty_groups <- which(
    colSums(is.na(mat_contact_age_eth_group_per_cap)) == n_age * n_eth * n_group
  )
  if(length(empty_groups) > 0){
    mat_contact_age_eth_group_per_cap <- 
      mat_contact_age_eth_group_per_cap[-empty_groups, -empty_groups]
  }
  
  return(mat_contact_age_eth_group_per_cap) 
}

#' Run the SEIR model, compute the number and proportion of infected per ethnicity
#' in each simulation.
#'
#' @param label Label of the simulation run
#' @param ... Parameters relevant to run_outbreaks
#'
#' @return Data frame containing one row by ethnicity and iteration, and four
#' columns: "ethnicity", "type" (which corresponds to the argument `label`),
#' "proportion" the proportion of individuals of the ethnicity infected over the
#' course of this iteration, and "n" the number of individuals of the ethnicity
#'  infected over the course of this iteration
run_and_aggreg_outbreak <- function(label, ...){
  ## Run the stochastic simulations. y_run is a list of named arrays containing 
  ## the number of individuals in each state, simulation, strata, and at each 
  ## time step (output from function_run_simulations).
  y_run <- run_outbreaks(...)
  
  ## Group by ethnicity
  groups <- c("eth1", "eth2", "eth3", "eth4", "eth5") 
  names(groups) <- c("Asian", "Black", "Mixed", "Other", "White")
  
  ## Line of code to ensure the function works if there's only 1 particle
  if(is.matrix(y_run[[1]])) 
    y_run <- lapply(y_run, function(x) 
      return(array(x, dim = c(nrow(x), 1, ncol(x)))))
  
  ## Number of inhabitants per strata
  n_pop <- (y_run$S + y_run$E + y_run$I + y_run$R)[,1,1]
  
  ## Compute the total number of individuals infected, by ethnicity
  df_prop_eth <- data.frame()
  
  # Extract the number of time step
  n_time <- dim(y_run$S)[3]
  
  # For each ethnicity:
  for(i in seq_len(length(groups))){
    # extract the rows of y_run$S that correspond to the current ethnicity
    which_rows_i <- which(grepl(groups[i], rownames(y_run$S)))
    
    # The number of infected throughout the course of the outbreak is 
    # the number of individuals in S at t=0 - number of individuals in S at the 
    # last time step
    n_infected <- 
      if(length(which_rows_i) > 1) { 
        (y_run$S[which_rows_i,, 1] + y_run$E[which_rows_i,, 1] -
           y_run$S[which_rows_i,, n_time]) |> colSums()
      } else {
        (y_run$S[which_rows_i,, 1] + y_run$E[which_rows_i,, 1] -
           y_run$S[which_rows_i,, n_time])
      }
    # The proportion of infection is the number of infected divided by the 
    # number of inhabitants of ith ethnicity
    prop_infected <- n_infected / sum(n_pop[which_rows_i])
    
    # Merge the current ethnicity, the label of this run, the proportion and 
    # the number of infected into a dataframe df_i
    df_i <- cbind.data.frame(ethnicity = names(groups)[i], 
                             type = label, 
                             proportion = prop_infected,
                             n = n_infected)
    df_prop_eth <- rbind.data.frame(df_prop_eth, df_i)
  }
  
  return(df_prop_eth)
}
