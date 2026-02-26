anonymised <- FALSE
source("R/library_and_scripts.R")

which_model <- "full_od_cathh"

model_sim <- readRDS(paste0(
  "results/regression_output", if(anonymised) "_anoun", ".rds"))[[which_model]]

## Define the size of the simulated population and the number of draws from the
## regression outputs
pop_size <- 20000
n_draws <- 5
ethnicity <- c("Asian_Urban", "Black_Urban", "Mixed_Urban", "Other_Urban", 
               "White_Urban")
region_sim <- "England"
n_run <- 50
prediction_pop <- data.frame()
for(i in seq_len(n_run)){
  ## Generate stochastic distribution of contacts in simulated populations
  prediction_pop <- rbind.data.frame(
    prediction_pop, 
    # At a given level
    create_contact_in_pop(model = model_sim, tot_pop_size = pop_size, 
                          n_draws = n_draws, which_type =  "at baseline", 
                          vec_ethnicity_rural = ethnicity, region = region_sim) |> 
      select(ethnicity_rural, contact, type) |> mutate(iter = i), 
    # In a simulated population (age distribution from UK population data,
    # income distribution by age from participant data)
    create_contact_in_pop(model = model_sim, tot_pop_size = pop_size, 
                          n_draws = n_draws, which_type = "population", 
                          vec_ethnicity_rural = ethnicity, region = region_sim) |>
      select(ethnicity_rural, contact, type) |> mutate(iter = i),
    # In a simulated population (age distribution from UK population data by ethnicity, 
    # income distribution by age and ethnicity from participant data)
    create_contact_in_pop(model = model_sim, tot_pop_size = pop_size, n_draws = n_draws,  
                          which_type = "ethnicity-stratified\n population", 
                          vec_ethnicity_rural = ethnicity, region = region_sim) |>
      select(ethnicity_rural, contact, type) |> mutate(iter = i)
  )
}

saveRDS(prediction_pop, paste0("result/contact_distribution_synthetic", 
                               if(anonymised) "_anoun", ".RDS"))
