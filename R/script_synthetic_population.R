anonymised <- FALSE
source("R/library_and_scripts.R")

model_sim <- readRDS(paste0("results/final_regression.RDS"))
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
    # Only ethnicity-rural impacts the distribution
    create_contact_in_pop(model = model_sim, tot_pop_size = pop_size, 
                          n_draws = n_draws, which_type =  "at baseline", 
                          vec_ethnicity_rural = ethnicity, region = region_sim) |> 
      select(ethnicity_rural, contact, type) |> mutate(iter = i), 
    # In a simulated population (age distribution from UK population data by ethnicity)
    create_contact_in_pop(model = model_sim, tot_pop_size = pop_size, n_draws = n_draws,  
                          which_type = "ethnicity-stratified\n population", 
                          vec_ethnicity_rural = ethnicity, region = region_sim) |>
      select(ethnicity_rural, contact, type) |> mutate(iter = i)
  )
}

saveRDS(prediction_pop, paste0("results/contact_distribution_synthetic", 
                               if(anonymised) "_anoun", ".RDS"))
