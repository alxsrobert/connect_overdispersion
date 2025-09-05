anonymised <- TRUE
source("R/library_and_scripts.R")

list_with_inc <- readRDS(paste0(
  "results/regression_output", if(anonymised) "_anoun", ".rds"))
which_model <- "full_od_cathh"

cols <- c("#002973", "#ffdd00", "#d53880", "#afb2b4", "#91bfdb")

## Define the size of the simulated population and the number of draws from the
## regression outputs
pop_size <- 20000
n_draws <- 200
ethnicity <- c("Asian_Urban", "Black_Urban", "Mixed_Urban", "White_Urban")

## Generate stochastic distribution of contacts in simulated populations
all_prediction_pop <- rbind.data.frame(
  # At a given level
  create_contact_in_pop(list_with_inc, pop_size, n_draws, which_model, 
                        "at baseline", seed = 1, vec_ethnicity_rural = ethnicity) |> 
    select(ethnicity_rural, contact, type), 
  # In a simulated population (age distribution from UK population data,
  # income distribution by age from participant data)
  create_contact_in_pop(list_with_inc, pop_size, n_draws, which_model, 
                        "population", seed = 1, vec_ethnicity_rural = ethnicity) |>
    select(ethnicity_rural, contact, type),
  # In a simulated population (age distribution from UK population data by ethnicity, 
  # income distribution by age and ethnicity from participant data)
  create_contact_in_pop(list_with_inc, pop_size, n_draws, which_model, 
                        "ethnicity-stratified\n population", seed = 1, 
                        vec_ethnicity_rural = ethnicity) |>
    select(ethnicity_rural, contact, type)
) 

## Histogram: distribution of contact
figure_nb_contact_hist(
  prediction_populations = all_prediction_pop, 
  breaks = c(-Inf, 2, 10, 20, Inf),
  label_breaks = c("<=2", "3-10", "11-20", ">20"), cols = cols)
## Density: distribution of contact
figure_density(prediction_populations = all_prediction_pop, cols = cols)
figure_density(prediction_populations = all_prediction_pop, log = FALSE, 
               vec_xlim = c(0, 50), cols = cols)

## Density: distribution of contact in top 25% by ethnicity
figure_density(prediction_populations = all_prediction_pop, prop_above = .75, 
               cols = cols)

## Density: distribution of contact in top 10% by ethnicity
figure_density(prediction_populations = all_prediction_pop, 
               prop_above =  .9, cols = cols)

