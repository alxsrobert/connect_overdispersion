anonymised <- TRUE
source("R/library_and_scripts.R")

## Load regression outputs
list_with_inc <- readRDS(paste0(
  "results/regression_output", if(anonymised) "_anoun", ".rds"))
## Compare parameter estimates in the different models
figure_compare_models(list_with_inc)

## Show the parameter estimates for the full model
figure_parameter_model(list_with_inc, which_model)


