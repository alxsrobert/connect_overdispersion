source("R/library_and_scripts.R")
anon_sim <- FALSE

### Import outputs and datasets
## list of regression models
file_regression_sim <- paste0(
  "results/regression_output", if(anon_sim) "_anoun", ".rds")
list_models_sim <- readRDS(file_regression_sim)
## regression model of interest
which_model_plot_sim <- "full_od_cathh"
model_regression_sim <- list_models_sim[[which_model_plot_sim]]
## Simulation distribution of contact
contact_dist_sim <- readRDS(paste0("results/contact_distribution_synthetic", 
                               if(anon_sim) "_anoun", ".RDS"))
## Simulation outputs with 3 transmitter groups
y_result_sim <- readRDS("results/outputmodel_byr0_region.RDS") |> 
  mutate(r0 = as.numeric(gsub(".*[_]", "", type)),
         type = gsub("[_].*", "", type)
  )
## Simulation outputs with 4 transmitter groups
y_result4_sim <- readRDS("results/outputmodel_byr0_region_4.RDS") |> 
  mutate(r0 = as.numeric(gsub(".*[_]", "", type)),
         type = gsub("[_].*", "", type)
  )
## Simulation outputs with r0 and proportion infected per value of beta
df_prop_r0_per_region_sim <- 
  readRDS("results/prop_and_r0_clust_region.RDS") |> 
  filter(!is.nan(prop) & prop > 0.001) |> 
  mutate(type = relevel(factor(type), ref = "England"))

## set colour sets
cols_ethnicity_sim <- c("#002973", "#ffdd00", "#d53880", "black", "#afb2b4")
cols_age_sim <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")
cols_contact_sim <- c("#1b9e77", "#d95f02", "#7570b3")
cols_region_sim <- c("black","#b94b46","#8750a6","#71a44c","#b74d86","#be8e3a","#6780d8")

generate_all_figures(
  anon = anon_sim, file_regression = file_regression_sim, 
  list_models = list_models_sim, which_model_plot = which_model_plot_sim, 
  model_regression = model_regression_sim, contact_dist = contact_dist_sim, 
  y_result = y_result_sim, y_result4 = y_result4_sim, 
  df_prop_r0_per_region = df_prop_r0_per_region_sim, 
  cols_ethnicity = cols_ethnicity_sim, cols_age = cols_age_sim, 
  cols_contact = cols_contact_sim, cols_region = cols_region_sim
)
