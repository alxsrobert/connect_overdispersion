source("R/library_and_scripts.R")
anon_sim <- FALSE

### Import outputs and datasets
## regression model of interest
file_regression_sim <- paste0("results/final_regression.RDS")
model_regression_sim <- readRDS(file_regression_sim)
## list of regression models
all_models_sim <- readRDS("results/all_regression_mix_nb.RDS")
## Model comparison
compar_model_sim <- readRDS("loo_results/model_comparison.rds")

## Simulation distribution of contact
contact_dist_sim <- readRDS(paste0("results/contact_distribution_synthetic", 
                               if(anon_sim) "_anoun", ".RDS"))
## Simulation outputs with 3 transmitter groups
y_result_sim <- readRDS("results/outputmodel_byr0_region_4.RDS") |> 
  mutate(r0 = as.numeric(gsub(".*[_]", "", type)),
         type = gsub("[_].*", "", type)
  )
## Simulation outputs with r0 and proportion infected per value of beta
df_prop_r0_per_region_sim <- 
  readRDS("results/prop_and_r0_clust_region_4.RDS") |> 
  filter(!is.nan(prop) & prop > 0.001) |> 
  mutate(type = relevel(factor(type), ref = "England"))

df_prop_r0_per_region_eth_sim <- 
  readRDS("results/prop_and_r0_clust_region_byeth_4.RDS") |> 
  mutate(beta = gsub(".*_", "", type),
         type = gsub("_.*", "", type)) |> 
  filter(!is.nan(proportion) & proportion > 0.001) |> 
  mutate(type = relevel(factor(type), ref = "England"))


## set colour sets
cols_ethnicity_sim <- c("#002973", "#ffdd00", "#d53880", "#525252", "#d9d9d9")
cols_age_sim <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")
cols_contact_sim <- c("#1b9e77", "#d95f02", "#c2a5cf", "#7b3294")
cols_region_sim <- c("black","#b94b46","#8750a6","#71a44c","#b74d86","#be8e3a","#6780d8")

generate_all_figures(
  anon = anon_sim, file_regression = file_regression_sim, 
  model_regression = model_regression_sim, contact_dist = contact_dist_sim, 
  y_result = y_result_sim, all_models = all_models_sim, 
  compar_model = compar_model_sim, 
  df_prop_r0_per_region_eth = df_prop_r0_per_region_eth_sim,
  df_prop_r0_per_region = df_prop_r0_per_region_sim, 
  cols_ethnicity = cols_ethnicity_sim, cols_age = cols_age_sim, 
  cols_contact = cols_contact_sim, cols_region = cols_region_sim
)
