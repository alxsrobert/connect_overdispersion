source("R/library_and_scripts.R")
anon <- TRUE

### Import outputs and datasets
## list of regression models
file_regression <- paste0(
  "results/regression_output", if(anon) "_anoun", ".rds")
list_models <- readRDS(file_regression)
## regression model of interest
which_model_plot <- "full_od_cathh"
model_regression <- list_models[[which_model_plot]]
## Simulation distribution of contact
contact_dist <- readRDS(paste0("results/contact_distribution_synthetic", 
                               if(anon) "_anoun", ".RDS"))
## Simulation outputs with 3 transmitter groups
y_result <- readRDS("results/outputmodel_byr0_region.RDS") |> 
  mutate(r0 = as.numeric(gsub(".*[_]", "", type)),
         type = gsub("[_].*", "", type)
  )
## Simulation outputs with 4 transmitter groups
y_result4 <- readRDS("results/outputmodel_byr0_region_4.RDS") |> 
  mutate(r0 = as.numeric(gsub(".*[_]", "", type)),
         type = gsub("[_].*", "", type)
  )
## Simulation outputs with r0 and proportion infected per value of beta
df_prop_r0_per_region <- 
  readRDS("results/prop_and_r0_clust_region.RDS")$df_region |> 
  filter(!is.nan(prop) & prop > 0.001) |> 
  mutate(type = relevel(factor(type), ref = "England"))

## set colour sets
cols_ethnicity <- c("#002973", "#ffdd00", "#d53880", "black", "#afb2b4")
cols_age <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")
cols_contact <- c("#1b9e77", "#d95f02", "#7570b3")
cols_region <- c("black","#b94b46","#8750a6","#71a44c","#b74d86","#be8e3a","#6780d8")

source("R/script_figures_main.R")
source("R/script_figures_supplement.R")
