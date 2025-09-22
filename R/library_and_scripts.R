library(rio)
library(tidyverse)
library(broom.mixed)
library(brms)
# Install odin2 and dust2 using:
# install.packages(
#   "odin2",
#   repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
library(odin2)
library(dust2)
source("R/function_data_cleaning.R")
source("R/function_clean_pop_data.R")
source("R/function_simulated_pop.R")
source("R/function_process_regression_outputs.R")
source("R/function_figures.R")
source("R/function_seir_model.R")
