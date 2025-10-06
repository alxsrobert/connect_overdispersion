## Define parameters and initialise result data frames
source("R/library_and_scripts.R")
type <- "short" # c("short", "long")

if(type == "long") {
  # Number of runs
  nb_run <- 50
  # Number of simulations per run (total number of simulations = nb_run * n_particle_sim)
  n_particle_sim <- 20
  # Define the size of the synthetic population used to simulate the number of 
  # contacts per individual by specifying the population size at each level of 
  # age and ethnicity
  each_sim <- 500
} else if (type == "short"){
  # Number of runs
  nb_run <- 10
  # Number of simulations per run (total number of simulations = nb_run * n_particle_sim)
  n_particle_sim <- 5
  # Define the size of the synthetic population used to simulate the number of 
  # contacts per individual by specifying the population size at each level of 
  # age and ethnicity
  each_sim <- 100
}


vec_ethnicity <- 
  c("Asian_Urban", "Black_Urban", "Mixed_Urban", "Other_Urban", "White_Urban")

y_result <- data.frame()
df_tot_clust <- data.frame()
df_tot_region <- data.frame()

#### Output by region + Impact of clustering + Impact of demography / regression ####

## use several runs to integrate the stochasticity associated with generating the  
## synthetic population and the transmitter groups
for(run in seq_len(nb_run)){
  # for each region:
  for(i in c("Birmingham", "Manchester", "York", "Liverpool", "Leicester", 
             "London", "England")){
    ## Create the transmitter groups from synthetic population
    list_prop_coef_sim <- create_contact_group(
      scenario = "reference", n_group = 3, n_draws = 5, region = i, each = each_sim,
      file_in = "results/regression_output.rds", which_model = "full_od_cathh", 
      vec_ethnicity_rural = vec_ethnicity)
    ## Use list_prop_coef_sim to generate n_particle outbreaks at different
    ## values of R0
    for(r0_i in seq(1.6, 8, .2)){
      if(r0_i < 2) {
        t_sim <- seq(0,750) 
      } else if(r0_i < 3){
        t_sim <- seq(0, 500)
      } else if(r0_i < 4){
        t_sim <- seq(0, 365)
      }  else t_sim <- seq(0, 250)
      
      
      y_i <- run_and_aggreg_outbreak(
        label = paste0(i, "_", r0_i), region = i, r0 = r0_i, 
        n_particles = n_particle_sim, t = t_sim, list_prop_coef = list_prop_coef_sim
      ) |>
        mutate(iter = rep((run - 1) * n_particle_sim + 
                            seq(1, n_particle_sim), length(vec_ethnicity)))
      
      ## Add the number and proportion of infected by ethnicity to the summary
      ## data frame
      y_result <- rbind.data.frame(y_result, y_i)
      
      ## If the region is England, we also generate simulations to analyse the 
      ## impact of clustering, and the impact of changing the demographic characteristics
      if(i == "England"){
        y_highclust <- run_and_aggreg_outbreak(
          label = paste0("highclust", "_", r0_i), 
          list_prop_coef = list_prop_coef_sim, region = "England", k = 2, 
          n_particles = n_particle_sim, t = t_sim, r0 = r0_i) |>
          mutate(iter = rep((run - 1) * n_particle_sim + seq(1, n_particle_sim),
                            length(vec_ethnicity)))
        y_lowclust <- run_and_aggreg_outbreak(
          label = paste0("lowclust", "_", r0_i), 
          list_prop_coef = list_prop_coef_sim, region = "England", k = .5, 
          n_particles = n_particle_sim, t = t_sim, r0 = r0_i) |>
          mutate(iter = rep((run - 1) * n_particle_sim + seq(1, n_particle_sim), 
                            length(vec_ethnicity)))
        
        # Samemix: set the per capita matrix by ethnicity as constant.
        y_samemix <- run_and_aggreg_outbreak(
          label = paste0("samemix", "_", r0_i),
          list_prop_coef = list_prop_coef_sim,
          region = "England", k = 1, n_particles = n_particle_sim, t = t_sim,
          r0 = r0_i, all_eth = TRUE) |>
          mutate(iter = rep((run - 1) * n_particle_sim + seq(1, n_particle_sim), 
                            length(vec_ethnicity)))
        # Samecoef: set the coef and prop matrix from list_prop_coef_sim to
        # be the same of all ethnicities
        y_samecoef <- run_and_aggreg_outbreak(
          label = paste0("samecoef", "_", r0_i),
          list_prop_coef = list_prop_coef_sim,
          region = "England", k = 1, n_particles = n_particle_sim, t = t_sim,
          r0 = r0_i, all_same_coef = TRUE) |>
          mutate(iter = rep((run - 1) * n_particle_sim + seq(1, n_particle_sim), 
                            length(vec_ethnicity)))
        # samepop: all ethnicities have the same age distribution
        y_samepop <- run_and_aggreg_outbreak(
          label = paste0("samepop", "_", r0_i),
          list_prop_coef = list_prop_coef_sim,
          region = "England", k = 1, n_particles = n_particle_sim, t = t_sim,
          r0 = r0_i, same_age_distribution = TRUE) |>
          mutate(iter = rep((run - 1) * n_particle_sim + seq(1, n_particle_sim), 
                            length(vec_ethnicity)))
        # samepopsamemix: all ethnicities have the same age distribution and
        # set the per capita matrix by ethnicity as constant.
        y_samepopsamemix <- run_and_aggreg_outbreak(
          label = paste0("samepopsamemix", "_", r0_i),
          list_prop_coef = list_prop_coef_sim,
          region = "England", k = 1, n_particles = n_particle_sim, t = t_sim,
          r0 = r0_i, same_age_distribution = TRUE, all_eth = TRUE) |>
          mutate(iter = rep((run - 1) * n_particle_sim + seq(1, n_particle_sim), 
                            length(vec_ethnicity)))
        # samepopsamemixsamecoef: all ethnicities have the same age distribution,
        # set the per capita matrix by ethnicity as constant, and set the coef and 
        # prop matrix from list_prop_coef_sim to be the same of all ethnicities.
        y_samepopsamemixsamecoef <- run_and_aggreg_outbreak(
          label = paste0("samepopsamemixsamecoef", "_", r0_i),
          list_prop_coef = list_prop_coef_sim,
          region = "England", k = 1, n_particles = n_particle_sim, t = t_sim,
          r0 = r0_i, same_age_distribution = TRUE, all_eth = TRUE, all_same_coef = TRUE) |>
          mutate(iter = rep((run - 1) * n_particle_sim + seq(1, n_particle_sim), length(vec_ethnicity)))
        
        ## Add the number and proportion of infected by ethnicity to the summary
        ## data frame
        y_result <- rbind.data.frame(
          y_result, y_highclust, y_lowclust, y_samemix, y_samecoef, y_samepop, 
          y_samepopsamemix, y_samepopsamemixsamecoef
        )
        gc()
      }
    }
  }
}
## Save y_result
saveRDS(y_result, "results/outputmodel_byr0_region.RDS")

#### Check R0 by beta: by city ####

all_betas <- seq(0.013, 0.08, 0.013)

for(run in seq_len(nb_run)){
  for(i in c("Birmingham", "Manchester", "York", "Liverpool", "Leicester", 
             "London", "England"
  )){
    ## Create the transmitter groups from synthetic population
    list_prop_coef_sim <- create_contact_group(
      scenario = "reference", n_group = 3, n_draws = 5, region = i, each = each_sim,
      file_in = "results/regression_output.rds", which_model = "full_od_cathh", 
      vec_ethnicity_rural = vec_ethnicity)
    for(j in seq_along(all_betas)){
      beta_j <- all_betas[j]
      # Compute r0 from beta_j and list_prop_coef_sim
      r0_ij <- run_outbreaks(
        region = i, beta = beta_j, list_prop_coef = list_prop_coef_sim, 
        return_only_r0 = TRUE)
      
      # Run the stochastic simulations and return the overall proportion of 
      # infected in the population
      prop_cases <- 
        (run_and_aggreg_outbreak(
          label = paste0(i, "_", all_betas[j]),
          region = i, beta = beta_j, n_particles = n_particle_sim, 
          list_prop_coef = list_prop_coef_sim
        ) |>
          mutate(iter = rep((run - 1) * n_particle_sim + 
                              seq(1, n_particle_sim), length(vec_ethnicity)),
                 tot_pop = n / proportion) |> 
          group_by(iter) |> 
          summarise(prop = sum(n) / sum(tot_pop)))$prop
      
      df_i <- cbind.data.frame(
        type = i, r0 = r0_ij, beta = beta_j, prop = prop_cases, 
        iter = (run - 1) * n_particle_sim + seq_len(n_particle_sim))
      df_tot_region <- rbind.data.frame(df_tot_region, df_i)
    }
  }
}

#### Check R0 by beta: impact of the clustering factor ####

all_k <- c(seq(.2, 1, .1), seq(2, 5, .5))
for(run in seq_len(nb_run)){
  list_prop_coef_england <- create_contact_group(
    scenario = "reference", n_group = 3, n_draws = 5, region = "England", each = each_sim, 
    file_in = "results/regression_output.rds", which_model = "full_od_cathh", 
    vec_ethnicity_rural = vec_ethnicity)
  
  for(i in seq_along(all_k)){
    k_i <- all_k[i]
    for(j in seq_along(all_betas)){
      beta_j <- all_betas[j]
      # Compute r0 from beta_j, k_i and list_prop_coef_sim
      r0_ij <- run_outbreaks(
        region = "England", beta = beta_j, list_prop_coef = list_prop_coef_sim, 
        k = k_i, return_only_r0 = TRUE)
      
      # Run the stochastic simulations and return the overall proportion of 
      # infected in the population
      prop_cases <- 
        (run_and_aggreg_outbreak(
          label = paste0(i, "_", all_betas[j]), region = "England", k = k_i, 
          beta = beta_j, n_particles = n_particle_sim, 
          list_prop_coef = list_prop_coef_england
        ) |>
          mutate(iter = rep((run - 1) * n_particle_sim + 
                              seq(1, n_particle_sim), length(vec_ethnicity)),
                 tot_pop = n / proportion) |> 
          group_by(iter) |> 
          summarise(prop = sum(n) / sum(tot_pop)))$prop
      
      df_i <- cbind.data.frame(
        k = k_i, r0 = r0_ij, beta = beta_j, prop = prop_cases, 
        iter = (run - 1) * n_particle_sim + seq_len(n_particle_sim))
      df_tot_clust <- rbind.data.frame(df_tot_clust, df_i)
      
    }
  }
}

saveRDS(list(df_clust = df_tot_clust, df_region = df_tot_region), 
        "results/prop_and_r0_clust_region.RDS")
