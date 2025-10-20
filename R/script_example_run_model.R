source("R/library_and_scripts.R")

#### Run outbreaks ####

n_group <- 3 # Define the number of transmitter groups (by default we use 3)
t_sim <- seq(0, 700) # Time  frame of the stochastic simulations
nsim <- 50

# Generate one set of 50 simulations, with r0 = 1.4
y_per_t <- run_outbreaks(n_group = n_group, t = t_sim, r0 = 1.4, region = "England",
                        n_particles = nsim, each = 200
)

## Compute aggregate number of cases across ethnicites at r0 = 2, 3, 4, and 5
vec_ethnicity <- 
  c("Asian_Urban", "Black_Urban", "Mixed_Urban", "Other_Urban", "White_Urban")
y_result <- data.frame()
for(ro_i in c(2, 3, 4, 5)){
  y_aggreg <- run_and_aggreg_outbreak(
    label = paste0("example_", ro_i), n_group = n_group, t = t_sim, r0 = ro_i, 
    region = "England", n_particles = nsim, each = 200
  ) |>
    mutate(iter = rep(seq(1, nsim), length(vec_ethnicity)))
  ## Add the number and proportion of infected by ethnicity to the summary
  ## data frame
  y_result <- rbind.data.frame(y_result, y_aggreg)
}

y_result <- 
  y_result |> 
  mutate(r0 = as.numeric(gsub(".*[_]", "", type)),
         type = gsub("[_].*", "", type)
  )

y_result <- 
  y_result |> 
  filter(paste0(type, r0, iter) %in% 
           (y_result |> 
              group_by(type, r0, iter) |> 
              summarise(n_tot = sum(n)) |> 
              mutate(id = paste0(type, r0, iter)) |> 
              filter(n_tot > 500))$id)


#### Figures: trajectories per group ####

## Trajectories by transmitter group
groups_contact <- list(
  "low transmitter" = which(grepl("group1", rownames(y_per_t$S))),
  "medium transmitter" = which(grepl("group2", rownames(y_per_t$S))),
  "high transmitter" = which(grepl("group3", rownames(y_per_t$S)))
)
if(n_group >= 4){
  groups_contact <- 
    append(groups_contact, 
           list("highest" = which(grepl("group4", rownames(y_per_t$S)))))
} 
if(n_group >= 5){
  groups_contact <- 
    append(groups_contact, 
           list("higher than the highest" = which(grepl("group5", rownames(y_per_t$S)))))
}
figure_plot_simulations(y = y_per_t, cols = c("#1b9e77", "#d95f02", "#7570b3", "grey", "black"),
                        t = t_sim, groups = groups_contact,
                        ymax_n_i = 100000,
                        ymax_prop_i = 0.01,
                        verbose = FALSE)
title("Transmitter group", outer = TRUE, line = -2, cex.main = 2)


## Trajectories by ethnicity
groups_eth <- list(
  which(grepl("eth1", rownames(y_per_t$S))),
  which(grepl("eth2", rownames(y_per_t$S))),
  which(grepl("eth3", rownames(y_per_t$S))),
  which(grepl("eth4", rownames(y_per_t$S))),
  which(grepl("eth5", rownames(y_per_t$S)))
)
names(groups_eth) <- c("Asian", "Black", "Mixed", "Other", "White")

figure_plot_simulations(y = y_per_t,
                        cols = c("#002973", "#ffdd00", "#d53880", "black", "#afb2b4"),
                        t = t_sim, groups = groups_eth, ymax_n_i = 6e4,
                        ymax_prop_i = 0.003, verbose = TRUE)
title("Ethnicity", outer = TRUE, line = -2, cex.main = 2)

## Trajectories by aggregated age groups
groups_age_aggreg <- list(
  c(which(grepl("age1g", rownames(y_per_t$S))),
    which(grepl("age2g", rownames(y_per_t$S))),
    which(grepl("age3g", rownames(y_per_t$S))),
    which(grepl("age4g", rownames(y_per_t$S)))),
  c(which(grepl("age5g", rownames(y_per_t$S))),
    which(grepl("age6g", rownames(y_per_t$S)))),
  c(which(grepl("age7g", rownames(y_per_t$S))),
    which(grepl("age8g", rownames(y_per_t$S))),
    which(grepl("age9g", rownames(y_per_t$S)))),
  c(which(grepl("age10g", rownames(y_per_t$S))),
    which(grepl("age11g", rownames(y_per_t$S))))
)
names(groups_age_aggreg) <- c("0-17", "18-29", "30-59", "60-93")
figure_plot_simulations(y = y_per_t,
                        cols = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a"),
                        t = t_sim, groups = groups_age_aggreg,
                        ymax_n_i = 5e4, ymax_prop_i = 0.003, verbose = FALSE)
title("Age group aggregated", outer = TRUE, line = -2, cex.main = 2)


#### Figure: Boxplot of the overall number of cases per group ####


figure_plot_total_simulations(y_per_t, 
                              groups_transmission = groups_contact, 
                              groups_age = groups_age_aggreg, 
                              groups_eth = groups_eth, verbose = TRUE)

#### Figure: Infection ratio per group ####

y_result |> 
  ## Compute the relative ratio compared to the incidence in White ethnicity
  left_join(
    y_result |> 
      filter(ethnicity == "White") |> mutate(prop_ref = proportion) |> 
      select(r0, iter, prop_ref)
  ) |> 
  mutate(relative_ratio = proportion / prop_ref) |> 
  ## Compute the median and 95% simulation intervals for relative ratio and proportion
  ## of each population infected
  group_by(ethnicity, r0) |> 
  summarise(median_relative_ratio = median(relative_ratio),
            low_relative_ratio = quantile(relative_ratio, 0.025),
            high_relative_ratio = quantile(relative_ratio, 0.975),
            median_proportion = median(proportion),
            low_proportion = quantile(proportion, 0.25),
            high_proportion = quantile(proportion, 0.75)
  ) |> 
  ## Pivot to a longer format: the 6 columns previously defined are turned into
  ## 4 columns: plot (equal to proportion or relative ratio), median, low, and high
  pivot_longer(cols = c(median_relative_ratio, low_relative_ratio, high_relative_ratio, 
                        median_proportion, low_proportion, high_proportion),
               names_to = c(".value", "plot"), names_pattern = "([a-z]+)_(.*)") |> 
  mutate(plot = case_when(plot == "proportion" ~ "Proportion of the\npopulation infected",
                          plot == "relative_ratio" ~ "Relative attack rate")) |>
  ## Plot the proportion infected and the relative attack rate in each ethnicity
  ggplot() + 
  geom_ribbon(aes(fill = ethnicity, x = r0, ymin = low, ymax = high), 
              alpha = .3) + 
  geom_line(aes(col = ethnicity, x = r0, y = median)) + 
  scale_fill_manual(values = c("#002973", "#ffdd00", "#d53880", "black", "#afb2b4")) + 
  scale_color_manual(values = c("#002973", "#ffdd00", "#d53880", "black", "#afb2b4")) + 
  facet_grid(plot ~ ., scales = "free", switch = "y") + 
  xlab("R0") + ylab("Relative attack rate\n") + theme_bw() + 
  theme(axis.text = element_text(size=16), strip.text = element_text(size = 14),
        strip.placement = "outside", strip.background.y = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_text(size=14,face="bold"), 
        legend.text = element_text(size = 12), legend.title = element_blank())

