source("R/library_and_scripts.R")
cols_ethnicity <- c("#002973", "#ffdd00", "#d53880", "black", "#afb2b4")
cols_region <- c("black","#b94b46","#8750a6","#71a44c","#b74d86","#be8e3a","#6780d8")


#### Visualise the impact of demography and number of contacts ####

## Import the outputs from run_and_aggreg_outbreak()
y_result <- readRDS("results/outputmodel_byr0_region.RDS")

## extract r0 and type
y_result <- 
  y_result |> 
  mutate(r0 = as.numeric(gsub(".*[_]", "", type)),
         type = gsub("[_].*", "", type)
  ) |> 
  filter(r0 > 1)

## select only outbreaks where the number of cases exceeded 500 cases
y_result <- 
  y_result |> 
  filter(paste0(type, r0, iter) %in% 
           (y_result |> 
              group_by(type, r0, iter) |> 
              summarise(n_tot = sum(n), .groups = "drop") |> 
              mutate(id = paste0(type, r0, iter)) |> 
              filter(n_tot > 500))$id)
## Rename the label of the simulations, and select scenarios of interest
y_result_pop <- 
  y_result |> 
  filter(grepl("same", type) | type == "England") |> 
  mutate(scenario = factor(case_when(
    type == "samecoef" ~ "Same coefficients",
    type == "samemix" ~ "Homogeneous mixing",
    type == "samepop" ~ "Same age distribution",
    type == "samepopsamemix" ~ "Homogeneous mixing\nSame age distribution",
    type == "samepopsamemixsamecoef" ~ "Homogeneous mixing\nSame age distribution\nSame coefficients",
    type == "England" ~ "England"
  ), levels = c("England", "Same coefficients", "Homogeneous mixing", 
                "Same age distribution", "Homogeneous mixing\nSame age distribution",
                "Homogeneous mixing\nSame age distribution\nSame coefficients"))) |> 
  filter(scenario %in% c(
    "Same coefficients", "Homogeneous mixing\nSame age distribution",
    "Homogeneous mixing\nSame age distribution\nSame coefficients", "England"
  ))


## Plot the relative ratio and proportion of infected in each type
plot_ratio_incidence <-
  y_result_pop |> 
  ## Compute the relative ratio in each ethnicity compared to White ethnicity
  left_join(
    y_result_pop |> 
      filter(ethnicity == "White") |> mutate(prop_ref = proportion) |> 
      select(r0, scenario, iter, prop_ref)
  ) |> 
  mutate(relative_ratio = proportion / prop_ref) |> 
  ## Compute the median and 95% simulation interval for relative ratio and 
  ## proportion of infected
  group_by(ethnicity, r0, scenario) |> 
  summarise(median_relative_ratio = median(relative_ratio),
            low_relative_ratio = quantile(relative_ratio, 0.025),
            high_relative_ratio = quantile(relative_ratio, 0.975),
            median_proportion = median(proportion),
            low_proportion = quantile(proportion, 0.25),
            high_proportion = quantile(proportion, 0.75), .groups = "drop"
  ) |> 
  ## Pivot to a longer format: the 6 columns previously defined are turned into
  ## 4 columns: plot (equal to proportion or relative ratio), median, low, and high
  pivot_longer(cols = c(median_relative_ratio, low_relative_ratio, high_relative_ratio, 
                        median_proportion, low_proportion, high_proportion),
               names_to = c(".value", "plot"), names_pattern = "([a-z]+)_(.*)") |> 
  mutate(plot = case_when(plot == "proportion" ~ "Proportion of the\npopulation infected",
                          plot == "relative_ratio" ~ "Relative attack rate")) |> 
  ggplot() + 
  geom_ribbon(aes(fill = ethnicity, x = r0, ymin = low, ymax = high), 
              alpha = .3) + 
  geom_line(aes(col = ethnicity, x = r0, y = median)) + 
  scale_fill_manual(values = cols_ethnicity) + 
  scale_color_manual(values = cols_ethnicity) + 
  facet_grid(plot ~ scenario, scales = "free", switch = "y") + 
  xlab("R0") + ylab("") + theme_bw()

proportion_of_cases <- 
  y_result_pop |> 
  filter(r0 %in% c(4)) |> 
  mutate(
    tot = n / proportion,
    r0 = factor(paste0("Overall proportion of the population infected\nR0 = ", r0))) |> 
  group_by(iter, r0, scenario) |> 
  summarise(tot_cases = sum(n) / sum(tot), .groups = "drop") |> 
  group_by(r0, scenario) |>
  ggplot(aes(x = tot_cases, y = scenario)) + 
  geom_density_ridges(scale = .5, alpha = 0.7, fill = "lightblue", col = NA) +
  theme_minimal() +
  coord_flip() +
  facet_grid(factor(r0) ~ ., scales = "free_y", switch = "y") + 
  xlab("") + ylab("") + 
  theme(axis.text=element_text(size=14), strip.text = element_text(size = 14),
        strip.placement = "outside", axis.title=element_blank(), 
        legend.text = element_text(size = 12), legend.title=element_blank())



print(plot_ratio_incidence / proportion_of_cases)

#### Relative ratio by cities ####

y_result <- readRDS("results/outputmodel_byr0_region.RDS")
cities <- c(
  "Birmingham", "Leicester", "Liverpool", "London", "Manchester", "York")

# extract r0 by splitting the column "type"
y_result <- 
  y_result |> 
  mutate(r0 = as.numeric(gsub(".*[_]", "", type)),
         type = gsub("[_].*", "", type)
  )

## select only outbreaks where the number of cases exceeded 500 cases
y_result <- 
  y_result |> 
  filter(paste0(type, r0, iter) %in% 
           (y_result |> 
              group_by(type, r0, iter) |> 
              summarise(n_tot = sum(n), .groups = "drop") |> 
              mutate(id = paste0(type, r0, iter)) |> 
              filter(n_tot > 500))$id)

## Select only simulations comparing the infection rate between cities
y_result_region <- y_result |> 
  filter(is.element(type, cities)) |> 
  mutate(
    scenario = factor(type, levels = cities)
  )

figure_region <- list()
## For each region, plot the relative ratio by ethnicity, and add a pie chart
## showing the distribution of ethnicities in the city
for(i in seq_along(cities)){
  region_i <- cities[i]
  ## Filter the rows from y_result_region that correspond to region_i
  y_plot_i <- y_result_region |> filter(type == region_i)
  
  ## Create the pie chart showing the distribution of ethnicity in the population
  p_piepop_i <- y_plot_i |>
    ## Compute the number of inhabitant per ethnicity
    mutate(pop = n / proportion) |> 
    group_by(iter, r0, scenario) |> 
    mutate(tot_pop = sum(pop)) |> 
    ## Compute the proportion of population by ethnicity
    group_by(ethnicity, scenario) |> 
    summarise(median = median(pop / tot_pop, na.rm = TRUE), .groups = "drop") |> 
    ## Create the pie chart
    ggplot(aes(fill = ethnicity, x = "", y = median, col = ethnicity)) + 
    geom_bar(stat="identity", width=1, alpha = .7) +
    scale_fill_manual(values = cols_ethnicity) + 
    scale_color_manual(values = cols_ethnicity) + 
    coord_polar("y", start=0) + theme_void() + 
    theme(legend.position = "none")
  
  ## Generate the relative ratio by ethnicity
  p_relative_ar_i <- y_plot_i |> 
    left_join(
      y_plot_i |> 
        filter(ethnicity == "White") |> 
        mutate(prop_ref = proportion) |> 
        select(r0, scenario, iter, prop_ref)
    ) |> 
    mutate(relative_ratio = proportion / prop_ref) |> 
    group_by(ethnicity, r0, scenario) |> 
    summarise(median = median(relative_ratio),
              low_95 = quantile(relative_ratio, 0.025),
              high_95 = quantile(relative_ratio, 0.975), .groups = "drop"
    ) |> 
    ggplot(aes(fill = ethnicity, x = r0, ymin = low_95, ymax = high_95)) + 
    geom_ribbon(alpha = .3) + 
    geom_line(aes(col = ethnicity, x = r0, y = median)) + 
    geom_line(aes(x = r0), y = 1, lty = 2, col = "Black") + 
    scale_fill_manual(values = cols_ethnicity) + 
    scale_color_manual(values = cols_ethnicity) + 
    facet_wrap(.~scenario) + xlab("R0") + 
    guides(fill = guide_legend(override.aes = list(alpha = .7)),
           col = "none") + 
    ylab("Relative attack rate\n") + theme_bw() + ylim(0.8, 2) + 
    theme(axis.text=element_text(size=16),
          strip.text.x = element_text(size = 14),
          axis.title=element_text(size=14,face="bold"), 
          legend.text = element_text(size = 12),
          legend.title=element_blank(), 
          strip.background = element_blank())
  
  # Place the distribution of the population as a grob object
  grob_i <- ggplotGrob(p_piepop_i)
  figure_region[[i]] <- 
    p_relative_ar_i +
    annotation_custom(
      grob = grob_i,
      xmin = 5,
      xmax = 8,
      ymin = 1.3,
      ymax = 2
    )
}

## Put all figures together
print(
  figure_region[[1]] + figure_region[[2]] + figure_region[[3]] +
    figure_region[[4]] + figure_region[[5]] + figure_region[[6]] + 
    plot_layout(axis_titles = "collect", guides = "collect", axes = "collect")
)

#### betas and r0 ####

## Extract dataset comparing r0 and beta
df_region <- 
  readRDS("results/prop_and_r0_clust_region.RDS")$df_region |> 
  filter(!is.nan(prop) & prop > 0.001) |> 
  mutate(type = relevel(factor(type), ref = "England"))
## Plot r0 by beta by city
r0_by_beta <- 
  df_region |> 
  ## Compute the median and 95%SI of R0
  group_by(type, beta) |> 
  summarise(med = median(r0), low_95 = quantile(r0, 0.025), 
            hi_95 = quantile(r0, 0.975), .groups = "drop") |> 
  ggplot(aes(x = beta, y = med, col = type, fill = type, ymin = low_95, 
             ymax = hi_95)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(col = NA, alpha = 0.2) + 
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "top") + 
  scale_color_manual(values = cols_region) +
  scale_fill_manual(values = cols_region) +
  guides(colour = guide_legend(nrow = 1)) + 
  xlab("beta") + ylab("R0")
## Proportion of change in R0 between cities
r0_by_region <- 
  df_region |> 
  ## Compare r0 by city to r0 in England
  left_join(df_region |> filter(type == "England") |> 
              rename(ref = r0) |> select(-type, -prop)) |> 
  mutate(percent_change = r0/ref - 1) |> 
  filter(type != "England", !is.na(percent_change)) |> 
  group_by(type) |> 
  ## Compute the median and 95% SI
  summarise(med = median(percent_change),
            low_95 = quantile(percent_change, 0.025),
            hi_95 = quantile(percent_change, 0.975), .groups = "drop") |> 
  ggplot(aes(x = type, y = med, col = type, ymin = low_95, ymax = hi_95)) + 
  geom_point() + 
  geom_pointrange() +
  scale_color_manual(values = cols_region[-1]) +
  geom_hline(yintercept = 0, lty = 2, col = "Black") + 
  theme_bw() + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  theme(legend.position = "none") +
  xlab("") + ylab("Proportion change in R0 across\nall betas compared to England")

## Plot the proportion of the population infected by city
proportion_infected_by_city <- 
  df_region |> 
  filter(r0 < 7) |> 
  group_by(beta) |> 
  group_by(type, beta) |> 
  ## Compute median and 95%SI
  summarise(med = median(prop),
            low_95 = quantile(prop, 0.025),
            hi_95 = quantile(prop, 0.975), .groups = "drop") |> 
  mutate(beta = paste0("beta = ", beta)) |> 
  ggplot(aes(x = type, y = med, col = type, ymin = low_95, ymax = hi_95)) + 
  geom_point() + 
  geom_pointrange() +
  scale_color_manual(values = cols_region) +
  facet_wrap(~beta, scales = "free_y", ncol = 1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  theme_bw() + 
  theme(strip.background = element_blank(), legend.position = "none") +
  xlab("") + ylab("Overall proportion of the population infected")

free(r0_by_beta / r0_by_region) + proportion_infected_by_city

