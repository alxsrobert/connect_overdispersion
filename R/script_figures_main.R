# RUN USING "R/script_generate_all_figures.R

#### Figure 2 ####

# Initialise figure
figure2 <- list()

theme_set()
## Generate forest plot
figure2 <- figure_forest_plot(list_models, which_model_plot)

pdf(file = "figures/figure2.pdf", useDingbats = TRUE, width = 8, height = 8)
print(figure2)
dev.off()

#### Figure 3 ####

# Initialise figure
figure3 <- list()

## Clean outbreak simulations
y_figure3 <- 
  y_result |> 
  filter(!grepl("0.035", type), ## Remove beta runs,
         paste0(type, r0, iter) %in% ## Filter out outbreaks that immediately died out
           (y_result |> filter(!grepl("0.035", type)) |>  ## Remove beta runs
           group_by(type, r0, iter) |> 
           summarise(n_tot = sum(n), .groups = "drop") |> 
           mutate(id = paste0(type, r0, iter)) |> 
           filter(n_tot > 500) |> 
           pull(id)),
         type %in% c("England", "samecoef", "samepop","samepopsamemix") ## select scenarios of interest
  ) |> ## Rename scenarios
  mutate(scenario = factor(case_when(
    type == "samecoef" ~ "Ethnicity-related regression\ncoefficients set to 1",
    type == "samemix" ~ "Homogeneous mixing\nbetween ethnicities",
    type == "samepop" ~ 
      "Same distribution of\ndemographic variables\nacross ethnicities",
    type == "samepopsamemix" ~ 
      "Homogeneous mixing \nacross ethnicities &\nSame demographic variables",
    type == "England" ~ "Representative of\nEngland census",
    .default = type
  ), levels = c("Representative of\nEngland census", 
                "Ethnicity-related regression\ncoefficients set to 1",
                "Homogeneous mixing\nbetween ethnicities",
                "Same distribution of\ndemographic variables\nacross ethnicities",
                "Homogeneous mixing \nacross ethnicities &\nSame demographic variables"
                )))

## Generate attack rate and relative attack rate
figure3[[1]] <-
  y_figure3 |> 
  filter(r0 > 0.035) |> 
  ## Compute relative attack rate
  left_join( # extract proportion infected in White population
    y_figure3 |> 
      filter(ethnicity == "White") |> mutate(prop_ref = proportion) |> 
      select(r0, scenario, iter, prop_ref), by = c("iter", "r0", "scenario")
  ) |> 
  mutate(relative_ratio = proportion / prop_ref) |> 
  ## Compute median and 95% Simulation intervals attack rates
  group_by(ethnicity, r0, scenario) |> 
  summarise(median_relative_ratio = median(relative_ratio),
            low_relative_ratio = quantile(relative_ratio, 0.025),
            high_relative_ratio = quantile(relative_ratio, 0.975),
            median_proportion = median(proportion),
            low_proportion = quantile(proportion, 0.25),
            high_proportion = quantile(proportion, 0.75), 
            .groups = "drop"
  ) |> 
  ## Create median, low and high columns
  pivot_longer(cols = c(median_relative_ratio, low_relative_ratio, high_relative_ratio, 
                        median_proportion, low_proportion, high_proportion),
               names_to = c(".value", "plot"), names_pattern = "([a-z]+)_(.*)") |> 
  ## Rename plots as attack rate and relative attack rate
  mutate(plot = case_when(plot == "proportion" ~ "Attack rate",
                          plot == "relative_ratio" ~ "Relative attack rate")) |> 
  ## Generate plot
  ggplot() + 
  geom_ribbon(aes(fill = ethnicity, x = r0, ymin = low, ymax = high, alpha = ethnicity)) + 
  scale_alpha_manual(values = c(.7, .5, .3, .5, 1)) +
  geom_line(aes(col = ethnicity, x = r0, y = median)) + 
  scale_fill_manual(values = cols_ethnicity) + scale_color_manual(values = cols_ethnicity) + 
  geom_line(aes(x = r0), y = 1, lty = 2, col = "Black") +
  facet_grid(plot ~ scenario, scales = "free", switch = "y") + xlab("R0") + 
  labs(tag = "A") + theme_bw() +
  theme(axis.text = element_text(size=16), strip.text = element_text(size = 14),
        strip.placement = "outside", strip.background.y = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_text(size=14,face="bold"),
        legend.text = element_text(size = 12), legend.title = element_blank(),
        strip.background = element_blank(), legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 20), 
        plot.tag.position = c(0,1))

## Figure comparing the total population infected in scenarios with beta and R0
figure3[[2]] <- 
  y_figure3 |> 
  filter(r0 == 4 | r0 < 0.04) |> 
  mutate(
    tot = n / proportion, ## Compute the total number of individuals per ethnicity
    which = case_when(r0 == 4 ~ paste0("R0 = ", r0), r0 < 0.04 ~ paste0("\u03B2 = ", r0)),
    ylab = "Attack rate"
  ) |> 
  ## Compute the total proportion of the population infected
  group_by(iter, which, scenario, ylab) |> 
  summarise(tot_cases = sum(n) / sum(tot), .groups = "drop") |> 
  group_by(which, scenario, ylab) |>
  ## Generate the figure
  ggplot(aes(x = tot_cases, y = scenario, fill = which)) + 
  geom_density_ridges(scale = .5, alpha = 0.7, col = NA) +
  scale_fill_manual(values = c("lightblue", "#FF7276")) +
  facet_grid(ylab ~ ., scales = "free", switch = "y") + 
  theme_minimal() + coord_flip() + xlab("") + ylab("") + labs(tag = "B") +
  theme(axis.text=element_text(size=14), strip.text = element_text(size = 14),
        strip.placement = "outside", axis.title=element_blank(), 
        legend.text = element_text(size = 12), legend.title=element_blank(), 
        legend.position = "top", plot.tag = element_text(face = "bold", size = 20), 
        plot.tag.position = c(0,1))

pdf(file = "figures/figure3.pdf", useDingbats = TRUE, width = 12, height = 9)
print(figure3[[1]] / (figure3[[2]]) + plot_layout(heights = c(.62, .38)))
dev.off()

#### Figure 4 ####

# Initialise figure
figure4 <- list()

regions <- c("Birmingham", "Leicester", "Liverpool", "London", "Manchester", "York")

## Select simulation sets of interest
y_result_region <- 
  y_result |> 
  filter(r0 > 0.8, is.element(type, regions)) |> 
  mutate(scenario = factor(type, levels = regions))

## Remove simulations where the outbreak immediately died off
y_result_region <- 
  y_result_region |> 
  filter(
    paste0(type, r0, iter) %in% 
      (y_result |> group_by(type, r0, iter) |> summarise(n_tot = sum(n), .groups = "drop") |> 
         mutate(id = paste0(type, r0, iter)) |> filter(n_tot > 500) |> pull(id)))

for(i in seq_along(regions)){
  region_i <- regions[i]
  y_plot_i <- y_result_region |> filter(type == region_i)
  
  ## Create pie chart with the distribution of ethnicity in region_i
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
    scale_fill_manual(values = cols_ethnicity) + scale_color_manual(values = cols_ethnicity) + 
    coord_polar("y", start=0) + theme_void() + theme(legend.position = "none")
  grob_i <- ggplotGrob(p_piepop_i)
  
  ## Compute and plot the relative attack rate
  p_relative_ar_i <- y_plot_i |> 
    left_join(
      y_plot_i |> filter(ethnicity == "White") |> mutate(prop_ref = proportion) |> 
        select(r0, scenario, iter, prop_ref), by = c("iter", "r0", "scenario")
    ) |> 
    mutate(relative_ratio = proportion / prop_ref) |> 
    group_by(ethnicity, r0, scenario) |> 
    summarise(median = median(relative_ratio),
              low_95 = quantile(relative_ratio, 0.025),
              high_95 = quantile(relative_ratio, 0.975), 
              .groups = "drop"
    ) |> 
    ggplot(aes(fill = ethnicity, x = r0, ymin = low_95, ymax = high_95)) + 
    geom_ribbon(aes(alpha = ethnicity)) + 
    scale_alpha_manual(values = c(.7, .5, .3, .5, 1)) +
    geom_line(aes(col = ethnicity, x = r0, y = median)) + 
    geom_line(aes(x = r0), y = 1, lty = 2, col = "Black") +
    scale_fill_manual(values = cols_ethnicity) + 
    scale_color_manual(values = cols_ethnicity) + 
    facet_wrap(.~scenario) + xlab("R0") + 
    guides(fill = guide_legend(override.aes = list(alpha = .7)),
           col = "none") + 
    ylab("Relative attack rate\n") + theme_bw() + ylim(0.8, 2.1) + 
    theme(axis.text=element_text(size=16),
          strip.text.x = element_text(size = 14),
          axis.title=element_text(size=14,face="bold"), 
          legend.text = element_text(size = 12),
          legend.title=element_blank(), 
          strip.background = element_blank())
  ## Add this panel to figure4
  figure4[[i]] <- 
    p_relative_ar_i +
    annotation_custom(grob = grob_i, xmin = 5, xmax = 8, ymin = 1.3, ymax = 2)
}

pdf(file = "figures/figure4.pdf", useDingbats = TRUE, width = 11, height = 7)
print(
  figure4[[1]] + figure4[[2]] + figure4[[3]] + figure4[[4]] + figure4[[5]] + figure4[[6]] + 
    plot_layout(axis_titles = "collect", guides = "collect", axes = "collect")
)
dev.off()
#### Figure 5 ####

# Initialise figure
figure5 <- list()

## Set ggplot theme figure 5
theme_set(
  theme_bw() +
    theme(axis.text=element_text(size=16),
          legend.title=element_blank(),
          strip.text.x = element_text(size = 14),
          axis.title=element_text(size=14,face="bold"), 
          legend.text = element_text(size = 12),
          legend.position = "top",
          strip.background = element_blank(),
          plot.tag = element_text(face = "bold", size = 20),
          plot.tag.position = c(0.1,1)
    )
)


## Panel A: r0 as a function of beta
figure5[[1]] <- 
  df_prop_r0_per_region |> 
  group_by(type, beta) |> 
  ## Compute 95% SI and median
  summarise(med = median(r0), low_95 = quantile(r0, 0.025), 
            hi_95 = quantile(r0, 0.975), .groups = "drop") |> 
  ggplot(aes(x = beta, y = med, col = type, fill = type, ymin = low_95, ymax = hi_95)) + 
  geom_point() + geom_line() + geom_ribbon(col = NA, alpha = 0.2) + 
  scale_color_manual(values = cols_region) + scale_fill_manual(values = cols_region) +
  guides(colour = guide_legend(nrow = 2)) + xlab("beta") + ylab("R0") +
  labs(tag = "A")

## Panel B Proportion by city for several values of beta
figure5[[2]] <- 
  df_prop_r0_per_region |> 
  filter(r0 < 7) |> 
  ## Compute minimum and maximum value of R0 for each beta
  group_by(beta) |> 
  mutate(min_r0 = round(min(r0), 1), max_r0 = round(max(r0), 1)) |> 
  group_by(type, beta, min_r0, max_r0) |> 
  ## Compute median and 95% SI of the attack rate
  summarise(med = median(prop),
            low_95 = quantile(prop, 0.025),
            hi_95 = quantile(prop, 0.975), .groups = "drop") |> 
  mutate(beta = paste0('beta *" = ', beta, ' (R0 between ', min_r0, ' and ', 
                       max_r0, ')"'),
         beta = factor(beta), beta = fct_rev(beta),
         ## Set y range for each value of beta
         ymin = case_when(grepl(0.052, beta) ~ 0.655,
                          grepl(0.039, beta) ~ 0.575,
                          grepl(0.026, beta) ~ 0.445,
                          grepl(0.013, beta) ~ 0.17),
         ymax = case_when(grepl(0.052, beta) ~ 0.695,
                          grepl(0.039, beta) ~ 0.615,
                          grepl(0.026, beta) ~ 0.485,
                          grepl(0.013, beta) ~ 0.210),
         ) |> 
  ## Generate figure
  ggplot(aes(x = type, y = med, col = type, ymin = low_95, ymax = hi_95)) + 
  geom_pointrange(lwd = 1.5) + geom_point() + 
  scale_color_manual(values = cols_region) +
  facet_wrap(~beta, scales = "free_y", ncol = 1, labeller = label_parsed) +
  geom_blank(aes(y = ymin)) + geom_blank(aes(y = ymax)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + xlab("") + ylab("Attack rate") + 
  theme(strip.text.x = element_text(size = 14, hjust = 0), legend.position = "none", 
        plot.margin = unit(c(0,0,0,.5), "cm")) +
  labs(tag = "B")

## Panel C: proportion change in R0 per beta
figure5[[3]] <- 
  df_prop_r0_per_region |> 
  ## Divide r0 by the value observed in England
  left_join(df_prop_r0_per_region |> filter(type == "England") |> 
              rename(ref = r0) |> select(-type, -prop), by = c("beta", "iter")) |> 
  mutate(percent_change = r0/ref - 1) |> 
  filter(type != "England", !is.na(percent_change)) |> 
  group_by(type) |> 
  summarise(med = median(percent_change),
            low_95 = quantile(percent_change, 0.025),
            hi_95 = quantile(percent_change, 0.975), .groups = "drop") |> 
  ggplot(aes(x = type, y = med, col = type, ymin = low_95, ymax = hi_95)) + 
  geom_pointrange(lwd = 1.5) +
  geom_point() + 
  scale_color_manual(values = cols_region[-1]) +
  geom_hline(yintercept = 0, lty = 2, col = "Black") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  theme(legend.position = "none") +
  xlab("") + ylab("Proportion change in R0 across\nall betas compared to England") +
  labs(tag = "C")

pdf(file = "figures/figure5.pdf", useDingbats = TRUE, width = 12, height = 9)
print(
  free(figure5[[1]] / figure5[[3]] + plot_layout(heights = c(.67,.33))) + 
  figure5[[2]] + plot_layout()
)
dev.off()
