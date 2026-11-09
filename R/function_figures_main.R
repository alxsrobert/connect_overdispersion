# RUN USING "R/script_generate_all_figures.R

generate_all_figures <- function(
    anon, file_regression, list_models, which_model_plot, model_regression,
    contact_dist, y_result, y_result4, df_prop_r0_per_region,
    cols_ethnicity, cols_age, cols_contact, cols_region
    ){
  
  ifelse(!dir.exists(file.path(getwd(), "figures")), 
         dir.create(file.path(getwd(), "figures"), showWarnings = FALSE), FALSE)
  
  generate_figure2(list_models, which_model_plot)
  generate_figure3(y_result, cols_ethnicity)
  generate_figure4(y_result, cols_ethnicity)
  generate_figure5(df_prop_r0_per_region, cols_region)
  
  generate_figureS4S9(model_regression)
  generate_figureS11(file_regression, which_model_plot)
  generate_figureS12(list_models)
  generate_figureS13(contact_dist, cols_ethnicity)
  generate_figureS14(y_result, cols_ethnicity)
  generate_figureS15S16(cols_age, cols_ethnicity, cols_contact)
  generate_figureS17(y_result4, cols_ethnicity)
  generate_figureS18S20(cols_ethnicity)
  generate_figureS21S22(y_result, cols_ethnicity)
}

  # Initialise figure
generate_figure2 <- function(model_regression, contact_dist, cols_ethnicity){
  ## First panel: Ethnicity in <18yo
  p1 <- show_regression_params(model_regression, FALSE, FALSE, "ethnicity_rural") + 
    guides(col="none") + 
    theme(strip.background = element_blank(),
          panel.border = element_blank(), 
          strip.text = element_text(size = 12))
  ## Second panel: Ethnicity in >= 18yo
  p2 <- show_regression_params(model_regression, TRUE, FALSE, "ethnicity_rural") + 
    theme(strip.background = element_blank(),
          panel.border = element_blank(), 
          strip.text = element_text(size = 12))
  
  ## Rename covariate label (used as strip title in ggplot)
  p1@data$covariate_label <- 
    p1@layers$geom_rect$data$covariate_label <- 
    p1@layers$geom_text$data$covariate_label <- "Under 18"
  p2@data$covariate_label <- 
    p2@layers$geom_rect$data$covariate_label <- 
    p2@layers$geom_text$data$covariate_label <- "18 and over"
  
  # Theme of the inset plots
  theme_inset <- 
    theme_minimal() + 
    theme(plot.title = element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust=1),
          axis.ticks = element_line(linewidth = 0.25),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.2),
          plot.margin = margin(3, 3, 3, 3),
          strip.text = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA)
    )
  
  # Distribution of contacts in simulated populations (regression coefficients only)
  p3_base <- figure_nb_contact_hist(
    contact_dist |> 
      filter(type == "at baseline") |> 
      mutate(type = "Ethnicity-related regression coefficients only"),
    c(-Inf, 1, 5, 10, 25, Inf), c("0-1", "2-5", "6-10", "11-25", "25+"),
    cols_ethnicity) + 
    ylim(0, 55) + 
    theme_minimal() + 
    theme(panel.grid.minor = element_blank(),
          plot.margin = margin(1, 0, 1, 0),
          strip.text = element_text(size = 12))
  
  # Distribution of contacts in simulated population (regression + pop)
  p3_all <- figure_nb_contact_hist(
    contact_dist |> 
      filter(type == "ethnicity-stratified\n population") |> 
      mutate(type = "Regression coefficients + Demographic structure"),
    c(-Inf, 1, 5, 10, 25, Inf), c("0-1", "2-5", "6-10", "11-25", "25+"),  
    cols_ethnicity) + 
    ylim(0, 55) + 
    theme_minimal() + 
    labs(y = NULL) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(size = 12))
  
  
  # Distribution of high contacts (regression coefficients only)
  p4_base <- dist_simulated_pop(
    contact_dist |> 
      filter(type == "at baseline") |> 
      mutate(type = "Ethnicity-related regression coefficients only"),
    cols_ethnicity, high_cut = 200) + xlim(20, NA) + 
    scale_y_log10(limits = c(0.003, 14)) + 
    labs(y = "Proportion above (%)", x = "Number of contacts") +
    guides(fill = "none", col = "none", alpha = "none") +  
    theme_inset
  
  # Distribution of high contacts (regression + pop)
  p4_all <- dist_simulated_pop(
    contact_dist |> 
      filter(type == "ethnicity-stratified\n population") |> 
      mutate(type = "Regression coefficients + demographic structure"),
    cols_ethnicity, high_cut = 200) + xlim(20, NA) + 
    scale_y_log10(limits = c(0.003, 14)) + 
    labs(y = "Proportion above (%)", x = "Number of contacts") +
    guides(fill = "none", col = "none", alpha = "none") + 
    theme_inset
  
  ## Add insets to histograms
  p_left <- p3_base +
    inset_element(p4_base, left = 0.40, bottom = 0.45, right = 0.98, top = .98,
                  align_to = "panel", on_top = TRUE)
  p_right <- p3_all +
    inset_element(p4_all, left = 0.40, bottom = 0.45, right = 0.98, top = .98,
                  align_to = "panel", on_top = TRUE)
  
  ## combine histograms
  final_plot <- (p_left + p_right) + plot_layout(guides = "collect") & 
    theme(legend.position = "right")
  
  ## Combine all plots
  figure2 <- ((p1 + p2 + plot_layout(axes = "collect")) / 
                (final_plot) + plot_layout(heights = c(.4, .6)))
  
  pdf(file = "figures/figure2.pdf", useDingbats = TRUE, width = 10, height = 8)
  print(figure2)
  dev.off()
}

generate_figure3 <- function(y_result, cols_ethnicity){
  # Initialise figure
  figure3 <- list()
  
  ## Clean outbreak simulations
  y_figure3 <- 
    y_result |> 
    filter(!grepl("0.035", type), ## Remove beta runs,
           paste0(type, r0, iter) %in% ## Filter out outbreaks that immediately died out
             (y_result |> 
                filter(!grepl("0.035", type)) |>  ## Remove beta runs
                group_by(type, r0, iter) |> 
                summarise(n_tot = sum(n), pop_tot = sum(n / proportion),
                          .groups = "drop") |> 
                mutate(prop_tot = n_tot / pop_tot, id = paste0(type, r0, iter)) |> 
                filter(prop_tot > 0.02) |> 
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
    ), levels = c("Homogeneous mixing \nacross ethnicities &\nSame demographic variables",
                  "Same distribution of\ndemographic variables\nacross ethnicities",
                  "Ethnicity-related regression\ncoefficients set to 1",
                  "Representative of\nEngland census", 
                  "Homogeneous mixing\nbetween ethnicities"
    )))
  
  y_figure3 <- rbind.data.frame(
    y_figure3,
    y_figure3 |> 
      mutate(pop = n / proportion) |> 
      group_by(type, iter, r0, scenario) |> 
      summarise(pop = sum(pop), n = sum(n), proportion = n / pop, 
                proportion_standard = n /pop, ethnicity = "All", .groups = "drop_last") |> 
      select(colnames(y_figure3))
  )
  
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
    filter(ethnicity != "White") |> 
    mutate(relative_ratio = case_when(
      ethnicity %in% c("All") ~ NA, 
      .default = proportion / prop_ref)) |> 
    mutate(proportion = case_when(
      ethnicity != "All" ~ NA, 
      .default = proportion)) |> 
    ## Compute median and 95% Simulation intervals attack rates
    group_by(ethnicity, r0, scenario) |> 
    summarise(median_relative_ratio = median(relative_ratio),
              low_relative_ratio = quantile(relative_ratio, 0.025, na.rm = TRUE),
              midlow_relative_ratio = quantile(relative_ratio, 0.25, na.rm = TRUE),
              midhigh_relative_ratio = quantile(relative_ratio, 0.75, na.rm = TRUE),
              high_relative_ratio = quantile(relative_ratio, 0.975, na.rm = TRUE),
              median_proportion = median(proportion),
              low_proportion = quantile(proportion, 0.025, na.rm = TRUE),
              midlow_proportion = quantile(proportion, 0.25, na.rm = TRUE),
              midhigh_proportion = quantile(proportion, 0.75, na.rm = TRUE),
              high_proportion = quantile(proportion, 0.975, na.rm = TRUE),
              .groups = "drop"
    ) |> 
    ## Create median, low and high columns
    pivot_longer(cols = c(median_relative_ratio, low_relative_ratio, high_relative_ratio, 
                          midlow_relative_ratio, midhigh_relative_ratio,
                          median_proportion, low_proportion, high_proportion,
                          midlow_proportion, midhigh_proportion),
                 names_to = c(".value", "plot"), names_pattern = "([a-z]+)_(.*)") |> 
    ## Rename plots as attack rate and relative attack rate
    mutate(plot = case_when(plot == "proportion" ~ "Attack rate",
                            plot == "relative_ratio" ~ "Relative attack rate")) |> 
    ## Generate plot
    ggplot() + 
    geom_ribbon(aes(fill = ethnicity, x = r0, ymin = low, ymax = high, alpha = ethnicity)) + 
    scale_alpha_manual(values = c(.4, .4, .5, .3, .3)) +
    geom_ribbon(aes(fill = ethnicity, x = r0, ymin = midlow, ymax = midhigh, alpha = ethnicity)) + 
    geom_line(aes(col = ethnicity, x = r0, y = median), lwd = 0.8) +
    scale_fill_manual(values = cols_ethnicity) + scale_color_manual(values = cols_ethnicity) + 
    geom_line(aes(x = r0), y = 1, lty = 2, col = "Black") +
    facet_grid(plot ~ scenario, scales = "free", switch = "y") + 
    theme_bw() + xlab(bquote(R[0])) + 
    theme(axis.text = element_text(size=16), 
          strip.text.x = element_blank(),
          strip.text.y = element_text(size = 14), 
          strip.placement = "outside", 
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=14,face="bold"),
          legend.text = element_text(size = 12), 
          legend.title = element_blank(),
          strip.background = element_blank(), 
          legend.position = "bottom",
          plot.tag = element_text(face = "bold", size = 20), 
          panel.grid.minor = element_blank(), 
          plot.tag.position = c(0,1))

  pdf(file = "figures/figure3.pdf", useDingbats = TRUE, width = 12, height = 6)
  print(figure3[[1]])
  dev.off()
}

generate_figure4 <- function(y_result, cols_ethnicity){
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
      facet_wrap(.~scenario) + xlab(bquote(R[0])) + 
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
}
generate_figure5 <- function(df_prop_r0_per_region, cols_region){
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
    guides(colour = guide_legend(nrow = 2)) + xlab("\u03B2") + ylab(bquote(R[0])) +
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
    mutate(beta = paste0('beta == ', beta, '  (R[0]~"between"~', 
                         min_r0, '~"and"~', max_r0, ')'),
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
    xlab("") + ylab(expression("Proportion change in R"[0], " across\nall betas compared to England")) +
    labs(tag = "C")
  
  pdf(file = "figures/figure5.pdf", useDingbats = TRUE, width = 12, height = 9)
  print(
    free(figure5[[1]] / figure5[[3]] + plot_layout(heights = c(.67,.33))) + 
      figure5[[2]] + plot_layout()
  )
  dev.off()
}
