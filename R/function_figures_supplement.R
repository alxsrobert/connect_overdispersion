# Figures: Fitting mixture models to distribution of contacts
generate_figure_distageeth <- function(model_regression){
  set_theme(
    theme_bw() + 
      theme(legend.title = element_blank(), 
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            strip.background = element_blank(),
            strip.text = element_text(size = 14, hjust = 0),
            panel.spacing.x = unit(.5, "lines")
      )
  )
  ethnicity <- c("Asian_Urban", "Black_Urban", "Mixed_Urban", "Other_Urban", 
                 "White_Urban")
  
  ## Generate a synthetic population, with the number of contact per individual
  ## drawn using the regression coefficients.
  # If scenario_contact_group == "same_pop", the demographics characteristics of
  # the population follow the country-wide distribution instead of the ethnicity
  # specific distribution.
  simulated_population <-
    create_contact_in_pop(
      model = model_regression, n_draws = 5, region = "England", each = 500, seed = 1,
      vec_ethnicity_rural = ethnicity, which_type = "ethnicity-stratified\n population") |>
    select(ethnicity_rural, p_age_group, contact) |> 
    mutate(contact = case_when(contact > 200 ~ 200, 
                               .default = contact))
  
  
  for(n_contact in c(4)){
    ## Create the transmitter groups, drawing the proportion of the population and 
    ## the number of contacts in each group.
    # The transmitter groups are specific to each level of age group and ethnicity
    # Extract the number of age groups and ethnicities
    tot_age <- length(unique(simulated_population$p_age_group))
    tot_eth <- length(unique(simulated_population$ethnicity_rural))
    for(j in seq_len(tot_age)){
      if(j %% 2 == 1) {
        plot_separated_distributions <- list()
        plot_full_distributions <- list()
        pdf(file = paste0(
          "figures/s_", n_contact, "_transmitter", 1 + j %/% 2, ".pdf"),
          useDingbats = TRUE, width = 10, height = if(j == tot_age) 3.5 else 7)
      }
      dt_all_pop <- data.frame()
      for(i in seq_len(tot_eth)){
        # Extract the individuals of the population of ethnicity i and age j
        pop_ij <- simulated_population |> 
          filter(ethnicity_rural == unique(simulated_population$ethnicity_rural)[i], 
                 p_age_group == unique(simulated_population$p_age_group)[j])
        
        # Fit the distribution of contact to n_group Poisson distributions
        flexfit_ij <- flexmix(contact ~ 1, 
                              data = pop_ij, #|> 
                                # filter(contact < 100),
                                # mutate(contact = case_when(contact > 100 ~ 100, 
                                #                            .default = contact)),
                              k = n_contact, 
                              model = FLXglm(family = "poisson"),
                              control = list(iter = 100, minprior = 0))
        
        # Extract the proportion of the population in each group
        size_fit <- flexfit_ij@prior
        # Extract the mean number of contacts in each group
        coef_fit <- parameters(flexfit_ij)
        
        n_contact_j <- length(coef_fit)
        
        # Set the coefficients in increasing orders
        rank_ij <- order(coef_fit[1:n_contact_j])
        size_fit <- size_fit[rank_ij]
        coef_fit <- coef_fit[rank_ij]
        
        # Compute the density distribution in the data and in the mixture models
        dt_all_pop <- rbind.data.frame(
          dt_all_pop,
          full_join(
            pop_ij |> group_by(contact) |> summarise(Data = n() / nrow(pop_ij), 
                                                     .groups = "drop"),
            data.frame(
              contact = seq(0, max(pop_ij$contact)), 
              Low = dpois(seq(0, max(pop_ij$contact)), exp(coef_fit[1])) * size_fit[1],
              Medium = dpois(seq(0, max(pop_ij$contact)), exp(coef_fit[2])) * size_fit[2], 
              High = dpois(seq(0, max(pop_ij$contact)), exp(coef_fit[3])) * size_fit[3],
              Highest = if(n_contact_j == 3) 0 else
                dpois(seq(0, max(pop_ij$contact)), exp(coef_fit[4])) * size_fit[4]
            ),
            by = "contact"
          ) |> 
            mutate(contact = case_when(contact == 0 ~ .5, .default = contact),
                   ethnicity = unique(simulated_population$ethnicity_rural)[i] |> 
                     gsub(pattern = "_Urban", replacement = ""))
        )
      }
      
      ## Plot showing the different distributions for each ethnicity
      plot_separated_distributions[[as.numeric(j %% 2 == 0) + 1]] <-
        dt_all_pop |> 
        pivot_longer(cols = -c(contact, ethnicity), 
                     values_to = "density", names_to = "group") |> 
        mutate(density = case_when(is.na(density) ~ 0, .default = density),
               group = case_when(group == "Data" ~ "Synthetic\npopulation", 
                                 .default = group),
               group = factor(group, levels = c("Synthetic\npopulation", "Low", 
                                                "Medium", "High", "Highest"))) |> 
        # filter(density > 0.0001) |> 
        ggplot() + 
        geom_area(
          aes(x = contact, y = density, fill = group), alpha = .3, position = "identity") + 
        scale_fill_manual(values = c("#34789a", "orange", "coral", "red", "brown")) + 
        scale_color_manual(values = c("#34789a", "orange", "coral", "red", "brown")) + 
        facet_grid(~ethnicity, scales = "free") +
        scale_x_log10() +
        theme(legend.position = if(i != 5) "none" else "right", 
              strip.text = element_text(size = 14, hjust = 0)) + 
        ylab("Density") + xlab("Number of contacts") +
        ggtitle(paste0(unique(simulated_population$p_age_group)[j], " years old")) + 
        theme(legend.position = if(i != 5) "none" else "right",
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ## Plot showing the overall mixture distribution
      plot_full_distributions[[as.numeric(j %% 2 == 0) + 1]] <- 
        dt_all_pop |> 
        mutate(Mixture = Low + Medium + High + Highest) |> 
        select(contact, ethnicity, Data, Mixture) |> 
        pivot_longer(cols = -c(contact, ethnicity), 
                     values_to = "density", names_to = "group") |> 
        mutate(density = case_when(is.na(density) ~ 0, .default = density),
               group = case_when(group == "Data" ~ "Synthetic\npopulation", 
                                 .default = group),
               group = factor(group, levels = c("Synthetic\npopulation", "Mixture"))) |> 
        # filter(density > 0.0001) |> 
        ggplot() + 
        geom_area(aes(x = contact, y = density, fill = group), alpha = .3, position = "identity") + 
        scale_fill_manual(values = c("#34789a", "#a95d62")) + 
        scale_color_manual(values = c("#34789a", "#a95d62")) + 
        scale_x_log10() +
        facet_grid(~ethnicity, scales = "free") + ylab("Density") + xlab("Number of contacts") + 
        theme(legend.position = if(i != 5) "none" else "right", strip.text = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      if(j %% 2 == 0) {
        print(
          plot_separated_distributions[[1]] / plot_full_distributions[[1]] /
            plot_separated_distributions[[2]] / plot_full_distributions[[2]] +
            plot_layout(axis_titles = "collect")
        )
        dev.off()
      } else if (j == tot_age){
        print(
          plot_separated_distributions[[1]] / plot_full_distributions[[1]] +
            plot_layout(axis_titles = "collect")
        ) 
        dev.off()
      }
    }
  }
  theme_set()
}

# Figure: Difference between # of contacts before and after per capita method
generate_figure_diffmethods <- function(file_regression){
  each_sim <- 500
  level_age <- c("0-4", "5-9", "10-14", "15-17", "18-24", "25-29", "30-39", "40-49",
                 "50-59", "60-69", "70+")
  level_ethnic <- c("Asian", "Black", "Mixed", "Other", "White")
  level_transmitter <- c("Low transmitter", "Medium transmitter", "High transmitter", "Highest transmitter")
  ## Number of contacts before per capita method (i.e. mixture distribution)
  transmission_groups <- create_contact_group(
    scenario_contact_group = "reference", n_group = 4, n_draws = 5, region = "England", 
    each = each_sim, file_in = file_regression, vec_ethnicity_rural = paste0(level_ethnic, "_Urban"))
  contact_from_mixture <- transmission_groups$coef
  ## Number of contacts in per capita matrix
  contact_from_per_cap <- 
    run_outbreaks(r0 = 2, region = "England", list_prop_coef = transmission_groups,
                  return_n_contact = TRUE)
  
  ## Join average number of contacts from mixture distribution and per capita matrix
  dt_contact <- 
    full_join(
      data.frame(ethnic = gsub("age.*", "", names(contact_from_per_cap)) |> 
                   gsub(pattern = "eth", replacement = ""),
                 age = gsub("group.*", "", names(contact_from_per_cap)) |> 
                   gsub(pattern = ".*age", replacement = ""),
                 transmitter = gsub(".*group", "", names(contact_from_per_cap)),
                 model = as.numeric(contact_from_per_cap)),
      data.frame(ethnic = as.character(rep(seq(5), each = 11)),
                 age = as.character(rep(seq(11), 5*4)),
                 transmitter = as.character(rep(seq(4), each = 11*5)),
                 group = c(contact_from_mixture[,1], contact_from_mixture[,2], 
                           contact_from_mixture[,3], 
                           contact_from_mixture[,4])),
      by = c("ethnic", "age", "transmitter")
    ) |> 
    ## Convert ethnicity, age, and contact group to factors
    mutate(
      ethnic = factor(level_ethnic[as.numeric(ethnic)], levels = level_ethnic),
      age = factor(level_age[as.numeric(age)], levels = level_age),
      transmitter = factor(level_transmitter[as.numeric(transmitter)], 
                           levels = level_transmitter),
    ) |> 
    pivot_longer(cols = c("model", "group"), names_to = "type", values_to = "n_contacts") |> 
    mutate(type = case_when(
      type == "model" ~ "Generated from per capita matrix",
      type == "group" ~ "Generated from mixture distributions"
    ))
  
  pdf(file = "figures/s_comparison_ncontacts.pdf", useDingbats = TRUE, 
      width = 12, height = 9)
  ## Generate figure
  print(
    dt_contact |> 
      ggplot(aes(x = age, y = n_contacts, colour = type, shape = transmitter, 
                 group = type)) +
      geom_point(position = position_dodge(width = 0.5)) + facet_wrap(ethnic ~ ., ncol = 1) + 
      scale_y_log10() + coord_cartesian(ylim = c(1, 200)) +
      xlab("Age group") + ylab("Mean number of contacts") + theme_bw() + 
      guides(col = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
            legend.title=element_blank(), axis.text=element_text(size=16),
            strip.text = element_text(hjust = 0,face="bold", size = 18),
            axis.title=element_text(size=16,face="bold"), legend.position = "top", 
            title = element_text(size=16), strip.background = element_blank(),
            legend.text = element_text(size = 14, face = "bold"))
  )
  dev.off()
}

# Figure: age standardised attack rate
generate_figure_arstandard <- function(y_result, cols_ethnicity){
  ## Clean outbreak simulations
  y_figure_age_standard <- 
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
  
  ## Compute age standardised attack rate, and relative to White ethnicity
  figure_age_standard <-
    y_figure_age_standard |> 
    left_join(## Add column with proportion in White ethnicity
      y_figure_age_standard |> 
        filter(ethnicity == "White") |> mutate(prop_ref = proportion_standard) |> 
        select(r0, scenario, iter, prop_ref),
      by = c("iter", "r0", "scenario")
    ) |>  ## Compute relative attack rate
    filter(ethnicity != "White") |> 
    mutate(relative_ratio = proportion_standard / prop_ref) |> 
    group_by(ethnicity, r0, scenario) |> ## Compute median and 95% SI
    summarise(median_relative_ratio = median(relative_ratio),
              low_relative_ratio = quantile(relative_ratio, 0.025),
              high_relative_ratio = quantile(relative_ratio, 0.975),
              .groups = "drop"
    ) |> 
    pivot_longer(cols = c(median_relative_ratio, low_relative_ratio, high_relative_ratio),
                 names_to = c(".value", "plot"), names_pattern = "([a-z]+)_(.*)") |> 
    mutate(plot = case_when(plot == "relative_ratio" ~ "Age-standardised\nrelative attack rate")) |> 
    ggplot() + 
    geom_ribbon(aes(fill = ethnicity, x = r0, ymin = low, ymax = high, alpha = ethnicity)) + 
    scale_alpha_manual(values = c(.7, .5, .3, .5, 1)) +
    geom_line(aes(col = ethnicity, x = r0, y = median)) + 
    scale_fill_manual(values = cols_ethnicity) + 
    scale_color_manual(values = cols_ethnicity) + 
    geom_line(aes(x = r0), y = 1, lty = 2, col = "Black") +
    facet_grid(plot ~ scenario, scales = "free", switch = "y") + xlab(bquote(R[0])) + theme_bw() + 
    theme(axis.text = element_text(size=16), 
          strip.text.x = element_blank(),
          strip.text.y = element_text(size = 14),
          strip.placement = "outside",
          axis.title.y = element_blank(), 
          axis.title.x = element_text(size=14,face="bold"), 
          legend.text = element_text(size = 12), 
          legend.title = element_blank(),
          strip.background = element_blank(), 
          legend.position = "bottom")
  
  pdf(file = "figures/s_propinf_AS.pdf", useDingbats = TRUE, width = 12, height = 4)
  print(figure_age_standard)
  dev.off()
}

# Figures: Trajectories per group at R0 = 2 and R0 = 4
generate_figure_traj <- function(cols_age, cols_ethnicity, cols_contact){
  t_sim <- seq(0, 700) # Time  frame of the stochastic simulations
  r0 <- c(2,4)
  
  for(i in r0){
    # Generate a set of 50 simulations, with r0 = i, and 500 individuals per level 
    # of age/ethnicity in the synthetic population
    y_r0i <- run_outbreaks(t = t_sim, r0 = i, region = "England", n_particles = 50, 
                           each = 500, n_group = 4)
    
    ## Extract column numbers corresponding to each transmitter group
    groups_contact <- list(
      "Low" = grepv("group1", rownames(y_r0i$S)),
      "Medium" = grepv("group2", rownames(y_r0i$S)),
      "High" = grepv("group3", rownames(y_r0i$S)),
      "Highest" = grepv("group4", rownames(y_r0i$S))
    )
    ## Extract column numbers corresponding to each ethnicity
    groups_eth <- list(
      grepv("eth1", rownames(y_r0i$S)), grepv("eth2", rownames(y_r0i$S)),
      grepv("eth3", rownames(y_r0i$S)), grepv("eth4", rownames(y_r0i$S)),
      grepv("eth5", rownames(y_r0i$S))
    )
    names(groups_eth) <- c("Asian", "Black", "Mixed", "Other", "White")
    
    ## Extract column numbers corresponding to each aggregated age group
    groups_age_aggreg <- list(
      c(grepv("age1g", rownames(y_r0i$S)), grepv("age2g", rownames(y_r0i$S)),
        grepv("age3g", rownames(y_r0i$S)), grepv("age4g", rownames(y_r0i$S))),
      c(grepv("age5g", rownames(y_r0i$S)), grepv("age6g", rownames(y_r0i$S))),
      c(grepv("age7g", rownames(y_r0i$S)), grepv("age8g", rownames(y_r0i$S)),
        grepv("age9g", rownames(y_r0i$S))), 
      c(grepv("age10g", rownames(y_r0i$S)), grepv("age11g", rownames(y_r0i$S)))
    )
    names(groups_age_aggreg) <- c("0-17", "18-29", "30-59", "60-93")
    
    ## Generate total number of cases, attack rates, and trajectory per age, per
    ## ethnicity, and per contact group
    pdf(file = paste0("figures/s_r0", i, ".pdf"), 
        useDingbats = TRUE, width = 8, height = 7)
    par(bty = "l", mar = c(3, 4, 3, 0), oma = c(0,1,0,1))
    layout(matrix(c(1,3,5,2,4,6,7,8,9), 3, 3))
    
    figure_plot_total_simulations(y_r0i, 
                                  groups_transmission = groups_contact, 
                                  groups_age = groups_age_aggreg, 
                                  groups_eth = groups_eth, verbose = FALSE)
    
    figure_plot_simulations(
      y = y_r0i, prop_cases_only = TRUE, t = 1:250, groups = groups_age_aggreg, 
      with_lab = FALSE, cols = cols_age, ymax_prop_i = if(i == 2) 0.012 else 0.025, 
      verbose = FALSE)
    figure_plot_simulations(
      y = y_r0i, prop_cases_only = TRUE, cols = cols_ethnicity, t = 1:250, with_lab = FALSE, 
      groups = groups_eth, ymax_prop_i = if(i == 2) 0.012 else 0.025, verbose = FALSE)
    figure_plot_simulations(
      y = y_r0i, cols = cols_contact, t = 1:250, groups = groups_contact, 
      with_lab = FALSE, verbose = FALSE, ymax_prop_i = if(i == 2) 0.035 else 0.06, 
      prop_cases_only = TRUE)
    title(xlab = "Time", outer = FALSE, line = 2, cex.lab = 1.5)
    dev.off()
  }
}

# Figures: Population by city
generate_figure_democity <- function(cols_ethnicity){
  ## Set ggplot theme
  theme_set(
    theme_minimal() +
      theme(legend.title=element_blank(), 
            legend.position = "top",
            legend.key = element_blank(),
            axis.text=element_text(size=14),
            strip.text.x = element_text(size = 14),
            axis.title=element_text(size=16,face="bold"), 
            legend.text = element_text(size = 12),
            plot.tag = element_text(face = "bold", size = 20),
            title = element_text(size=16),
            plot.tag.position = c(0.1,1)
      )
  )
  
  age_group_level <- c("0-4", "5-9", "10-14", "15-17", "18-24", "25-29", "30-39",
                       "40-49", "50-59", "60-69", "70-100")
  
  regions <- c(
    "Birmingham", "Leicester", "London", "Manchester", "Liverpool", "York")
  figure_i <- list()
  ## Plotting two cities side by side
  pdf(file = "figures/s_dem_city.pdf", useDingbats = TRUE, 
      width = 15, height = 12)
  for(i in seq(1, length(regions), 2)){
    region_i <- regions[i]
    region_i2 <- regions[i + 1]
    
    ## Import age distribution, household size, employment status for each region
    age_eth <- 
      rbind.data.frame(
        cbind.data.frame(
          clean_age_eth(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), region = region_i),
          region = region_i
        ),
        cbind.data.frame(
          clean_age_eth(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), region = region_i2),
          region = region_i2
        )
      )
    hh_eth <- 
      rbind.data.frame(
        cbind.data.frame(
          clean_hh_size(age_group_level, region = region_i),
          region = region_i
        ),
        cbind.data.frame(
          clean_hh_size(age_group_level, region = region_i2),
          region = region_i2
        )
      )
    employ_eth <- 
      rbind.data.frame(
        cbind.data.frame(
          clean_employ(age_group_level, region = region_i),
          region = region_i
        ),
        cbind.data.frame(
          clean_employ(age_group_level, region = region_i2),
          region = region_i2
        )
      )
    
    # Import hiqual category, clean levels
    hiqual_eth <- 
      rbind.data.frame(
        cbind.data.frame(
          clean_hiqual(age_group_level, region = region_i),
          region = region_i
        ),
        cbind.data.frame(
          clean_hiqual(age_group_level, region = region_i2),
          region = region_i2
        )
      )
    
    
    

    ## Set 0% for 0-4 age group
    age_eth <- 
      rbind.data.frame(
        data.frame(age_group = "0-4", 
                   ethnicity = rep(unique(age_eth$ethnicity), each = 2), 
                   nb = 0, prop = 0, sum_prop = 0, low = -1,
                   region = c(region_i, region_i2)), 
        age_eth |> 
          group_by(ethnicity, region) |> 
          mutate(sum_prop = cumsum(prop),
                 low = as.numeric(gsub(".*[-]", "", age_group)) + 1))
    
    ## age distribution by ethnicity
    age_distribution <-
      age_eth |> 
      filter(ethnicity != "All") |> 
      ggplot(aes(x = low, y = sum_prop, col = ethnicity, group = ethnicity)) + 
      scale_color_manual(values = c(cols_ethnicity)) + 
      geom_line(lwd = .5) + geom_point() + xlab("Age") + ylab("Cumulative proportion") + 
      facet_grid(~ region) + guides(col = "none")
    ## Household size by ethnicity
    hh_size_per_eth <-
      hh_eth |> 
      filter(ethnic_group != "Does not apply") |> 
      group_by(ethnic_group, hh_size, region) |> 
      summarise(tot = sum(n), .groups = "drop") |> 
      group_by(ethnic_group, region) |> 
      mutate(prop = tot / sum(tot),
             hh_size = case_when(hh_size == "Alone" ~ "1", 
                                 hh_size == "Two" ~ "2", 
                                 hh_size == "Three" ~ "3", 
                                 hh_size == "Four" ~ "4", 
                                 hh_size == "More than 4" ~ ">4"),
             hh_size = factor(hh_size, levels = c("1", "2", "3", "4", ">4")),
             ethnic_group = gsub(",", "", gsub("[ ].*", "", ethnic_group))) |>
      ggplot(aes(y = prop, x = hh_size, fill = ethnic_group)) +
      scale_fill_manual(values = c(cols_ethnicity)) + 
      geom_col(position = "dodge",width = .6) +
      facet_grid(~ region, scales = "free") + scale_x_discrete(drop = TRUE) +
      ylim(0, .6) + ylab("Proportion") + xlab("Household size") + 
      theme(strip.text.x = element_blank())
    ## Employment status by ethnicity
    employ_per_eth <-
      employ_eth |> 
      filter(ethnic_group != "Does not apply",
             econ != "Does not apply") |> 
      mutate(econ = case_when(
        econ == "Long-term sick or disabled" ~ "Long-term sick\nor disabled",
        econ == "Looking after home or family" ~ "Looking after\nhome or family",
        econ == "employed" ~ "Employed",
        econ == "unemployed" ~ "Unemployed",
        .default = econ
      )) |> 
      mutate(econ = factor(econ, levels = c(
        "Employed", "Unemployed", "Student", "Retired", 
        "Looking after\nhome or family", "Long-term sick\nor disabled", "Other"
      ))) |> 
      group_by(ethnic_group, econ, region) |> 
      summarise(tot = sum(n), .groups = "drop") |> 
      group_by(ethnic_group, region) |> 
      mutate(prop = tot / sum(tot),
             ethnic_group = gsub(",", "", gsub("[ ].*", "", ethnic_group))) |> 
      ggplot(aes(y = prop, x = econ, fill = ethnic_group)) +
      scale_fill_manual(values = c(cols_ethnicity)) + 
      geom_col(position = "dodge",width = .6) + facet_grid(~ region, scales = "free") +
      scale_x_discrete(drop = TRUE, guide = guide_axis(n.dodge = 2)) + 
      ylab("Proportion") + xlab("Employment status") +
      theme(legend.position = "none",
            # axis.text.x = element_text(angle = 90),
            strip.text.x = element_blank(),
      )
    
    hiqual_per_eth <- 
      hiqual_eth |> 
      filter(ethnic_group != "Does not apply") |> 
      mutate(hiqual = case_when(
        hiqual == "Apprenti" ~ "Apprenticeship",
        hiqual == "Level1" ~ "Level 1",
        hiqual == "Level2" ~ "Level 2", 
        hiqual == "Level3" ~ "Level 3", 
        hiqual == "Level4" ~ "Level 4", 
        hiqual == "Noquali" ~ "No qualification",
        .default = hiqual
      )) |> 
      mutate(hiqual = factor(hiqual, levels = c(
        "No qualification", "Level 1", "Level 2", "Level 3", "Level 4", "Apprenticeship"
      ))) |> 
      group_by(ethnic_group, hiqual, region) |> 
      summarise(tot = sum(n), .groups = "drop") |> 
      group_by(ethnic_group, region) |> 
      mutate(prop = tot / sum(tot),
             ethnic_group = gsub(",", "", gsub("[ ].*", "", ethnic_group))) |> 
      ggplot(aes(y = prop, x = hiqual, fill = ethnic_group)) +
      scale_fill_manual(values = c(cols_ethnicity)) + 
      geom_col(position = "dodge",width = .6) + facet_grid(~ region, scales = "free") +
      xlab("Highest qualification") + ylab("Proportion") + guides(fill = "none") + 
      scale_x_discrete(drop = TRUE, guide = guide_axis(n.dodge = 2)) + 
      theme(legend.position = "none",
            # axis.text.x = element_text(angle = 90),
            strip.text.x = element_blank(),
      )
    
    
    ## Pie charts showing the distribution of ethnicity in each city
    piepop1 <- 
      age_eth |> 
      filter(ethnicity != "All", region == region_i) |> 
      group_by(ethnicity) |> 
      summarise(nb = sum(nb), .groups = "drop") |> 
      group_by() |> 
      mutate(prop = nb / sum(nb)) |> 
      ggplot(aes(fill = ethnicity, x = "", y = prop, col = ethnicity)) + 
      geom_bar(stat="identity", width=1, alpha = .7) +
      scale_fill_manual(values = cols_ethnicity) + 
      scale_color_manual(values = cols_ethnicity) + 
      coord_polar("y", start=0) + theme_void() + 
      theme(legend.position = "none")
    
    piepop2 <- 
      age_eth |> 
      filter(ethnicity != "All", region == region_i2) |> 
      group_by(ethnicity) |> 
      summarise(nb = sum(nb), .groups = "drop") |> 
      group_by() |> 
      mutate(prop = nb / sum(nb)) |> 
      ggplot(aes(fill = ethnicity, x = "", y = prop, col = ethnicity)) + 
      geom_bar(stat="identity", width=1, alpha = .7) +
      scale_fill_manual(values = cols_ethnicity) + 
      scale_color_manual(values = cols_ethnicity) + 
      coord_polar("y", start=0) + theme_void() + 
      theme(legend.position = "none")
    
    grob_i1 <- ggplotGrob(piepop1)
    grob_i2 <- ggplotGrob(piepop2)
    
    df_grobs <- data.frame(
      region = c(region_i, region_i2),
      x = 70,     # position in data coordinates
      y = .35,
      stringsAsFactors = FALSE
    )
    
    # Add grobs as a list-column
    df_grobs$grob <- list(grob_i1, grob_i2)
    
    
    figure_i[[1]] <- 
      age_distribution +
      ggpp::geom_grob(
        data = df_grobs,
        aes(x, y, label = grob),
        vp.width = 0.7,    # relative size of pies
        vp.height = 0.7
      )
    
    
    figure_i[[2]] <- 
      hh_size_per_eth / employ_per_eth / hiqual_per_eth + plot_layout(axis_titles =  "collect_y")
    
    print(figure_i[[1]] / figure_i[[2]] + 
            plot_layout(heights = c(1/4, 3/4)))
    
  }
  dev.off()
  theme_set()
}

# Figures: Proportion infected by city
generate_figure_figcity <- function(y_result, cols_ethnicity){
  theme_set(
    theme_bw() + 
      theme(axis.text = element_text(size=16), strip.text = element_text(size = 14),
            strip.placement = "outside", strip.background.y = element_blank(),
            axis.title = element_text(size=14,face="bold"), 
            legend.text = element_text(size = 12), legend.title = element_blank(),
            strip.background = element_blank())
  )
  regions <- c("Birmingham", "Leicester", "Liverpool", "London", "Manchester", "York")
  
  ## Select simulation at a city level
  y_result_region <- y_result |> 
    filter(r0 > 0.8, is.element(type, regions))
  ## Filter out outbreaks that immediately died off 
  y_result_region <- y_result_region |> 
    filter(paste0(type, r0, iter) %in% 
             (y_result_region |> group_by(type, r0, iter) |> 
                summarise(n_tot = sum(n), .groups = "drop") |> 
                mutate(id = paste0(type, r0, iter)) |> 
                filter(n_tot > 500) |> pull(id))) |> 
    mutate(scenario = factor(type, levels = regions))
  
  ## Figure: attack rate per ethnicity in each city
  p_prop <- 
    y_result_region |> 
    group_by(ethnicity, r0, scenario) |> 
    summarise(median = median(proportion),
              low_95 = quantile(proportion, 0.025),
              high_95 = quantile(proportion, 0.975), .groups = "drop"
    ) |> 
    ggplot(aes(fill = ethnicity, x = r0, ymin = low_95, ymax = high_95)) + 
    geom_ribbon(aes(alpha = ethnicity)) + 
    geom_line(aes(col = ethnicity, x = r0, y = median)) + 
    scale_alpha_manual(values = c(.7, .5, .3, .5, .4)) +
    scale_fill_manual(values = cols_ethnicity) + 
    scale_color_manual(values = cols_ethnicity) + 
    facet_wrap(.~scenario) + xlab(bquote(R[0])) + 
    guides(fill = guide_legend(override.aes = list(alpha = .7)), col = "none") + 
    ylab("Attack rate") + ylim(0.05, .70)
  
  pdf(file = "figures/s_propinf_city.pdf", useDingbats = TRUE, width = 11, height = 5)
  print(p_prop)
  dev.off()
  ## Figure: Age standardised attack rate per ethnicity in each city
  p_prop_AS <- 
    y_result_region |> 
    left_join(
      y_result_region |> 
        filter(ethnicity == "White") |> mutate(prop_ref = proportion_standard) |> 
        select(r0, type, scenario, iter, prop_ref),
      by = c("iter", "r0", "scenario")
    )  |> 
    filter(ethnicity != "White") |> 
    mutate(relative_ratio = proportion_standard / prop_ref) |> 
    group_by(ethnicity, r0, scenario) |> 
    summarise(median = median(relative_ratio),
              low_50 = quantile(relative_ratio, 0.25),
              high_50 = quantile(relative_ratio, 0.75), 
              low_95 = quantile(relative_ratio, 0.025),
              high_95 = quantile(relative_ratio, 0.975), 
              .groups = "drop"
    ) |> 
    ggplot() + 
    geom_ribbon(aes(fill = ethnicity, x = r0, ymin = low_50, ymax = high_50, alpha = ethnicity)) + 
    geom_ribbon(aes(fill = ethnicity, x = r0, ymin = low_95, ymax = high_95, alpha = ethnicity)) + 
    scale_alpha_manual(values = c(.4, .4, .5, .3)) +
    # geom_line(aes(col = ethnicity, x = r0, y = median)) + 
    scale_fill_manual(values = cols_ethnicity[-5]) +
    ylab("Age-standardised\nrelative attack rate") +
    scale_color_manual(values = cols_ethnicity[-5]) + 
    geom_line(aes(x = r0), y = 1, lty = 2, col = "Black") +
    facet_wrap(.~scenario) + xlab(bquote(R[0])) + 
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  
  pdf(file = "figures/s_propinf_city_AS.pdf", useDingbats = TRUE, width = 11, height = 5)
  print(p_prop_AS)
  dev.off()
}

# Figure: model comparison
generate_figure_looic <- function(compar_model){
  # Split and rename <18 and >=18 models
  plot_data <- compar_model |>
    mutate(
      age_label = case_when(
        age_group == "bel18" ~ "Children (<18 years)",
        age_group == "ov18"  ~ "Adults (18+ years)",
        TRUE ~ age_group
      ),
      # Unique model IDs allow ordering within each age category.
      model_id = paste(age_group, model, sep = "::")
    ) |>
    arrange(age_group, rank)
  
  # First-ranked model appears at the top of each panel.
  plot_data$model_id <- factor(
    plot_data$model_id, levels = rev(unique(plot_data$model_id))
  )
  
  ## Rename models
  plot_data <- 
    plot_data |> 
    mutate(model_id = case_when(
      model == "bel18_model_1" ~ "Mixture of hurdle NB distributions",
      model == "bel18_model_3" ~ "Mixture of hurdle NB distributions\nEthnic group and age only",
      model == "bel18_model_4" ~ "Mixture of NB distributions",
      model == "bel18_model_2" ~ "Single hurdle NB distribution",
      model == "ov18_model_1" ~  "Mixture of NB distributions\nEthnic group, Employment status",
      model == "ov18_model_2" ~  "Mixture of NB distributions\nSubethnic group, Employment status",
      model == "ov18_model_3" ~  "Mixture of NB distributions\nEthnic group, NS-SEC",
      model == "ov18_model_4" ~  "Mixture of NB distributions\nEthnic group, Employment status,\nHighest qualification",
      model == "ov18_model_5" ~  "Mixture of NB distributions\nSubethnic group, Employment status,\nHighest qualification",
      model == "ov18_model_6" ~  "Mixture of NB distributions\nEthnic group and age only",
      model == "ov18_model_7" ~  "Single NB distribution\n Ethnic group, Employment status,\nHighest qualification"
    ),
    model_id = factor(model_id, levels = c(
      "Mixture of hurdle NB distributions",
      "Mixture of hurdle NB distributions\nEthnic group and age only",
      "Mixture of NB distributions",
      "Single hurdle NB distribution",
      "Mixture of NB distributions\nSubethnic group, Employment status,\nHighest qualification",
      "Mixture of NB distributions\nEthnic group, Employment status",
      "Mixture of NB distributions\nSubethnic group, Employment status",
      "Mixture of NB distributions\nEthnic group, NS-SEC",
      "Mixture of NB distributions\nEthnic group, Employment status,\nHighest qualification",
      "Mixture of NB distributions\nEthnic group and age only",
      "Single NB distribution\n Ethnic group, Employment status,\nHighest qualification"
    )),
    age_label = case_when(
      age_label == "Children (<18 years)" ~ paste0(age_label, ",\nReference: Mixture of hurdle negative binomial distributions"),
      age_label == "Adults (18+ years)" ~ 
        paste0(age_label, ",\nReference: Mixture of negative binomial distributions\nEthnic group, Employment status, Highest qualification"),
    ),
    age_label = factor(age_label, levels = c(
      "Children (<18 years),\nReference: Mixture of hurdle negative binomial distributions", 
      "Adults (18+ years),\nReference: Mixture of negative binomial distributions\nEthnic group, Employment status, Highest qualification"
    ))) |> 
    filter(!model %in% c("ov18_model_4", "bel18_model_1"))
  
  p_loo <- (
    plot_data |> 
      ggplot(aes(x = delta_looic, y = model_id)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
      geom_segment(aes(x = delta_looic - se_delta_looic, xend = delta_looic + se_delta_looic,
                       yend = model_id), linewidth = 0.6) +
      geom_point(size = 2.8) +
      facet_wrap(~ age_label, scales = "free",
                 space = "free_y", 
                 ncol = 1) +
      scale_y_discrete(expand = expansion(add = 0.5)) +
      labs(x = expression(Delta * "LOOIC relative to best-ranked model"),
           y = NULL, colour = NULL) +  
      theme_bw(base_size = 12) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        strip.background = element_blank()
      )) 
  
  p_combined <- 
    (p_loo + coord_cartesian(xlim = c(-10, 150))) +
    (p_loo + xlim(200, NA)) +
    plot_layout(axis_titles = "collect", axes = "collect")
  
  # Convert to a gtable so facet strips can span both columns.
  g <- patchwork::patchworkGrob(p_combined)
  
  # Find the facet strips placed above the panels.
  strip_ids <- grep("^strip-t", g$layout$name)
  
  # Group strips occupying the same horizontal band.
  strip_rows <- split(strip_ids, paste(g$layout$t[strip_ids], g$layout$b[strip_ids],
                                       sep = "_"))
  
  for (ids in strip_rows) {
    keep <- ids[which.min(g$layout$l[ids])]
    
    g$layout$l[keep] <- min(g$layout$l[ids])
    g$layout$r[keep] <- max(g$layout$r[ids])
    
    # Hide the duplicate title on the right.
    for (remove in setdiff(ids, keep)) {
      g$grobs[[remove]] <- grid::nullGrob()
    }
  }
  
  
  adult_title <- g$grobs[[16]]
  
  adult_strip_ids <- grep(
    "^strip-t",
    adult_title$layout$name
  )
  
  # Keep only the strip; hide panels, axes, and guides.
  for (j in setdiff(seq_along(adult_title$grobs),adult_strip_ids)) {
    adult_title$grobs[[j]] <- grid::nullGrob()
  }
  
  adult_title$widths <- grid::unit(1, "null")
  
  g <- gtable::gtable_add_grob(
    g, grobs = adult_title, t = g$layout$t[16], b = g$layout$b[16],
    l = min(g$layout$l[c(16, 31)]), r = max(g$layout$r[c(16, 31)]), z = Inf, 
    clip = "off", name = "shared-adult-strip"
  )
  
  g$grobs[[31]]$grobs[[5]] <- grid::nullGrob()
  g$grobs[[16]]$grobs[[5]] <- grid::nullGrob()
  
  ggsave(
    "figures/s_looic.pdf",
    plot = g,
    width = 8,
    height = 8
  )
}

