#' Plot parameter estimates from several models
#'
#' @param list_regression List of named bayesian regression objects
#'
#' @return ggplot object
figure_compare_models <- function(list_regression){
  # Create a tibble containing all coefficient estimates and CIs for all models
  dt_coef <- clean_list_regression_output(list_regression)
  
  # Plot the mean and 95% CI for each model, with one panel per variable
  dt_coef |>
    ggplot(aes(x = term, ymin = conf.low, ymax = conf.high, col = model, 
               group = model)) + 
    geom_errorbar(width = 0.4, position = position_dodge(0.5)) +
    geom_point(aes(y = estimate), position = position_dodge(0.5)) + 
    geom_hline(yintercept = 1, lty = 2) +
    facet_wrap(.~group, scales = "free", ncol = 2, nrow = 5)
}

#' Show all parameters from one model (including reference levels)
#'
#' @param list_regression list of regression object
#' @param which_model Model(s) to plot
#' @param filter_group Group(s) to plot (if set to NULL, all groups are plotted),
#' set of possible groups: "intercept", "age", "ethnicity_rural", "income",
#' "employment", "household", "shape", "others"
#'
#' @return ggplot object
figure_parameter_model <- function(list_regression, which_model, filter_group = NULL){
  # Create a tibble containing all coefficient estimates and CIs for all models
  dt_coef <- clean_list_regression_output(list_regression)
  lev_ref <- 
    unique(c("hh_size: One", "White_Urban", "Employed", "Lessthan20000", 
             "20000-39999", "Female", "Male", "weekday", "0-4", "5-9",
             "10-14", "15-17", "18-24", levels(dt_coef$term)))

  # Add reference levels
  dt_coef <- rbind.data.frame(
    dt_coef,
    cbind.data.frame(
      model = which_model, term = "18-24", estimate = 1, conf.low = 1,  
      conf.high = 1, group = "age"),
    cbind.data.frame(
      model = which_model, term = "White_Urban", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "ethnicity_rural"),
    cbind.data.frame(
      model = which_model, term = "Employed", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "employment"),
    cbind.data.frame(
      model = which_model, term = "White_Urban", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "shape"),
    cbind.data.frame(
      model = which_model, term = "20000-39999", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "income"),
    cbind.data.frame(
      model = which_model, term = "hh_size: One", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "household"),
    cbind.data.frame(
      model = which_model, term = "Female", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "gender"),
    cbind.data.frame(
      model = which_model, term = "weekday", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "day_of_the_week")
  )
  
  dt_coef <- dt_coef |>
    mutate(
      term = factor(term, levels = lev_ref, ordered = TRUE)
    )
  
  # Generate figure after removing Intercept values
  dt_coef_plot <- 
    dt_coef |>
    filter(model %in% which_model &
             !group %in% "intercept" & 
             !term %in% "(Intercept)" & 
             !term %in% c("Other", "ethOther", "shape_ethOther")) |> 
    mutate(
      model = factor(model, levels = which_model),
      group = case_when(
      group %in% c("gender", "day_of_the_week", "urban_rural") ~ "others",
      !group %in% c("gender", "day_of_the_week", "urban_rural") ~ group),
      group = factor(group, levels = c("age", "ethnicity", "ethnicity_rural", "employment",
                                       "income", "shape", "household", "others"))
    )
  
  if(!is.null(filter_group)){
    which_group <- which(is.element(levels(dt_coef_plot$group), filter_group))
    if(length(which_group) == 0)
      stop(paste0("If filter_group is not null, the only possible values are ", 
                  paste(levels(dt_coef_plot$group), collapse = ", ")))
  } 
  
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "age"] <- "Age group"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "employment"] <- 
    "Employment status"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "income"] <- 
    "Household income"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "shape"] <- 
    "Overdispersion parameter (shape)"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "household"] <- 
    "Household size"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "others"] <- 
    "Others"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "ethnicity_rural"] <- 
    "Ethnicity and  urban / rural status"

  levels(dt_coef_plot$term) <- gsub("_Rural", " and\n Rural", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("_Urban", " and\n Urban", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("hh_size: ", "", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Lessthan", "<", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Over", ">=", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("000_", "000 to ", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Lookingafterhomeorfamily", 
                                    "Looking after\n home or family", 
                                    levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Long_termsickordisabled", 
                                    "Long-term sick\n or disabled", 
                                    levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("000_", "000 to ", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("unemployed", "Unemployed", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("week", "Week", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Morethan", ">", levels(dt_coef_plot$term))
  
  if(is.null(filter_group)){
    filter_group <- unique(dt_coef_plot$group)
  } else filter_group <- levels(dt_coef_plot$group)[which_group]
  dt_coef_plot |> 
    filter(group %in% filter_group) |>
    ggplot(aes(x = term, ymin = conf.low, ymax = conf.high, col = model, 
               group = model)) + 
    geom_errorbar(width = 0.4, position = position_dodge(0.5)) +
    geom_point(aes(y = estimate), position = position_dodge(0.5)) + 
    geom_line(lty = 2, col = "black", y = 1) +
    facet_wrap(.~group, scales = "free", 
               ncol = ifelse(length(filter_group) > 4, 2, 1)) + 
    theme_bw() + labs(col = "") + 
    ylim(c(min(.3, dt_coef_plot$conf.low), max(2, dt_coef_plot$conf.high))) + 
    xlab("Coefficient") + ylab("Value") + 
    if(length(which_model) == 1) guides(col="none")
}

#' Show all parameters from one model (including reference levels)
#'
#' @param list_regression list of regression object
#' @param which_model Model to plot
#' @param filter_group Group(s) to plot (if set to NULL, all groups are plotted),
#' set of possible groups: "intercept", "age", "ethnicity_rural", "income",
#' "employment", "household", "shape", "others"
#'
#' @return forest plot of the parameter estimates
figure_forest_plot <- function(list_regression, which_model){
  # Create a tibble containing all coefficient estimates and CIs for all models
  dt_coef <- clean_list_regression_output(list_regression)
  # Extract the terms from the regression, putting the reference levels first
  lev_ref <- 
    unique(c("hh_size: One", grepv("_Urban", levels(dt_coef$term)), "White_Urban",
             "Employed", "Lessthan20000", "20000-39999", "Female", "Male",
             "weekday", "0-4", "5-9", "10-14", "15-17", "18-24", levels(dt_coef$term)))
  
  # Add reference levels
  dt_coef <- rbind.data.frame(
    dt_coef,
    cbind.data.frame(
      model = which_model, term = "18-24", estimate = 1, conf.low = 1,  
      conf.high = 1, group = "age"),
    cbind.data.frame(
      model = which_model, term = "White_Urban", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "ethnicity_rural"),
    cbind.data.frame(
      model = which_model, term = "Employed", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "employment"),
    cbind.data.frame(
      model = which_model, term = "White_Urban", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "shape"),
    cbind.data.frame(
      model = which_model, term = "20000-39999", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "income"),
    cbind.data.frame(
      model = which_model, term = "hh_size: One", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "household"),
    cbind.data.frame(
      model = which_model, term = "Female", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "gender"),
    cbind.data.frame(
      model = which_model, term = "weekday", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "day_of_the_week")
  )
  
  dt_coef <- dt_coef |>
    mutate(
      term = factor(term, levels = lev_ref, ordered = TRUE)
    )
  
  ## Remove Intercept values
  dt_coef_plot <- 
    dt_coef |>
    filter(model %in% which_model &
             !group %in% "intercept" & 
             !term %in% "(Intercept)") |> 
    mutate(
      group = factor(group, levels = c("age", "ethnicity", "ethnicity_rural", "employment",
                                       "income", "shape", "household", "gender",
                                       "day_of_the_week"))
    )
  # Set ethnicity rural and shape as the top panels
  dt_coef_plot$group <- relevel(dt_coef_plot$group, "shape")
  dt_coef_plot$group <- relevel(dt_coef_plot$group, "ethnicity_rural")
  
  # Rename covariates
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "age"] <- "Age group"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "employment"] <- 
    "Employment\n status"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "income"] <- 
    "Household\n income"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "shape"] <- 
    "Ethnicity\nurban / rural\n(overdispersion)"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "household"] <- 
    "Household\n size"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "ethnicity_rural"] <- 
    "Ethnicity\nurban / rural\n(mean)"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "gender"] <- 
    "Sex"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "day_of_the_week"] <- 
    "Day"
  # Rename coefficient labels
  levels(dt_coef_plot$term) <- gsub("_Rural", " and Rural", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("_Urban", " and Urban", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("hh_size: ", "", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Lessthan", "<", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Over", ">=", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("000_", "000 to ", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Lookingafterhomeorfamily", 
                                    "Looking after home or family", 
                                    levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Long_termsickordisabled", 
                                    "Long-term sick or disabled", 
                                    levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("000_", "000 to ", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("unemployed", "Unemployed", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("week", "Week", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("Morethan", ">", levels(dt_coef_plot$term))
  
  dt_coef_plot$term <- factor(dt_coef_plot$term, 
                              levels = rev(levels(dt_coef_plot$term)))
  
  # Generate the forest plot
  dt_coef_plot |> 
    filter(term != "Other") |> 
    ggplot(aes(x = estimate, y = term, colour = group)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_color_manual(
      values = c("red", "red", rep("grey50", length(unique(dt_coef_plot$group))))) + 
    labs(
      x = "Coefficient (Estimate)",
      y = NULL,
      title = ""
    ) +
    theme_minimal() +
    guides(col = "none") + 
    theme(
      panel.grid.major.y = element_blank(), 
      strip.placement = "outside",  
      strip.text.y.left = element_text(angle = 90, vjust = 0),
      strip.background = element_blank()
    )
  
  
}

#' Barplot showing the distribution of the number of contacts per individual in
#' synthetic populations 
#'
#' @param prediction_populations dataframe where each row corresponds to an 
#' individual in a synthetic population, and three columns: "ethnicity_rural",
#' the ethnicity of an individual, "contact" the simulated number of contact of that 
#' individual, and "type" the type of simulation process that was used to simulate
#' the number of contacts
#' @param breaks The breaks used to separate the total number of contacts into 
#' categories
#' @param label_breaks the level of each category of number of contacts  
#' @param cols Vector defining the colour for each ethnicity
#' 
figure_nb_contact_hist <- function(prediction_populations, breaks, label_breaks, cols){
  # Check if the population size is the same in each type, otherwise generate an
  # error
  if(!(table(paste0(prediction_populations$ethnicity_rural, 
                    prediction_populations$type)) |> unique() |> length()) == 1)
    stop("All type should have the same number of individuals per ethnicity")
  
  # Extract the population size
  pop_size_i <- 
    table(paste0(prediction_populations$ethnicity_rural, 
                 prediction_populations$type))[1]
  # Extract the label of each type of simulation
  label_predictions <- unique(prediction_populations$type)
  
  ## Generate the barplot
  prediction_populations |>
    # Shift type to a factor
    mutate(type = factor(type, label_predictions)) |>
    # Cut contact into categories (defined by breaks and label_break)
    mutate(group_contact = cut(
      contact, breaks = breaks, labels = label_breaks)) |>    
    ggplot(aes(x = group_contact, fill = ethnicity_rural)) + 
    geom_bar(aes(y = after_stat(count / pop_size_i)), width=.5, position = "dodge") +
    labs(
      x = "Number of contacts",
      y = "Proportion",
      title = ""
    ) +
    labs(fill = "ethnicity") + 
    facet_wrap(~type, ncol = 1) + 
    scale_fill_manual(values = cols)
  
}

#' Density plot showing the distribution of the number of contacts per individual 
#' in synthetic populations 
#'
#' @param prediction_populations dataframe where each row corresponds to an 
#' individual in a synthetic population, and three columns: "ethnicity_rural",
#' the ethnicity of an individual, "contact" the simulated number of contact of that 
#' individual, and "type" the type of simulation process that was used to simulate
#' the number of contacts
#' @param prop_above If above 0, only the top "1-prop_above" number of contacts
#' will be shown for each level of type and ethnicity (prop_above = 0.9 shows 
#' the density of top 10% individuals in each ethnicity)  
#' @param log Should a log scale be used on the x-axis
#' @param vec_xlim vector of length 2: boundaries of the x-axis
#' @param cols Vector defining the colour for each ethnicity
#'
figure_density <- function(prediction_populations, prop_above = 0, log = TRUE, 
                           vec_xlim = NULL,cols){
  # Extract the label of each type of simulation
  label_predictions <- unique(prediction_populations$type)
  
  # If using a log axis, set individuals with 0 contact to 0.5
  if(log){
    prediction_populations <- prediction_populations |> 
      mutate(contact = case_when(contact == 0 ~ .5, .default = contact))
      
  }
  
  ## Generate the density plot
  gg <- 
    prediction_populations |> 
    mutate(type = factor(type, label_predictions)) |> 
    group_by(ethnicity_rural, type) |> 
    # Rank observations in each ethnicity and type
    mutate(rank_contact = rank(contact, ties.method = "first")/length(contact)) |> 
    # Filter out the bottom prop_above proportion of individuals
    filter(rank_contact > prop_above) |>
    ggplot(aes(x = contact, fill = ethnicity_rural, col = ethnicity_rural)) + 
    geom_density(alpha = .2) +
    ylab("Density") + xlab("Number of contacts") +
    theme_bw() +
    labs(fill = "ethnicity", col = "ethnicity") + facet_grid(type~.) + 
    scale_fill_manual(values = cols) + scale_color_manual(values = cols)
  if(log) gg <- gg + scale_x_log10()
  if(!is.null(vec_xlim)){
    if(log & min(vec_xlim) <= 0) vec_xlim[1] <- .5
    gg <- gg + coord_cartesian(xlim = vec_xlim)
  }
  return(gg)
}

#' Plot the outbreak trajectories in each group
#'
#' @param y list of named 3-d array, returned by run_outbreaks and 
#' function_run_simulations. Contains the number of individuals in each state, 
#' simulation, strata, and at each time step.
#' @param cols Colour associated with each group
#' @param t Numeric vector of length 2 showing the time span of the plots
#' @param groups List of numeric vectors, indicates the rows of each element 
#' of y that belong to each group. For instance, if groups = list(c(1,2), c(3,4)),
#' the first and second populations in y belong to group 1, the third and fourth 
#' belong to group 2. All figures will show the aggregated number of new cases
#' (or susceptible individuals) in each group.
#' @param ymax_n_i upper limit of the y-axis in panel 1.
#' @param ymax_prop_i upper limit of the y-axis in panel 3.
#' @param verbose If TRUE, print the overall number of cases in the outbreak.
#'
#' @returns Basic R plot with four panels:
#' 1- daily number of infected cases in each level of "groups" per day
#' 2- daily overall number of infected cases per day
#' 3- daily incidence in each level of "groups" per day
#' 4- Number of susceptible individuals in each level of "groups" per day
figure_plot_simulations <- function(y, cols, t, groups, ymax_n_i = NA, 
                                    ymax_prop_i = NA, verbose = TRUE){
  ## Line of code to ensure the plotting function works if there's only 1 particle
  if(is.matrix(y[[1]])) 
    y <- lapply(y, function(x) return(array(x, dim = c(nrow(x), 1, ncol(x)))))
  
  ## Number of inhabitants per strata
  n_pop <- (y$S + y$E + y$I + y$R)[,1,1]
  
  # Only plot 10 of the trajectories
  sample_sim <- sample(seq_len(ncol(y$S)), min(10, ncol(y$S)))
  
  ## Four panels
  # 1- daily number of infected cases in each level of "groups" per day
  # 2- daily overall number of infected cases per day
  # 3- daily incidence  in each level of "groups" per day
  # 4- Number of susceptible individuals in each level of "groups" per day
  par(mfrow = c(2,2), bty = "l", mar = c(3, 4, 3, 0), oma = c(3,1,0,1))
  
  ## Panel 1: daily number of new cases per group
  # start with group 1
  plot_y <- matrix(if(length(groups[[1]]) > 1) y$new_cases[groups[[1]],,] |> colSums() else 
    y$new_cases[groups[[1]],,], ncol = length(t))
  
  matplot(t, t(plot_y[sample_sim,]), type = "l", xlab = "Time", 
          ylab = "new Infected", col = cols[1], lty = 1, ylim = c(0, ymax_n_i))
  # add other groups to the plot
  for(i in 2:length(groups)){
    plot_y <- matrix(if(length(groups[[i]]) > 1) y$new_cases[groups[[i]],,] |> colSums() else 
      y$new_cases[groups[[i]],,], ncol = length(t))
    matplot(t, t(plot_y[sample_sim,]), type = "l", xlab = "Time", 
            ylab = "new Infected", col = cols[i], lty = 1, add = TRUE)
  }
  # add legend
  legend("topright", col = cols, lwd = 2, legend = names(groups), bty = "n")
  
  ## Panel 2: Daily number of new cases in the whole population
  matplot(t, t(matrix(y$new_cases |> colSums(), ncol = length(t))[sample_sim,]), 
          type = "l", xlab = "Time", ylab = "Total new infected", 
          col = "black", lty = 1, 
          ylim = c(0, max(colSums(y$new_cases)) * 1.5))
  
  ## Panel 3: Daily proportion of new cases in each group
  plot_y <- matrix(if(length(groups[[1]]) > 1) y$new_cases[groups[[1]],,] |> colSums() else 
    y$new_cases[groups[[1]],,], ncol = length(t))
  
  # Start with group 1
  matplot(t, t(plot_y[sample_sim,]) / sum(n_pop[groups[[1]]]), type = "l", 
          xlab = "Time", ylab = "new infected, proportion", col = cols[1],
          lty = 1, ylim = c(0, ymax_prop_i), las = 1)
  # Add other groups
  for(i in 2:length(groups)){
    plot_y <- matrix(if(length(groups[[i]]) > 1) y$new_cases[groups[[i]],,] |> colSums() else 
      y$new_cases[groups[[i]],,], ncol = length(t))
    matplot(t, t(plot_y[sample_sim,]) / sum(n_pop[groups[[i]]]), type = "l", 
            xlab = "Time", col = cols[i], lty = 1, add = TRUE)
  }
  
  ## Panel 4: Number of susceptible through time per group
  plot_y <- matrix(if(length(groups[[1]]) > 1) y$S[groups[[1]],,] |> colSums() else 
    y$S[groups[[1]],,], ncol = length(t))
  
  matplot(t, t(plot_y[sample_sim,]), type = "l", xlab = "Time", 
          ylab = "Susceptible", col = cols[1], lty = 1, 
          ylim = c(0, sum(n_pop) * .9), lwd = 2)
  for(i in 2:length(groups)){
    plot_y <- matrix(if(length(groups[[i]]) > 1) y$S[groups[[i]],,] |> colSums() else 
      y$S[groups[[i]],,], ncol = length(t))
    matplot(t, t(plot_y[sample_sim,]), type = "l", xlab = "Time", col = cols[i], 
            lty = 1, add = TRUE, lwd = 2)
  }
  
  # If verbose is TRUE, print the total number of cases and proportion of the population
  # that got infected over the course of the outbreak
  if(verbose){
    n_cases <- sum(n_pop) - colSums(matrix(y$S[,,length(t)], ncol = ncol(y$S)))
    
    print(
      paste0("Overall number of cases: ", round(median(n_cases)), " [",
             round(quantile(n_cases, .025)), ", ", 
             round(quantile(n_cases, .975)), "]"
      ))
    
    print(paste0("Proportion of population infected: ",
                 round(median(n_cases / sum(n_pop)), 3), " [",
                 round(quantile(n_cases / sum(n_pop), .025), 3), ", ",
                 round(quantile(n_cases / sum(n_pop), .975), 3), "]%"))
  }
  title(xlab = "Time", outer = TRUE, line = - 0.5, cex.lab = 1)
  
}

#' Boxplot showing the overall number of cases per group
#'
#' @param y list of named 3-d array, returned by run_outbreaks and 
#' function_run_simulations. Contains the number of individuals in each state, 
#' simulation, strata, and at each time step.
#' @param groups_age List of numeric vectors, indicates the rows of each element 
#' of y that belong to each age group. For instance, if groups = list(c(1,2), c(3,4)),
#' the first and second populations in y belong to the first age group, the 
#' third and fourth to the second age group. 
#' @param groups_eth List of numeric vectors, indicates the rows of each element 
#' of y that belong to each ethnic group. For instance, if groups = list(c(1,2), c(3,4)),
#' the first and second populations in y belong to the first ethnic group, the 
#' third and fourth to the second ethnic group. 
#' @param groups_transmission List of numeric vectors, indicates the rows of each element 
#' of y that belong to each transmitter group. For instance, if groups = list(c(1,2), c(3,4)),
#' the first and second populations in y belong to the first transmitter group, the 
#' third and fourth to the second transmitter group. 
#' @param verbose If TRUE, return the proportion of the population infected over
#' the course of the outbreak in each ethnic group.
#'
#' @returns Six boxplot panels, showing the number and proportion of cases infected
#' by age group, ethnic group, and transmitter group.
figure_plot_total_simulations <- function(y, groups_age, groups_eth, 
                                          groups_transmission, verbose = TRUE){
  ## Line of code to ensure the plotting function works if there's only 1 particle
  if(is.matrix(y[[1]])) 
    y <- lapply(y, function(x) return(array(x, dim = c(nrow(x), 1, ncol(x)))))
  
  ## Number of inhabitants per strata
  n_pop <- (y$S + y$E + y$I + y$R)[,1,1]
  n_time <- dim(y$S)[3]
  
  ## Six panels
  # 1- Overall number of infected by groups_age
  # 2- Overall proportion of infected by groups_age
  # 3- Overall number of infected by groups_eth
  # 4- Overall proportion of infected by groups_eth
  # 5- Overall number of infected by groups_transmission
  # 6- Overall proportion of infected by groups_transmission
  par(mfrow = c(3,2), bty = "l", mar = c(3, 4, 3, 0), oma = c(3,1,0,1))
  
  ## Compute the total number and proportion of individuals infected over the 
  ## course of the outbreak by age group
  # Initialise the matrices
  mat_nb_age <- matrix(nrow = length(groups_age), ncol = ncol(y$new_cases))
  mat_prop_age <- matrix(nrow = length(groups_age), ncol = ncol(y$new_cases))
  for(i in 1:length(groups_age)){
    ## Compute the total number of infected
    mat_nb_age[i,] <- 
      if(length(groups_age[[i]]) > 1) {
        (y$S[groups_age[[i]],,1] - y$S[groups_age[[i]],,n_time]) |> colSums()        
      } else {
        (y$S[groups_age[[i]],,1] - y$S[groups_age[[i]],,n_time])        
      }
    ## Compute the proportion of infected individuals
    mat_prop_age[i,] <- 
      if(length(groups_age[[i]]) > 1) {
        ((y$S[groups_age[[i]],,1] - y$S[groups_age[[i]],,n_time]) |> 
           colSums()) / sum(n_pop[groups_age[[i]]])
      } else {
        (y$S[groups_age[[i]],,1] - y$S[groups_age[[i]],,n_time])/ 
          sum(n_pop[groups_age[[i]]])
      }
  }
  # Set rownames
  rownames(mat_nb_age) <- rownames(mat_prop_age) <- names(groups_age)
  
  ## Do the same as before, by ethnic groups
  mat_nb_eth <- matrix(nrow = length(groups_eth), ncol = ncol(y$new_cases))
  mat_prop_eth <- matrix(nrow = length(groups_eth), ncol = ncol(y$new_cases))
  for(i in 1:length(groups_eth)){
    mat_nb_eth[i,] <- 
      if(length(groups_eth[[i]]) > 1) {
        (y$S[groups_eth[[i]],,1] - y$S[groups_eth[[i]],,n_time]) |> colSums()        
      } else {
        (y$S[groups_eth[[i]],,1] - y$S[groups_eth[[i]],,n_time])        
      }
    mat_prop_eth[i,] <- 
      if(length(groups_eth[[i]]) > 1) {
        ((y$S[groups_eth[[i]],,1] - y$S[groups_eth[[i]],,n_time]) |> 
           colSums()) / sum(n_pop[groups_eth[[i]]])
      } else {
        (y$S[groups_eth[[i]],,1] - y$S[groups_eth[[i]],,n_time])/ 
          sum(n_pop[groups_eth[[i]]])
      }
  }
  rownames(mat_nb_eth) <- rownames(mat_prop_eth) <- names(groups_eth)
  
  ## Do the same as before, by transmitter groups
  mat_nb_transmission <- matrix(nrow = length(groups_transmission), ncol = ncol(y$new_cases))
  mat_prop_transmission <- matrix(nrow = length(groups_transmission), ncol = ncol(y$new_cases))
  for(i in 1:length(groups_transmission)){
    mat_nb_transmission[i,] <- 
      if(length(groups_transmission[[i]]) > 1) {
        (y$S[groups_transmission[[i]],,1] - 
           y$S[groups_transmission[[i]],,n_time]) |> colSums()        
      } else {
        (y$S[groups_transmission[[i]],,1] - y$S[groups_transmission[[i]],,n_time])        
      }
    mat_prop_transmission[i,] <- 
      if(length(groups_transmission[[i]]) > 1) {
        ((y$S[groups_transmission[[i]],,1] - y$S[groups_transmission[[i]],,n_time]) |> 
           colSums()) / sum(n_pop[groups_transmission[[i]]])
      } else{
        (y$S[groups_transmission[[i]],,1] - y$S[groups_transmission[[i]],,n_time])/ 
          sum(n_pop[groups_transmission[[i]]])
      }
  }
  rownames(mat_nb_transmission) <- rownames(mat_prop_transmission) <- names(groups_transmission)
  
  ## Generate boxplots for each group type
  boxplot(t(mat_nb_age), ylim = c(0, max(mat_nb_age) * 1.2))
  boxplot(t(mat_prop_age), ylim = c(0, 1))
  boxplot(t(mat_nb_eth), ylim = c(0, max(mat_nb_eth) * 1.2))
  boxplot(t(mat_prop_eth), ylim = c(0, 1))
  boxplot(t(mat_nb_transmission), ylim = c(0, max(mat_nb_transmission) * 1.2))
  boxplot(t(mat_prop_transmission), ylim = c(0, 1))
  
  title(xlab = "Group", outer = TRUE, line = - 0.5, cex.lab = 2)
  title(ylab = "Number / Proportion of cases", outer = TRUE, line = - 1, 
        cex.lab = 2)
  
  ## If verbose is TRUE, print the proportion of individuals infected by ethnic group
  if(verbose) {
    print(mat_prop_eth |> apply(1, summary) |> round(3) |> t())
    
  }
}
