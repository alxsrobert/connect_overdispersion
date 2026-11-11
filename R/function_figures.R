#' Show all parameters from one model (including reference levels)
#'
#' @param list_regression list of regression object
#' @param which_model Model(s) to plot
#' @param filter_group Group(s) to plot (if set to NULL, all groups are plotted),
#' set of possible groups: "intercept", "age", "ethnicity_rural", "hiqual",
#' "employment", "household", "shape", "others"
#'
#' @return ggplot object
figure_parameter_model <- function(list_regression, which_model, filter_group = NULL){
  # Create a tibble containing all coefficient estimates and CIs for all models
  dt_coef <- clean_list_regression_output(list_regression)
  lev_ref <- 
    unique(c("hh_size: One", "White_Urban", "Employed", "Female", "Male", "Level1",
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
      model = which_model, term = "Level1", estimate = 1, conf.low = 1, 
      conf.high = 1, group = "hiqual"),
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
                                       "hiqual", "shape", "household", "others"))
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
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "hiqual"] <- 
    "Highest qualification"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "shape"] <- 
    "Dispersion parameter"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "household"] <- 
    "Household size"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "others"] <- 
    "Others"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "ethnicity_rural"] <- 
    "Ethnicity and urban/rural status"

  levels(dt_coef_plot$term) <- gsub("_Rural", " and\n Rural", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("_Urban", " and\n Urban", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("hh_size: ", "", levels(dt_coef_plot$term))
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
#' set of possible groups: "intercept", "age", "ethnicity_rural", "hiqual",
#' "employment", "household", "shape", "others"
#'
#' @return forest plot of the parameter estimates
figure_forest_plot <- function(list_regression, which_model){
  # Create a tibble containing all coefficient estimates and CIs for all models
  dt_coef <- clean_list_regression_output(list_regression)
  # Extract the terms from the regression, putting the reference levels first
  lev_ref <- 
    unique(c("hh_size: One", grepv("_Urban", levels(dt_coef$term)), "White_Urban",
             "Employed", "Female", "Male", "weekday", "0-4", "5-9", "10-14", 
             "15-17", "18-24", levels(dt_coef$term)))
  
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
      model = which_model, term = "Level1", estimate = 1, conf.low = 1,
      conf.high = 1, group = "hiqual"),
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
                                       "hiqual", "shape", "household", "gender",
                                       "day_of_the_week"))
    )
  # Set ethnicity rural and shape as the top panels
  dt_coef_plot$group <- relevel(dt_coef_plot$group, "shape")
  dt_coef_plot$group <- relevel(dt_coef_plot$group, "ethnicity_rural")
  
  # Rename covariates
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "age"] <- "Age group"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "employment"] <- 
    "Employment\n status"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "hiqual"] <- 
    "Highest\n qualification"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "shape"] <- 
    "Ethnicity\nurban/rural\n(dispersion)"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "household"] <- 
    "Household\n size"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "ethnicity_rural"] <- 
    "Ethnicity\nurban/rural\n(mean)"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "gender"] <- 
    "Gender"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "day_of_the_week"] <- 
    "Day"
  # Rename coefficient labels
  levels(dt_coef_plot$term) <- gsub("_Rural", " and Rural", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("_Urban", " and Urban", levels(dt_coef_plot$term))
  levels(dt_coef_plot$term) <- gsub("hh_size: ", "", levels(dt_coef_plot$term))
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
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_color_manual(
      values = c("red", "red", rep("grey50", length(unique(dt_coef_plot$group))))) + 
    labs(
      x = "Coefficient (Estimate)",
      y = NULL,
      title = ""
    ) +
    xlim(.5, 2)+
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
  if(!any(colnames(prediction_populations) == "iter")) 
    prediction_populations$iter <- 1
  
  # Extract the population size
  pop_size_i <- 
    table(paste0(prediction_populations$ethnicity_rural, 
                 prediction_populations$type, prediction_populations$iter))[1]
  # Extract the label of each type of simulation
  label_predictions <- unique(prediction_populations$type)
  
  ## Generate the barplot
  prediction_populations |>
    # Shift type to a factor
    mutate(type = factor(type, label_predictions)) |>
    # Cut contact into categories (defined by breaks and label_break)
    mutate(group_contact = cut(contact, breaks = breaks, labels = label_breaks),
           ethnicity_rural = gsub("_U.*", "", ethnicity_rural),
           ) |>
    group_by(ethnicity_rural, type, iter, group_contact) |> 
    summarise(prop = n() / pop_size_i * 100, .groups = "drop") |> 
    group_by(ethnicity_rural, type, group_contact) |> 
    summarise(med = median(prop),
              hi = quantile(prop, prob = .975), 
              low = quantile(prop, prob = .025), .groups = "drop") |> 
    ggplot(aes(x = group_contact, fill = ethnicity_rural, y = med)) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.7) +
    geom_errorbar(aes(ymin = low, ymax = hi),
                  position = position_dodge(width = 0.7),
                  width = 0.4
    ) + 
    labs(
      x = "Number of contacts",
      y = "Proportion (%)"
    ) +
    labs(fill = "") + 
    facet_grid(~type) + 
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
    # Empirical PMF: probability mass at each integer contact value
    count(ethnicity_rural, type, contact, name = "n") |>
    ungroup() |>
    complete(
      ethnicity_rural, type,
      contact = c(0.5, 1:500),
      fill = list(n = 0)
      ) |>
    group_by(ethnicity_rural, type) |>
    # Convert to probability mass and then cumulative
    mutate(
      prob = n / sum(n),
      cum_prob = cumsum(prob)
      ) |>
    ungroup() |>
    mutate(cum_prob = case_when(cum_prob > 0.999 ~ 1, .default = cum_prob)) |> 
    ggplot(aes(x = contact, col = ethnicity_rural, fill = ethnicity_rural)) +
    geom_line(aes(y = 1 - cum_prob), lwd = .5) +
    ylab("Probability") +
    xlab("Number of contacts") +
    labs(fill = "ethnicity", col = "ethnicity") +
    facet_wrap(type ~ ., ncol = 1) +
    scale_fill_manual(values = cols) +
    scale_color_manual(values = cols) +
    coord_cartesian(ylim = c(0, .25), xlim = c(5, 200))
  
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
#' @param prop_cases_only if TRUE, only return the plot showing the daily 
#' incidence in each level of "groups"
#' @param with_lab Boolean indicating whether to add a y label, can be set to 
#' FALSE if prop_cases_only is TRUE so an outer label can be added after using 
#' the function
#'
#' @returns Basic R plot with four panels:
#' 1- daily number of infected cases in each level of "groups"
#' 2- daily overall number of infected cases
#' 3- daily incidence in each level of "groups"
#' 4- Daily number of susceptible individuals in each level of "groups"
figure_plot_simulations <- function(y, cols, t, groups, ymax_n_i = NA, 
                                    ymax_prop_i = NA, verbose = TRUE, 
                                    prop_cases_only = FALSE, with_lab = TRUE){
  ## Line of code to ensure the plotting function works if there's only 1 particle
  if(is.matrix(y[[1]])) 
    y <- lapply(y, function(x) return(array(x, dim = c(nrow(x), 1, ncol(x)))))

  y$S <- y$S[,y$new_cases |> colSums() |> rowSums() > 1000,]
  y$E <- y$E[,y$new_cases |> colSums() |> rowSums() > 1000,]
  y$I <- y$I[,y$new_cases |> colSums() |> rowSums() > 1000,]
  y$R <- y$R[,y$new_cases |> colSums() |> rowSums() > 1000,]
  y$new_cases <- y$new_cases[,y$new_cases |> colSums() |> rowSums() > 1000,]
    
  ## Number of inhabitants per strata
  n_pop <- (y$S + y$E + y$I + y$R)[,1,1]
  # Only plot 5 of the trajectories
  sample_sim <- sample(seq_len(ncol(y$S)), min(5, ncol(y$S)))
  
  # Restrict to t
  y$S <- y$S[,,t]
  y$new_cases <- y$new_cases[,,t]

  ## Four panels
  # 1- daily number of infected cases in each level of "groups" per day
  # 2- daily overall number of infected cases per day
  # 3- daily incidence  in each level of "groups" per day
  # 4- Number of susceptible individuals in each level of "groups" per day
  if(!prop_cases_only){ 
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
  }
  
  ## Panel 3: Daily proportion of new cases in each group
  plot_y <- matrix(if(length(groups[[1]]) > 1) y$new_cases[groups[[1]],,] |> colSums() else 
    y$new_cases[groups[[1]],,], ncol = length(t))
  
  # Start with group 1
  matplot(t, t(plot_y[sample_sim,]) / sum(n_pop[groups[[1]]]), type = "l", 
          xlab = "Time", ylab = if(with_lab) "new infected, proportion" else "", 
          col = cols[1], lty = 1, ylim = c(0, ymax_prop_i), las = 1)
  # Add other groups
  for(i in 2:length(groups)){
    plot_y <- matrix(if(length(groups[[i]]) > 1) y$new_cases[groups[[i]],,] |> colSums() else 
      y$new_cases[groups[[i]],,], ncol = length(t))
    matplot(t, t(plot_y[sample_sim,]) / sum(n_pop[groups[[i]]]), type = "l", 
            xlab = "Time", col = cols[i], lty = 1, add = TRUE)
  }
  
  if(prop_cases_only){
    # add legend
    legend("topright", col = cols, lwd = 2, legend = names(groups), bty = "n")
  }
  
  if(!prop_cases_only){
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
figure_plot_total_simulations <- function(
    y, groups_age, groups_eth, groups_transmission, verbose = TRUE,
    cols_ethnicity = c("#002973", "#ffdd00", "#d53880", "black", "#afb2b4"),
    cols_age = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a"),
    cols_transmission = c("#1b9e77", "#d95f02", "#c2a5cf", "#7b3294")){
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
  mat_nb_age <- mat_nb_age[, which(colSums(mat_nb_transmission) > 1000)]
  mat_nb_eth <- mat_nb_eth[, which(colSums(mat_nb_transmission) > 1000)]
  mat_nb_transmission <- mat_nb_transmission[, which(colSums(mat_nb_transmission) > 1000)]
  
  ## Generate boxplots for each group type
  boxplot(t(mat_nb_age), ylim = c(0, max(mat_nb_age) * 1.2), border = cols_age,
          outline = FALSE)
  boxplot(t(mat_prop_age), ylim = c(0, 1), border = cols_age, outline = FALSE)
  boxplot(t(mat_nb_eth), ylim = c(0, max(mat_nb_eth) * 1.2), border = cols_ethnicity,
          outline = FALSE)
  title(ylab = "Number of cases", outer = FALSE, line = 2, cex.lab = 1.5)
  boxplot(t(mat_prop_eth), ylim = c(0, 1), border = cols_ethnicity, outline = FALSE)
  title(ylab = "Attack rate", outer = FALSE, line = 2, cex.lab = 1.5)
  boxplot(t(mat_nb_transmission), ylim = c(0, max(mat_nb_transmission) * 1.2),
          border = cols_transmission, outline = FALSE)
  boxplot(t(mat_prop_transmission), ylim = c(0, 1), border = cols_transmission,
          outline = FALSE)
  
  
  ## If verbose is TRUE, print the proportion of individuals infected by ethnic group
  if(verbose) {
    print(mat_prop_eth |> apply(1, summary) |> round(3) |> t())
    
  }
}

#' Extract the parameter estimates from a list of regression objects
#'
#' @param list_regression List of named bayesian regression objects
#'
#' @return Tibble containing 6 columns: model name, name of coefficient 
#' (column `term`), model estimate, lower and upper bound of the 95\% credible 
#' interval, and group of coefficients (e.g. the group of `25-29` is `age`).
clean_list_regression_output <- function(list_regression){
  
  ## Tidy outputs from the set of regression analysis:
  ## Extract 95% CI for each parameter
  df_all_results <- 
    lapply(list_regression, function(X){
      tidy(X, conf.int = TRUE, conf.level = 0.95, exponentiate = T) |> 
        select(term, estimate, conf.low, conf.high)
    }) |> bind_rows(.id = "model")
  
  ## For models without ethnicity-specific dispersion parameter, add the 
  ## estimated dispersion as shape_intercept
  df_all_results <- 
    rbind.data.frame(
      df_all_results,
      lapply(list_regression[grepl(pattern = "no_od", x = names(list_regression))],
             function(X){
               data.frame(
                 term = "shape_(Intercept)",
                 estimate = summary(X)$spec_pars$Estimate,
                 conf.low = summary(X)$spec_pars$`u-95% CI`,
                 conf.high = summary(X)$spec_pars$`l-95% CI`
               )
             }
      ) |> bind_rows(.id = "model")
    )
  
  ## Group each coefficient into covariates
  ## Re-format the label of each coefficient
  df_all_results <-  
    df_all_results |> 
    mutate(term = gsub("p_hiqualchild_", "p_age_group", term)) |> 
    mutate(
      group = case_when(
        grepl("shape", term) ~ "shape",
        grepl("Intercept", term) ~ "intercept",
        grepl("p_age_group", term) ~ "age",
        grepl("p_gender", term) ~ "gender",
        (grepl("ethnicity_rural", term) & !grepl("shape", term)) ~ "ethnicity_rural",
        grepl("p_urban_rural", term) ~ "urban_rural",
        grepl("day_week", term) ~ "day_of_the_week",
        grepl("employ", term) ~ "employment",
        grepl("p_hiqual", term) ~ "hiqual",
        grepl("household", term) ~ "household"),
      term = case_when(
        grepl("p_age_group", term) ~ gsub(pattern = "p_age_group", "", gsub("M", "-", term)),
        grepl("p_gender", term) ~ gsub(pattern = "p_gender", "", term),
        grepl("p_urban_rural", term) ~ gsub(pattern = "p_urban_rural", "", term),
        grepl("day_week", term) ~ gsub(pattern = "day_week", "", term),
        grepl("p_hiqual", term) ~ gsub(pattern = "p_hiqual|p_hiqual_", "", gsub("M", "_", term)),
        grepl("cat_household", term) ~ gsub(pattern = "cat_household_members", "hh_size: ", term),
        grepl("household", term) ~ "household_linear",
        grepl("employ", term) ~ gsub(pattern = "employ_", "", term),
        grepl("ethnicity_rural", term) ~ gsub(pattern = "ethnicity_rural", "", term),
        grepl("Intercept", term) ~ term),
      term = gsub(pattern = "shape_", "", term)
    )
  
  
  ## Set reference levels
  df_all_results$term <- factor(df_all_results$term)
  df_all_results$term <- relevel(df_all_results$term, ref = "(Intercept)")
  if(any(df_all_results$term == "5-9"))
    df_all_results$term <- relevel(df_all_results$term, ref = "5-9")
  if(any(df_all_results$term == "0-4"))
    df_all_results$term <- relevel(df_all_results$term, ref = "0-4")
  
  if(any(df_all_results$term == "hh_size: Morethan4", na.rm = TRUE)){
    df_all_results$term <- relevel(df_all_results$term, ref = "hh_size: Morethan4")
    df_all_results$term <- relevel(df_all_results$term, ref = "hh_size: Four")
    df_all_results$term <- relevel(df_all_results$term, ref = "hh_size: Three")
    if(any(df_all_results$term == "hh_size: Two"))
      df_all_results$term <- relevel(df_all_results$term, ref = "hh_size: Two")
  }
  
  return(df_all_results)
}


#' Figure with four quadrants showing the parameter estimates from the regression
#' models
#' @param model List of named bayesian regression objects (bel18 and ov18)
#' @param ov18 Boolean, if TRUE, show parameter estimates in the ov18 model, otherwise
#' show the parameter estimates in bel18
#' @param viz_mu1 boolean, if TRUE, show the covariates on the mu1 / mu2 axes 
#' (typically that would be household size), otherwise, show the covariates on 
#' the pi / mu2 axis
#' @param which_cov Optional, vector containing the names of the covariates
#' that should be printed. If NULL, all covariates are printed,
#' @param label optional, boolean indicating whether to show the quadrant labels.
#' 
#'
#' @return Figure with four quadrants showing the parameter estimates from the regression
#' models
show_regression_params <- function(model, ov18, viz_mu1, which_cov = NULL, 
                                   label = TRUE){
  # Extract coefficients
  if(ov18){
    coef_model <- fixef(model$ov18)
  } else coef_model <- fixef(model$bel18)
  
  # Define axis limits
  x_min <- -2.2; x_max <- 2.2
  y_min <- -1.8; y_max <- 1.8

  ## Extract coefficient estimates by covariate
  coef_model <- 
    cbind.data.frame(param = rownames(coef_model), coef_model, row.names = NULL)
  
  covariate_patterns <- c(
    "cat_household_members",
    "p_age_group",
    "p_gender",
    "day_week",
    "employ_",
    "hiqual_",
    "ethnicity_rural"
  )
  # Add reference level for every covariate
  coef_model <- coef_model |>
    rbind.data.frame(
      list(param = "mu1_cat_household_membersOne (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu1_cat_household_membersTwo (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu2_p_age_group18M24 (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu2_p_age_group0M4 (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu2_p_genderFemale (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu2_day_weekweekday (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu2_ethnicity_ruralWhite_Urban (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu2_cat_household_membersOne (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu2_cat_household_membersTwo (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu2_employ_Employed (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "mu2_hiqual_Level1 (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "theta2_p_age_group18M24 (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "theta2_p_age_group0M4 (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "theta2_p_genderFemale (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "theta2_day_weekweekday (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "theta2_hiqual_Level1 (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "theta2_employ_Employed (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0),
      list(param = "theta2_ethnicity_ruralWhite_Urban (ref)", Estimate = 0, Est.Error = 0, Q2.5 = 0, Q97.5 = 0)
    ) |> 
    ## Extract component
    mutate(
      distpar = str_extract(param, "^(mu1|mu2|theta2)"),
      remainder = str_remove(param, "^(mu1|mu2|theta2)_"),
      covariate = case_when(
        ## Extract intercept
        remainder == "Intercept" ~ "Intercept",
        TRUE ~ map_chr(remainder, function(x) {
          hit <- covariate_patterns[str_starts(x, covariate_patterns)]
          if (length(hit) == 0) NA_character_ else hit[1]
        })
      ),
      level = if_else(
        covariate == "Intercept",
        "Intercept",
        str_remove(remainder, covariate)
      )
    ) |>
    select(-remainder)
  
  if(!ov18) {
    ## Remove the employ and highest qualification covariates in <18
    coef_model <- coef_model |> 
      filter(!grepl("employ", covariate), !grepl("hiqual", covariate),
             !level %in% c("One (ref)", "18M24 (ref)"))
  } else {
    coef_model <- coef_model |> 
      filter(!level %in% c("Two (ref)", "0M4 (ref)"))
  }
  
  ## Reshape so that mu1, mu2, theta2 estimates (and their CrIs)
  ## for the *same covariate level* sit on the same row, to plot one
  ## distributional parameter against another.
  
  wide <- coef_model |>
    select(distpar, covariate, level, Estimate, Q2.5, Q97.5) |>
    mutate(
      Estimate = exp(Estimate),
      Q2.5 = exp(Q2.5),
      Q97.5 = exp(Q97.5),
    ) |> 
    pivot_wider(
      names_from  = distpar,
      values_from = c(Estimate, Q2.5, Q97.5),
      names_glue  = "{distpar}_{.value}"
    )
  
  if(is.null(which_cov)) which_cov <- unique(wide$covariate)
  
  ## Decide x/y pair per row: prefer mu1 vs mu2; if mu1 is missing
  ## for that covariate (e.g. age/gender/day/ethnicity only model mu2 &
  ## theta2), use mu2 vs theta2 instead.
  wide <- wide |>
    mutate(
      has_mu1 = !is.na(mu1_Estimate),
      x_est   = if_else(has_mu1, true = mu1_Estimate, false = theta2_Estimate),
      x_lo    = if_else(has_mu1, true = mu1_Q2.5,     false = theta2_Q2.5),
      x_hi    = if_else(has_mu1, true = mu1_Q97.5,    false = theta2_Q97.5),
      x_lab   = if_else(has_mu1, true = "mu1", false = "theta2"),
      y_est   = mu2_Estimate,
      y_lo    = mu2_Q2.5,
      y_hi    = mu2_Q97.5,
      y_lab   = "mu2"
    ) |>
    filter(!is.na(x_est) & !is.na(y_est))   # drop rows where the pairing has no data
  
  ## Nice facet labels
  wide <- wide |> 
    mutate(
      area_type = case_when(
        str_detect(level, regex("urban", ignore_case = TRUE)) ~ "Urban",
        str_detect(level, regex("rural", ignore_case = TRUE)) ~ "Rural",
        TRUE ~ NA_character_
      ),
      level_short = level %>%
        str_remove(regex(",?\\s*urban", ignore_case = TRUE)) %>%
        str_remove(regex(",?\\s*rural", ignore_case = TRUE)) %>%
        str_trim()
    ) |> filter(covariate != "Intercept",
                level %notin% c("Other", "NonWhite_Rural")) |>
    mutate(
      ## Rename covariates
      covariate_label = recode(covariate,
                               "cat_household_members" = "Household size",
                               "p_age_group"            = "Age group",
                               "p_gender"               = "Gender",
                               "day_week"               = "Weekday",
                               "hiqual_"                = "Highest qualification",
                               "employ_"                = "Employment status",
                               "ethnicity_rural"        = "Ethnicity / rurality"
      ),
      ## Rename levels
      level = recode(level,
                     "Morethan4" = "More than\nFour",
                     "0M4 (ref)" = "0 to 4 (ref)",
                     "5M9" = "5 to 9",
                     "10M14" = "10 to 14",
                     "15M17" = "15 to 17",
                     "18M24 (ref)" = "18 to 24 (ref)",
                     "25M29" = "25 to 29",
                     "30M39" = "30 to 39",
                     "40M49" = "40 to 49",
                     "50M59" = "50 to 59",
                     "60M69" = "60 to 69",
                     "70M93" = ">70",
                     "weekday" = "Weekday",
                     "unemployed" = "Unemployed",
                     "Long_termsickordisabled" = "Sick, disabled",
                     "Lookingafterhomeorfamily" = "Home",
                     "Noquali" = "0",
                     "Level1 (ref)" = "1 (ref)",
                     "Level2" = "2",
                     "Level3" = "3",
                     "Level4" = "4",
                     "Apprenti" = "Apprentice",
                     "Asian_Urban" = "Asian",
                     "Black_Urban" = "Black",
                     "Mixed_Urban" = "Mixed",
                     "NonWhite_Rural" = "Non-white",
                     "Other_Urban" = "Other",
                     "White_Rural" = "White",
                     "White_Urban (ref)" = "White (ref)"
      ),
      axis_label = paste0(x_lab, " vs ", y_lab)
    )
  
  area_colours <- c("Urban" = "#1B9E77", "Rural" = "#D95F02")
  
  ## Set ethnicity / rurality as the first panel if applicable
  if(viz_mu1){
    wide <- wide |> filter(has_mu1) 
    which_row <- which(wide$covariate %in% which_cov)
    wide <- wide |> filter(covariate %in% which_cov)
  } else {
    wide <- wide |> filter(!has_mu1)
    which_row <- which(wide$covariate %in% which_cov)
    wide <- wide |> filter(covariate %in% which_cov) |> 
      mutate(covariate_label = factor(covariate_label))
    if(any(wide$covariate_label == "Ethnicity / rurality")) {
      wide <- wide |> 
      mutate(
        covariate_label = relevel(factor(covariate_label), ref = "Ethnicity / rurality")
      )
    } else {
      wide <- wide |> 
        mutate(
          covariate_label = relevel(factor(covariate_label), ref = "Gender")
        )
    }
  }
  
  ## small data frame of quadrant rectangles, one set per facet panel
  
  quadrant_rects <- tibble::tribble(
    ~quadrant,        ~xmin,  ~xmax, ~ymin,  ~ymax,   ~fill,
    "top_right",        0,    x_max,  0,     y_max,  "#4DAF4A",
    "top_left",       x_min,   0,     0,     y_max,  "#377EB8",
    "bottom_right",     0,    x_max, y_min,  0,       "#FF7F00",
    "bottom_left",    x_min,   0,    y_min,  0,       "#E41A1C"
  ) |> 
    mutate(
      xmin = exp(xmin),
      xmax = exp(xmax),
      ymin = exp(ymin),
      ymax = exp(ymax)
    )
  
  ## Expand across all facet levels present in `wide`
  quadrant_rects_all <- quadrant_rects %>%
    tidyr::crossing(covariate_label = unique(wide$covariate_label))
  
  ## Quadrant text labels
  if(viz_mu1){
    quadrant_labels <- tibble::tribble(
      ~x,              ~y,             ~hjust, ~vjust, ~label,
      x_max * 0.98,    y_max * 0.95,   1,      1,      "Higher mean in low-mean\nHigher mean in high-mean",
      x_min * 0.98,    y_max * 0.95,   0,      1,      "Lower mean in low-mean\nHigher mean in high-mean",
      x_max * 0.98,    y_min * 0.95,   1,      0,      "Higher mean in low-mean\nLower mean in high-mean",
      x_min * 0.98,    y_min * 0.95,   0,      0,      "Lower mean in low-mean\nLower mean in high-mean"
    ) %>%
      mutate(covariate_label = "Household size")   # matches the top-left panel's facet label
  } else {
    quadrant_labels <- tibble::tribble(
      ~x,              ~y,             ~hjust, ~vjust, ~label,
      x_max * 0.98,    y_max * 0.95,   1,      1,      "More individuals in high-mean\nHigher mean in high-mean",
      x_min * 0.98,    y_max * 0.95,   0,      1,      "Fewer individuals in high-mean\nHigher mean in high-mean",
      x_max * 0.98,    y_min * 0.95,   1,      0,      "More individuals in high-mean\nLower mean in high-mean",
      x_min * 0.98,    y_min * 0.95,   0,      0,      "Fewer individuals in high-mean\nLower mean in high-mean"
    ) %>%
      mutate(covariate_label = factor(levels(wide$covariate_label)[1], 
                                      levels = levels(wide$covariate_label)))   # matches the top-left panel's facet label
  }
  if(!label) quadrant_labels$label <- ""
  quadrant_labels <- quadrant_labels |> 
    mutate(
      x = exp(x),
      y = exp(y)
    )
  
  if(ov18 & !viz_mu1){
    # shift label start position before repelling
    vec_nudge_x = c(-1, .5, -.5, -.2, -1, 1, -1, 1, -1, 0, -1, -.2, -.5, -.1, -.5, 1, 1.5, 
                    -1, -1, 1, -.5, 1, .2, 1, .2, .2, 1, 2, -1)[which_row]
    # shift label start position before repelling
    vec_nudge_y = c(-.5, -1, 1, -.8, -.1, .2, -.5, -.5, .5, -1, .2, .2, -1, -1, 1, .5, -.2,
                    .5, .5, .5, -1, .5, .2, 1, .2, .2, .3, -0.5, -1)[which_row]
    vec_nudge_x[wide$level == "Asian"] <- -1
    vec_nudge_y[wide$level == "Asian"] <- -.5
    vec_nudge_x[wide$level == "White (ref)"] <- -.5
    vec_nudge_y[wide$level == "White (ref)"] <- 1
    vec_nudge_x[wide$level == "Mixed"] <- 1
    vec_nudge_y[wide$level == "Mixed"] <- .5
    vec_nudge_x[wide$level == "White"] <- -1
    vec_nudge_y[wide$level == "White"] <- .7
    vec_nudge_x[wide$level == "Other"] <- -.3
    vec_nudge_y[wide$level == "Other"] <- -.8
    vec_nudge_x[wide$level == "Black"] <- .5
    vec_nudge_y[wide$level == "Black"] <- -.5
    
  } else {
    vec_nudge_x <- rep(0, nrow(wide))
    vec_nudge_y <- rep(0, nrow(wide))
    
    if(!viz_mu1){ 
      vec_nudge_x[wide$level == "Asian"] <- -1
      vec_nudge_y[wide$level == "Asian"] <- -.5
      vec_nudge_x[wide$level == "White (ref)"] <- .5
      vec_nudge_y[wide$level == "White (ref)"] <- 1
      vec_nudge_x[wide$level == "Mixed"] <- .5
      vec_nudge_y[wide$level == "Mixed"] <- .8
      vec_nudge_x[wide$level == "White"] <- -1
      vec_nudge_y[wide$level == "White"] <- 1
      vec_nudge_x[wide$level == "Other"] <- .5
      vec_nudge_y[wide$level == "Other"] <- -1
      vec_nudge_x[wide$level == "Black"] <- .5
      vec_nudge_y[wide$level == "Black"] <- -.5
    } else {
      vec_nudge_x[wide$level == "Three"] <- -.2
      vec_nudge_y[wide$level == "Three"] <- -.75
      vec_nudge_x[wide$level == "Four"] <- -.2
      vec_nudge_y[wide$level == "Four"] <- .75
      vec_nudge_x[wide$level == "More than\nFour"] <- .2
      vec_nudge_y[wide$level == "More than\nFour"] <- -.75
    }
  }
  vec_nudge_x <- vec_nudge_x / 2.5
  vec_nudge_y <- vec_nudge_y / 2.5
  
  ## Plot: one panel per covariate, mean point + horizontal & vertical 95% CrI 
  ## error bars, with a text label naming the parameter level next to each point.
  
  p <- 
    ggplot(wide) +
    facet_wrap(~ covariate_label) +
    geom_rect(
      data = quadrant_rects_all,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      alpha = 0.12, colour = NA, show.legend = FALSE
    ) +
    scale_fill_identity() +
    coord_cartesian(xlim = exp(c(x_min*.9, x_max*.9)), ylim = exp(c(y_min*.9, y_max*.9))) +
    geom_errorbar(
      aes(x = x_est, ymin = y_lo, ymax = y_hi, colour = area_type), width = 0, # colour = "grey40", 
      lwd = .2) +
    geom_errorbar(
      aes(y = y_est, xmin = x_lo, xmax = x_hi, colour = area_type), height = 0, # colour = "grey40", 
      lwd = .2) +
    geom_point(aes(x = x_est, y = y_est, colour = area_type), size = 1.5) +
    ggrepel::geom_text_repel(
      aes(x = x_est, y = y_est, label = level),
      size = 3.2,
      box.padding = 1,
      point.padding = 0.1,
      force = 2,
      max.time = 50,
      segment.linetype = 2,
      segment.size = .2,
      nudge_y = vec_nudge_y,     # shift label start position before repelling
      nudge_x = vec_nudge_x,     # shift label start position before repelling
      min.segment.length = 0,     # always draw the connecting line segment
      max.overlaps = Inf,         # avoid dropping labels that can't fit
      seed = 2
    ) +
    geom_text(
      data = quadrant_labels,
      aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      size = 3, colour = "grey20", lineheight = 0.9, fontface = "italic"
    ) +
    scale_colour_manual(
      values = area_colours, na.value = "black", breaks = c("Urban", "Rural"),
      name = NULL) +
    scale_x_log10() + scale_y_log10() + 
    labs(
      x = ifelse(viz_mu1, "Number of contacts in low-mean component\n(Ratio of mean contacts)", 
                 "Proportion of individuals in high-mean component (Odds Ratio)"),
      y = "Number of contacts in high-mean component\n(Ratio of mean contacts)"
    ) +
    theme(
      legend.position = "right", 
      legend.justification = "left", 
      legend.box.spacing = unit(0, "pt"),
      legend.margin = margin(0,0,0,0),
      legend.key = element_blank(), 
      plot.margin = margin(0, 2, 2, 2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    ) 
  
  return(p)
}

#' Figure showing the proportion of simulated individuals with at least X contacts
#' 
#' @param prediction_pop dataframe in which each row corresponds to an 
#' individual in a synthetic population, and three columns: "ethnicity_rural",
#' the ethnicity of an individual, "contact" the simulated number of contact of that 
#' individual, and "type" the type of simulation process that was used to simulate
#' the number of contacts
#' @param cols_ethnicity Vector defining the colour for each ethnicity
#' @param high_cut optional, numeric value, end of the x-axis
#' 
#'
#' @return Figure showing the proportion of simulated individuals with at least X contacts
dist_simulated_pop <- function(prediction_pop, cols_ethnicity, high_cut = 100){
  p_ecdf_C <-
    prediction_pop |>
    reframe(
      threshold = 0:high_cut,
      nb = vapply(
        threshold,
        function(k) sum(contact >= k),
        integer(1)
      ),
      prop = nb / n() * 100,
      .by = c(iter, ethnicity_rural, type)
    ) |>
    select(contact = threshold, iter, nb, prop, ethnicity_rural, type) |> 
    group_by(ethnicity_rural, type, contact) |>
    summarise(
      med = median(prop),
      low = quantile(prop, 0.25),
      high = quantile(prop, 0.75)
    ) |> 
    ungroup() |> 
    filter(type != "population") |> 
    # Remove 0s (log scale) and rename ethnicity_rural
    mutate(contact = case_when(contact == 0 ~ .5, .default = contact),
           ethnicity_rural = gsub("_U", ", u", ethnicity_rural)) |>
    filter(contact >= 1, contact <= high_cut) |> 
    ## Generate the figure
    ggplot(aes(x = contact, ymin = low, ymax = high,
               y = med, fill = ethnicity_rural, alpha = ethnicity_rural)) +
    geom_ribbon() +
    geom_line(aes(colour = ethnicity_rural), lwd = .1, alpha = 1) +
    facet_grid( ~ type, scales = "free") +
    scale_y_log10() +
    scale_alpha_manual(values = c(.7, .7, .7, .7, .7)) +
    scale_color_manual(values = cols_ethnicity) +
    scale_fill_manual(values = cols_ethnicity) +
    guides(fill = guide_legend(override.aes = list(alpha = .7)),
           col = "none") +
    labs(
      x = "Number of contacts",
      y = "Proportion with more than x contacts",
      colour = "Scenario"
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.minor = element_blank())  
  
  return(p_ecdf_C)
}

