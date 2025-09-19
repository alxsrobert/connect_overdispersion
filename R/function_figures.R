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
#' @param which_model Model to plot
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
             "20000-40000", "Female", "Male", "weekday", "0-4", "5-9",
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
      model = which_model, term = "20000-40000", estimate = 1, conf.low = 1, 
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
    mutate(group = case_when(
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
    theme_bw() + 
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
    unique(c("hh_size: One", "White_Urban", "Employed", "Lessthan20000", 
             "20000-40000", "Female", "Male", "weekday", "0-4", "5-9",
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
      model = which_model, term = "20000-40000", estimate = 1, conf.low = 1, 
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
    "Overdispersion\n (shape)"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "household"] <- 
    "Household\n size"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "ethnicity_rural"] <- 
    "Ethnicity\nurban / rural"
  levels(dt_coef_plot$group)[levels(dt_coef_plot$group) == "gender"] <- 
    "Gender"
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
    xlim(c(0.1, 1.9)) + 
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

