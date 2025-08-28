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
    filter(model == which_model &
             !group %in% "intercept" & 
             !term %in% "(Intercept)" & 
             !term %in% c("Other", "ethOther", "shape_ethOther")) |> 
    mutate(group = case_when(
      group %in% c("gender", "day_of_the_week", "urban_rural") ~ "others",
      !group %in% c("gender", "day_of_the_week", "urban_rural") ~ group),
      group = factor(group, levels = c("age", "ethnicity", "ethnicity_rural", 
                                       "income", "shape", "household", "others"))
    )
  
  if(is.null(filter_group)) filter_group <- unique(dt_coef$group)
  
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
    guides(col="none") + 
    theme(strip.text = element_blank()) +
    xlab("Coefficient") + ylab("Value")
  
}

