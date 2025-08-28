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
#' set of possible groups: "intercept", "age", "gender", "ethnicity_rural",
#' "day_of_the_week", "income","employment", "household", "shape".
#'
#' @return ggplot object
figure_parameter_model <- function(list_regression, which_model, filter_group = NULL){
  # Create a tibble containing all coefficient estimates and CIs for all models
  dt_coef <- clean_list_regression_output(list_regression)
  
  # Create reference levels for each variable (would be better to automatise this)
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
  
  # Re-assign reference levels
  dt_coef$term <- relevel(dt_coef$term, ref = "hh_size: One")
  dt_coef$term <- relevel(dt_coef$term, ref = "weekday")
  dt_coef$term <- relevel(dt_coef$term, ref = "weekend")
  dt_coef$term <- relevel(dt_coef$term, ref = "Female")
  dt_coef$term <- relevel(dt_coef$term, ref = "Male")
  dt_coef$term <- relevel(dt_coef$term, ref = "20000-40000")
  dt_coef$term <- relevel(dt_coef$term, ref = "Lessthan20000")
  dt_coef$term <- relevel(dt_coef$term, ref = "18-24")
  dt_coef$term <- relevel(dt_coef$term, ref = "15-17")
  dt_coef$term <- relevel(dt_coef$term, ref = "10-14")
  dt_coef$term <- relevel(dt_coef$term, ref = "5-9")
  dt_coef$term <- relevel(dt_coef$term, ref = "0-4")
  
  if(is.null(filter_group)) filter_group <- unique(dt_coef$group)
  # Generate figure after removing Intercept values
  dt_coef |>
    filter(model == which_model &
             !group %in% "intercept" & 
             !term %in% "(Intercept)" & 
             !term %in% c("Other", "ethOther", "shape_ethOther")) |> 
    filter(group %in% filter_group) |>
    mutate(group = case_when(
      group %in% c("gender", "day_of_the_week", "urban_rural") ~ "others",
      !group %in% c("gender", "day_of_the_week", "urban_rural") ~ group),
      group = factor(group, levels = c("age", "ethnicity", "ethnicity_rural", 
                                       "income", "shape", "household", "others"))
    ) |> 
    ggplot(aes(x = term, ymin = conf.low, ymax = conf.high, col = model, 
               group = model)) + 
    geom_errorbar(width = 0.4, position = position_dodge(0.5)) +
    geom_point(aes(y = estimate), position = position_dodge(0.5)) + 
    geom_line(lty = 2, col = "black", y = 1) +
    facet_wrap(.~group, scales = "free", 
               ncol = ifelse(is.null(filter_group), 2, 1)) + 
    theme_bw() + ylim(c(.3, 2)) + 
    xlab("Coefficient") + ylab("Value")
  
}

