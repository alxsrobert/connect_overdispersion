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
  
  ## For models without ethnicity-specific overdispersion parameter, add the 
  ## estimated overdispersion as shape_intercept
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
    mutate(term = gsub("p_incomechild_", "p_age_group", term)) |> 
    mutate(
      group = case_when(
        grepl("shape", term) ~ "shape",
        grepl("Intercept", term) ~ "intercept",
        grepl("p_age_group", term) ~ "age",
        grepl("p_gender", term) ~ "gender",
        (grepl("ethnicity_rural", term) & !grepl("shape", term)) ~ "ethnicity_rural",
        grepl("p_urban_rural", term) ~ "urban_rural",
        grepl("day_week", term) ~ "day_of_the_week",
        grepl("p_income", term) ~ "income",
        grepl("household", term) ~ "household"),
      term = case_when(
        grepl("p_age_group", term) ~ gsub(pattern = "p_age_group", "", gsub("M", "-", term)),
        grepl("p_gender", term) ~ gsub(pattern = "p_gender", "", term),
        grepl("p_urban_rural", term) ~ gsub(pattern = "p_urban_rural", "", term),
        grepl("day_week", term) ~ gsub(pattern = "day_week", "", term),
        grepl("p_income", term) ~ gsub(pattern = "p_income", "", gsub("M", "-", term)),
        grepl("cat_household", term) ~ gsub(pattern = "cat_household_members", "hh_size: ", term),
        grepl("household", term) ~ "household_linear",
        grepl("ethnicity_rural", term) ~ gsub(pattern = "ethnicity_rural", "", term),
        grepl("Intercept", term) ~ term),
      term = gsub(pattern = "shape_", "", term)
    )
  
  
  ## Set reference levels
  df_all_results$term <- factor(df_all_results$term)
  df_all_results$term <- relevel(df_all_results$term, ref = "(Intercept)")
  df_all_results$term <- relevel(df_all_results$term, ref = "5-9")
  df_all_results$term <- relevel(df_all_results$term, ref = "Lessthan20000")
  df_all_results$term <- relevel(df_all_results$term, ref = "0-4")
  df_all_results$term <- relevel(df_all_results$term, ref = "hh_size: Morethan4")
  df_all_results$term <- relevel(df_all_results$term, ref = "hh_size: Four")
  df_all_results$term <- relevel(df_all_results$term, ref = "hh_size: Three")
  df_all_results$term <- relevel(df_all_results$term, ref = "hh_size: Two")
  
  return(df_all_results)
}
