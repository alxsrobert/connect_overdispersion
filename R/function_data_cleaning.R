import_and_clean <- function(anonymous = TRUE){
  ## Import contact data
  if(anonymous) {
    part_reg <- readRDS("data/participants_anonymous.RDS")
    # Risk of co linearity between p_age_group and p_income as the income of all 
    # individuals aged below 18 is set as "Child - Not Applicable"
    # To fix this:
    # - Given age_above_18 the maximum age of the age group containing 18, set 
    #   the age group of all individuals aged below age_above_18 to "0-age_above_18"
    # - For all individuals aged under 18, set p_income to their age group instead 
    #   of "Child - Not Applicable"
    ## This way, in the regression:
    # the nb of contact in adults aged between 18 and age_above_18 is intercept * income
    # the nb of contact in adults above age_above_18 is intercept * age_group * income
    # the nb of contact in children below 18 is is intercept * age_group
    min_age_group <- as.numeric(substr(part_reg$p_age_group, 1, 2))
    age_above_18 <- min(min_age_group[min_age_group > 18], na.rm = TRUE)
    
    part_reg <- 
      part_reg |> 
      mutate(
        p_income_ref = p_income,
        p_age_group_ref = p_age_group,
        p_income = case_when(
          grepl("Child", p_income) ~ paste0("child_", p_age_group),
          .default = p_income
        ),
        p_age_group = case_when(
          grepl("child", p_income) ~ paste0("0-", age_above_18),
          p_age_group == paste0("18-", age_above_18 - 1) ~ paste0("0-", age_above_18),
          .default = p_age_group
        )
      )
  } else {
  }
  
  return(part_reg)
}

