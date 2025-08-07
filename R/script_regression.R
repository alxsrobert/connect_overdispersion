anonymised <- TRUE
source("R/library_and_scripts.R")

## Import and clean contact data
part_reg <- import_and_clean(anonymised)

# define equations of the regression models
equation_no_household <- as.formula(
  tot_contact ~ p_age_group + p_gender + ethnicity_rural + day_week + p_income)
equation_full <- 
  update(equation_no_household, reformulate(c(".", "household_members")))

## Model: all covariates, ethnicity-specific overdispersion
full_withod <- 
  brm(bf(equation_full, shape ~ ethnicity_rural), data = part_reg, 
      family = negbinomial)
