anonymised <- TRUE
source("R/library_and_scripts.R")

## Import and clean contact data
part_reg <- import_and_clean(anonymised)

# define equations of the regression models
equation_no_household <- as.formula(
  tot_contact ~ p_age_group + p_gender + ethnicity_rural + day_week + p_income)
equation_full <- 
  update(equation_no_household, reformulate(c(".", "household_members")))

## Model 1: all covariates, no ethnicity-specific overdispersion
full <- brm(bf(equation_full), data = part_reg, family = negbinomial)

## Model 2: only ethnicity, ethnicity-specific overdispersion
ethnicity_with_od <- 
  brm(bf(tot_contact ~ ethnicity_rural, shape ~ ethnicity_rural),
      data = part_reg, family = negbinomial)

## Model 3: all covariates, ethnicity-specific overdispersion
full_withod <- 
  brm(bf(equation_full, shape ~ ethnicity_rural), data = part_reg, 
      family = negbinomial)

## Model 4: all covariates except household size, 
#           ethnicity-specific overdispersion
full_withod_nohh <- 
  brm(bf(equation_no_household, shape ~ ethnicity_rural),
  data = part_reg, family = negbinomial)

## Model 5: all covariates, categorical household covariate, 
#           ethnicity-specific overdispersion
full_with_od_cathh <- 
  brm(bf(
    update(equation_no_household, reformulate(c(".", "cat_household_members"))), 
    shape ~ ethnicity_rural
  ), data = part_reg, family = negbinomial)

## Save all models
saveRDS(list(full_no_od = full, 
             ethnicity = ethnicity_with_od, 
             full_od = full_withod, 
             full_od_nohh = full_withod_nohh,
             full_od_cathh = full_with_od_cathh),
        paste0("results/regression_output", if(anonymised) "_anoun", ".rds"))
