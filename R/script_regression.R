anonymised <- TRUE
source("R/library_and_scripts.R")

## Import and clean contact data
part_reg <- import_and_clean(anonymised)

fit_mixnb_bel18 <- fit_mixnb_ov18 <- list()

#### Below 18 ####

## Define priors
bprior_bel18_mix_hnb <- c(
  prior_string("normal(-5,1)", class = "hu1"),
  prior_string("normal(-5,1)", class = "hu2"),
  prior_string("uniform(-2,2)", dpar  = "mu2", lb = -2, ub = 2),
  prior_string("uniform(-2,2)", dpar  = "theta2", lb = -2, ub = 2),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban", dpar = "mu2"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban", dpar = "theta2"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural", dpar = "mu2"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural", dpar = "theta2")
)

bprior_bel18_single <- c(
  prior_string("normal(-5,1)", class = "hu"),
  prior_string("uniform(-2,2)", dpar = "shape", lb = -2, ub = 2),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban", dpar = "shape"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural", dpar = "shape")
)

bprior_bel18_mixnb <- c(
  prior_string("uniform(-2,2)", dpar  = "mu2", lb = -2, ub = 2),
  prior_string("uniform(-2,2)", dpar  = "theta2", lb = -2, ub = 2),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban", dpar = "mu2"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban", dpar = "theta2"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural", dpar = "mu2"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural", dpar = "theta2")
)

## Run models
## full mixture HNB
fit_mixnb_bel18[[1]] <- 
  brm(
    bf(
      tot_contact ~ 1,
      mu1 ~ cat_household_members,
      mu2 ~ p_age_group + p_gender + day_week + ethnicity_rural + cat_household_members,
      theta2 ~ p_age_group + p_gender + day_week + ethnicity_rural
    ), 
    data = part_reg |> filter(age_below18 == 1, cat_household_members != "Alone") %>% 
      mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)), 
    family = mixture(hurdle_negbinomial(), hurdle_negbinomial()), iter = 5000,
    prior = bprior_bel18_mix_hnb
  )

## full single HNB
fit_mixnb_bel18[[2]] <- 
  brm(bf(
    tot_contact ~ p_age_group + p_gender + day_week + ethnicity_rural + 
      cat_household_members, 
    shape ~ ethnicity_rural
  ), 
  data = part_reg |> filter(age_below18 == 1, cat_household_members != "Alone") %>% 
    mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)), 
  family = hurdle_negbinomial(), iter = 5000, prior = bprior_bel18_single)

## reduced mixture HNB
fit_mixnb_bel18[[3]] <- 
  brm(
    bf(
      tot_contact ~ 1,
      mu1 ~ 1,
      mu2 ~ ethnicity_rural + p_age_group,
      theta2 ~ ethnicity_rural + p_age_group
    ), 
    data = part_reg |> filter(age_below18 == 1, cat_household_members != "Alone") %>% 
      mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)), 
    family = mixture(hurdle_negbinomial(), hurdle_negbinomial()), seed = 123,
    prior = bprior_bel18_mix_hnb, iter = 5000
  )

## full Mixture NB
fit_mixnb_bel18[[4]] <- 
  brm(
    bf(
      tot_contact ~ 1,
      mu1 ~ cat_household_members,
      mu2 ~ p_age_group + p_gender + day_week + ethnicity_rural + cat_household_members,
      theta2 ~ p_age_group + p_gender + day_week + ethnicity_rural
    ), 
    data = part_reg |> filter(age_below18 == 1, cat_household_members != "Alone") %>% 
      mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)), 
    family = mixture(negbinomial(), negbinomial()), iter = 5000,
    prior = bprior_bel18_mixnb
  )

#### 18 and over ####

## Define priors
bprior_ov18_mnb <- c(
  prior_string("uniform(-2,2)", dpar  = "mu2", lb = -2, ub = 2),
  prior_string("uniform(-2,2)", dpar  = "theta2", lb = -2, ub = 2),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban", dpar = "mu2"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban", dpar = "theta2"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural", dpar = "mu2"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural", dpar = "theta2")
  
)
bprior_ov18_detailed_ethnic_groups <- c(
  prior_string("uniform(-2,2)", dpar  = "mu2", lb = -2, ub = 2),
  prior_string("uniform(-2,2)", dpar  = "theta2", lb = -2, ub = 2)
  
)

bprior_ov18_single <- c(
  prior_string("uniform(-2,2)", dpar = "shape", lb = -2, ub = 2),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralOther_Urban", dpar = "shape"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural"),
  prior_string("normal(0,0.8)", class = "b", coef = "ethnicity_ruralNonWhite_Rural", dpar = "shape")
)


# employ + broad ethnic group, mixture model
fit_mixnb_ov18[[1]] <- brm(
  bf(
    tot_contact ~ 1,
    mu1 ~ cat_household_members,
    mu2 ~ p_age_group + ethnicity_rural + cat_household_members + day_week +
      employ_Lookingafterhomeorfamily + employ_Retired + employ_unemployed +
      employ_Student + employ_Long_termsickordisabled + employ_Other + p_gender,
    theta2 ~ p_age_group + ethnicity_rural + day_week +
      employ_Lookingafterhomeorfamily + employ_Retired + employ_unemployed +
      employ_Student + employ_Long_termsickordisabled + employ_Other + p_gender
  ),
  data = part_reg |> filter(age_below18 == 0) %>% 
    mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)),
  family = mixture(negbinomial(), negbinomial()), iter = 5000, prior = bprior_ov18_mnb
)

# employ + detailed ethnic group, mixture model
fit_mixnb_ov18[[2]] <- brm(
  bf(
    tot_contact ~ 1,
    mu1 ~ cat_household_members,
    mu2 ~ p_age_group + subethnicity_rural  + cat_household_members + day_week +
      employ_Lookingafterhomeorfamily + employ_Retired + employ_unemployed +
      employ_Student + employ_Long_termsickordisabled + employ_Other + p_gender,
    theta2 ~ p_age_group + subethnicity_rural  + day_week +
      employ_Lookingafterhomeorfamily + employ_Retired + employ_unemployed +
      employ_Student + employ_Long_termsickordisabled + employ_Other + p_gender
  ),
  data = part_reg |> filter(age_below18 == 0) %>% 
    mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)),
  family = mixture(negbinomial(), negbinomial()), iter = 5000, 
  prior = bprior_ov18_detailed_ethnic_groups
)


# employ + NS-SEC + broad ethnic group, mixture model
fit_mixnb_ov18[[3]] <- brm(
  bf(
    tot_contact ~ 1,
    mu1 ~ cat_household_members,
    mu2 ~ p_age_group + ethnicity_rural + cat_household_members + day_week +
      employ2_Unknown + employ2_Lookingafterhomeorfamily + employ2_Retired + 
      employ2_2 + employ2_3 + employ2_4 + employ2_5 + employ2_6 + employ2_7 + 
      employ2_unemployed + employ2_Student + employ2_Long_termsickordisabled + 
      employ2_Other + p_gender,
    theta2 ~ p_age_group + ethnicity_rural + day_week +
      employ2_Unknown + employ2_Lookingafterhomeorfamily + employ2_Retired + 
      employ2_2 + employ2_3 + employ2_4 + employ2_5 + employ2_6 + employ2_7 + 
      employ2_unemployed + employ2_Student + employ2_Long_termsickordisabled + 
      employ2_Other + p_gender
  ),
  data = part_reg |> filter(age_below18 == 0) %>% 
    mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)),
  family = mixture(negbinomial(), negbinomial()), iter = 5000, prior = bprior_ov18_mnb
)

# employ + hiqual + broad ethnic group, mixture model
fit_mixnb_ov18[[4]] <- brm(
  bf(
    tot_contact ~ 1,
    mu1 ~ cat_household_members,
    mu2 ~ p_age_group + ethnicity_rural + cat_household_members + day_week +
      employ_Lookingafterhomeorfamily + employ_Retired + employ_unemployed +
      employ_Student + employ_Long_termsickordisabled + employ_Other +
      hiqual_Noquali + hiqual_Apprenti + hiqual_Level2 + hiqual_Level3 + hiqual_Level4 + 
      p_gender,
    theta2 ~ p_age_group + ethnicity_rural + day_week +
      employ_Lookingafterhomeorfamily + employ_Retired + employ_unemployed +
      employ_Student + employ_Long_termsickordisabled + employ_Other +
      hiqual_Noquali + hiqual_Apprenti + hiqual_Level2 + hiqual_Level3 + hiqual_Level4 + 
      p_gender
  ),
  data = part_reg |> filter(age_below18 == 0) %>% 
    mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)),
  family = mixture(negbinomial(), negbinomial()), iter = 5000, prior = bprior_ov18_mnb
)

# employ + hiqual + detailed ethnicity, mixture model
fit_mixnb_ov18[[5]] <- brm(
  bf(
    tot_contact ~ 1,
    mu1 ~ cat_household_members,
    mu2 ~ p_age_group + subethnicity_rural  + cat_household_members + day_week +
      employ_Lookingafterhomeorfamily + employ_Retired + employ_unemployed +
      employ_Student + employ_Long_termsickordisabled + employ_Other +
      hiqual_Noquali + hiqual_Apprenti + hiqual_Level2 + hiqual_Level3 + hiqual_Level4 + 
      p_gender,
    theta2 ~ p_age_group + subethnicity_rural  + day_week +
      employ_Lookingafterhomeorfamily + employ_Retired + employ_unemployed +
      employ_Student + employ_Long_termsickordisabled + employ_Other +
      hiqual_Noquali + hiqual_Apprenti + hiqual_Level2 + hiqual_Level3 + hiqual_Level4 + 
      p_gender
  ),
  data = part_reg |> filter(age_below18 == 0) %>% 
    mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)),
  family = mixture(negbinomial(), negbinomial()), iter = 5000, 
  prior = bprior_ov18_detailed_ethnic_groups
)

# Reduced model, mixture model
fit_mixnb_ov18[[6]] <- brm(
  bf(
    tot_contact ~ 1,
    mu1 ~ 1,
    mu2 ~ ethnicity_rural + p_age_group,
    theta2 ~ ethnicity_rural + p_age_group
  ),
  data = part_reg |> filter(age_below18 == 0) %>% 
    mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)),
  family = mixture(negbinomial(), negbinomial()), prior = bprior_ov18_mnb, iter = 5000
)

# Full model, single distribution
fit_mixnb_ov18[[7]] <- 
  brm(bf(
    tot_contact ~ p_age_group + ethnicity_rural + cat_household_members + day_week +
      employ_Lookingafterhomeorfamily + employ_Retired + employ_unemployed +
      employ_Student + employ_Long_termsickordisabled + employ_Other +
      hiqual_Noquali + hiqual_Apprenti + hiqual_Level2 + hiqual_Level3 + hiqual_Level4 + 
      p_gender, 
    shape ~ ethnicity_rural
  ), 
  data = part_reg |> filter(age_below18 == 0) %>% 
    mutate(tot_contact = case_when(tot_contact > 200 ~ 200, .default = tot_contact)),
  family = negbinomial(), iter = 5000, prior = bprior_ov18_single)

#### Save output ####

saveRDS(list(bel18 = fit_mixnb_bel18, ov18 = fit_mixnb_ov18),
        paste0("results/all_regression_mix_nb", if(anonymised) "_anoun", ".RDS"))

#### Compute model performance ####

## comparison model performance
all_model <- readRDS(
  paste0("results/all_regression_mix_nb", if(anonymised) "_anoun", ".RDS"))

compar_models(all_model, anonymised)
