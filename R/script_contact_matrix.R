library(rio)
per_capita <- TRUE
n_boots <- 1000
# choose age groups for analysis
age_breaks <- c(-Inf, 5, 10, 15, 18, 25, 30, 40, 50, 60, 70, Inf)
age_vals <- age_breaks[is.finite(age_breaks)]
age_labels <- c(paste0(c(0, age_vals[1:length(age_vals)-1]), '-', c(age_vals-1)), 
                paste0(age_vals[length(age_vals)], '+'))



# censor any categories of participant with less than x:
censor_low_val <- 5
# right-censor total contact counts at:
max_n_contacts <- 10000 # deliberately redundant

## install appropriate packages ##
source("https://raw.githubusercontent.com/cmmid/reconnect_uk_social_contact_survey/main/scripts/analyses/install_packages.R")
source("https://raw.githubusercontent.com/cmmid/reconnect_uk_social_contact_survey/main/scripts/analyses/functions.R")
source("https://raw.githubusercontent.com/cmmid/reconnect_uk_social_contact_survey/main/scripts/analyses/negative_binom/negative_binomial_fcns.R")
source("https://raw.githubusercontent.com/cmmid/reconnect_uk_social_contact_survey/main/scripts/analyses/age_structure.R")

## Import participant and contact data
reconnect_participant_common <- import(
  "https://zenodo.org/records/17257918/files/reconnect_participant_common.csv?download=1", 
  show_col_types = F)
reconnect_participant_extra <- import(
  "https://zenodo.org/records/17257918/files/reconnect_participant_extra.csv?download=1", 
  show_col_types = F)
reconnect_participant_sday <- import(
  "https://zenodo.org/records/17257918/files/reconnect_sday.csv?download=1", 
  show_col_types = F)
reconnect_participant_hh <- import(
  "https://zenodo.org/records/17257918/files/reconnect_hh_common.csv?download=1", 
  show_col_types = F)
reconnect_contact_common <- import(
  "https://zenodo.org/records/17257918/files/reconnect_contact_common.csv?download=1", 
  show_col_types = F) |> 
  filter(!is.na(cnt_age_exact)) # remove large group contacts
reconnect_contact_extra <- import(
  "https://zenodo.org/records/17257918/files/reconnect_contact_extra.csv?download=1", 
  show_col_types = F) %>% 
  filter(!is.na(cnt_location)) # remove large group contacts

reconnect_participant <- 
  left_join(reconnect_participant_common, 
            reconnect_participant_extra, 
            by = 'part_id') |> 
  left_join(reconnect_participant_sday, by = 'part_id') |> 
  left_join(reconnect_participant_hh, by = 'hh_id')

reconnect_contact <- 
  left_join(reconnect_contact_common, 
            reconnect_contact_extra, 
            by = c('cont_id','part_id'))

## Rename columns and changes breaks of p_age_group
part <- reconnect_participant |> 
  rename(p_id = part_id,
         p_age_group = part_age_group,
         p_adult_child = part_adult_child,
         p_gender = part_gender,
         p_ethnicity = part_ethnicity,
         p_sec_input = part_ses) |> 
  mutate(p_gender = case_when(
    p_gender == 'F' ~ 'Female', p_gender == 'M' ~ 'Male', T ~ NA),
    p_age_group = cut(
      part_age_exact, breaks = age_breaks, labels = age_labels, right = F))

contacts <- reconnect_contact |> 
  rename(c_id = cont_id,
         p_id = part_id,
         c_location = cnt_location,
         c_age_group = cnt_age_group,
         c_sex = cnt_gender,
         c_ethnicity = cnt_ethnicity,
         c_sec_input = cnt_ses) |> 
  mutate(c_sex = case_when(
    c_sex == 'F' ~ 'Female', c_sex == 'M' ~ 'Male', T ~ NA),
    c_age_group = cut(
      cnt_age_exact, breaks = age_breaks, labels = age_labels, right = F))

# load age weights for large_n
polymod_wts <- polymod_weights(locations = "total")

## Compute contact matrix
nb_age_group <- nb_matrix_fit(
  participant_data = part,
  contact_data = contacts,
  participant_var = "p_age_group",
  contact_var = "c_age_group",
  n_bootstrap = n_boots, 
  trunc = max_n_contacts,
  polymod_weighting = polymod_wts,
  weighting_vec = c("p_gender", "p_ethnicity", "day_week"),
  locations = c("Total")
)

# normalise wrt population structure
nb_age_group_norm <- map(
  .x = nb_age_group,
  .f = ~normalise_matrix(
    matrix = .x,
    p_var_name = "p_age_group",
    per_capita = per_capita,
    population_dt = age_structure_fine
  )
)

nb_eth_group <- nb_matrix_fit(
  participant_data = part,
  contact_data = contacts,
  participant_var = "p_ethnicity",
  contact_var = "c_ethnicity",
  n_bootstrap = n_boots, 
  trunc = max_n_contacts,
  polymod_weighting = polymod_wts,
  weighting_vec = c('p_gender','p_age_group','day_week'),
  locations = c("Total")
)

# normalise wrt population structure
nb_eth_group_norm <- map(
  .x = nb_eth_group,
  .f = ~normalise_matrix(
    matrix = .x,
    p_var_name = "p_ethnicity",
    per_capita = per_capita,
    population_dt = ons_ethnicity
  )
)

# Merge nb_age_group_norm into a dataframe
save_matrices_values <- 
  rbind(cbind(rbindlist(nb_age_group_norm), var = 'Age groups'),
        cbind(rbindlist(nb_eth_group_norm), var = 'Ethnicity groups'))
# Extract mean and 95% CI of mean number of contacts
save_matrices_values <- save_matrices_values %>% 
  drop_na() %>% # remove 'Prefer not to say' ethnicity
  group_by(var,c_location,p_var,c_var) %>% 
  mutate(c_location = firstup(c_location)) %>%
  summarise(mean = mean(mu),
            lower = quantile(mu, 0.025),
            upper = quantile(mu, 0.975))

## Pivor to wider 
save_matrices_values_w <- save_matrices_values %>%
  pivot_wider(names_from = c_location, values_from = mean)

# Reorganise groups
save_matrices_values_w$p_var <- factor(
  save_matrices_values_w$p_var, levels = c(age_labels, 'White','Asian','Black','Mixed','Other'))
save_matrices_values_w$c_var <- factor(
  save_matrices_values_w$c_var, levels = c(age_labels, 'White','Asian','Black','Mixed','Other'))

# Reformat and return
save_age_values_w <- save_matrices_values_w %>% 
  arrange(p_var, c_var) %>% 
  select(var, p_var, c_var, Total) |> 
  filter(var == "Age groups") |>
  group_by() |> 
  select(p_var, c_var, Total) |> 
  mutate(mean_mu = as.numeric(gsub("[ ].*", "", Total))) |> 
  select(p_var, c_var, mean_mu)

save_eth_values_w <- save_matrices_values_w %>% 
  arrange(p_var, c_var) %>% 
  select(var, p_var, c_var, Total) |> 
  filter(var == "Ethnicity groups") |>
  group_by() |> 
  select(p_var, c_var, Total) |> 
  mutate(mean_mu = as.numeric(gsub("[ ].*", "", Total))) |> 
  select(p_var, c_var, mean_mu)

saveRDS(save_age_values_w, "results/dt_contact_age.RDS")
saveRDS(save_eth_values_w, "results/dt_contact_eth.RDS")
