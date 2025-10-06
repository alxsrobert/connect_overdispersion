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


# Read in ONS age structure data
ons_data <- import(
  "https://raw.githubusercontent.com/cmmid/reconnect_uk_social_contact_survey/main/data/age_structure_dat/ons_2022_age_structure.xlsx", 
  skip = 5) |> 
  filter(`Area name` == "UNITED KINGDOM") |> 
  #Select columns age and population contains 2022
  select("age" = Age, contains("2022")) |> 
  # Sum across Female and Male columns
  mutate(pop = `Mid-2022 population (Female)` + `Mid-2022 population (Male)`) |> 
  select(age, pop)

age_structure_fine <- ons_data |>
  mutate(p_age_group = cut(age,
                           right = F,
                           breaks = age_breaks,
                           labels = age_labels)) |> 
  group_by(p_age_group) |>
  summarise(
    n = sum(pop),
    proportion = n / sum(ons_data$pop))

# from census 2021 (https://www.ons.gov.uk/peoplepopulationandcommunity/
# culturalidentity/ethnicity/bulletins/ethnicgroupenglandandwales/census2021)
ons_ethnicity <- data.table(
  p_ethnicity = c('Asian','Black','Mixed','White','Other'),
  proportion = c(9.3,
                 4.0,
                 2.9,
                 81.7,
                 2.1)/100)


# from census 2021 (https://www.ons.gov.uk/peoplepopulationandcommunity/culturalidentity
# /ethnicity/datasets/ethnicgroupbyageandsexinenglandandwales)
eth_age_sex <- suppressWarnings(data.table(import(
  "https://raw.githubusercontent.com/cmmid/reconnect_uk_social_contact_survey/main/data/age_structure_dat/ethnicgroupagesex11.xlsx",
  sheet = 6, skip = 3)))
eth_age_sex <- eth_age_sex[get(colnames(eth_age_sex)[1])=='K04000001',]

eth_age_sex[grepl('100 or over', Age), Age := 100]

c_to_0 <- function(v){if(length(v[v=='c']) > 0){v[v=='c'] <- 0; v}else{v}}
eth_age_sex <- eth_age_sex[, lapply(.SD, c_to_0)]

eth_age_sex[, 3:ncol(eth_age_sex)] <- lapply(eth_age_sex[, 3:ncol(eth_age_sex)], as.numeric)

for(ethn in c('Asian','Black','Mixed','White','Other')){
  for(sex in c('Female','Male')){
    vec <- (substr(colnames(eth_age_sex),1,5) == ethn) & grepl(sex,colnames(eth_age_sex))
    eth_age_sex$next_col <- rowSums(eth_age_sex[, ..vec])
    colnames(eth_age_sex)[length(colnames(eth_age_sex))] <- paste0(ethn,'_',sex)
  }
}

eth_age_sex <- eth_age_sex |> 
  select('Age',contains('_')) |>
  mutate('p_age_group' = cut(Age, breaks = age_breaks, labels = age_labels, right = F)) |> 
  mutate('p_adult_child' = cut(Age, breaks = c(-Inf, 18, Inf), labels = c('Child','Adult'), right = F)) |> 
  select(!Age) |> 
  pivot_longer(!c(p_age_group, p_adult_child)) |> 
  separate_wider_delim(name, delim = "_", names = c("p_ethnicity", "p_gender")) |> 
  group_by(p_age_group, p_adult_child, p_ethnicity, p_gender) |> 
  summarise(value = sum(value)) |> 
  ungroup() |> 
  mutate(proportion = value/sum(value)) |> 
  complete(p_adult_child, p_age_group, p_ethnicity, p_gender,
           fill = list(value = 0, proportion = 0))

age_sex_strata <- # Read in ONS age structure data
  import(
    "https://raw.githubusercontent.com/cmmid/reconnect_uk_social_contact_survey/main/data/age_structure_dat/ons_2022_age_structure.xlsx",
    skip = 5) %>% 
  filter(`Area name` == "UNITED KINGDOM") %>% 
  #Select columns age and population contains 2022
  select("age" = Age, contains("2022")) %>% 
  rename(female = `Mid-2022 population (Female)`, male = `Mid-2022 population (Male)`) %>% 
  pivot_longer(cols = c("female", "male"), names_to = "p_gender", values_to = "pop") %>% 
  mutate(p_age_group = cut(age,
                           right = F,
                           # from 0 to 75+ by 5 year age groups
                           breaks = age_breaks,
                           labels = age_labels)) %>% 
  group_by(p_age_group, p_gender) %>% 
  summarise(
    n = sum(pop),
    proportion = n / sum(ons_data$pop)) %>% ungroup() 

## Import participant and contact data
reconnect <- socialmixr::get_survey('https://zenodo.org/records/17257918')

reconnect_participant <- as.data.frame(reconnect$participants)
reconnect_contact <- as.data.frame(reconnect$contacts)

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
  filter(!is.na(cnt_age_group)) |> 
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

## Compute age-stratified contact matrix
nb_age_group <- nb_matrix_fit(
  participant_data = part,
  contact_data = contacts,
  participant_var = "p_age_group",
  contact_var = "c_age_group",
  n_bootstrap = n_boots, 
  trunc = max_n_contacts,
  polymod_weighting = polymod_wts,
  weighting_vec = c("p_gender", "p_ethnicity", "day_week"),
  locations = c("Total"),
  save = FALSE
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

## Compute ethnicity-stratified contact matrix
nb_eth_group <- nb_matrix_fit(
  participant_data = part,
  contact_data = contacts,
  participant_var = "p_ethnicity",
  contact_var = "c_ethnicity",
  n_bootstrap = n_boots, 
  trunc = max_n_contacts,
  polymod_weighting = polymod_wts,
  weighting_vec = c('p_gender','p_age_group','day_week'),
  locations = c("Total"), 
  save = FALSE
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
save_matrices_values <- save_matrices_values |> 
  drop_na() |> # remove 'Prefer not to say' ethnicity
  group_by(var,c_location,p_var,c_var) |> 
  mutate(c_location = firstup(c_location)) |>
  summarise(mean = mean(mu),
            lower = quantile(mu, 0.025),
            upper = quantile(mu, 0.975))

## Pivor to wider 
save_matrices_values_w <- save_matrices_values |>
  pivot_wider(names_from = c_location, values_from = mean)

# Reorganise groups
save_matrices_values_w$p_var <- factor(
  save_matrices_values_w$p_var, 
  levels = c(age_labels,'Asian','Black','Mixed','Other', 'White'))
save_matrices_values_w$c_var <- factor(
  save_matrices_values_w$c_var, 
  levels = c(age_labels,'Asian','Black','Mixed','Other', 'White'))

# Reformat and return
save_age_values_w <- save_matrices_values_w |> 
  arrange(p_var, c_var) |> 
  select(var, p_var, c_var, Total) |> 
  filter(var == "Age groups") |>
  group_by() |> 
  select(p_var, c_var, Total) |> 
  mutate(mean_mu = as.numeric(gsub("[ ].*", "", Total))) |> 
  select(p_var, c_var, mean_mu)

save_eth_values_w <- save_matrices_values_w |> 
  arrange(p_var, c_var) |> 
  select(var, p_var, c_var, Total) |> 
  filter(var == "Ethnicity groups") |>
  group_by() |> 
  select(p_var, c_var, Total) |> 
  mutate(mean_mu = as.numeric(gsub("[ ].*", "", Total))) |> 
  select(p_var, c_var, mean_mu)

saveRDS(save_age_values_w, "results/dt_contact_age.RDS")
saveRDS(save_eth_values_w, "results/dt_contact_eth.RDS")
