import_and_clean <- function(anonymous = TRUE){
  ## Import contact data
  if(anonymous) {
    part_reg <- readRDS("data/participants_anonymous.RDS")
  } else {
  }
  
  # Risk of co linearity between p_age_group and p_income as the income and 
  # employment of all individuals aged below 18 is set as "Child - Not Applicable"
  # To fix this:
  # - Use pivot_wider on "employ" and "p_income"
  # - remove reference levels (respectively "employed" and "20000-39999") AND 
  #   "Child - Not Applicable"
  # This way, in the regression:
  # - p_age group shows the impact of changing only age compared to the reference 
  #   ("18-24 + employed + 20000-39999")
  # - no colinearity between age groups below 18 and "Child - Not applicable"
  part_reg <- part_reg |> 
    mutate(flag = 1, 
           id_indiv = seq_len(nrow(part_reg)),
           p_income = gsub(" |,", "", p_income),
           p_income = gsub("-", "_", p_income),
           employ = gsub(" |,", "", employ),
           employ = gsub("-", "_", employ)
    ) |> 
    pivot_wider(names_from = p_income, values_from = flag, 
                values_fill = 0, names_prefix = "p_income_"
    ) |> 
    mutate(flag = 1) |> 
    pivot_wider(names_from = employ, values_from = flag, values_fill = 0, 
                names_prefix = "employ_") |> 
    select(-contains("child")) |> 
    select(-p_income_20000_39999) |> 
    select(-employ_employed) |> 
    mutate(p_age_group = relevel(p_age_group, ref = "18-24"))
  
  return(part_reg)
}

