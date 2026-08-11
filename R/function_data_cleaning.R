import_and_clean <- function(anonymous = TRUE){
  ## Import contact data
  if(anonymous) {
    part_reg <- readRDS("data/participants_anonymous.RDS")
  } else {
  }
  
  # Risk of co linearity between p_age_group and p_employ as the employment of 
  # all individuals aged below 18 is set as "Child - Not Applicable.
  # To fix this:
  # - Use pivot_wider on "employ" and "p_hiqual"
  # - remove reference levels (respectively "employed" and "20000-39999") AND 
  #   "Child - Not Applicable"
  # This way, in the regression:
  # - p_age group shows the impact of changing only age compared to the reference 
  #   ("18-24 + employed + 20000-39999")
  # - no colinearity between age groups below 18 and "Child - Not applicable"
  part_reg <- part_reg |> 
    mutate(flag = 1, 
           id_indiv = seq_len(nrow(part_reg)),
           employ = gsub(" |,", "", employ),
           employ = gsub("-", "_", employ),
           employ2 = gsub(" |,", "", employ2),
           employ2 = gsub("-", "_", employ2)
    ) |> 
    pivot_wider(names_from = employ, values_from = flag, values_fill = 0, 
                names_prefix = "employ_") |> 
    mutate(flag = 1) |> 
    pivot_wider(names_from = p_hiqual, values_from = flag, values_fill = 0, 
                names_prefix = "hiqual_") |> 
    mutate(flag = 1) |> 
    pivot_wider(names_from = employ2, values_from = flag, values_fill = 0, 
                names_prefix = "employ2_") |> 
    select(-contains("child")) |> 
    select(-hiqual_Level1) |> 
    select(-employ2_1) |>
    select(-employ_employed) |> 
    mutate(p_age_group = relevel(p_age_group, ref = "18-24"),
           age_below18 = as.integer(part_reg$p_age_group %in% 
                                      c("0-4", "5-9", "10-14", "15-17")),
           id_indiv = factor(row_number())) |> 
    mutate(
      subethnicity_rural = case_when(
        age_below18 == 1 ~ ethnicity_rural,
        p_urban_rural == "Rural" ~ ethnicity_rural,
        p_ethnicity2 == "Gypsy or Irish Traveller" ~ "White Other",
        p_ethnicity2 == "Other" ~ "White Other",
        p_ethnicity2 == "Any other mixed / multiple ethnic background" ~ "Mixed Other",
        p_ethnicity2 == "White and Black African" ~ "White and Black",
        p_ethnicity2 == "White and Black Caribbean" ~ "White and Black",
        ethnicity_rural == "Other_Urban" ~ "Other_Urban",
        p_ethnicity2 == "English / Welsh / Scottish / Northern Irish / British" ~ "White British",
        p_ethnicity2 == "Any other Black/African/Caribbean background" ~ "Black Other",
        p_ethnicity2 == "Any other Asian background" ~ "Asian Other",
        .default = p_ethnicity2
      ),
      subethnicity_rural = relevel(factor(subethnicity_rural), ref = "White British"))
  
  return(part_reg)
}

