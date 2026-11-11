compar_models <- function(regression_models, anoun){
  bel18 <- regression_models$bel18
  ov18  <- regression_models$ov18
  
  # Diagnosis threshold
  k_threshold <- 0.7
  
  # Start with ordinary PSIS-LOO and inspect its diagnostics.
  loo_bel18 <- compare_loo_models(
    fits = bel18, age_group = "bel18", k_threshold = k_threshold)
  
  loo_ov18 <- compare_loo_models(
    fits = ov18, age_group = "ov18", k_threshold = k_threshold)
  
  # Combined reporting table, retaining separate within-age-group rankings.
  compar_models <- bind_rows(loo_bel18$summary, loo_ov18$summary)
  
  # Save complete objects and reporting tables.
  saveRDS(list(bel18 = loo_bel18, ov18 = loo_ov18), 
          paste0("loo_results/full_loo_comparison", if(anoun) "_anoun", ".rds"))
  saveRDS(compar_models, 
          "loo_results/model_comparison", if(anoun) "_anoun", ".rds")
  
}


compare_loo_models <- function(fits, age_group, model_labels = NULL, k_threshold = 0.7) {
  if (is.null(model_labels)) model_labels <- paste0(age_group, "_model_", seq_along(fits))
  
  names(fits) <- model_labels
  
  # Check the observed outcome vectors. 
  responses <- lapply(fits, function(fit) fit$data$tot_contact)
  
  same_response <- vapply(responses,
                          function(y) isTRUE(all.equal(
                            y, responses[[1]],
                            check.attributes = FALSE
                          )),logical(1))
  
  loo_objects <- vector("list", length(fits))
  names(loo_objects) <- model_labels
  
  for (i in seq_along(fits)) {
    message("Computing LOO: ", age_group, " — ",model_labels[i], 
            " (", i, "/", length(fits), ")")
    loo_objects[[i]] <- brms::loo(fits[[i]],k_threshold = k_threshold)
  }
  
  # Paired comparison relative to the model with highest ELPD.
  comparison <- loo::loo_compare(x = loo_objects)
  
  comparison_table <- as.data.frame(comparison)
  
  # Older loo versions store model names only in row names.
  if (!"model" %in% names(comparison_table)) {
    comparison_table$model <- rownames(comparison_table)
  }
  
  rownames(comparison_table) <- NULL
  
  comparison_table <- comparison_table |>
    mutate(
      age_group = .env$age_group,
      rank = row_number(),
      delta_looic = -2 * elpd_diff,
      se_delta_looic = 2 * se_diff
    ) |>
    relocate(model)
  
  # Absolute estimates and uncertainty for each model.
  estimate_table <- bind_rows(lapply(
    seq_along(loo_objects),
    function(i) {
      
      est <- loo_objects[[i]]$estimates
      
      data.frame(
        model = names(loo_objects)[i],
        n_observations = nrow(loo_objects[[i]]$pointwise),
        elpd_loo = est["elpd_loo", "Estimate"],
        se_elpd_loo = est["elpd_loo", "SE"],
        p_loo = est["p_loo", "Estimate"],
        se_p_loo = est["p_loo", "SE"],
        looic = est["looic", "Estimate"],
        se_looic = est["looic", "SE"]
      )
    }
  ))
  
  # Observation-level Pareto-k diagnostics.
  diagnostics <- bind_rows(lapply(
    seq_along(loo_objects),
    function(i) {
      
      k <- loo::pareto_k_values(loo_objects[[i]])
      
      data.frame(
        model = names(loo_objects)[i],
        observation = seq_along(k),
        pareto_k = k,
        flagged = ifelse(is.na(k), NA, k > k_threshold)
      )
    }
  ))
  
  diagnostic_summary <- diagnostics |>
    group_by(model) |>
    summarise(
      max_pareto_k = if (all(is.na(pareto_k))) {
        NA_real_
      } else {
        max(pareto_k, na.rm = TRUE)
      },
      n_k_above_threshold = sum(flagged, na.rm = TRUE),
      n_k_above_1 = sum(pareto_k > 1, na.rm = TRUE),
      n_k_missing = sum(is.na(pareto_k)),
      .groups = "drop"
    ) |>
    mutate(
      k_threshold = k_threshold,
      psis_check = case_when(
        n_k_missing > 0 ~ "Inspect missing diagnostics",
        n_k_above_threshold > 0 ~ "Inspect/repair PSIS estimates",
        TRUE ~ "No k values above threshold"
      )
    )
  
  # Retain paired differences from loo_compare and add other estimates.
  summary_table <- comparison_table |>
    select(age_group, model, rank, elpd_diff, se_diff, delta_looic, se_delta_looic) |>
    left_join(estimate_table, by = "model") |>
    left_join(diagnostic_summary, by = "model")
  
  flagged_observations <- diagnostics |>
    filter(is.na(pareto_k) | pareto_k > k_threshold) |> arrange(model, desc(pareto_k))
  
  list(
    loo = loo_objects,
    comparison = comparison,
    summary = summary_table,
    diagnostics = diagnostics,
    flagged_observations = flagged_observations
  )
}
