# _targets.R

# Set-up ---------------------------------------------------------------------

# Load packages
library(targets)
# Options:
tar_option_set(
  # Require packages
  packages = c(
    "dplyr",
    "dtplyr",
    "data.table",
    "clock",
    "tidyr",
    "tibble",
    "tidyselect",
    "MatchIt",
    "cobalt",
    "glmmTMB",
    "broom.mixed",
    "flextable",
    "scales"
  ),
  # Faster {targets}; requires {qs2}
  format = "qs",
  # Parallel processing, requires {crew}
  controller = crew::crew_controller_local(workers = 8)
)

# Source scripts in R/
tar_source()

# Targets list
list(
  # Data preparation ---------------------------------------------------------
  # Load SIR data
  tar_target(
    name = sir_data,
    command = load_sir_data("data/SIR_table_for_VfM_linked.csv")
  ),
  # Load main data table
  tar_target(
    name = main_data,
    command = load_main_data("data/K3anon_FullDataset_for_VfM.csv")
  ),
  # Merge data tables
  tar_target(
    name = data,
    command = merge_data(main_data, sir_data)
  ),
  # Clean data
  tar_target(
    name = cleaned_data,
    command = clean_data(data)
  ),
  # Code variables
  tar_target(
    name = model_data,
    command = code_data_variables(cleaned_data)
  ),
  # Summarise dataset
  tar_target(
    name = model_data_summary,
    command = summarise_sample(model_data)
  ),
  # Propensity score matching ------------------------------------------------
  # Run PSM model
  tar_target(
    name = match_model,
    command = ps_match_data(model_dt = model_data, clean_dt = cleaned_data)
  ),
  # Get PSM matched dataset
  tar_target(
    name = matched_data,
    command = extract_matched_data(match_model)
  ),
  # Balance table
  tar_target(
    name = balance_table,
    command = cobalt::bal.tab(match_model)
  ),

  # Fit mixed-effect logistic regressions ------------------------------------

  tar_target(
    model_data_died_fit,
    fit_random_effects(model_data, "died")
  ),
  tar_target(
    model_data_successful_completion_fit,
    fit_random_effects(model_data, "successful_completion")
  ),
  tar_target(
    matched_data_died_fit,
    fit_random_effects(matched_data, "died", weight = weights)
  ),
  tar_target(
    matched_data_successful_completion_fit,
    fit_random_effects(matched_data, "successful_completion", weight = weights)
  ),
  # Summarise mixed effects logistic regressions ------------------------------
  tar_target(
    model_data_died_summary,
    summarise_fit(model_data_died_fit)
  ),
  tar_target(
    model_data_successful_completion_summary,
    summarise_fit(model_data_successful_completion_fit)
  ),
  tar_target(
    matched_data_died_summary,
    summarise_fit(matched_data_died_fit)
  ),
  tar_target(
    matched_data_successful_completion_summary,
    summarise_fit(matched_data_successful_completion_fit)
  ),

  # Fit fixed effects logistic regressions -----------------------------------

  tar_target(
    model_data_died_fit_fixef,
    fit_fixed_effects(model_data, "died")
  ),
  tar_target(
    model_data_successful_completion_fit_fixef,
    fit_fixed_effects(model_data, "successful_completion")
  ),
  tar_target(
    matched_data_died_fit_fixef,
    fit_fixed_effects(matched_data, "died", weight = weights)
  ),
  tar_target(
    matched_data_successful_completion_fit_fixef,
    fit_fixed_effects(matched_data, "successful_completion", weight = weights)
  ),
  # Summarise fixed effects logistic regressions -------------------------------
  tar_target(
    model_data_died_summary_fixef,
    summarise_fit(model_data_died_fit_fixef)
  ),
  tar_target(
    model_data_successful_completion_summary_fixef,
    summarise_fit(model_data_successful_completion_fit_fixef)
  ),
  tar_target(
    matched_data_died_summary_fixef,
    summarise_fit(matched_data_died_fit_fixef)
  ),
  tar_target(
    matched_data_successful_completion_summary_fixef,
    summarise_fit(matched_data_successful_completion_fit_fixef)
  ),
  # Combine model summaries
  tar_target(
    all_model_summaries,
    bind_rows(
      model_data_died_summary %>%
        mutate(
          dataset = "unmatched",
          outcome = "died",
          model = "random effects"
        ),
      model_data_successful_completion_summary %>%
        mutate(
          dataset = "unmatched",
          outcome = "successful completion",
          model = "random effects"
        ),
      matched_data_died_summary %>%
        mutate(
          dataset = "matched",
          outcome = "died",
          model = "random effects"
        ),
      matched_data_successful_completion_summary %>%
        mutate(
          dataset = "matched",
          outcome = "successful completion",
          model = "random effects"
        ),
      model_data_died_summary_fixef %>%
        mutate(
          dataset = "unmatched",
          outcome = "died",
          model = "fixed effecs"
        ),
      model_data_successful_completion_summary_fixef %>%
        mutate(
          dataset = "unmatched",
          outcome = "successful completion",
          model = "fixed effects"
        ),
      matched_data_died_summary_fixef %>%
        mutate(
          dataset = "matched",
          outcome = "died",
          model = "fixed effects"
        ),
      matched_data_successful_completion_summary_fixef %>%
        mutate(
          dataset = "matched",
          outcome = "successful completion",
          model = "fixed effects"
        ),
    )
  )
)
