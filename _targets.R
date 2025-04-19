# _targets.R

# Set-up ---------------------------------------------------------------------

# Load packages
library(targets)
library(tarchetypes)
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
    "MatchIt",
    "cobalt",
    "glmmTMB",
    "broom.mixed"
  ),
  # Faster {targets}; requires {qs2}
  format = "qs",
  # Parallel processing, requires {crew}
  controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
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
  # Propensity score matching ------------------------------------------------
  # Run PSM model
  tar_target(
    name = match_model,
    command = match_data(model_dt = model_data, clean_dt = cleaned_data)
  ),
  # Get PSM matched dataset
  tar_target(
    name = matched_data,
    command = extract_matched_data(match_model)
  ),
  # Fit mixed-effect logistic regressions ------------------------------------

  # Individual targets for model_data
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
    fit_random_effects(matched_data, "died")
  ),

  tar_target(
    matched_data_successful_completion_fit,
    fit_random_effects(matched_data, "successful_completion")
  ),

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

  tar_target(
    all_model_summaries,
    bind_rows(
      model_data_died_summary %>%
        mutate(dataset = "unmatched", outcome = "died"),
      model_data_successful_completion_summary %>%
        mutate(dataset = "unmatched", outcome = "successful completion"),
      matched_data_died_summary %>%
        mutate(dataset = "matched", outcome = "died"),
      matched_data_successful_completion_summary %>%
        mutate(dataset = "matched", outcome = "successful completion"),
      .id = "model"
    )
  )
)
