# R/get-data.R

# Sub-intervention Review (SIR) table ------------------------------------------
load_sir_data <- function(sir_file = "data/SIR_table_for_VfM_linked.csv") {
  ## Load SIR table
  sir_dt <-
    data.table::fread(sir_file)

  ## Remove duplicated rows (812)

  sir_dt <- sir_dt[!duplicated(sir_dt), ]

  ## Use latest journeys only for comparing outcomes
  ## Get max n_jy per client
  # latest_journeys <- sir_dt[, .(max_njy = max(n_jy)), by = client_random_id]

  ## Join back and filter
  # sir_dt <-
  #   data.table::merge.data.table(
  #     sir_dt,
  #     latest_journeys,
  #     by = "client_random_id"
  #   )
  #
  # sir_dt <- sir_dt[n_jy == max_njy, ]

  ## From here on I use {dtplyr} to manipulate the data
  ## it remains in the `data.table` format but can take
  ## the usual {dplyr} verbs.

  ## The line `dtplyr::lazy_dt(immutable = FALSE)` allows
  ## the {dplyr} functions to change the object rather than
  ## making copies see:
  ## https://dtplyr.tidyverse.org/reference/lazy_dt.html

  ## Remove post-discharge SIRs
  sir_dt <-
    sir_dt |>
    dtplyr::lazy_dt(immutable = FALSE) |>
    filter(post_discharge == "N")

  ## Select relevant columns
  sir_dt <-
    sir_dt |>
    select(
      client_random_id,
      n_jy,
      submoddt,
      phbudi_any,
      date_order
    )
  ## Return SIR table
  data.table::as.data.table(sir_dt)
}

# Main table -------------------------------------------------------------------
load_main_data <-
  function(main_data_file = "data/K3anon_FullDataset_for_VfM.csv") {
    main_dt <-
      data.table::fread(main_data_file)

    # Use latest journeys only for comparing outcomes
    # Compute maximum n_jy per client
    latest_journeys <- main_dt[, .(max_njy = max(n_jy)), by = client_random_id]

    # Join back
    main_dt <- data.table::merge.data.table(
      main_dt,
      latest_journeys,
      by = "client_random_id"
    )

    # Filter to latest journey
    main_dt <- main_dt[n_jy == max_njy, ]

    # Select relevant columns
    main_dt <-
      main_dt |>
      dtplyr::lazy_dt(immutable = FALSE) |>
      select(
        client_random_id,
        n_jy,
        triaged,
        disrsn,
        disd,
        utla23cd,
        sex,
        age,
        ethnic,
        sexualo,
        disable,
        housing_start,
        homeless_start,
        drug_alcohol,
        drug_heroin,
        drug_crack,
        drug_cocaine,
        drug_benzodiazepine,
        injstat
      ) |>
      # Convert back to data table for next step
      data.table::as.data.table()

    # Filter to unique clients using *latest journey* only
    # I've switched back to `data.table` syntax for this operation
    # because it's verbose in dplyr
    main_dt
  }

# Clean data
clean_data <- function(dt) {
  dt <- dtplyr::lazy_dt(dt, immutable = FALSE)

  # Where discharge date is NA and exit reason is inconsistent
  # call exit reason "Continued in treatment"
  dt <-
    dt |>
    mutate(
      disrsn = if_else(
        is.na(disd) & disrsn == "Exit reason inconsistent",
        "Continued in treatment",
        disrsn
      )
    ) |>
    # Convert from data.table::IDate to date
    mutate(across(c(triaged, disd, submoddt), as.Date))

  # If discharge date is missing use submoddt,
  # if submoddt also missing use cutoff

  cutoff <- as.Date("2024-12-01")

  dt <-
    dt |>
    mutate(disd = case_when(!is.na(disd) ~ disd, is.na(disd) ~ cutoff))

  dt <-
    dt |>
    mutate(year = year(disd)) |>
    filter(year > 2020) |>
    filter(year < 2025)

  # Rutland and City of London are too small and cause problems in the analysis
  dt <-
    filter(dt, !utla23cd %in% c("E09000001", "E06000017"))

  # Convert back to data table to save
  dt <-
    dt |> data.table::as.data.table()

  dt
}
