# R/code-data.R
code_data_variables <- function(dt) {
  dt <- data.table::as.data.table(dt)

  # Convert IDate columns to standard Date
  dt <-
    dt |>
    dtplyr::lazy_dt(immutable = FALSE) |>
    mutate(across(c(triaged, disd, submoddt), as.Date))

  # Calculate time between discharge or cut-off and SIR
  # Use months since data is rounded to year-month.

  dt <-
    dt |>
    mutate(
      time_since_submod = clock::date_count_between(
        submoddt,
        disd,
        precision = "month"
      )
    )

  # Intervention (binary) -----------------------------------------------------

  # The intervention (t) is: 'Recieved depot buprenorphine in the 12 months
  # preceding discharge or, if still in treatment, last recorded SIR.'

  # Assign to treatment group 1 if depot buprenorphine was
  # indicated at a SIR in the 12 months before discharge
  # Assumes that where there is no SIR there is no depot buprenorphine

  dt <-
    dt |>
    mutate(t = if_else(phbudi_any == 1 & time_since_submod <= 12, 1L, 0L)) |>
    mutate(t = if_else(is.na(t), 0, t)) |>
    group_by(
      client_random_id,
      triaged,
      year,
      disd,
      disrsn,
      age,
      sex,
      utla23cd,
      injstat,
      housing_start,
      homeless_start,
      disable,
      drug_benzodiazepine
    ) |>
    summarise(t = max(t), .groups = "drop")

  # Outcomes (binary) ----------------------------------------------------------

  # Mortality (`died`), if discharge reason is recorded as "Died",
  # outcome variable `died` = 1, any other discharge reason, or continued
  # treatment, `died` = 0

  dt <-
    dt |>
    mutate(died = if_else(disrsn == "Died", 1, 0)) |>
    mutate(
      successful_completion = if_else(disrsn == "Successful completion", 1, 0)
    )

  # Covariates -----------------------------------------------------------------

  # 1. Age group (categorical, 5)

  # Reduce number of age groups from 11 to 5 to mitigate problems
  # with separation. There's an explanation of separation and the
  # valid methods of resolving it here:
  # https://www.bookdown.org/rwnahhas/RMPH/blr-separation.html

  dt <-
    dt |>
    dplyr::mutate(
      age = dplyr::case_when(
        age %in% c("18-24", "25-29", "30-34") ~ "18-34",
        age %in% c("35-39", "40-44") ~ "35-44",
        age %in% c("45-49", "50-54") ~ "45-54",
        age %in% c("55-59", "60-64") ~ "55-64",
        age %in% c("65-69", "70+") ~ "65+",
        TRUE ~ NA_character_
      )
    )

  # Sort the factor levels
  agelevels <-
    tibble::as_tibble(dt) |> pull(age) |> unique()

  dt <-
    dt |>
    mutate(age = factor(age, sort(agelevels)))

  # 2. Sex (binary)

  dt <-
    dt |>
    mutate(sex = if_else(sex == "F", 1, 0))

  dt

  # 3. Currently injecting (binary)

  dt <-
    dt |>
    mutate(
      injstat = if_else(
        injstat %chin%
          c("Never injected", "Previously injected", "Currently injecting"),
        injstat,
        "Unknown"
      )
    ) |>
    mutate(
      currently_injecting = factor(
        injstat,
        levels = c(
          "Never injected",
          "Previously injected",
          "Currently injecting",
          "Unknown"
        )
      )
    )

  dt <-
    dt |>
    filter(currently_injecting != "Unknown")

  # 4. Any disability

  dt <-
    dt |>
    mutate(disabled_any = if_else(disable == "Any disability", 1, 0))

  # 6. Any housing problem

  dt <-
    dt |>
    mutate(
      housing_problem = if_else(
        housing_start %in%
          c("No fixed abode", "Rough sleeping") |
          homeless_start == "Risk of homelessness",
        1,
        0
      )
    )

  dt <-
    dt |>
    mutate(year = year(disd)) |>
    select(
      year,
      client_random_id,
      sex,
      age,
      utla23cd,
      drug_benzodiazepine,
      t:last_col()
    ) |>
    unique()

  dt <- as_tibble(dt)

  dt |>
    group_by(t) |>
    tally()

  dt <-
    dt |>
    filter(year > 2020, year < 2025)

  readr::write_rds(dt, "data/model-data.rds")
  dt
}
