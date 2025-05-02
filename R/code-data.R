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
    mutate(
      t = as.numeric(phbudi_any == 1 & time_since_submod <= 12),
      t = dplyr::if_else(is.na(t), 0, t)
    )

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
  # with separation in logistic regression. There's an explanation
  # of separation and the valid methods of resolving it here:
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

  # 2. Sex (binary)

  dt <-
    dt |>
    mutate(sex = if_else(sex == "F", 1, 0))

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
    )

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
  colnames(as_tibble(dt))
  dt <-
    dt |>
    mutate(year = year(disd)) |>
    select(-phbudi_any, -submoddt, -date_order, -time_since_submod) |>
    unique()

  dt <- data.table::as.data.table(dt)

  factor_cols <- c("age", "injstat", "housing_problem")

  dt[, (factor_cols) := lapply(.SD, factor), .SDcols = factor_cols]

  dt
}
