source("R/attach-packages.R")
source("R/merge-data.R")

load_main_data <-
  function(main_data_file = "data/K3anon_FullDataset_for_VfM.csv", yr) {
    yr <- as.integer(yr)

    main_dt <-
      data.table::fread(main_data_file)

    # main_dt <- main_dt[
    #   data.table::year(triaged) == yr |
    #     data.table::year(triaged) == yr - 1,
    # ]

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


# Sub-intervention Review (SIR) table ------------------------------------------
load_sir_data <-
  function(sir_file = "data/SIR_table_for_VfM_linked.csv", yr) {
    yr <- as.integer(yr)

    ## Load SIR table
    sir_dt <-
      data.table::fread(sir_file)

    ## Remove duplicated rows (812)

    sir_dt <- sir_dt[!duplicated(sir_dt), ]

    sir_dt <-
      sir_dt[
        data.table::year(submoddt) == (yr - 1) |
        data.table::year(submoddt) == yr
      ]

    ## Use latest journeys only for comparing outcomes
    ## Get max n_jy per client
    latest_journeys <- sir_dt[, .(max_njy = max(n_jy)), by = client_random_id]

    ## Join back and filter
    sir_dt <-
      data.table::merge.data.table(
        sir_dt,
        latest_journeys,
        by = "client_random_id"
      )

    # Filter to latest latest journeys,
    # remove post-discharge SIRs
    sir_dt <- sir_dt[n_jy == max_njy, ][post_discharge == "N", ]

    ## Select relevant columns
    sir_dt <-
      sir_dt[, .(client_random_id, n_jy, submoddt, phbudi_any, date_order)]

    sir_dt
  }


# Clean data
clean_data <-
  function(dt, yr) {
    yr <- as.integer(yr)
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
      )

    # If discharge date is missing use submoddt,
    # if submoddt also missing use cutoff

    cutoff <-
      data.table::as.IDate(as.Date(paste0(yr, "-12-31")))

    dt <-
      dt |>
      mutate(disd = case_when(!is.na(disd) ~ disd, is.na(disd) ~ cutoff)) |>
      filter(data.table::year(as.integer(disd)) == yr)

    dt <-
      dt |>
      dplyr::filter(disd <= cutoff)

    # Rutland and City of London are too small
    dt <-
      dplyr::filter(dt, !utla23cd %in% c("E09000001", "E06000017"))

    # Convert back to data table to save
    dt <-
      dt |> data.table::as.data.table()

    dt
  }

get_annual_mortality_data <-
  function(yr) {
    d1 <- load_main_data(yr = yr)
    d2 <- load_sir_data(yr = yr)
    dt <- merge_data(d1, d2)
    dt <- clean_data(dt = dt, yr = yr)

    # Calculate time between discharge or cut-off and SIR
    # Use months since data is rounded to year-month.

    dt[,
      time_since_submod :=
      (data.table::year(as.integer(disd)) - data.table::year(as.integer(submoddt))) * 12 +
      (data.table::month(as.integer(disd)) - data.table::month(as.integer(submoddt)))
    ]
    # dt[, time_since_submod := clock::date_count_between(
    #   submoddt,
    #   disd,
    #   precision = "month"
    # )]

    # The intervention (t) is: 'Recieved depot buprenorphine in the 12 months
    # preceding discharge or, if still in treatment, last recorded SIR.'

    # Assign to treatment group 1 if depot buprenorphine was
    # indicated at a SIR in the 12 months before discharge
    # Assumes that where there is no SIR there is no depot buprenorphine

    dt[,
      t := data.table::fifelse(
        phbudi_any == 1 & time_since_submod <= 12L,
        1L,
        0L
      )
    ][,
      t := data.table::fifelse(
        is.na(t),
        0L,
        t
        # Mortality (`died`), if discharge reason is recorded as "Died",
        # outcome variable `died` = 1, any other discharge reason, or continued
        # treatment, `died` = 0
      )
    ][,
      died := data.table::fifelse(
        disrsn == "Died",
        1L,
        0L
      )
    ]
print(dt)
    dt <-
      dt[,
        .(t = max(t), died = max(died)),
        by = .(
          client_random_id,
          n_jy,
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
        )
      ]

    dt[, year := yr]

    dt
  }


yrs <- seq(2022, 2024L, 1L)

mortality_data_list <- lapply(yrs, get_annual_mortality_data)

dt <- data.table::rbindlist(mortality_data_list)

mortality_summary <- dt[, .N, by = .(year, t, died)]

mortality_summary <-
  tidyr::pivot_wider(mortality_summary, names_from = died, values_from = N) |>
  dplyr::mutate(total = `0` + `1`) |>
  dplyr::mutate(
    died_p = `1` / total,
  )

colnames(mortality_summary) <- c(
  "Year",
  "Treatment group",
  "Did not die",
  "Died",
  "Total",
  "Died (%)"
)

data.table::fwrite("data/mortality_summary.csv")

library(gglot2)
library(afcharts)
afcharts::use_afcharts()

mortality_rate_plot  <-
  mortality_summary |>
  dplyr::mutate(Year = factor(Year)) |>
  dplyr::select(Year, `Treatment group`, `Died (%)`) |>
  tidyr::pivot_wider(
    names_from = `Treatment group`,
    values_from = `Died (%)`
  ) |>
  dplyr::rename("Control" = `0`, "Long-acting buprenorphine" = `1`) |>
  tidyr::pivot_longer(
    c(Control, `Long-acting buprenorphine`),
    values_to = "Mortality (%)",
    names_to = "Treatment group"
  ) |>
  ggplot(aes(x = Year, y = `Mortality (%)`, group = `Treatment group`)) +
  geom_col(aes(fill = `Treatment group`), position = "dodge", width = 0.6) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 0.1)) +
  theme(legend.position = "bottom", plot.title.position = "plot") +
  labs(
    fill = NULL,
    title = "Mortality rate",
    y = NULL,
    subtitle = "Long-acting buprenorphine v any other opioid treatment"
  )

png(
  "plots/mortality-plot.png",
  width = 24,
  height = 24,
  units = "cm",
  res = 200
)
mortality_rate_plot
dev.off()


model_data <- targets::tar_read(model_data)

data.table::setDT(model_data)

mortality_summary <- model_data[, .N, by = .(t, died)]


