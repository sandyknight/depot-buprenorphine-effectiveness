library(data.table)
library(dplyr)
library(dtplyr)
library(lubridate)
library(stringr)

main_dt <- data.table::fread("data/K3anon_FullDataset_for_VfM.csv")


dt <- main_dt[,
  .(triaged, disrsn, disd),
  keyby = .(client_random_id, n_jy)
]

# All successful completions
succ_comps <-
  dt[disrsn == "Successful completion", .(client_random_id, n_jy, disd)]

# All representations
scnr.start <- data.table::as.IDate("2021-01-01")
mon.end <- data.table::as.IDate("2024-12-31")

repres <-
  dt[
    pmax(triaged, na.rm = TRUE) >= scnr.start &
      pmin(triaged, na.rm = TRUE) <= mon.end,
    .(client_random_id, n_jy, triaged)
  ]

repres <-
  tibble::as_tibble(repres) |>
  tidyr::pivot_wider(
    names_from = n_jy,
    names_glue = "{.value}.{n_jy}",
    values_from = c(triaged)
  )


repres <- merge(succ_comps, repres, by = "client_random_id", all.x = TRUE)

as.IDate(scnr.start) %m+% months(6)

f <- function(x) {
  if_else(
    disd > x |
      is.na(x) |
      x >= disd %m+% months(6),
    as.IDate("1000-01-01"),
    disd
  )
}

class(repres[["triaged"]][1])

repres <-
  repres |>
  mutate(
    across(
      where(is.Date),
      as.Date
    )
  )

repres <-
  repres |>
  mutate(across(
    starts_with("triaged"),
    \(x) {
      if_else(
        disd > x |
          is.na(x) |
          x >= disd %m+% months(6) |
          n_jy == as.integer(stringr::str_remove(cur_column(), "triaged.d")),
        as.IDate("1000-01-01"),
        disd
      )
    },
    .names = 'repres_date.{.col}'
  )) |>
  rowwise() |>
  mutate(
    scne.repres_triage_jy = max(
      c_across(starts_with("repres_date")),
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  mutate(
    scrn.repres_triage_jy = if_else(
      scnr.repres_triage_jy == as.IDate("1000-01-01"),
      as.IDate(NA),
      scnr.repres_triage_jy
    )
  ) |>
  select(client_random_id, n_jy, starts_with("scnr"))

dt <- merge(dt, repres, by = c("client_random_id", "n_jy", "disd"))
