library(targets)

df <- tar_read(model_data)

f <- function(df, characteristic) {
  df |>
    dplyr::group_by(t, .data[[characteristic]]) |>
    dplyr::tally() |>
    tidyr::pivot_wider(names_from = t, values_from = n) |>
    dplyr::filter(!is.na(.data[[characteristic]])) |>
    dplyr::mutate(characteristic = characteristic) |>
    dplyr::rename("value" = all_of(characteristic)) |>
    dplyr::mutate(value = as.character(value))
}

characteristics <- c(
  "sex",
  "age",
  "currently_injecting",
  "housing_problem",
  "disabled_any",
  "drug_benzodiazepine"
)

df <-
  dplyr::bind_rows(
    mapply(
      f,
      characteristics,
      MoreArgs = list(df = df),
      SIMPLIFY = FALSE
    )
  )
