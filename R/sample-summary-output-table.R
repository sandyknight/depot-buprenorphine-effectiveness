create_output_sample_summary <- function(df) {
  df <-
    df |>
    dplyr::mutate(
      value = dplyr::case_when(
        (characteristic == "Sex" & value == "1") ~ "Female",
        (characteristic == "Sex" & value == "0") ~ "Male",
        (characteristic != "Sex" & value == "1") ~ "Yes",
        (characteristic != "Sex" & value == "0") ~ "No",
        TRUE ~ value
      )
    ) |>
    dplyr::select(
      characteristic,
      value,
      long_acting_buprenorphine,
      control
    )

  totals <-
    df |>
    dplyr::group_by(characteristic) |>
    dplyr::summarise(across(c(long_acting_buprenorphine, control), sum)) |>
    dplyr::mutate(characteristic = "Total", value = "Total") |>
    unique()

  df <-
    df |>
    dplyr::mutate(
      long_acting_buprenorphine_p = long_acting_buprenorphine /
        dplyr::pull(totals, long_acting_buprenorphine)
    ) |>
    dplyr::mutate(control_p = control / dplyr::pull(totals, control)) |>
    dplyr::mutate(dplyr::across(
      tidyselect::contains("_p"),
      \(x) paste0(paste0("(", scales::percent(x, accuracy = 0.01)), ")")
    )) |>
    dplyr::mutate(dplyr::across(
      c(long_acting_buprenorphine, control),
      scales::comma
    ))

  df <-
    df |>
    dplyr::select(
      characteristic,
      value,
      long_acting_buprenorphine,
      long_acting_buprenorphine_p,
      control,
      control_p
    ) |>
    dplyr::rename_with(
      \(x) gsub("Long acting", "Long-acting", snakecase::to_sentence_case(x))
    )

  df |>
    flextable::flextable() |>
    flextable::merge_v(j = 1) |>
    flextable::merge_h_range(j1 = 1, j2 = 2, part = "head") |>
    flextable::merge_h_range(j1 = 3, j2 = 4, part = "head") |>
    flextable::merge_h_range(j1 = 5, j2 = 6, part = "head") |>
    flextable::valign(j = 1, valign = "top") |>
    flextable::border(
      i = c(2, 7, 10, 12, 14),
      border.bottom = officer::fp_border()
    ) |>
    flextable::align(j = c(1:6), align = "left", part = "all") |>
    flextable::padding(j = c(1, 2, 3, 5), padding.left = 12, part = "all") |>
    flextable::width(j = c(1, 2, 4, 6), width = 3.5, unit = "cm") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::italic(j = c(4, 6)) |>
    flextable::bold(j = c(1:6), part = "head")
}
