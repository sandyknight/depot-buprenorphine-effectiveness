# R/psm.R
# This is divided into two short functions so that the PSM model
# is accesible as an object for diagnostics.
ps_match_data <-
  function(model_dt, clean_dt) {
    njy <- unique(clean_dt[, .(client_random_id, n_jy)])

    dt <- data.table::merge.data.table(model_dt, njy, by = "client_random_id")

    m_out <-
      MatchIt::matchit(
        t ~
          n_jy +
          sex +
          age +
          utla23cd +
          currently_injecting +
          housing_problem,
        data = dt,
        method = "nearest"
      )

    m_out
  }

extract_matched_data <- function(match_model) {
  m_data <- MatchIt::match.data(match_model)

  m_data
}
