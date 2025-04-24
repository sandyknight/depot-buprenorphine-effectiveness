# R/merge-data.R
# Merge data tables
merge_data <-
  function(main_dt, sir_dt) {
    # Merge SIR table with main table
    # There are ~21k journeys in the main table with no recorded SIRs
    dt <-
      data.table::merge.data.table(
        main_dt,
        sir_dt,
        by = c("client_random_id", "n_jy")
      )
    dt
  }
