# R/summarise_fits.R
summarise_fit <-
  function(fit) {
    nm <- names(fit)

    print(nm)

    broom.mixed::tidy(fit, exponentiate = TRUE, conf.int = TRUE)
  }
