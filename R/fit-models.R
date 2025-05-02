# Fixed-effects logistic regression

fit_fixed_effects <-
  function(dt, outcome, weights = NULL) {
    glm(
      get(outcome) ~
        t +
        age +
        sex +
        currently_injecting +
        drug_benzodiazepine,
      weights = weights,
      family = binomial(link = "logit"),
      data = dt
    )
  }

# Mixed-effects logistic regression
# Random effects on utla23cd

# Using glmmTMB here is a considerable speed up on `lme4::glmer`
# but `lme4` is more widely used and mighy be better for the final
# version

fit_random_effects <-
  function(dt, outcome, weights = NULL) {
    glmmTMB::glmmTMB(
      get(outcome) ~
        t +
        age +
        sex +
        currently_injecting +
        drug_benzodiazepine +
        (1 | utla23cd),
      weights = weights,
      family = binomial,
      data = dt,
      control = glmmTMB::glmmTMBControl(parallel = 8)
    )
  }
