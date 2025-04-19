# Fixed-effects logistic regression

fit_fixed_effects <-
  function(dt, outcome) {
    glm(
      get(outcome) ~
        t +
          age +
          sex +
          currently_injecting +
          drug_benzodiazepine,
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
  function(dt, outcome) {
    glmmTMB::glmmTMB(
      get(outcome) ~
        t +
          age +
          sex +
          currently_injecting +
          drug_benzodiazepine +
          (1 | utla23cd),
      family = binomial,
      data = dt,
      control = glmmTMB::glmmTMBControl(parallel = 4)
    )
  }
