# a model with a covariance or correlation matrix runs uncompiled

    Code
      m <- model(correlation, compile = TRUE)
    Condition
      Warning:
      XLA compilation is turned off for this model.
      i XLA cannot compile a model with a covariance or correlation matrix variable, such as one from `wishart()`, `lkj_correlation()` or `cholesky_variable()`.
      i Set `compile = FALSE` to silence this warning.

