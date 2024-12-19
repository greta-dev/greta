# mixtures of fixed and continuous distributions errors

    Code
      mixture(bernoulli(0.5), normal(0, 1), weights = weights)
    Condition
      Error in `initialize()`:
      ! Cannot construct a mixture distribution from a combination of discrete and continuous distributions

# mixtures of multivariate and univariate errors

    Code
      mixture(multivariate_normal(zeros(1, 3), diag(3)), normal(0, 1, dim = c(1, 3)),
      weights = weights)
    Condition
      Error in `initialize()`:
      ! Cannot construct a mixture from a combination of multivariate and univariate distributions

# mixtures of supports errors

    Code
      mixture(normal(0, 1, truncation = c(0, Inf)), normal(0, 1), weights = weights)
    Condition
      Error in `initialize()`:
      ! Component distributions must have the same support
      However the component distributions have different support:
      "0 to Inf vs. -Inf to Inf"

---

    Code
      mixture(lognormal(0, 1), normal(0, 1), weights = weights)
    Condition
      Error in `initialize()`:
      ! Component distributions must have the same support
      However the component distributions have different support:
      "0 to Inf vs. -Inf to Inf"

# incorrectly-shaped weights errors

    Code
      mixture(normal(0, 1), normal(0, 2), weights = weights)
    Condition
      Error in `initialize()`:
      ! The first dimension of weights must be the number of distributions in the mixture (2)
      However it was 1

# mixtures with insufficient distributions errors

    Code
      mixture(normal(0, 2), weights = weights)
    Condition
      Error in `initialize()`:
      ! `mixture()` must be passed at least 2 distributions
      The number of distributions passed was: 1

---

    Code
      mixture(weights = weights)
    Condition
      Error in `initialize()`:
      ! `mixture()` must be passed at least 2 distributions
      The number of distributions passed was: 0

