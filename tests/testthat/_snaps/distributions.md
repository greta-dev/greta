# uniform distribution errors informatively

    Code
      uniform(min = 0, max = NA)
    Condition
      Error in `initialize()`:
      ! `max` must be a numeric vector of length 1
      However its class, and length are:
      `max`:
      * (class: <logical>)
      * (length: 1)

---

    Code
      uniform(min = 0, max = head)
    Condition
      Error in `initialize()`:
      ! `max` must be a numeric vector of length 1
      However its class, and length are:
      `max`:
      * (class: <function>)
      * (length: 1)

---

    Code
      uniform(min = 1:3, max = 5)
    Condition
      Error in `initialize()`:
      ! `min` must be a numeric vector of length 1
      However its class, and length are:
      `min`:
      * (class: <integer>)
      * (length: 3)

---

    Code
      uniform(min = -Inf, max = Inf)
    Condition
      Error in `initialize()`:
      ! `-Inf` must be a finite scalar
      But their values are:
      `-Inf`: -Inf

---

    Code
      uniform(min = 1, max = 1)
    Condition
      Error in `initialize()`:
      ! `max` must be greater than `min`
      Their values are:
      `min`: 1
      `max`: 1

# poisson() and binomial() error informatively in glm

    Code
      glm(1 ~ 1, family = poisson)
    Condition
      Error in `family()`:
      ! Wrong function name provided in another model
      It looks like you're using greta's `poisson()` function in the family argument of another model.
      Maybe you want to use `family = stats::poisson`,instead?

---

    Code
      glm(1 ~ 1, family = binomial)
    Condition
      Error in `family()`:
      ! Wrong function name provided in another model
      It looks like you're using greta's `binomial()` function in the family argument of another model.
      Maybe you want to use `family = stats::binomial`,instead?

---

    Code
      glm(1 ~ 1, family = poisson())
    Condition
      Error in `poisson()`:
      ! Wrong function name provided in another model
      It looks like you're using greta's `poisson()` function in the family argument of another model.
      Maybe you want to use `family = stats::poisson`,instead?

---

    Code
      glm(1 ~ 1, family = poisson("sqrt"))
    Condition
      Error in `poisson()`:
      ! Wrong function name provided in another model
      It looks like you're using greta's `poisson()` function in the family argument of another model.
      Maybe you want to use `family = stats::poisson`,instead?

# wishart distribution errors informatively

    Code
      wishart(3, b)
    Condition
      Error in `initialize()`:
      ! `Sigma` must be a square 2D greta array
      However, `Sigma` has dimensions "3x3x3"

---

    Code
      wishart(3, c)
    Condition
      Error in `initialize()`:
      ! `Sigma` must be a square 2D greta array
      However, `Sigma` has dimensions "3x2"

# lkj_correlation distribution errors informatively

    Code
      lkj_correlation(-1, dim)
    Condition
      Error in `initialize()`:
      ! `eta` must be a positive scalar value, or a scalar <greta_array>

---

    Code
      lkj_correlation(c(3, 3), dim)
    Condition
      Error in `initialize()`:
      ! `eta` must be a positive scalar value, or a scalar <greta_array>

---

    Code
      lkj_correlation(uniform(0, 1, dim = 2), dim)
    Condition
      Error in `initialize()`:
      ! `eta` must be a scalar
      However `eta` had dimensions: 2x1

---

    Code
      lkj_correlation(4, dimension = -1)
    Condition
      Error in `initialize()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

---

    Code
      lkj_correlation(4, dim = c(3, 3))
    Condition
      Error in `initialize()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

---

    Code
      lkj_correlation(4, dim = NA)
    Condition
      Error in `initialize()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

# multivariate_normal distribution errors informatively

    Code
      multivariate_normal(m_c, a)
    Condition
      Error in `check_dimension()`:
      ! the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    Code
      multivariate_normal(m_d, a)
    Condition
      Error in `check_dimension()`:
      ! the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    Code
      multivariate_normal(m_a, b)
    Condition
      Error in `lapply()`:
      ! Dimensions of parameters not compatible with multivariate distribution parameters of multivariate distributions cannot have more than two dimensions
      object `x` has dimensions: 3x3x3

---

    Code
      multivariate_normal(m_a, c)
    Condition
      Error in `lapply()`:
      ! Object must be 2D square array
      x But it had dimension: "3x2"

---

    Code
      multivariate_normal(m_a, d)
    Condition
      Error in `check_dimension()`:
      ! distribution dimensions do not match implied dimensions
      The distribution dimension should be 3, but parameters implied dimensions: 3 vs 4
      Multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    Code
      multivariate_normal(0, 1)
    Condition
      Error in `check_dimension()`:
      ! the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    Code
      multivariate_normal(m_a, a, n_realisations = -1)
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    Code
      multivariate_normal(m_a, a, n_realisations = c(1, 3))
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    Code
      multivariate_normal(m_a, a, dimension = -1)
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

---

    Code
      multivariate_normal(m_a, a, dimension = c(1, 3))
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

# multinomial distribution errors informatively

    Code
      multinomial(c(1), 1)
    Condition
      Error in `check_dimension()`:
      ! the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    Code
      multinomial(10, p_a, n_realisations = -1)
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    Code
      multinomial(10, p_a, n_realisations = c(1, 3))
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    Code
      multinomial(10, p_a, dimension = -1)
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

---

    Code
      multinomial(10, p_a, dimension = c(1, 3))
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

# categorical distribution errors informatively

    Code
      categorical(1)
    Condition
      Error in `check_dimension()`:
      ! the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    Code
      categorical(p_a, n_realisations = -1)
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    Code
      categorical(p_a, n_realisations = c(1, 3))
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    Code
      categorical(p_a, dimension = -1)
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

---

    Code
      categorical(p_a, dimension = c(1, 3))
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

# dirichlet distribution errors informatively

    Code
      dirichlet(1)
    Condition
      Error in `check_dimension()`:
      ! the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    Code
      dirichlet(alpha_a, n_realisations = -1)
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    Code
      dirichlet(alpha_a, n_realisations = c(1, 3))
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    Code
      dirichlet(alpha_a, dimension = -1)
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

---

    Code
      dirichlet(alpha_a, dimension = c(1, 3))
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

# dirichlet-multinomial distribution errors informatively

    Code
      dirichlet_multinomial(c(1), 1)
    Condition
      Error in `check_dimension()`:
      ! the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    Code
      dirichlet_multinomial(10, alpha_a, n_realisations = -1)
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    Code
      dirichlet_multinomial(10, alpha_a, n_realisations = c(1, 3))
    Condition
      Error in `check_n_realisations()`:
      ! `n_realisations is not a positive scalar interger`
      `n_realisations` must be a positive scalar integer giving the number of rows of the output
      x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    Code
      dirichlet_multinomial(10, alpha_a, dimension = -1)
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

---

    Code
      dirichlet_multinomial(10, alpha_a, dimension = c(1, 3))
    Condition
      Error in `check_multivariate_dims()`:
      ! `dimension` must be a positive scalar integer giving the dimension of the distribution
      `dim(target)` returns:

