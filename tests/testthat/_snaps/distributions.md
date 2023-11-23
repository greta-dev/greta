# uniform distribution errors informatively

    `min` and `max` must be numeric vectors of length 1
    They have class and length:
    `min`: numeric, 1
    `max`: logical, 1

---

    `min` and `max` must be numeric vectors of length 1
    They have class and length:
    `min`: numeric, 1
    `max`: function, 1

---

    `min` and `max` must be numeric vectors of length 1
    They have class and length:
    `min`: integer, 3
    `max`: numeric, 1

---

    `min` and `max` must finite scalars
    Their values are:
    `min`: -Inf
    `max`: Inf

---

    `max` must be greater than `min`
    Their values are:
    `min`: 1
    `max`: 1

# poisson() and binomial() error informatively in glm

    Wrong function name provided in another model
    It looks like you're using greta's `poisson()` function in the family argument of another model.
    Maybe you want to use `family = stats::poisson`,instead?

---

    Wrong function name provided in another model
    It looks like you're using greta's `binomial()` function in the family argument of another model.
    Maybe you want to use `family = stats::binomial`,instead?

---

    Wrong function name provided in another model
    It looks like you're using greta's `poisson()` function in the family argument of another model.
    Maybe you want to use `family = stats::poisson`,instead?

---

    Wrong function name provided in another model
    It looks like you're using greta's `poisson()` function in the family argument of another model.
    Maybe you want to use `family = stats::poisson`,instead?

# wishart distribution errors informatively

    `Sigma` must be a square 2D greta array
    However, `Sigma` has dimensions "3x3x3"

---

    `Sigma` must be a square 2D greta array
    However, `Sigma` has dimensions "3x2"

# lkj_correlation distribution errors informatively

    `eta` must be a positive scalar value, or a scalar <greta_array>

---

    `eta` must be a positive scalar value, or a scalar <greta_array>

---

    `eta` must be a scalar
    However `eta` had dimensions: 2, 1

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

# multivariate_normal distribution errors informatively

    the dimension of this distribution must be at least 2, but was 1
    multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    the dimension of this distribution must be at least 2, but was 1
    multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    Dimensions of parameters not compatible with multivariate distribution parameters of multivariate distributions cannot have more than two dimensions
    object `x` has dimensions: 3x3x3

---

    Not 2D square greta array
    x expected a 2D square greta array, but object `x` had dimension: 3x2

---

    distribution dimensions do not match implied dimensions
    The distribution dimension should be 3, but parameters implied dimensions: 3 vs 4
    Multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    the dimension of this distribution must be at least 2, but was 1
    multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

# multinomial distribution errors informatively

    the dimension of this distribution must be at least 2, but was 1
    multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

# categorical distribution errors informatively

    the dimension of this distribution must be at least 2, but was 1
    multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

# dirichlet distribution errors informatively

    the dimension of this distribution must be at least 2, but was 1
    multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

# dirichlet-multinomial distribution errors informatively

    the dimension of this distribution must be at least 2, but was 1
    multivariate distributions treat each row as a separate realisation - perhaps you need to transpose something?

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `-1` having class: <numeric> and length `1`

---

    `n_realisations is not a positive scalar interger`
    `n_realisations` must be a positive scalar integer giving the number of rows of the output
    x We see `n_realisations` = `1` and `3` having class: <numeric> and length `2`

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

---

    `dimension` must be a positive scalar integer giving the dimension of the distribution
    `dim(target)` returns:

# Cholesky factor of Wishart should be an upper triangular matrix

    When using `calculate()` to sample a greta array with a cholesky factor, the output can sometimes be unreliable.
    See issue here on github for more details:
    <>

# Cholesky factor of LJK_correlation should be an upper triangular matrix

    When using `calculate()` to sample a greta array with a cholesky factor, the output can sometimes be unreliable.
    See issue here on github for more details:
    <>

