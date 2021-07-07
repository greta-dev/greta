# uniform distribution errors informatively

    Code
      uniform(min = 0, max = NA)
    Error <simpleError>
      `min` and `max` must be numeric vectors of length 1
      They have class and length:
      `min`: numeric, 1
      `max`: logical, 1

---

    Code
      uniform(min = 0, max = head)
    Error <simpleError>
      `min` and `max` must be numeric vectors of length 1
      They have class and length:
      `min`: numeric, 1
      `max`: function, 1

---

    Code
      uniform(min = 1:3, max = 5)
    Error <simpleError>
      `min` and `max` must be numeric vectors of length 1
      They have class and length:
      `min`: integer, 3
      `max`: numeric, 1

---

    Code
      uniform(min = -Inf, max = Inf)
    Error <simpleError>
      `min` and `max` must finite scalars
      Their values are:
      `min`: -Inf
      `max`: Inf

---

    Code
      uniform(min = 1, max = 1)
    Error <simpleError>
      `max` must be greater than `min`
      Their values are:
      `min`: 1
      `max`: 1

# poisson() and binomial() error informatively in glm

    Code
      glm(1 ~ 1, family = poisson)
    Error <simpleError>
      Wrong function name provided in another model
      It looks like you're using greta's `poisson()` function in the family argument
      of another model.
      Maybe you want to use `family = stats::poisson`,instead?

---

    Code
      glm(1 ~ 1, family = binomial)
    Error <simpleError>
      Wrong function name provided in another model
      It looks like you're using greta's `binomial()` function in the family argument
      of another model.
      Maybe you want to use `family = stats::binomial`,instead?

---

    Code
      glm(1 ~ 1, family = poisson())
    Error <simpleError>
      Wrong function name provided in another model
      It looks like you're using greta's `poisson()` function in the family argument
      of another model.
      Maybe you want to use `family = stats::poisson`,instead?

---

    Code
      glm(1 ~ 1, family = poisson("sqrt"))
    Error <simpleError>
      Wrong function name provided in another model
      It looks like you're using greta's `poisson()` function in the family argument
      of another model.
      Maybe you want to use `family = stats::poisson`,instead?

# wishart distribution errors informatively

    Code
      wishart(3, b)
    Error <simpleError>
      `Sigma` must be a square 2D greta array
      However, `Sigma` has dimensions
      "3x3x3"

---

    Code
      wishart(3, c)
    Error <simpleError>
      `Sigma` must be a square 2D greta array
      However, `Sigma` has dimensions
      "3x2"

# lkj_correlation distribution errors informatively

    Code
      lkj_correlation(-1, dim)
    Error <simpleError>
      `eta` must be a positive scalar value, or a scalar <greta_array>

---

    Code
      lkj_correlation(c(3, 3), dim)
    Error <simpleError>
      `eta` must be a positive scalar value, or a scalar <greta_array>

---

    Code
      lkj_correlation(uniform(0, 1, dim = 2), dim)
    Error <simpleError>
      `eta` must be a scalar
      However `eta` had dimensions: 2:1

---

    Code
      lkj_correlation(4, dimension = -1)
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

---

    Code
      lkj_correlation(4, dim = c(3, 3))
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

---

    Code
      lkj_correlation(4, dim = NA)
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

# multivariate_normal distribution errors informatively

    Code
      multivariate_normal(m_c, a)
    Error <simpleError>
      the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps
      you need to transpose something?

---

    Code
      multivariate_normal(m_d, a)
    Error <simpleError>
      the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps
      you need to transpose something?

---

    Code
      multivariate_normal(m_a, b)
    Error <simpleError>
      Dimensions of parameters not compatible with multivariate distribution
      parameters of multivariate distributions cannot have more than two dimensions
      object `x` has dimensions: 3x3x3

---

    Code
      multivariate_normal(m_a, c)
    Error <simpleError>
      Not 2D square greta array
      x expected a 2D square greta array, but object `x` had dimension: 3x2

---

    Code
      multivariate_normal(m_a, d)
    Error <simpleError>
      distribution dimensions do not match implied dimensions
      The distribution dimension should be 3, but parameters implied dimensions: 3 vs
      4
      Multivariate distributions treat each row as a separate realisation - perhaps
      you need to transpose something?

---

    Code
      multivariate_normal(0, 1)
    Error <simpleError>
      the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps
      you need to transpose something?

---

    Code
      multivariate_normal(m_a, a, n_realisations = -1)
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      multivariate_normal(m_a, a, n_realisations = c(1, 3))
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      multivariate_normal(m_a, a, dimension = -1)
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

---

    Code
      multivariate_normal(m_a, a, dimension = c(1, 3))
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

# multinomial distribution errors informatively

    Code
      multinomial(c(1), 1)
    Error <simpleError>
      the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps
      you need to transpose something?

---

    Code
      multinomial(10, p_a, n_realisations = -1)
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      multinomial(10, p_a, n_realisations = c(1, 3))
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      multinomial(10, p_a, dimension = -1)
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

---

    Code
      multinomial(10, p_a, dimension = c(1, 3))
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

# categorical distribution errors informatively

    Code
      categorical(1)
    Error <simpleError>
      the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps
      you need to transpose something?

---

    Code
      categorical(p_a, n_realisations = -1)
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      categorical(p_a, n_realisations = c(1, 3))
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      categorical(p_a, dimension = -1)
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

---

    Code
      categorical(p_a, dimension = c(1, 3))
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

# dirichlet distribution errors informatively

    Code
      dirichlet(1)
    Error <simpleError>
      the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps
      you need to transpose something?

---

    Code
      dirichlet(alpha_a, n_realisations = -1)
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      dirichlet(alpha_a, n_realisations = c(1, 3))
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      dirichlet(alpha_a, dimension = -1)
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

---

    Code
      dirichlet(alpha_a, dimension = c(1, 3))
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

# dirichlet-multinomial distribution errors informatively

    Code
      dirichlet_multinomial(c(1), 1)
    Error <simpleError>
      the dimension of this distribution must be at least 2, but was 1
      multivariate distributions treat each row as a separate realisation - perhaps
      you need to transpose something?

---

    Code
      dirichlet_multinomial(10, alpha_a, n_realisations = -1)
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      dirichlet_multinomial(10, alpha_a, n_realisations = c(1, 3))
    Error <simpleError>
      object 'n_realisations' not found

---

    Code
      dirichlet_multinomial(10, alpha_a, dimension = -1)
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

---

    Code
      dirichlet_multinomial(10, alpha_a, dimension = c(1, 3))
    Error <simpleError>
      `dimension` must be a positive scalar integer giving the dimension of
      the distribution
      `dim(target)` returns:

