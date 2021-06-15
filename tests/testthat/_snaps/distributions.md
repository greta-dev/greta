# uniform distribution errors informatively

    Code
      uniform(min = 0, max = NA)
    Error <simpleError>
      `min` and `max` must be numeric vectors of length 1

---

    Code
      uniform(min = 0, max = head)
    Error <simpleError>
      `min` and `max` must be numeric vectors of length 1

---

    Code
      uniform(min = 1:3, max = 5)
    Error <simpleError>
      `min` and `max` must be numeric vectors of length 1

---

    Code
      uniform(min = -Inf, max = Inf)
    Error <simpleError>
      `min` and `max` must finite scalars

---

    Code
      uniform(min = 1, max = 1)
    Error <simpleError>
      `max` must be greater than `min`

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
      '3 x 3 x 3'

---

    Code
      wishart(3, c)
    Error <simpleError>
      `Sigma` must be a square 2D greta array
      However, `Sigma` has dimensions
      '3 x 2'

# lkj_correlation distribution errors informatively

    Code
      lkj_correlation(-1, dim)
    Error <simpleError>
      `eta` must be a positive scalar value, or a scalar greta array

---

    Code
      lkj_correlation(c(3, 3), dim)
    Error <simpleError>
      `eta` must be a positive scalar value, or a scalar greta array

---

    Code
      lkj_correlation(uniform(0, 1, dim = 2), dim)
    Error <simpleError>
      <text>:2:0: unexpected end of input
      1: capture.output(dput(dim(eta))
         ^

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
      object `0.80879368637449`, `0.523551600411954`, `0.558323887413688`,
      `1.63837136928363`, `-2.17159107548467`, `-0.987808958860901`,
      `-0.501129424892012`, `-0.735205878172307`, `0.256539032685884`,
      `-1.11631912530813`, `0.0904909449411677`, `-0.200248544611042`,
      `0.30815485104994`, `0.773113482054443`, `1.53064642005146`,
      `1.13538247879829`, `0.134754358771958`, `-1.20807347981108`,
      `-1.60414220622414`, `0.0426957694535352`, `-0.758806868294973`,
      `0.767696253812092`, `2.74028803081958`, `-0.0554293337767402`,
      `-0.38676313591413`, `0.228759165457811`, and `-0.158003221826718` has
      dimensions: 3x3x3

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

