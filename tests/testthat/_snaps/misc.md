# check_tf_version works

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x The expected python packages are not available
      i We recommend installing them (in a fresh R session) with:
      `install_greta_deps()`
      or
      `reinstall_greta_deps()`
      (Note: Your R session should not have initialised Tensorflow yet.)
      i For more information, see `?install_greta_deps`

---

    x The expected python packages are not available
    i We recommend installing them (in a fresh R session) with:
    `install_greta_deps()`
    or
    `reinstall_greta_deps()`
    (Note: Your R session should not have initialised Tensorflow yet.)
    i For more information, see `?install_greta_deps`

---

    Code
      check_tf_version("message")
    Message
      x The expected python packages are not available
      i We recommend installing them (in a fresh R session) with:
      `install_greta_deps()`
      or
      `reinstall_greta_deps()`
      (Note: Your R session should not have initialised Tensorflow yet.)
      i For more information, see `?install_greta_deps`

# define and mcmc error informatively

    Code
      model(variable())
    Condition
      Error in `model()`:
      ! none of the <greta_array>s in the model are associated with a probability density, so a model cannot be defined

---

    Code
      model(x)
    Condition
      Error in `model()`:
      ! none of the <greta_array>s in the model are associated with a probability density, so a model cannot be defined

---

    Code
      model()
    Condition
      Error in `model()`:
      ! could not find any non-data <greta_array>s

---

    Code
      model(bernoulli(0.5))
    Condition
      Error in `check_unfixed_discrete_distributions()`:
      ! Model contains a discrete random variable that doesn't have a fixed value, so inference cannot be carried out.

---

    Code
      model(x)
    Condition
      Error in `model()`:
      ! none of the <greta_array>s in the model are unknown, so a model cannot be defined

---

    Code
      draws <- mcmc(m, verbose = FALSE)
    Condition
      Error in `mcmc()`:
      ! Data <greta_array>s cannot be sampled
      `x` is a data <greta_array>(s)

# check_dims errors informatively

    Code
      greta:::check_dims(a, c)
    Condition
      Error:
      ! incompatible dimensions: 3x3, 2x2

# disjoint graphs are checked

    Code
      m <- model(a, b, c)
    Condition
      Error in `model()`:
      ! the model contains 2 disjoint graphs one or more of these sub-graphs does not contain any <greta_array>s that are associated with a probability density, so a model cannot be defined

---

    Code
      m <- model(a, b, d)
    Condition
      Error in `model()`:
      ! the model contains 2 disjoint graphs one or more of these sub-graphs does not contain any <greta_array>s that are unknown, so a model cannot be defined

# cleanly() handles TF errors nicely

    Code
      cleanly(other_stop())
    Condition
      Error in `cleanly()`:
      ! greta hit a tensorflow error:
      Error in other_stop(): Fetchez la vache!

