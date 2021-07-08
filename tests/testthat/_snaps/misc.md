# check_tf_version works

    Code
      check_tf_version("error")
    Error <simpleError>
      We have detected that you do not have the expected python packages
      setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

---

    Code
      check_tf_version("warn")
    Warning <simpleWarning>
      We have detected that you do not have the expected python packages setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

---

    Code
      check_tf_version("message")
    Message <simpleMessage>
      We have detected that you do not have the expected python packages setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

# check_tf_version errors when have_python, _tf, or _tfp is FALSE

    Code
      check_tf_version("error")
    Error <simpleError>
      We have detected that you do not have the expected python packages
      setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

---

    Code
      check_tf_version("warn")
    Warning <simpleWarning>
      We have detected that you do not have the expected python packages setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

---

    Code
      check_tf_version("message")
    Message <simpleMessage>
      We have detected that you do not have the expected python packages setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

# define and mcmc error informatively

    Code
      model(variable())
    Error <simpleError>
      none of the <greta_array>s in the model are associated with a
      probability density, so a model cannot be defined

---

    Code
      model(x)
    Error <simpleError>
      none of the <greta_array>s in the model are associated with a
      probability density, so a model cannot be defined

---

    Code
      model()
    Error <simpleError>
      could not find any non-data <greta_array>s

---

    Code
      model(bernoulli(0.5))
    Error <simpleError>
      model contains a discrete random variable that doesn't have a fixed
      value, so inference cannot be carried out

---

    Code
      model(x)
    Error <simpleError>
      none of the <greta_array>s in the model are unknown, so a model cannot
      be defined

---

    Code
      draws <- mcmc(m)
    Error <simpleError>
      data <greta_array>s cannot be sampled
      `x` is a data <greta_array>(s)

# check_dims errors informatively

    Code
      greta:::check_dims(a, c)
    Error <simpleError>
      incompatible dimensions: 3x3, 2x2

# disjoint graphs are checked

    Code
      m <- model(a, b, c)
    Error <simpleError>
      the model contains 2 disjoint graphs
      one or more of these sub-graphs does not contain any <greta_array>s that are
      associated with a probability density, so a model cannot be defined

---

    Code
      m <- model(a, b, d)
    Error <simpleError>
      the model contains 2 disjoint graphs
      one or more of these sub-graphs does not contain any <greta_array>s that are
      unknown, so a model cannot be defined

# cleanly() handles TF errors nicely

    Code
      cleanly(other_stop())
    Error <simpleError>
      Fetchez la vache!

