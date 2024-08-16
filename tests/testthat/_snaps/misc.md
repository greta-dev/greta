# check_tf_version works

    x The expected python packages are not available
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

    none of the <greta_array>s in the model are associated with a probability density, so a model cannot be defined

---

    none of the <greta_array>s in the model are associated with a probability density, so a model cannot be defined

---

    could not find any non-data <greta_array>s

---

    Model contains a discrete random variable that doesn't have a fixed value, so inference cannot be carried out.

---

    none of the <greta_array>s in the model are unknown, so a model cannot be defined

---

    Data <greta_array>s cannot be sampled
    `x` is a data <greta_array>(s)

# check_dims errors informatively

    incompatible dimensions: 3x3, 2x2

# disjoint graphs are checked

    the model contains 2 disjoint graphs one or more of these sub-graphs does not contain any <greta_array>s that are associated with a probability density, so a model cannot be defined

---

    the model contains 2 disjoint graphs one or more of these sub-graphs does not contain any <greta_array>s that are unknown, so a model cannot be defined

# cleanly() handles TF errors nicely

    greta hit a tensorflow error:
    Error in other_stop(): Fetchez la vache!

