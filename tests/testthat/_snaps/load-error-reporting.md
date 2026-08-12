# check_tf_version names which requirement failed

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load TensorFlow Probability.
      > TensorFlow Probability: No module named 'tf_keras'
      i Run `greta::greta_sitrep()` to check your installation.
      i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

# check_tf_version reports every failed requirement

    Code
      check_tf_version("error")
    Message
      i Working out why Python could not be loaded, this may take a moment.
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load Python, TensorFlow, and TensorFlow Probability.
      > Python: python is a no-show
      > TensorFlow: tensorflow is a no-show
      > TensorFlow Probability: tfp is a no-show
      i Run `greta::greta_sitrep()` to check your installation.
      i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.
      i What Python and uv reported is in `greta::open_greta_install_log()`.

# a failed check without a reason still errors cleanly

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load TensorFlow.
      i Run `greta::greta_sitrep()` to check your installation.
      i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

# braces in a Python message are not read as cli interpolation

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load TensorFlow Probability.
      > TensorFlow Probability: bad dict {'a': 1} in {config}
      i Run `greta::greta_sitrep()` to check your installation.
      i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

