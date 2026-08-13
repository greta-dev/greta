# check_tf_version names which requirement failed

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load TensorFlow Probability.
      > TensorFlow Probability: No module named 'tf_keras'
      i If greta has just been updated, restart R: the managed environment installs the versions greta asks for on the next load.
      i Run `greta::greta_sitrep()` to check your installation.
      i For offline and conda installs, see the installation vignette (`vignette(greta::installation)`).

# a too-old TensorFlow on conda says what to run

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load TensorFlow.
      > TensorFlow: TensorFlow 2.15.0 is installed, but greta needs 2.18.0 or later
      i Your Python comes from the "greta-env-tf2" conda environment, which greta never updates on its own.
      * Recommended: switch to greta's managed environment. Run `greta::greta_remove("env")`, restart R, and greta installs what it needs. Add `greta::greta_remove("preference")` if you have used `greta_set_python()`.
      * To stay on conda: run `greta::reinstall_greta_deps()`.
      i Run `greta::greta_sitrep()` to check your installation.
      i For offline and conda installs, see the installation vignette (`vignette(greta::installation)`).

# a too-old TensorFlow on the managed backend says to restart

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load TensorFlow.
      > TensorFlow: TensorFlow 2.15.0 is installed, but greta needs 2.18.0 or later
      i If greta has just been updated, restart R: the managed environment installs the versions greta asks for on the next load.
      i Run `greta::greta_sitrep()` to check your installation.
      i For offline and conda installs, see the installation vignette (`vignette(greta::installation)`).

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
      i If greta has just been updated, restart R: the managed environment installs the versions greta asks for on the next load.
      i Run `greta::greta_sitrep()` to check your installation.
      i For offline and conda installs, see the installation vignette (`vignette(greta::installation)`).
      i What Python and uv reported is in `greta::open_greta_install_log()`.

# a failed check without a reason still errors cleanly

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load TensorFlow.
      i If greta has just been updated, restart R: the managed environment installs the versions greta asks for on the next load.
      i Run `greta::greta_sitrep()` to check your installation.
      i For offline and conda installs, see the installation vignette (`vignette(greta::installation)`).

# braces in a Python message are not read as cli interpolation

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load TensorFlow Probability.
      > TensorFlow Probability: bad dict {'a': 1} in {config}
      i If greta has just been updated, restart R: the managed environment installs the versions greta asks for on the next load.
      i Run `greta::greta_sitrep()` to check your installation.
      i For offline and conda installs, see the installation vignette (`vignette(greta::installation)`).

