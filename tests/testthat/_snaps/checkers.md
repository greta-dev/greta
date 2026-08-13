# check_tf_version() hints at restarting R after greta_remove() this session

    Code
      check_tf_version("message")
    Message
      ! It looks like you ran `greta_remove()` without restarting R - greta is still pointing at the environment you removed.
      i Restart R, then try again.
      x greta could not load Python, TensorFlow, and TensorFlow Probability.
      i Run `greta::greta_sitrep()` to check your installation.
      i For offline and conda installs, see the installation vignette (`vignette(greta::installation)`).

# check_tf_version() omits the restart hint when nothing was removed

    Code
      check_tf_version("message")
    Message
      x greta could not load Python, TensorFlow, and TensorFlow Probability.
      i If greta has just been updated, restart R: the managed environment installs the versions greta asks for on the next load.
      i Run `greta::greta_sitrep()` to check your installation.
      i For offline and conda installs, see the installation vignette (`vignette(greta::installation)`).

