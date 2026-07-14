# check_tf_version() hints at restarting R after greta_remove() this session

    Code
      check_tf_version("message")
    Message
      ! It looks like you ran `greta_remove()` without restarting R - greta is still pointing at the environment you removed.
      i Restart R, then try again.
      x greta could not load Python with TensorFlow and TensorFlow Probability.
      i Run `greta::greta_sitrep()` to check your installation.
      i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

# check_tf_version() omits the restart hint when nothing was removed

    Code
      check_tf_version("message")
    Message
      x greta could not load Python with TensorFlow and TensorFlow Probability.
      i Run `greta::greta_sitrep()` to check your installation.
      i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

