# check_tf_version errors when have_python, _tf, or _tfp is FALSE

    Code
      check_tf_version("error")
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load Python with TensorFlow and TensorFlow Probability.
      i Run `greta::greta_sitrep()` to check your installation.
      i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

---

    x greta could not load Python with TensorFlow and TensorFlow Probability.
    i Run `greta::greta_sitrep()` to check your installation.
    i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

---

    Code
      check_tf_version("message")
    Message
      x greta could not load Python with TensorFlow and TensorFlow Probability.
      i Run `greta::greta_sitrep()` to check your installation.
      i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

# check_tf_version fails when tfp not available

    Code
      check_tf_version("error")
    Message
      i Initialising python and checking dependencies, this may take a moment.
      x Initialising python and checking dependencies, this may take a moment. ... ...
      
    Condition
      Error in `check_tf_version()`:
      ! x greta could not load Python with TensorFlow and TensorFlow Probability.
      i Run `greta::greta_sitrep()` to check your installation.
      i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

# greta_sitrep warns when have_python, _tf, or _tfp is FALSE

    x greta could not load Python with TensorFlow and TensorFlow Probability.
    i Run `greta::greta_sitrep()` to check your installation.
    i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

---

    x greta could not load Python with TensorFlow and TensorFlow Probability.
    i Run `greta::greta_sitrep()` to check your installation.
    i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

---

    x greta could not load Python with TensorFlow and TensorFlow Probability.
    i Run `greta::greta_sitrep()` to check your installation.
    i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

---

    x greta could not load Python with TensorFlow and TensorFlow Probability.
    i Run `greta::greta_sitrep()` to check your installation.
    i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

---

    x greta could not load Python with TensorFlow and TensorFlow Probability.
    i Run `greta::greta_sitrep()` to check your installation.
    i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

---

    x greta could not load Python with TensorFlow and TensorFlow Probability.
    i Run `greta::greta_sitrep()` to check your installation.
    i For help, including offline or conda installs, see the installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.

---

    Code
      greta_sitrep()
    Message
      i checking if python available
      v python (v3.11) available
      
      i checking if TensorFlow available
      v TensorFlow (v2.15.1) available
      
      i checking if TensorFlow Probability available
      v TensorFlow Probability (v0.23.0) available
      
      i checking if greta conda environment available
      x greta conda environment not available
      
      * backend: "managed (uv) environment"
      * selected via: default
      i Conda environment not set up, but all dependencies available
      
      greta is ready to use!

# greta_sitrep warns when different versions of python, tf, tfp

    Code
      greta_sitrep()
    Message
      i checking if python available
      v python (v3.6) available
      
      i checking if TensorFlow available
      v TensorFlow (v2.15.1) available
      
      i checking if TensorFlow Probability available
      v TensorFlow Probability (v0.23.0) available
      
      i checking if greta conda environment available
      x greta conda environment not available
      
      * backend: "managed (uv) environment"
      * selected via: default
      i Conda environment not set up, but all dependencies available
      
      greta is ready to use!

---

    Code
      greta_sitrep()
    Message
      i checking if python available
      v python (v3.6) available
      
      i checking if TensorFlow available
      v TensorFlow (v2.0.0) available
      
      i checking if TensorFlow Probability available
      v TensorFlow Probability (v0.23.0) available
      
      i checking if greta conda environment available
      x greta conda environment not available
      
      * backend: "managed (uv) environment"
      * selected via: default
      i Conda environment not set up, but all dependencies available
      
      greta is ready to use!

---

    Code
      greta_sitrep()
    Message
      i checking if python available
      v python (v3.6) available
      
      i checking if TensorFlow available
      v TensorFlow (v2.0.0) available
      
      i checking if TensorFlow Probability available
      v TensorFlow Probability (v0.9.0) available
      
      i checking if greta conda environment available
      x greta conda environment not available
      
      * backend: "managed (uv) environment"
      * selected via: default
      i Conda environment not set up, but all dependencies available
      
      greta is ready to use!

# greta_sitrep warns greta conda env not available

    Code
      greta_sitrep()
    Message
      i checking if python available
      v python (v3.11) available
      
      i checking if TensorFlow available
      v TensorFlow (v2.15.1) available
      
      i checking if TensorFlow Probability available
      v TensorFlow Probability (v0.23.0) available
      
      i checking if greta conda environment available
      x greta conda environment not available
      
      * backend: "managed (uv) environment"
      * selected via: default
      i Conda environment not set up, but all dependencies available
      
      greta is ready to use!

# greta_sitrep works with quiet, minimal, and detailed options

    Code
      greta_sitrep(verbosity = "quiet")

---

    Code
      greta_sitrep(verbosity = "minimal")
    Message
      i checking if python available
      v python (v3.11) available
      
      i checking if TensorFlow available
      v TensorFlow (v2.15.1) available
      
      i checking if TensorFlow Probability available
      v TensorFlow Probability (v0.23.0) available
      
      i checking if greta conda environment available
      x greta conda environment not available
      
      * backend: "<backend>"
      * selected via: <source>
      i Conda environment not set up, but all dependencies available
      
      greta is ready to use!

---

    Code
      greta_sitrep(verbosity = "bananas")
    Condition
      Error in `greta_sitrep()`:
      ! `verbosity` must be one of "minimal", "detailed", or "quiet", not "bananas".

