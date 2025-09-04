# check_tf_version errors when have_python, _tf, or _tfp is FALSE

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

# check_tf_version fails when tfp not available

    Code
      check_tf_version("error")
    Message
      i Initialising python and checking dependencies, this may take a moment.
      x Initialising python and checking dependencies, this may take a moment. ... ...
      
    Condition
      Error in `check_tf_version()`:
      ! x The expected python packages are not available
      i We recommend installing them (in a fresh R session) with:
      `install_greta_deps()`
      or
      `reinstall_greta_deps()`
      (Note: Your R session should not have initialised Tensorflow yet.)
      i For more information, see `?install_greta_deps`

# greta_sitrep warns when have_python, _tf, or _tfp is FALSE

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
      greta_sitrep()
    Message
      i checking if python available
      v python (v3.11) available
      
      i checking if TensorFlow available
      v TensorFlow (v2.15.1) available
      
      i checking if TensorFlow Probability available
      v TensorFlow Probability (v0.23.0) available
      
      i checking if greta conda environment available
      v greta conda environment available
      
      i greta is ready to use!

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
      v greta conda environment available
      

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
      v greta conda environment available
      

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
      v greta conda environment available
      

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
      v greta conda environment available
      
      i greta is ready to use!

---

    Code
      greta_sitrep(verbosity = "bananas")
    Condition
      Error in `greta_sitrep()`:
      ! `verbosity` must be one of "minimal", "detailed", or "quiet", not "bananas".

---

    Code
      greta_sitrep(verbosity = "detailed")
    Message
      
      -- R ---------------------------------------------------------------------------
      * version: 4.5.1
      * path: '/Library/Frameworks/R.framework/Resources'
      
      -- greta -----------------------------------------------------------------------
      * version: 0.5.1
      * path: '/Users/nick_1/github/greta-dev/greta'
      
      -- python ----------------------------------------------------------------------
      i checking if python available
      v python (v3.11) available
      
      * path: '/Users/nick_1/Library/r-miniconda-arm64'
      
      -- greta conda environment -----------------------------------------------------
      i checking if greta conda environment available
      v greta conda environment available
      
      * path: '/Users/nick/Library/r-miniconda-arm64/envs/greta-env-tf2/bin/python'
      * Encountered an error in running:
      * `conda list -n greta-env-tf2`
      * `error in running command`
      * It is possible conda is not installed
      
      -- TensorFlow ------------------------------------------------------------------
      i checking if TensorFlow available
      v TensorFlow (v2.15.1) available
      
      * R path: '/Users/nick_1/Library/R/arm64/4.5/library/tensorflow'
      * Exists in conda env:
      
      -- TensorFlow Probability ------------------------------------------------------
      i checking if TensorFlow Probability available
      v TensorFlow Probability (v0.23.0) available
      
      * Exists in conda env:
      
      -- Is greta ready to use? ------------------------------------------------------
      i greta is ready to use!
      i Use the following code to list available python modules in `greta-env-tf2`:
      `system(paste('conda list -n', 'greta-env-tf2'), intern = TRUE)`

