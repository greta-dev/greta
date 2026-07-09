# install_greta_deps errors appropriately

    Code
      install_greta_deps(timeout = 0.001)
    Message
      i Most users do not need `install_greta_deps()`: greta installs TensorFlow and TensorFlow Probability automatically (via uv) on first use.
      i Use `install_greta_deps()` to install a conda environment (e.g. offline, or to pin versions), then select it with `greta_set_python("conda")`.
      i See the installation vignette: `vignette(greta::installation)`.
      i Creating 'greta-env-tf2' conda environment using python v3.11, this may tak...
      x Creating 'greta-env-tf2' conda environment using python v3.11, this may tak...
      
    Condition
      Error in `new_install_process()`:
      ! Stopping as installation of greta dependencies took longer than 0.001 minutes You can increase the timeout time by increasing the `timeout` argument. For example, to wait 5 minutes: `install_greta_deps(timeout = 5)` Alternatively, you can perform the entire installation with: `reticulate::install_miniconda()` Then: `reticulate::conda_create(envname = 'greta-env-tf2', python_version = '3.11')` Then: `reticulate::py_install( packages = c( 'numpy', 'tensorflow==2.15.1', 'tensorflow-probability==0.23.0' ), envname = 'greta-env-tf2', pip = TRUE )` Then select it with `greta_set_python('conda')`, restart R, and load greta with: `library(greta)`

# install timeout message is captured

    Code
      cat(timeout_install_msg(timeout = 5, py_error = ""))
    Output
      Stopping as installation of greta dependencies took longer than 5 minutes
      You can increase the timeout time by increasing the `timeout` argument.
      For example, to wait 5 minutes:
      `install_greta_deps(timeout = 5)`
      Alternatively, you can perform the entire installation with:
      `reticulate::install_miniconda()`
      Then:
      `reticulate::conda_create(envname = 'greta-env-tf2', python_version = '3.11')`
      Then:
      `reticulate::py_install( packages = c( 'numpy', 'tensorflow==2.15.1', 'tensorflow-probability==0.23.0' ), envname = 'greta-env-tf2', pip = TRUE )`
      Then select it with `greta_set_python('conda')`, restart R, and load greta with: `library(greta)`

# install timeout message includes an underlying python error

    Code
      cat(timeout_install_msg(timeout = 5, py_error = "could not resolve env"))
    Output
      Stopping as installation of greta dependencies took longer than 5 minutes
      You can increase the timeout time by increasing the `timeout` argument.
      For example, to wait 5 minutes:
      `install_greta_deps(timeout = 5)`
      Alternatively, you can perform the entire installation with:
      `reticulate::install_miniconda()`
      Then:
      `reticulate::conda_create(envname = 'greta-env-tf2', python_version = '3.11')`
      Then:
      `reticulate::py_install( packages = c( 'numpy', 'tensorflow==2.15.1', 'tensorflow-probability==0.23.0' ), envname = 'greta-env-tf2', pip = TRUE )`
      Then select it with `greta_set_python('conda')`, restart R, and load greta with: `library(greta)`
      Additionally, the following error appeared:
      could not resolve env

# install failure message is captured

    Code
      cat(other_install_fail_msg("could not resolve env"))
    Output
      Stopping as installation of greta dependencies failed
      An error occured:
      could not resolve env
      You can perform the installation manually by doing the following:
      Restarting R, then running:
      `Sys.unsetenv('RETICULATE_PYTHON')`
      `Sys.setenv(RETICULATE_USE_MANAGED_VENV = 'yes')`
      `library(reticulate)`
      `py_require(packages = c('tensorflow==2.15.1', 'tensorflow-probability==0.23.0'), python_version = '3.11')`
      `py_require()`
      `py_config()`
      If this does not work, read through installation vignette (`vignette(greta::installation)`), or install a conda environment with `install_greta_deps()`.
      Also feel free to lodge an issue on github at:
      <https://github.com/greta-dev/greta/issues/new>

