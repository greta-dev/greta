# install_greta_deps errors appropriately

    Code
      install_greta_deps(timeout = 0.001)
    Message
      i Creating 'greta-env-tf2' conda environment using python v3.10, this may tak...
      x Creating 'greta-env-tf2' conda environment using python v3.10, this may tak...
      
    Condition
      Error in `new_install_process()`:
      ! Stopping as installation of greta dependencies took longer than 0.001 minutes You can increase the timeout time by increasing the `timeout` argument. For example, to wait 5 minutes: `install_greta_deps(timeout = 5)` Alternatively, you can perform the entire installation with: `reticulate::install_miniconda()` Then: `reticulate::conda_create(envname = 'greta-env-tf2', python_version = '3.8')` Then: `reticulate::py_install( packages = c( 'numpy', 'tensorflow', 'tensorflow-probability' ), pip = TRUE )` Then, restart R, and load greta with: `library(greta)`

