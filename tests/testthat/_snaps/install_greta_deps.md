# install_greta_deps errors appropriately

    Stopping as installation of greta dependencies took longer than 0.001 minutes
    You can increase the timeout time by increasing the `timeout` argument.
    For example, to wait 5 minutes:
    `install_greta_deps(timeout = 5)`
    Alternatively, you can perform the entire installation with:
    `reticulate::install_miniconda()`
    Then:
    `reticulate::conda_create(envname = 'greta-env', python_version = '3.7')`
    Then:
    `reticulate::conda_install(envname = 'greta-env', packages = c('numpy==1.16.4', 'tensorflow-probability==0.7.0', 'tensorflow==1.14.0'))`
    Then, restart R, and load greta with:
    `library(greta)`

