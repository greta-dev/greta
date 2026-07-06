# setters warn when RETICULATE_PYTHON overrides the stored preference

    Code
      greta_set_python_uv()
    Message
      v Stored preference: the uv-managed Python environment.
    Condition
      Warning:
      ! `RETICULATE_PYTHON` is set to '/fake/python' and takes precedence over the stored preference.
      i greta resolves Python in this order:
        1. `RETICULATE_PYTHON` - usually set in '~/.Renviron' or your shell environment
        2. Stored preference - set with `greta_set_python_uv()`, `greta_set_python_conda_env()`, or `greta_set_python_path()`
        3. Auto-detected "greta-env-tf2" conda environment - created by `install_greta_deps()`
        4. The managed uv environment - the default, no setup needed
      i To use your stored preference, remove `RETICULATE_PYTHON` from '~/.Renviron' (or wherever it is set), then restart R.
    Message
      i After you restart R, greta will use:
      * backend: "user-specified Python"
      * python: '/fake/python'
      * selected via: RETICULATE_PYTHON environment variable

# setters do not warn when RETICULATE_PYTHON is unset

    Code
      greta_set_python_uv()
    Message
      v Stored preference: the uv-managed Python environment.
      i After you restart R, greta will use:
      * backend: "managed (uv) environment"
      * selected via: greta preference (see ?greta_set_python)

