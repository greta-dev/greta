# setters warn when RETICULATE_PYTHON overrides the stored preference

    Code
      greta_set_python()
    Message
      v Stored preference: the managed (uv) Python environment.
    Condition
      Warning:
      ! `RETICULATE_PYTHON` is set to '/fake/python' and takes precedence over the stored preference.
      i greta resolves Python in this order:
        1. `RETICULATE_PYTHON` - usually set in '~/.Renviron' or your shell environment
        2. Stored preference - set with `greta_set_python()`
        3. Auto-detected "greta-env-tf2" conda environment - created by `install_greta_deps()`
        4. The managed (uv) environment - the default, no setup needed
      i To use your stored preference, remove `RETICULATE_PYTHON` from '~/.Renviron' (or wherever it is set), then restart R.
      i See the installation vignette: `vignette(greta::installation)`.
    Message
      i After you restart R, greta will use:
      * backend: "user-specified Python"
      * python: '/fake/python'
      * selected via: RETICULATE_PYTHON environment variable

# setters do not warn when RETICULATE_PYTHON is unset

    Code
      greta_set_python()
    Message
      v Stored preference: the managed (uv) Python environment.
      i After you restart R, greta will use:
      * backend: "managed (uv) environment"
      * selected via: greta preference (see ?greta_set_python)

# greta_set_python() hints when a path is passed as backend

    Code
      greta_set_python("/usr/bin/python")
    Condition
      Error in `greta_set_python()`:
      ! `backend` must be one of "uv", "conda", or "path".
      i To use the Python at '/usr/bin/python', run `greta_set_python("path", path = "/usr/bin/python")`.

# greta_set_deps() reports what it stored

    Code
      greta_set_deps()
    Message
      v Stored dependency versions: TensorFlow "2.15.1", TensorFlow Probability "0.23.0", Python "3.11".
      i Restart R for this to take effect.

---

    Code
      greta_set_deps(NULL)
    Message
      v Cleared stored dependency versions; greta uses its defaults.
      i Restart R for this to take effect.

# report_offline_readiness() reports non-managed backends as ready

    Code
      report_offline_readiness(plan = plan)
    Message
      v offline-ready: this environment is already on disk; greta never downloads
      into it

# report_offline_readiness() reports managed backend states

    Code
      report_offline_readiness(plan, uv_cache, uv_offline = "")
    Message
      x will need internet on next start: uv cache not yet populated
      i See the installation vignette: `vignette(greta::installation)`.

---

    Code
      report_offline_readiness(plan, uv_cache, uv_offline = "1")
    Message
      x `UV_OFFLINE`=1 is set but the uv cache is not yet populated, so the next
      start may fail to resolve dependencies
      i See the installation vignette: `vignette(greta::installation)`.

---

    Code
      report_offline_readiness(plan, uv_cache, uv_offline = "")
    Message
      v offline-ready: uv cache present, offline mode will engage

---

    Code
      report_offline_readiness(plan, uv_cache, uv_offline = "1")
    Message
      v offline-ready: `UV_OFFLINE`=1 is set and the uv cache is populated

---

    Code
      report_offline_readiness(plan, uv_cache, uv_offline = "0")
    Message
      x will need internet on next start: `UV_OFFLINE`=0 forces online resolution
      i See the installation vignette: `vignette(greta::installation)`.

