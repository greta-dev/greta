# greta_remove_all_deps reports honestly when nothing exists

    Code
      res <- greta_remove_all_deps(ask = FALSE)
    Message
      i No 'greta-env-tf2' conda environment found.
      i No miniconda files found at /greta-test/no-such-miniconda
      i No reticulate-managed uv cache found at '/greta-test/cache/R/reticulate/uv'.
      i If reticulate is using a system uv, its cache is managed by uv itself (e.g. `uv cache clean`); greta does not remove it.
      i Nothing to remove.
      v Cleared any stored greta Python preference.

# greta_remove_all_deps reports removal when something was removed

    Code
      res <- greta_remove_all_deps(ask = FALSE)
    Message
      i removing 'greta-env-tf2' conda environment
      v greta-env-tf2 environment removed!
      i No miniconda files found at /greta-test/no-such-miniconda
      i No reticulate-managed uv cache found at '/greta-test/cache/R/reticulate/uv'.
      i If reticulate is using a system uv, its cache is managed by uv itself (e.g. `uv cache clean`); greta does not remove it.
      v Successfully removed the 'greta-env-tf2' conda environment.
      v Cleared any stored greta Python preference.
      i Restart R; greta will reinstall what it needs on next use.

# destroy_greta_deps reports honestly when nothing exists

    Code
      res <- destroy_greta_deps(ask = FALSE)
    Condition
      Warning:
      `destroy_greta_deps()` was deprecated in greta 0.6.0.
      i Please use `greta_remove()` instead.
      i Use `greta_remove("env")` and `greta_remove("miniconda")`, or `greta_remove()` to also clear the uv cache and stored preference.
    Message
      i No 'greta-env-tf2' conda environment found.
      i No miniconda files found at /greta-test/no-such-miniconda

# destroy_greta_deps reports removal when something was removed

    Code
      res <- destroy_greta_deps(ask = FALSE)
    Condition
      Warning:
      `destroy_greta_deps()` was deprecated in greta 0.6.0.
      i Please use `greta_remove()` instead.
      i Use `greta_remove("env")` and `greta_remove("miniconda")`, or `greta_remove()` to also clear the uv cache and stored preference.
    Message
      i removing 'greta-env-tf2' conda environment
      v greta-env-tf2 environment removed!
      i No miniconda files found at /greta-test/no-such-miniconda
      v Successfully removed the 'greta-env-tf2' conda environment.

