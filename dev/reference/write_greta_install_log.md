# Write greta dependency installation log file

Writes what
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
and the loading of Python reported in this R session. Steps that have
not run in this session are marked as such, so run it before restarting
R.
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
writes it itself, whether installation succeeds or fails.

## Usage

``` r
write_greta_install_log(path = greta_install_logfile())
```

## Arguments

- path:

  a path with an HTML (.html) extension. Defaults to the
  `GRETA_INSTALLATION_LOG` environment variable if set, otherwise
  "greta-installation-logfile.html" in `tools::R_user_dir("greta")`.

## Value

nothing - writes to file
