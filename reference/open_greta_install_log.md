# Read a greta logfile

This is a convenience function to facilitate reading logfiles. It opens
a HTML browser using
[`utils::browseURL()`](https://rdrr.io/r/utils/browseURL.html). It will
search for the environment variable "GRETA_INSTALLATION_LOG" or default
to `tools::R_user_dir("greta")`. To set "GRETA_INSTALLATION_LOG" you can
use `Sys.setenv('GRETA_INSTALLATION_LOG'='path/to/logfile.html')`. Or
use
[`greta_set_install_logfile()`](https://greta-dev.github.io/greta/reference/greta_set_install_logfile.md)
to set the path, e.g.,
`greta_set_install_logfile('path/to/logfile.html')`.

## Usage

``` r
open_greta_install_log()
```

## Value

opens a URL in your default HTML browser.
