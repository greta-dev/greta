# Greta Situation Report

This checks if Python, Tensorflow, Tensorflow Probability, and the greta
conda environment are available, and also loads and initialises python

## Usage

``` r
greta_sitrep(verbosity = c("minimal", "detailed", "quiet"))
```

## Arguments

- verbosity:

  character. How verbose the output of the situation report. Possible
  options: "minimal" (default), "detailed", and "quiet". "Minimal"
  provides just information in python version, tensorflow version,
  tensorflow probability, and whether greta conda environment is
  available. "Quiet" presents no information, but prepares greta to be
  used. "Detailed" gives information on the version and path for R,
  greta, python, tensorflow, tensorflow probability, the greta conda
  environment, and a statement on greta usability.

## Value

Message on greta situation report. See "verbosity" parameter details
above for more information.

## Examples

``` r
if (FALSE) { # \dontrun{
greta_sitrep()
} # }
```
