# FAQ

## How do I install `greta` dependencies?

Under the hood, `greta` uses Google’s
[TensorFlow](https://www.tensorflow.org/) and
[tensorflow-probability](https://github.com/tensorflow/probability)
python packages. For most users there is nothing to install: the first
time you use `greta` in a session, it automatically installs a
compatible Python, TensorFlow, and TensorFlow Probability via
[`uv`](https://docs.astral.sh/uv/). So
[`library(greta)`](https://greta-dev.github.io/greta/) followed by your
first model usually just works.

If you need a conda environment instead (for example on an offline
network), or want to pin specific versions, `greta` provides an
installation helper,
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md),
which installs the needed versions into a “greta-env-tf2” conda
environment. This isolates these exact python modules from other python
installations, so that only `greta` will see them. After running it,
point `greta` at the environment with `greta_set_python("conda")` and
restart R. See the “Installing dependencies” vignette for the full
details.

If you need to use `greta` without internet access, see the “Using greta
offline” section of the “Installing dependencies” vignette.

## How do I know if `greta` has the right versions of Python dependencies installed?

The most reliable check is
[`greta_sitrep()`](https://greta-dev.github.io/greta/dev/reference/greta_sitrep.md)
(“situation report”). It prints the Python, TensorFlow, and TensorFlow
Probability that greta has resolved, which environment they came from,
and whether greta is ready to use:

``` r

library(greta)
greta_sitrep()
```

If everything is in place it reports the versions and tells you greta is
ready; if something is missing or incompatible it tells you what.

You can also just use greta and watch what happens. Running
[`library(greta)`](https://greta-dev.github.io/greta/) first prints a
message about which objects are masked from base R (these only apply to
greta arrays, so they don’t affect functions like `%*%` or `rowMeans` on
ordinary R objects). Then run some greta code, such as:

``` r

normal(0, 1)
```

The first time you use greta in a session, Python is initialised and
greta installs or checks its dependencies (TensorFlow and TensorFlow
Probability). You’ll see a short progress message:

    #> ℹ Initialising python and checking dependencies, this may take a moment.
    #> ✔ Initialising python and checking dependencies ... done!

After that, your greta code returns as normal. If Python can’t be found,
or the dependencies can’t be resolved, greta raises an error describing
the problem (and reticulate prints the underlying resolver output above
it). In that case we recommend restarting R and trying again. If the
automatic setup can’t reach the internet, or you need a conda
environment, run
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
and then `greta_set_python("conda")`. If that does not work there is
another installation approach below.

## Is there an alternative way to install Python dependencies?

If the previous installation helper did not work, you can try the
following:

``` r

reticulate::install_miniconda()
reticulate::conda_create(
  envname = "greta-env-tf2",
  python_version = "3.11"
)
reticulate::py_install(
  packages = c(
    "numpy",
    "tensorflow==2.15.1",
    "tensorflow-probability==0.23.0"
  ),
  envname = "greta-env-tf2",
  pip = TRUE
)
```

Which will install the python modules into a conda environment named
“greta-env-tf2”. Point `greta` at it with `greta_set_python("conda")`
and restart R.

If these instructions do not work for you, please post on the [greta
forum](https://forum.greta-stats.org/) and we will respond to you as
soon as we can.

## I get the message: “Your CPU supports instructions that this TensorFlow binary was not compiled to use: AVX AVX2”

Briefly, this is a warning that you can safely ignore. Less briefly, it
means that there can be some optimisations made with a special install
of tensorflow that mean it will run faster on your machine. For more
details, see [this stack overflow
thread](https://stackoverflow.com/a/47227886/3764040). We have noted
this issue in [this github
issue](https://github.com/greta-dev/greta/issues/472), and might in the
future make it easier to resolve.
