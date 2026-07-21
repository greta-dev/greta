# Installing dependencies

``` r

library(greta)
#> 
#> Attaching package: 'greta'
#> The following objects are masked from 'package:stats':
#> 
#>     binomial, cov2cor, poisson
#> The following objects are masked from 'package:base':
#> 
#>     %*%, %o%, apply, backsolve, beta, chol2inv, colMeans, colSums,
#>     diag, eigen, forwardsolve, gamma, identity, outer, rowMeans,
#>     rowSums, sweep, tapply
```

The greta R package requires Google’s
[TensorFlow](https://www.tensorflow.org/) and [TensorFlow
Probability](https://www.tensorflow.org/probability) python modules in
order to work. This vignette discusses how to install these.

## Quick start

For almost everyone, installing greta’s python dependencies will now
happen automatically.

The first time you use greta, if it doesn’t have the dependencies set
up, it will automatically install Python, TensorFlow, and TensorFlow
Probability.

``` r

install.packages("greta")

library(greta)
x <- normal(0, 1)
```

That’s it. No separate Python install, no conda, nothing else to run
first.

**What you’ll see:** the first greta array or distribution you create in
a session triggers a one-off setup step. greta prints a short progress
message while it downloads and configures Python, TensorFlow, and
TensorFlow Probability:

    #> ℹ Initialising python and checking dependencies, this may take a moment.
    #> ✔ Initialising python and checking dependencies ... done!

This requires an internet connection and can take a few minutes,
depending on your connection speed. It only happens **once** – greta
caches the result, so every session after that starts straight away. If
you don’t have internet access at all, see [Using greta
offline](#using-greta-offline) below for suggestions.

In previous (\< 0.6.0) versions of greta, you needed to call
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
to install the dependencies. This function is still usable, but it is
for an alternative, conda-based install, and is covered below in [I want
the conda workflow](#i-want-the-conda-workflow).

If anything about this doesn’t go to plan, skip to
[Troubleshooting](#troubleshooting).

## Did it work?

Run
[`greta_sitrep()`](https://greta-dev.github.io/greta/dev/reference/greta_sitrep.md)
(a “situation report”). It’s the first thing to run if you’re not sure
what greta has picked up, and the first thing to include if you ask for
help:

``` r

greta_sitrep()
```

On a machine that’s ready to go, you’ll see something like this:

    #> ℹ checking if python available
    #> ✔ python (v3.11) available
    #>
    #> ℹ checking if TensorFlow available
    #> ✔ TensorFlow (v2.15.1) available
    #>
    #> ℹ checking if TensorFlow Probability available
    #> ✔ TensorFlow Probability (v0.23.0) available
    #>
    #> ℹ greta conda environment: not used (managed (uv) environment active)
    #>
    #> • backend: "managed (uv) environment"
    #> • selected via: default
    #> ✔ offline-ready: uv cache present, offline mode will engage
    #> ℹ All dependencies available; greta is ready to use!

The “conda environment: not used” line is expected and fine for most
users: greta only uses a conda environment in the alternative conda
workflow, and by default runs on the managed (uv) environment (more on
that distinction below). The line that matters is the last one:
`greta is ready to use!`. If you see something else, or an error
instead, see [Troubleshooting](#troubleshooting).

As a final check, fit a tiny model and confirm sampling runs end to end:

``` r

x <- normal(0, 1)
m <- model(x)
draws <- mcmc(m)
draws
```

If that runs without error and returns some draws, greta is fully
working.

## How greta uses Python

You don’t need to understand this section to use greta – it’s here for
context, and because it helps make sense of the options further down.

As we said above, greta relies on Google’s
[TensorFlow](https://www.tensorflow.org/) and [TensorFlow
Probability](https://github.com/tensorflow/probability) to do its fast,
scalable linear algebra and MCMC. Those are Python packages, so greta
needs a working Python installation to hand them off to – even though
you only ever write R code. This is different from most R packages,
where CRAN builds and manages every dependency for you.

By default, greta manages this for you using
[`uv`](https://docs.astral.sh/uv/), a fast Python package installer, via
the reticulate package. The first time it’s needed, reticulate uses `uv`
to create a private, isolated Python environment containing just the
packages greta needs (currently Python 3.11, TensorFlow 2.15.1, and
TensorFlow Probability 0.23.0). It lives in its own cache directory,
away from any other Python you might have – so greta can’t interfere
with, or be broken by, Python you use for other things.

For most people, that’s the whole story: install greta, load it, use it.
The sections below cover the cases where you want something different.

## When the default isn’t right

The managed (uv) environment covers most users. Reach for one of these
instead if:

- you’re offline or on a restricted network – see [Using greta
  offline](#using-greta-offline),
- you already have a Python, or a TensorFlow install, you want greta to
  use – see [I already have Python or TensorFlow
  installed](#i-already-have-python-or-tensorflow-installed),
- you need specific, pinned versions of TensorFlow, TensorFlow
  Probability, or Python – see [I need specific dependency
  versions](#i-need-specific-dependency-versions), or
- you’d rather have a conda environment than the managed (uv) one – see
  [I want the conda workflow](#i-want-the-conda-workflow).

### Using greta offline

Both the default managed (uv) setup and
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
download packages from the internet, so a blocked or air-gapped network
stops either approach on a machine that has never set greta up before.
If your network blocks PyPI but allows conda, the conda route via
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
may work where the managed (uv) one does not.

There are three escape routes, in order of robustness:

1.  point greta at a Python environment that is already on disk,
2.  prime the managed (uv) environment once while online, or
3.  pin dependency versions so an offline environment matches what greta
    expects.
    [`greta_sitrep()`](https://greta-dev.github.io/greta/dev/reference/greta_sitrep.md)
    reports whether your current setup is ready to start offline (see
    the “offline-ready” line in [Did it work?](#did-it-work)).

**Already have a working Python with TF and TFP on disk** (for example,
provided by your institution, or copied onto an air-gapped machine)?
Point greta straight at it – see [I already have Python or TensorFlow
installed](#i-already-have-python-or-tensorflow-installed). This route
only ever *reads* what is already there; it never downloads anything, so
it works with no network at all.

#### Prime the managed (uv) environment while online

If you’re using the default managed (uv) environment, greta can start
offline once reticulate’s `uv` cache has been populated once, while
online: run [`library(greta)`](https://greta-dev.github.io/greta/) and
use greta once (for example `normal(0, 1)`), and you’re done. On its
next start, greta detects the populated cache and enables `uv`’s offline
mode automatically, so it no longer reaches out to PyPI.

You can override this yourself by setting the `UV_OFFLINE` environment
variable before loading greta; greta never touches a value you’ve
already set:

``` r

# force offline mode: e.g. before greta detects the cache, or on a machine
# where you copied the cache over
Sys.setenv(UV_OFFLINE = "1")

# force uv back online: for example to refresh the environment
Sys.setenv(UV_OFFLINE = "0")
```

To make either choice persistent, set `UV_OFFLINE=1` (or `UV_OFFLINE=0`)
in `~/.Renviron` instead (no
[`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html) needed there).

#### Pin dependency versions

If someone else installs a Python environment for your offline machine,
pin the versions greta expects with
[`greta_set_deps()`](https://greta-dev.github.io/greta/dev/reference/greta_set_deps.md)
so the two match (see [I need specific dependency
versions](#i-need-specific-dependency-versions)). The stored versions
are used by both the managed (uv) environment and
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md).
Note that pinning *non-default* versions this way means the managed
environment may still need one further online resolve even after priming
the cache, since greta’s offline shortcut is tied to its own default
pins.

If your connection drops partway through – for example greta errors on
first use with a message from `uv` like `error: Failed to fetch` (or,
with `UV_OFFLINE=1` set,
`Network connectivity is disabled, but the requested data wasn't found in the cache`)
– see [Troubleshooting](#troubleshooting).

### I already have Python or TensorFlow installed

If you manage your own Python, point greta at it with
[`greta_set_python()`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md),
and **restart R**. These routes only ever *read* what is already on disk
– they never download anything – so they work with no network at all.

`greta_set_python("path", ...)` accepts either a Python binary or an
environment directory (a virtualenv or conda prefix). Given a directory,
greta looks for `bin/python` inside it (`Scripts/python.exe` on
Windows):

``` r

# point at a specific Python binary
greta_set_python("path", path = "/opt/python-envs/greta/bin/python")

# or point at the environment directory and let greta find the binary
greta_set_python("path", path = "/opt/python-envs/greta")

# on Windows, an environment directory works the same way; greta looks for
# Scripts\\python.exe inside it
greta_set_python("path", path = "C:/python-envs/greta")
```

If your environment is a conda environment, you can instead select it by
name (this also never downloads anything):

``` r

greta_set_python("conda", name = "greta-env-tf2")
```

Alternatively, set the `RETICULATE_PYTHON` environment variable (for
example in `~/.Renviron`) to the Python binary, which takes precedence
over any stored preference. See [Advanced: how greta chooses a Python
environment](#advanced-how-greta-chooses-a-python-environment) for the
full precedence order.

Either way, the Python you point at needs TensorFlow and TensorFlow
Probability already installed for greta to work; run
[`greta_sitrep()`](https://greta-dev.github.io/greta/dev/reference/greta_sitrep.md)
afterwards to confirm.

### I need specific dependency versions

Most users never need to think about this: greta’s defaults (TensorFlow
2.15.1, TensorFlow Probability 0.23.0, Python 3.11) are the newest
versions greta supports, and are used automatically. Reach for this if
you need a different, specific combination – for example to match an
existing institutional setup, or an offline machine someone else
provisioned.

Build a version choice with
[`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md):

``` r

greta_deps_spec(
  tf_version = "2.15.0",
  tfp_version = "0.23.0",
  python_version = "3.10"
)
```

If you specify versions of TF, TFP, and Python that are not compatible
with each other,
[`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md)
errors before installation begins and suggests alternatives. The
combinations greta knows about are recorded in the `greta_deps_tf_tfp`
dataset, which we built from
<https://www.tensorflow.org/install/source#tested_build_configurations>,
<https://www.tensorflow.org/install/source_windows#tested_build_configurations>,
and the TFP release notes. Inspect it with:

``` r

View(greta_deps_tf_tfp)
```

To persist a version choice, store it with
[`greta_set_deps()`](https://greta-dev.github.io/greta/dev/reference/greta_set_deps.md).
The stored versions are then used by both the managed (uv) environment
(installed on the next load) and
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
(as its default `deps`):

``` r

greta_set_deps(
  greta_deps_spec(
    tf_version = "2.14.0",
    tfp_version = "0.22.1",
    python_version = "3.10"
  )
)

# clear the stored versions and return to greta's defaults
greta_remove("deps")
```

[`greta_set_deps()`](https://greta-dev.github.io/greta/dev/reference/greta_set_deps.md)
and
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
take effect on the next restart – see [I want the conda
workflow](#i-want-the-conda-workflow) if you’re building a conda
environment, or just restart R if you’re using the managed (uv)
environment.

### I want the conda workflow

You might want a dedicated conda environment instead of the managed (uv)
one when you:

- need a specific, pinned, reproducible set of versions (alongside
  [`greta_set_deps()`](https://greta-dev.github.io/greta/dev/reference/greta_set_deps.md),
  above),
- are setting up GPU / CUDA support, or
- are on a network that blocks PyPI but allows conda (see [Using greta
  offline](#using-greta-offline)).

[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
builds a conda environment named `greta-env-tf2`, installing TensorFlow
and TensorFlow Probability into it. By default it uses the same versions
as the managed environment (TensorFlow 2.15.1, TensorFlow Probability
0.23.0, Python 3.11), or your stored
[`greta_set_deps()`](https://greta-dev.github.io/greta/dev/reference/greta_set_deps.md)
choice if you’ve made one:

``` r

install_greta_deps()
```

Follow any prompts, then **restart R**. To make greta use this
environment, set it as your preference (then restart R again):

``` r

greta_set_python("conda")
```

Using a conda environment isolates these exact Python modules from other
Python installations, so only greta sees them. This avoids a class of
problems where, for example, updating TensorFlow elsewhere on your
machine would otherwise overwrite the version greta needs.

[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
also takes a `deps` argument (built with
[`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md)
– see [I need specific dependency
versions](#i-need-specific-dependency-versions)), `timeout` (minutes to
wait before a component times out, default 5), and `restart` (`"ask"`
(default), `"force"`, or `"no"`).

#### How `install_greta_deps()` works

For users who want the detail: greta runs the installation in a
separate, clean R session using [`callr`](https://callr.r-lib.org/), so
Python and reticulate are not already loaded. This also lets us route
the large amount of console output into a logfile, which you can open
with
[`open_greta_install_log()`](https://greta-dev.github.io/greta/dev/reference/open_greta_install_log.md).

If miniconda isn’t installed, greta installs it (a lightweight Python
distribution). If the `greta-env-tf2` environment doesn’t exist, greta
creates it for a Python version compatible with the requested TF and
TFP, then installs the TF and TFP modules. In interactive RStudio
sessions it then asks whether to restart R.

### Advanced: how greta chooses a Python environment

You don’t need this to use greta day to day – it’s here for when you’re
customising your setup, or diagnosing why greta picked up a Python you
weren’t expecting.

greta resolves which Python to use, in this order:

1.  The `RETICULATE_PYTHON` environment variable, if set – usually in
    `~/.Renviron`, your `.Rprofile`, or your shell environment. This
    always wins: it takes precedence over any stored preference.
2.  Your stored preference, set with
    [`greta_set_python()`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md).
3.  An auto-detected `greta-env-tf2` conda environment, created by
    [`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
    – kept so setups from older greta versions keep working after
    upgrading. If you’ve upgraded from an older greta that used this
    conda environment, greta detects and keeps using it, so upgrading to
    greta 0.6.0 does not change your setup.
4.  Otherwise, the managed (uv) environment – the default, no setup
    needed.

To see which Python environment greta is using right now – and which it
will use after you restart R – run
[`greta_sitrep()`](https://greta-dev.github.io/greta/dev/reference/greta_sitrep.md)
(see [Did it work?](#did-it-work)).

You can switch environments at any time with
[`greta_set_python()`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md),
then **restart R**. Each call reports what greta will resolve to on its
next load:

``` r

# use the managed (uv) environment (the default)
greta_set_python()
# use the "greta-env-tf2" conda environment
greta_set_python("conda")
# use a specific Python
greta_set_python("path", path = "/path/to/python")
# clear the choice; resolve automatically
greta_reset_python()
```

These store your choice (under `tools::R_user_dir("greta", "config")`)
so it persists across sessions.

For finer control, setting the `RETICULATE_PYTHON` environment variable
to a path (e.g. in your `.Rprofile`) takes precedence over the stored
preference:

``` r

Sys.setenv(RETICULATE_PYTHON = "/path/to/your/python")
library(greta)
```

Because `RETICULATE_PYTHON` takes precedence, a stored preference will
appear to be ignored while it is set. To go back to your stored
preference, remove `RETICULATE_PYTHON` from wherever it is set (for
example `~/.Renviron`, which you can open with
`usethis::edit_r_environ()`), then restart R.

See
[`?greta_set_python`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md)
for the full argument reference.

## Starting over

If your setup is in a confusing state and you just want a clean slate,
use
[`greta_remove()`](https://greta-dev.github.io/greta/dev/reference/greta_remove.md).
It takes a `what` argument to choose how much to remove:

``` r

# everything at once (the default) -- asks for confirmation once, then
# removes everything it finds
greta_remove()
# the "greta-env-tf2" conda environment
greta_remove("env")
# the miniconda installation
greta_remove("miniconda")
# reticulate's uv cache
greta_remove("uv_cache")
# just the stored Python preference (from greta_set_python())
greta_remove("preference")
# just the stored dependency versions (from greta_set_deps())
greta_remove("deps")
```

After removing everything, restart R; greta reinstalls what it needs the
next time you use it (an internet connection is needed again at that
point, unless you [prime the managed (uv) environment
offline](#prime-the-managed-uv-environment-while-online) or [point greta
at an existing Python](#i-already-have-python-or-tensorflow-installed)).

If you specifically want to rebuild the `greta-env-tf2` conda
environment,
[`reinstall_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
removes it and miniconda, then reinstalls both in one step.

## Troubleshooting

Work through these in order. Most problems are one of the first two.

**I changed something (ran
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md),
or called
[`greta_set_python()`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md)
/
[`greta_set_deps()`](https://greta-dev.github.io/greta/dev/reference/greta_set_deps.md))
and greta doesn’t seem to have picked it up.**

- Restart R.
- greta resolves its Python backend when the package loads, so a change
  only takes effect on the next fresh session – within the same session,
  greta keeps using whatever it already loaded.

**I’m not sure what greta is using, or whether it’s working.**

- Run
  [`greta_sitrep()`](https://greta-dev.github.io/greta/dev/reference/greta_sitrep.md)
  (see [Did it work?](#did-it-work)).
- It reports the resolved backend and the versions of Python, TF, and
  TFP, which usually points straight at the problem.

**greta says something like `error: Failed to fetch`, or (with
`UV_OFFLINE=1` set)
`Network connectivity is disabled, but the requested data wasn't found in the cache`.** -
`uv` tried to resolve dependencies without a working connection. To
recover:

1.  If greta’s environment had already been installed on this machine,
    force offline mode with `Sys.setenv(UV_OFFLINE = "1")` (or in
    `~/.Renviron`), then restart R: `uv` then resolves from its cache
    without touching the network.
2.  If it had not, reconnect once and use greta (for example
    `normal(0, 1)`) to prime the environment; after that greta starts
    offline automatically.
3.  If you cannot reconnect, point greta at an already-installed Python
    – see [I already have Python or TensorFlow
    installed](#i-already-have-python-or-tensorflow-installed) – and
    restart R.

**I used
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
and want to see what happened, or it seems to have failed.**

- Open the logfile with
  [`open_greta_install_log()`](https://greta-dev.github.io/greta/dev/reference/open_greta_install_log.md)
  and search it (Ctrl/Cmd+F) for “error” or “warn”.
- Even when there’s no obvious fix, the logfile is useful to share on a
  forum or a [greta GitHub
  issue](https://github.com/greta-dev/greta/issues).

**None of the above helped.**

- Start from a clean slate – see [Starting over](#starting-over) – then
  try
  [`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
  again.

**My network blocks package downloads entirely.**

- See [Using greta offline](#using-greta-offline).

**The helpers above still don’t work.** As a last resort, you can
install the Python modules yourself into a conda environment:

``` r

reticulate::install_miniconda()
reticulate::conda_create(
  envname = "greta-env-tf2",
  python_version = "3.11"
)
reticulate::conda_install(
  envname = "greta-env-tf2",
  packages = c(
    "tensorflow-probability==0.23.0",
    "tensorflow==2.15.1"
  )
)
```

- Then point greta at it with `greta_set_python("conda")` and restart R.

- If this still doesn’t work for you, please post on the [greta
  forum](https://forum.greta-stats.org/), or open a [GitHub
  issue](https://github.com/greta-dev/greta/issues/new), and include
  your
  [`greta_sitrep()`](https://greta-dev.github.io/greta/dev/reference/greta_sitrep.md)
  output.
