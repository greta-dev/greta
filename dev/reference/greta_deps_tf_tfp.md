# Suggested valid Python dependencies for greta

**\[deprecated\]**

This dataset is deprecated and will be removed in a future release.
greta stopped validating against it in 0.6.0, and it is a fixed snapshot
that stops at TensorFlow 2.15.1 – well behind the versions greta now
installs – so consulting it gives the wrong answer about what works.

greta instead checks a range of TensorFlow versions weekly, on Linux,
macOS and Windows. Open the most recent run from
<https://github.com/greta-dev/greta/actions/workflows/install-check.yaml>
to see which combinations were tried and what each resolved to.

## Usage

``` r
greta_deps_tf_tfp
```

## Format

### `greta_deps_tf_tfp`

A data frame with 63 rows and 5 columns:

- os:

  Operating System

- tfp_version, tf_version:

  numeric versions in format major.minor.patch for TFP and TF

- python_version_min, python_version_max:

  numeric versions range in format major.minor.patch for Python

## Details

This is a dataset that contains suggested valid versions of Tensorflow
(TF), Tensorflow Probability (TFP), and Python for linux, mac, and
windows machines. It was constructed from
<https://www.tensorflow.org/install/source> and
<https://www.tensorflow.org/install/source_windows>, and by inspecting
<https://github.com/tensorflow/probability/releases>.

We recommend using the default versions provided in
[`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md).
