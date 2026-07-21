# Dispatch optimisation method to right class

Should also allow for building other methods in the future

## Usage

``` r
run_optimiser(self)
```

## Arguments

- self:

  optimiser of class: `tf_optimiser`, `tfp_optimiser`, or
  `tf_compat_optimiser`.

## Value

Invisibly returns `NULL`; called for its side effect of running the
optimiser and updating its internal state.
