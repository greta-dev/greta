# opt gives appropriate warning with deprecated optimisers in TFP

    This optimiser is deprecated and will be removed in greta "0.6.0".
    Please use a different optimiser.

---

    This optimiser is deprecated and will be removed in greta "0.6.0".
    Please use a different optimiser.

---

    This optimiser is deprecated and will be removed in greta "0.6.0".
    Please use a different optimiser.

# opt converges with TFP optimisers

    Code
      o <- opt(m, optimiser = bfgs(), max_iterations = 500)

---

    Code
      o <- opt(m, optimiser = nelder_mead(), max_iterations = 500)

# opt fails with defunct optimisers

    Code
      o <- opt(m, optimiser = powell())
    Condition
      Error in `optimiser_defunct_error()`:
      ! The optimiser, `powell()`, is defunct and has been removed in greta 0.5.0.
      Please use a different optimiser.
      See `?optimisers` for detail on which optimizers are removed.

---

    Code
      o <- opt(m, optimiser = momentum())
    Condition
      Error in `optimiser_defunct_error()`:
      ! The optimiser, `momentum()`, is defunct and has been removed in greta 0.5.0.
      Please use a different optimiser.
      See `?optimisers` for detail on which optimizers are removed.

---

    Code
      o <- opt(m, optimiser = cg())
    Condition
      Error in `optimiser_defunct_error()`:
      ! The optimiser, `cg()`, is defunct and has been removed in greta 0.5.0.
      Please use a different optimiser.
      See `?optimisers` for detail on which optimizers are removed.

---

    Code
      o <- opt(m, optimiser = newton_cg())
    Condition
      Error in `optimiser_defunct_error()`:
      ! The optimiser, `newton_cg()`, is defunct and has been removed in greta 0.5.0.
      Please use a different optimiser.
      See `?optimisers` for detail on which optimizers are removed.

---

    Code
      o <- opt(m, optimiser = l_bfgs_b())
    Condition
      Error in `optimiser_defunct_error()`:
      ! The optimiser, `l_bfgs_b()`, is defunct and has been removed in greta 0.5.0.
      Please use a different optimiser.
      See `?optimisers` for detail on which optimizers are removed.

---

    Code
      o <- opt(m, optimiser = tnc())
    Condition
      Error in `optimiser_defunct_error()`:
      ! The optimiser, `tnc()`, is defunct and has been removed in greta 0.5.0.
      Please use a different optimiser.
      See `?optimisers` for detail on which optimizers are removed.

---

    Code
      o <- opt(m, optimiser = cobyla())
    Condition
      Error in `optimiser_defunct_error()`:
      ! The optimiser, `cobyla()`, is defunct and has been removed in greta 0.5.0.
      Please use a different optimiser.
      See `?optimisers` for detail on which optimizers are removed.

---

    Code
      o <- opt(m, optimiser = slsqp())
    Condition
      Error in `optimiser_defunct_error()`:
      ! The optimiser, `slsqp()`, is defunct and has been removed in greta 0.5.0.
      Please use a different optimiser.
      See `?optimisers` for detail on which optimizers are removed.

# TF opt with `gradient_descent` fails with bad initial values

    Code
      o <- opt(m, hessian = TRUE, optimiser = gradient_descent())
    Condition
      Error in `self$run_minimiser()`:
      ! Detected numerical overflow during optimisation
      Please try one of the following:
      i Using different initial values
      i Using another optimiser. (E.g., instead of `gradient_descent()`, try `adam()`)

