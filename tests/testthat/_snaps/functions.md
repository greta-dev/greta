# cummax and cummin functions error informatively

    `cummax()` not yet implemented for greta

---

    `cummin()` not yet implemented for greta

# complex number functions error informatively

    greta does not yet support complex numbers

---

    greta does not yet support complex numbers

---

    greta does not yet support complex numbers

---

    greta does not yet support complex numbers

---

    greta does not yet support complex numbers

# cumulative functions error as expected

    `x` must be a column vector
    but `x` has dimensions 1x5

---

    `x` must be a column vector
    but `x` has dimensions 5x1x1

---

    `x` must be a column vector
    but `x` has dimensions 1x5

---

    `x` must be a column vector
    but `x` has dimensions 5x1x1

# solve and sweep and kronecker error as expected

    Code
      solve(b, a)
    Condition
      Error in `solve()`:
      ! `a must be two dimensional`
      However, `a` has dimensions: 5x25x2

---

    Code
      solve(c, b)
    Condition
      Error in `solve()`:
      ! `b must be two dimensional`
      However, `b` has dimensions: 5x25x2

---

    Code
      solve(a, a)
    Condition
      Error in `solve()`:
      ! `a` is not square
      x `a` must be square, but has 5 rows and 25 columns

---

    Code
      solve(a)
    Condition
      Error in `solve()`:
      ! `a` is not square
      x `a` must be square, but has 5 rows and 25 columns

---

    Code
      solve(c, t(a))
    Condition
      Error in `solve()`:
      ! Number of rows not equal
      x `b` must have the same number of rows as `a` (5), but has 25 rows instead

---

    Code
      sweep(b, 1, stats)
    Condition
      Error in `sweep()`:
      ! `x must be two dimensional`
      However, `x` has dimensions: 5x25x2

---

    Code
      sweep(a, 3, stats)
    Condition
      Error in `sweep()`:
      ! MARGIN can only be 1 or 2

---

    Code
      sweep(a, 1, c(stats, stats))
    Condition
      Error in `sweep()`:
      ! The number of elements of `stats` does not match `dim(x)[MARGIN]`

---

    Code
      sweep(a, 1, t(stats))
    Condition
      Error in `sweep()`:
      ! `stats` not a column vector array
      `stats` must be a column vector array
      x `stats` has dimensions 1x5

---

    Code
      sweep(a, 2, stats)
    Condition
      Error in `sweep()`:
      ! The number of elements of `stats` does not match `dim(x)[MARGIN]`

---

    Code
      kronecker(a, b)
    Condition
      Error in `kronecker()`:
      ! `Y must be two dimensional`
      However, `Y` has dimensions: 5x25x2

---

    Code
      kronecker(b, c)
    Condition
      Error in `kronecker()`:
      ! `X must be two dimensional`
      However, `X` has dimensions: 5x25x2

# colSums etc. error as expected

    invalid `dims`

---

    invalid `dims`

---

    invalid `dims`

---

    invalid `dims`

# forwardsolve and backsolve error as expected

    `k` must equal `ncol(l)` for <greta_array>s

---

    `k` must equal `ncol(r)` for <greta_array>s

---

    transpose must be FALSE for <greta_array>s

---

    transpose must be FALSE for <greta_array>s

# tapply errors as expected

    `x` must be 2D greta array with one column
    However `x` has dimensions 10x2

---

    INDEX cannot be a greta array

# ignored options are errored/warned about

    the "digits" argument of `round()` cannot be set for <greta_array>s
    <greta_array>s can only be rounded to the nearest integer, so the "digits" argument cannot be set

---

    `chol()` options are ignored for <greta_array>s

---

    The `LINPACK` argument is ignored for <greta_array>s, and has also been defunct since R 3.1.0

---

    `size` is ignored for <greta_array>s

---

    `compact` is ignored for <greta_array>s

# incorrect dimensions are errored about

    only 2D arrays can be transposed

---

    `perm` must be a reordering of the dimensions: 1, 2, and 3
    but was: 2 and 1

---

    only two-dimensional, square, symmetric <greta_array>s can be Cholesky decomposed
    `dim(x)` returns: 3, 3, and 3

---

    only two-dimensional, square, symmetric <greta_array>s can be Cholesky decomposed
    `dim(x)` returns: 3 and 4

---

    `chol2symm()` must have two-dimensional, square, upper-triangular <greta_array>s
    `dim(x)` returns: 3, 3, and 3

---

    `chol2symm()` must have two-dimensional, square, upper-triangular <greta_array>s
    `dim(x)` returns: 3 and 4

---

    only two-dimensional, square, symmetric <greta_array>s can be eigendecomposed

---

    only two-dimensional, square, symmetric <greta_array>s can be eigendecomposed

---

    `x1` and `x2` must have the same number of columns
    However `ncol(x1)` = 1 and `ncol(x2)` = 4

