# cummax and cummin functions error informatively

    Code
      fun(x)
    Error <simpleError>
      cummax not yet implemented for greta

---

    Code
      fun(x)
    Error <simpleError>
      cummin not yet implemented for greta

# complex number functions error informatively

    Code
      fun(x)
    Error <simpleError>
      greta does not yet support complex numbers

---

    Code
      fun(x)
    Error <simpleError>
      greta does not yet support complex numbers

---

    Code
      fun(x)
    Error <simpleError>
      greta does not yet support complex numbers

---

    Code
      fun(x)
    Error <simpleError>
      greta does not yet support complex numbers

---

    Code
      fun(x)
    Error <simpleError>
      greta does not yet support complex numbers

# cumulative functions error as expected

    Code
      cumsum(a)
    Error <simpleError>
      `x` must be a column vector
      but `x` has dimensions 1x 5

---

    Code
      cumsum(b)
    Error <simpleError>
      `x` must be a column vector
      but `x` has dimensions 5x 1x 1

---

    Code
      cumprod(a)
    Error <simpleError>
      `x` must be a column vector
      but `x` has dimensions 1x 5

---

    Code
      cumprod(b)
    Error <simpleError>
      `x` must be a column vector
      but `x` has dimensions 5x 1x 1

# solve and sweep and kronecker error as expected

    Code
      solve(b, a)
    Error <simpleError>
      Arrays are not both 2D
      `a` and `b` must both be 2D, but `a` has dimensions: 5 x 25 x 2

---

    Code
      solve(c, b)
    Error <simpleError>
      Arrays are not both 2D
      `a` and `b` must both be 2D, but `b` has dimensions: 5 x 25 x 2

---

    Code
      solve(a, a)
    Error <simpleError>
      `a` is not square
      x `a` must be square, but has 5 rows and 25 columns

---

    Code
      solve(a)
    Error <simpleError>
      `a` is not square
      x `a` must be square, but has 5 rows and 25 columns

---

    Code
      solve(c, t(a))
    Error <simpleError>
      Number of rows not equal
      x `b` must have the same number of rows as `a` (5), but has 25 rows instead

---

    Code
      sweep(b, 1, stats)
    Error <simpleError>
      Array not 2D
      x `x` must be a 2D array, but has 3 dimensions

---

    Code
      sweep(a, 3, stats)
    Error <simpleError>
      MARGIN can only be 1 or 2

---

    Code
      sweep(a, 1, c(stats, stats))
    Error <simpleError>
      The number of elements of `stats` does not match `dim(x)[MARGIN]`

---

    Code
      sweep(a, 1, t(stats))
    Error <simpleError>
      `stats` not a column vector array
      `stats` must be a column vector array
      x `stats` has dimensions 1 x 5

---

    Code
      sweep(a, 2, stats)
    Error <simpleError>
      The number of elements of `stats` does not match `dim(x)[MARGIN]`

---

    Code
      kronecker(a, b)
    Error <simpleError>
      Not a 2D array
      `X` must be a 2D array, but has 2 dimensions

---

    Code
      kronecker(b, c)
    Error <simpleError>
      Not a 2D array
      `X` must be a 2D array, but has 3 dimensions

# colSums etc. error as expected

    Code
      colSums(x, dims = 3)
    Error <simpleError>
      invalid `dims`

---

    Code
      rowSums(x, dims = 3)
    Error <simpleError>
      invalid `dims`

---

    Code
      colMeans(x, dims = 3)
    Error <simpleError>
      invalid `dims`

---

    Code
      rowMeans(x, dims = 3)
    Error <simpleError>
      invalid `dims`

# forwardsolve and backsolve error as expected

    Code
      forwardsolve(a, b, k = 1)
    Error <simpleError>
      `k` must equal `ncol(l)` for greta arrays

---

    Code
      backsolve(a, b, k = 1)
    Error <simpleError>
      `k` must equal `ncol(r)` for greta arrays

---

    Code
      forwardsolve(a, b, transpose = TRUE)
    Error <simpleError>
      transpose must be FALSE for greta arrays

---

    Code
      backsolve(a, b, transpose = TRUE)
    Error <simpleError>
      transpose must be FALSE for greta arrays

# tapply errors as expected

    Code
      tapply(b, group, "sum")
    Error <simpleError>
      `x` must be 2D greta array with one column
      However `x` has dimensions 10 x 2

---

    Code
      tapply(a, as_data(group), "sum")
    Error <simpleError>
      INDEX cannot be a greta array

# ignored options are errored/warned about

    Code
      round(x, 2)
    Error <simpleError>
      the 'digits' argument of `round()` cannot be set for greta arrays
      greta arrays can only be rounded to the nearest integer, so the 'digits'
      argument cannot be set

---

    Code
      chol(x, pivot = TRUE)
    Warning <simpleWarning>
      `chol()` options are ignored for greta arrays
    Output
      greta array (operation)
      
           [,1] [,2] [,3]
      [1,]  ?    ?    ?  
      [2,]  ?    ?    ?  
      [3,]  ?    ?    ?  

---

    Code
      chol2inv(x, LINPACK = TRUE)
    Warning <simpleWarning>
      The `LINPACK` argument is ignored for greta arrays, and has also been defunct
      since R 3.1.0
    Output
      greta array (operation)
      
           [,1] [,2] [,3]
      [1,]  ?    ?    ?  
      [2,]  ?    ?    ?  
      [3,]  ?    ?    ?  

---

    Code
      chol2inv(x, size = 1)
    Warning <simpleWarning>
      `size` is ignored for greta arrays
    Output
      greta array (operation)
      
           [,1] [,2] [,3]
      [1,]  ?    ?    ?  
      [2,]  ?    ?    ?  
      [3,]  ?    ?    ?  

---

    Code
      rdist(x, compact = TRUE)
    Warning <simpleWarning>
      `compact` is ignored for greta arrays
    Output
      greta array (operation)
      
           [,1] [,2] [,3]
      [1,]  ?    ?    ?  
      [2,]  ?    ?    ?  
      [3,]  ?    ?    ?  

# incorrect dimensions are errored about

    Code
      t(x)
    Error <simpleError>
      only 2D arrays can be transposed

---

    Code
      aperm(x, 2:1)
    Error <simpleError>
      perm must be a reordering of the dimensions: 1, 2, and 3
      but was: 2 and 1

---

    Code
      chol(x)
    Error <simpleError>
      Expecting '}'

---

    Code
      chol(y)
    Error <simpleError>
      Expecting '}'

---

    Code
      chol2symm(x)
    Error <simpleError>
      `chol2symm()` must have two-dimensional, square, upper-triangular greta
      arrays
      `dim(x)` returns: 3, 3, and 3

---

    Code
      chol2symm(y)
    Error <simpleError>
      `chol2symm()` must have two-dimensional, square, upper-triangular greta
      arrays
      `dim(x)` returns: 3 and 4

---

    Code
      eigen(x)
    Error <simpleError>
      only two-dimensional, square, symmetric greta arrays can be
      eigendecomposed

---

    Code
      eigen(y)
    Error <simpleError>
      only two-dimensional, square, symmetric greta arrays can be
      eigendecomposed

---

    Code
      rdist(x, y)
    Error <simpleError>
      `x1` and `x2` must have the same number of columns
      However `ncol(x1)` = 1 and `ncol(x2)` = 4

