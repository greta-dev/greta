# Cholesky factor of Wishart should be an upper triangular matrix

    Code
      calculate(chol_x, nsim = 1)
    Condition
      Warning:
      Cannot use `calculate()` to sample a cholesky factor of a greta array
      E.g., `x_chol <- chol(wishart(df = 4, Sigma = diag(3)))`
      `calculate(x_chol)`
      This is due to an internal issue with how greta handles cholesky representations.
      See issue here on github for more details:
      <https://github.com/greta-dev/greta/issues/593>
    Output
      $chol_x
      , , 1
      
           [,1] [,2] [,3]
      [1,]    1    1    1
      
      , , 2
      
           [,1] [,2] [,3]
      [1,]    1    1    1
      
      , , 3
      
           [,1] [,2] [,3]
      [1,]    1    1    1
      
      

---

    When using `calculate()` to sample a greta array with a cholesky factor, the output can sometimes be unreliable.
    See issue here on github for more details:
    <>

# Cholesky factor of LJK_correlation should be an upper triangular matrix

    When using `calculate()` to sample a greta array with a cholesky factor, the output can sometimes be unreliable.
    See issue here on github for more details:
    <>

