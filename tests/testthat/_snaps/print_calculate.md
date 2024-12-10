# calculate print method is different for different inputs

    Code
      x_sim_10
    Output
      $x
      , , 1
      
                  [,1]
       [1,] -1.3716505
       [2,] -0.3086943
       [3,] -1.2029317
       [4,] -0.7109460
       [5,] -0.4889447
       [6,]  1.1541374
       [7,]  0.8869046
       [8,] -0.3983025
       [9,]  0.4208952
      [10,] -1.1759923
      
      

---

    Code
      x_draws_values
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 10
      * Chains = 4
      * Thinning = 1
      
      -- Chain 1 (iterations 1...5) --------------------------------------------------
    Output
                     x
      [1,] -0.08002652
      [2,] -1.83388724
      [3,] -0.16133841
      [4,] -0.01941975
      [5,] -0.43649773
    Message
      i 5 more draws
      Use `print(n = ...)` to see more draws
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

---

    Code
      x_draws_10
    Output
      $x
      , , 1
      
                  [,1]
       [1,]  0.1224691
       [2,]  1.6876720
       [3,] -2.0192331
       [4,] -1.2172102
       [5,]  0.8676729
       [6,] -0.1613384
       [7,] -1.0970895
       [8,] -2.0362168
       [9,]  0.9766937
      [10,]  0.9396561
      
      

