# %*% errors informatively

    incompatible dimensions: "3x4" vs "1x4"

---

    only two-dimensional <greta_array>s can be matrix-multiplied
    dimensions recorded were 3 and 4

# %*% works when one is a non-greta array

    Code
      x %*% y
    Output
           [,1]
      [1,]    3
      [2,]    3

---

    Code
      x %*% as_data(y)
    Output
      greta array (operation)
      
           [,1]
      [1,]  ?  
      [2,]  ?  

---

    Code
      as_data(x) %*% y
    Output
      greta array (operation)
      
           [,1]
      [1,]  ?  
      [2,]  ?  

---

    Code
      as_data(x) %*% as_data(y)
    Output
      greta array (operation)
      
           [,1]
      [1,]  ?  
      [2,]  ?  

---

    Code
      calculate(res_1, nsim = 1)
    Output
      $res_1
      , , 1
      
           [,1] [,2]
      [1,]    3    3
      
      

---

    Code
      calculate(res_2, nsim = 1)
    Output
      $res_2
      , , 1
      
           [,1] [,2]
      [1,]    3    3
      
      

---

    Code
      calculate(res_3, nsim = 1)
    Output
      $res_3
      , , 1
      
           [,1] [,2]
      [1,]    3    3
      
      

