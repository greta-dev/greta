# %*% errors informatively

    Code
      a %*% b
    Condition
      Error in `a %*% b`:
      ! Incompatible dimensions: "3x4" vs "1x4"

---

    Code
      a %*% c
    Condition
      Error in `a %*% c`:
      ! Only two-dimensional <greta_array>s can be matrix-multiplied
      Dimensions for each are:
      `x`: "3x4"
      `y`: "2x2x2"

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
    Message
      greta array <operation>
      
    Output
           [,1]
      [1,]  ?  
      [2,]  ?  
    Message
      

---

    Code
      as_data(x) %*% y
    Message
      greta array <operation>
      
    Output
           [,1]
      [1,]  ?  
      [2,]  ?  
    Message
      

---

    Code
      as_data(x) %*% as_data(y)
    Message
      greta array <operation>
      
    Output
           [,1]
      [1,]  ?  
      [2,]  ?  
    Message
      

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
      
      

