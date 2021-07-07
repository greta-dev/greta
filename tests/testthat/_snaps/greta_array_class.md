# print and summary work

    Code
      ga_data
    Output
      greta array (data)
      
           [,1] [,2] [,3]
      [1,]    1    4    7
      [2,]    2    5    8
      [3,]    3    6    9

---

    Code
      summary(ga_data)
    Output
      'data' greta array with 9 elements (3x3)  
      
             V1            V2            V3     
       Min.   :1.0   Min.   :4.0   Min.   :7.0  
       1st Qu.:1.5   1st Qu.:4.5   1st Qu.:7.5  
       Median :2.0   Median :5.0   Median :8.0  
       Mean   :2.0   Mean   :5.0   Mean   :8.0  
       3rd Qu.:2.5   3rd Qu.:5.5   3rd Qu.:8.5  
       Max.   :3.0   Max.   :6.0   Max.   :9.0  

---

    Code
      ga_stochastic
    Output
      greta array (variable following a normal distribution)
      
           [,1]
      [1,]  ?  

---

    Code
      summary(ga_stochastic)
    Output
      'variable' greta array with 1 element following a normal distribution 
      
        (values currently unknown)

---

    Code
      ga_operation
    Output
      greta array (operation)
      
           [,1] [,2] [,3]
      [1,]  ?    ?    ?  
      [2,]  ?    ?    ?  
      [3,]  ?    ?    ?  

---

    Code
      summary(ga_operation)
    Output
      'operation' greta array with 9 elements (3x3)  
      
        (values currently unknown)

---

    Code
      z
    Output
      greta array (operation)
      
           [,1] [,2] [,3]
      [1,] 1     ?   0   
      [2,] 1     ?   0   
      [3,] 1     ?   0   

---

    Code
      n
    Output
      greta array (operation)
      
           [,1] [,2] [,3]
      [1,] 1     ?    ?  
      [2,] 1     ?    ?  
      [3,] 1     ?    ?  

