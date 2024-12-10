# print and summary work

    Code
      ga_data
    Message
      greta array <data>
      
    Output
           [,1] [,2] [,3]
      [1,]    1    4    7
      [2,]    2    5    8
      [3,]    3    6    9
    Message
      

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
    Message
      greta array <variable following a normal distribution>
      
    Output
           [,1]
      [1,]  ?  
    Message
      

---

    Code
      summary(ga_stochastic)
    Output
      'variable' greta array with 1 element following a normal distribution 
      
        (values currently unknown)

---

    Code
      ga_operation
    Message
      greta array <operation>
      
    Output
           [,1] [,2] [,3]
      [1,]  ?    ?    ?  
      [2,]  ?    ?    ?  
      [3,]  ?    ?    ?  
    Message
      

---

    Code
      summary(ga_operation)
    Output
      'operation' greta array with 9 elements (3x3)  
      
        (values currently unknown)

---

    Code
      z
    Message
      greta array <operation>
      
    Output
           [,1] [,2] [,3]
      [1,] 1     ?   0   
      [2,] 1     ?   0   
      [3,] 1     ?   0   
    Message
      

---

    Code
      n
    Message
      greta array <operation>
      
    Output
           [,1] [,2] [,3]
      [1,] 1     ?    ?  
      [2,] 1     ?    ?  
      [3,] 1     ?    ?  
    Message
      

# print method works for longer greta arrays

    Code
      ga_data_long
    Message
      greta array <data>
      
    Output
            [,1]
       [1,]    1
       [2,]    2
       [3,]    3
       [4,]    4
       [5,]    5
       [6,]    6
       [7,]    7
       [8,]    8
       [9,]    9
      [10,]   10
    Message
      
      i 10 more values
      Use `print(n = ...)` to see more values

---

    Code
      ga_stochastic_long
    Message
      greta array <variable following a normal distribution>
      
    Output
            [,1]
       [1,]  ?  
       [2,]  ?  
       [3,]  ?  
       [4,]  ?  
       [5,]  ?  
       [6,]  ?  
       [7,]  ?  
       [8,]  ?  
       [9,]  ?  
      [10,]  ?  
    Message
      
      i 10 more values
      Use `print(n = ...)` to see more values

---

    Code
      ga_operation_long
    Message
      greta array <operation>
      
    Output
            [,1]
       [1,]  ?  
       [2,]  ?  
       [3,]  ?  
       [4,]  ?  
       [5,]  ?  
       [6,]  ?  
       [7,]  ?  
       [8,]  ?  
       [9,]  ?  
      [10,]  ?  
    Message
      
      i 10 more values
      Use `print(n = ...)` to see more values

---

    Code
      print(ga_data_long, n = 19)
    Message
      greta array <data>
      
    Output
            [,1]
       [1,]    1
       [2,]    2
       [3,]    3
       [4,]    4
       [5,]    5
       [6,]    6
       [7,]    7
       [8,]    8
       [9,]    9
      [10,]   10
      [11,]   11
      [12,]   12
      [13,]   13
      [14,]   14
      [15,]   15
      [16,]   16
      [17,]   17
      [18,]   18
      [19,]   19
    Message
      
      i 1 more values
      Use `print(n = ...)` to see more values

---

    Code
      print(ga_data_long, n = 20)
    Message
      greta array <data>
      
    Output
            [,1]
       [1,]    1
       [2,]    2
       [3,]    3
       [4,]    4
       [5,]    5
       [6,]    6
       [7,]    7
       [8,]    8
       [9,]    9
      [10,]   10
      [11,]   11
      [12,]   12
      [13,]   13
      [14,]   14
      [15,]   15
      [16,]   16
      [17,]   17
      [18,]   18
      [19,]   19
      [20,]   20
    Message
      

---

    Code
      print(ga_data_long, n = 21)
    Message
      greta array <data>
      
    Output
            [,1]
       [1,]    1
       [2,]    2
       [3,]    3
       [4,]    4
       [5,]    5
       [6,]    6
       [7,]    7
       [8,]    8
       [9,]    9
      [10,]   10
      [11,]   11
      [12,]   12
      [13,]   13
      [14,]   14
      [15,]   15
      [16,]   16
      [17,]   17
      [18,]   18
      [19,]   19
      [20,]   20
    Message
      

---

    Code
      print(ga_stochastic_long, n = 19)
    Message
      greta array <variable following a normal distribution>
      
    Output
            [,1]
       [1,]  ?  
       [2,]  ?  
       [3,]  ?  
       [4,]  ?  
       [5,]  ?  
       [6,]  ?  
       [7,]  ?  
       [8,]  ?  
       [9,]  ?  
      [10,]  ?  
      [11,]  ?  
      [12,]  ?  
      [13,]  ?  
      [14,]  ?  
      [15,]  ?  
      [16,]  ?  
      [17,]  ?  
      [18,]  ?  
      [19,]  ?  
    Message
      
      i 1 more values
      Use `print(n = ...)` to see more values

---

    Code
      print(ga_stochastic_long, n = 20)
    Message
      greta array <variable following a normal distribution>
      
    Output
            [,1]
       [1,]  ?  
       [2,]  ?  
       [3,]  ?  
       [4,]  ?  
       [5,]  ?  
       [6,]  ?  
       [7,]  ?  
       [8,]  ?  
       [9,]  ?  
      [10,]  ?  
      [11,]  ?  
      [12,]  ?  
      [13,]  ?  
      [14,]  ?  
      [15,]  ?  
      [16,]  ?  
      [17,]  ?  
      [18,]  ?  
      [19,]  ?  
      [20,]  ?  
    Message
      

---

    Code
      print(ga_stochastic_long, n = 21)
    Message
      greta array <variable following a normal distribution>
      
    Output
            [,1]
       [1,]  ?  
       [2,]  ?  
       [3,]  ?  
       [4,]  ?  
       [5,]  ?  
       [6,]  ?  
       [7,]  ?  
       [8,]  ?  
       [9,]  ?  
      [10,]  ?  
      [11,]  ?  
      [12,]  ?  
      [13,]  ?  
      [14,]  ?  
      [15,]  ?  
      [16,]  ?  
      [17,]  ?  
      [18,]  ?  
      [19,]  ?  
      [20,]  ?  
    Message
      

---

    Code
      print(ga_operation_long, n = 19)
    Message
      greta array <operation>
      
    Output
            [,1]
       [1,]  ?  
       [2,]  ?  
       [3,]  ?  
       [4,]  ?  
       [5,]  ?  
       [6,]  ?  
       [7,]  ?  
       [8,]  ?  
       [9,]  ?  
      [10,]  ?  
      [11,]  ?  
      [12,]  ?  
      [13,]  ?  
      [14,]  ?  
      [15,]  ?  
      [16,]  ?  
      [17,]  ?  
      [18,]  ?  
      [19,]  ?  
    Message
      
      i 1 more values
      Use `print(n = ...)` to see more values

---

    Code
      print(ga_operation_long, n = 20)
    Message
      greta array <operation>
      
    Output
            [,1]
       [1,]  ?  
       [2,]  ?  
       [3,]  ?  
       [4,]  ?  
       [5,]  ?  
       [6,]  ?  
       [7,]  ?  
       [8,]  ?  
       [9,]  ?  
      [10,]  ?  
      [11,]  ?  
      [12,]  ?  
      [13,]  ?  
      [14,]  ?  
      [15,]  ?  
      [16,]  ?  
      [17,]  ?  
      [18,]  ?  
      [19,]  ?  
      [20,]  ?  
    Message
      

---

    Code
      print(ga_operation_long, n = 21)
    Message
      greta array <operation>
      
    Output
            [,1]
       [1,]  ?  
       [2,]  ?  
       [3,]  ?  
       [4,]  ?  
       [5,]  ?  
       [6,]  ?  
       [7,]  ?  
       [8,]  ?  
       [9,]  ?  
      [10,]  ?  
      [11,]  ?  
      [12,]  ?  
      [13,]  ?  
      [14,]  ?  
      [15,]  ?  
      [16,]  ?  
      [17,]  ?  
      [18,]  ?  
      [19,]  ?  
      [20,]  ?  
    Message
      

