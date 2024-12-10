# as_data errors informatively

    Code
      as_data(NULL)
    Condition
      Error in `as.greta_array()`:
      ! Object cannot be coerced to <greta_array>
      Objects of class <NULL> cannot be coerced to a <greta_array>

---

    Code
      as_data(list())
    Condition
      Error in `as.greta_array()`:
      ! Object cannot be coerced to <greta_array>
      Objects of class <list> cannot be coerced to a <greta_array>

---

    Code
      as_data(environment())
    Condition
      Error in `as.greta_array()`:
      ! Object cannot be coerced to <greta_array>
      Objects of class <environment> cannot be coerced to a <greta_array>

---

    Code
      as_data(cha_vec)
    Condition
      Error in `as.greta_array()`:
      ! Object cannot be coerced to <greta_array>
      Objects of class <character> cannot be coerced to a <greta_array>

---

    Code
      as_data(cha_mat)
    Condition
      Error in `as.greta_array()`:
      ! <greta_array> must contain the same type
      Cannot coerce <matrix> to a <greta_array> unless it is <numeric>, <integer> or <logical>.
      This <matrix> had type: <character>

---

    Code
      as_data(cha_arr)
    Condition
      Error in `as.greta_array()`:
      ! <greta_array> must contain the same type
      Cannot coerce <matrix> to a <greta_array> unless it is <numeric>, <integer> or <logical>.
      This <matrix> had type: <character>

---

    Code
      as_data(cha_df)
    Condition
      Error in `as.greta_array()`:
      ! <greta_array> must contain the same type
      Cannot coerce a <data.frame> to a <greta_array> unless all columns are <numeric, integer> or <logical>.
      This dataframe had columns of type: <character>

---

    Code
      as_data(cha_df2)
    Condition
      Error in `as.greta_array()`:
      ! <greta_array> must contain the same type
      Cannot coerce a <data.frame> to a <greta_array> unless all columns are <numeric, integer> or <logical>.
      This dataframe had columns of type: <factor>

---

    Code
      as_data(arr_inf)
    Condition
      Error in `as.greta_array.numeric()`:
      ! <greta_array> must not contain missing or infinite values

---

    Code
      as_data(arr_minf)
    Condition
      Error in `as.greta_array.numeric()`:
      ! <greta_array> must not contain missing or infinite values

---

    Code
      as_data(arr_na)
    Condition
      Error in `as.greta_array.numeric()`:
      ! <greta_array> must not contain missing or infinite values

---

    Code
      as_data(stoch)
    Condition
      Error in `as_data()`:
      ! cannot coerce a non-data <greta_array> to data

---

    Code
      as_data(op)
    Condition
      Error in `as_data()`:
      ! cannot coerce a non-data <greta_array> to data

