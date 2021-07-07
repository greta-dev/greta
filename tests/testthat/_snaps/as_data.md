# as_data errors informatively

    Code
      as_data(NULL)
    Error <simpleError>
      Object cannot be coerced to <greta_array>
      Objects of class <NULL> cannot be coerced to a <greta_array>

---

    Code
      as_data(list())
    Error <simpleError>
      Object cannot be coerced to <greta_array>
      Objects of class <list> cannot be coerced to a <greta_array>

---

    Code
      as_data(environment())
    Error <simpleError>
      Object cannot be coerced to <greta_array>
      Objects of class <environment> cannot be coerced to a <greta_array>

---

    Code
      as_data(cha_vec)
    Error <simpleError>
      Object cannot be coerced to <greta_array>
      Objects of class <character> cannot be coerced to a <greta_array>

---

    Code
      as_data(cha_mat)
    Error <simpleError>
      <greta_array> must contain the same type
      Cannot coerce <matrix> to a <greta_array> unless all columns are <numeric>,
      <integer> or <logical>. This <matrix> had type:
      <character>

---

    Code
      as_data(cha_arr)
    Error <simpleError>
      <greta_array> must contain the same type
      Cannot coerce <array> to a <greta_array> unless it is <numeric>, <integer> or
      <logical>. This <array> had type:
      <character>

---

    Code
      as_data(cha_df)
    Error <simpleError>
      <greta_array> must contain the same type
      Cannot coerce a <data.frame> to a <greta_array> unless all columns are
      <numeric, integer> or <logical>. This dataframe had columns of type:
      <character>

---

    Code
      as_data(cha_df2)
    Error <simpleError>
      <greta_array> must contain the same type
      Cannot coerce a <data.frame> to a <greta_array> unless all columns are
      <numeric, integer> or <logical>. This dataframe had columns of type: <factor>

---

    Code
      as_data(arr_inf)
    Error <simpleError>
      <greta_array> must not contain missing or infinite values

---

    Code
      as_data(arr_minf)
    Error <simpleError>
      <greta_array> must not contain missing or infinite values

---

    Code
      as_data(arr_na)
    Error <simpleError>
      <greta_array> must not contain missing or infinite values

---

    Code
      as_data(stoch)
    Error <simpleError>
      cannot coerce a non-data greta_array to data

---

    Code
      as_data(op)
    Error <simpleError>
      cannot coerce a non-data greta_array to data

