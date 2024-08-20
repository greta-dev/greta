# greta python range detection works correctly

    Code
      check_greta_python_range("3.11")

---

    Code
      check_greta_python_range("3.9")

---

    Code
      check_greta_python_range("3.3")

---

    Code
      check_greta_python_range("3.8.2")

---

    Code
      check_greta_python_range("3.3.3")

---

    Code
      check_greta_python_range("3.1")
    Condition
      Error:
      ! Python version must be between "3.3"-"3.11"
      x The version provided was "3.1".
      i Try: "3.3"

---

    Code
      check_greta_python_range("3.12")
    Condition
      Error:
      ! Python version must be between "3.3"-"3.11"
      x The version provided was "3.12".
      i Try: "3.11"

---

    Code
      check_greta_python_range("2.7")
    Condition
      Error:
      ! Python version must be between "3.3"-"3.11"
      x The version provided was "2.7".
      i Try: "3.3"

---

    Code
      check_greta_python_range("3.1.1")
    Condition
      Error:
      ! Python version must be between "3.3"-"3.11"
      x The version provided was "3.1.1".
      i Try: "3.3"

---

    Code
      check_greta_python_range("3.14")
    Condition
      Error:
      ! Python version must be between "3.3"-"3.11"
      x The version provided was "3.14".
      i Try: "3.11"

# greta_deps_spec fails appropriately

    Code
      greta_deps_spec()
    Output
        tf_version tfp_version python_version
      1     2.15.0      0.23.0           3.10

---

    Code
      greta_deps_spec(tf_version = "2.14.0", tfp_version = "0.22.1", python_version = "3.9")
    Output
        tf_version tfp_version python_version
      1     2.14.0      0.22.1            3.9

---

    Code
      greta_deps_spec(tf_version = "2.12.0", tfp_version = "0.20.0", python_version = "3.9")
    Output
        tf_version tfp_version python_version
      1     2.12.0      0.20.0            3.9

---

    Code
      greta_deps_spec(tf_version = "2.16.1", tfp_version = "0.11.0", python_version = "3.8")
    Condition
      Error in `greta_deps_spec()`:
      ! greta Does not yet support TF > 2.15.0
      i See <https://github.com/greta-dev/greta/issues/675> for more information
      x The provided version was 2.16.1
      i The nearest valid version that is supported by greta is: 2.15.0
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "1.9.0", tfp_version = "0.11.0", python_version = "3.8")
    Condition
      Error in `greta_deps_spec()`:
      ! "TF" version provided does not match supported versions
      The version 1.9.0 was not in 2.15.0, 2.14.0, 2.14.0, 2.13.0, 2.12.0, 2.11.0, 2.10.0, 2.9.1, 2.8.0, 2.7.0, 2.6.0, 2.6.0, 2.5.0, 2.4.0, 2.4.0, 2.4.0, 2.3.0, 2.3.0, ..., 2.1.0, and 2.0.0
      i The nearest valid version that is supported by greta is: 2.0.0
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "2.15.0", tfp_version = "0.24.0", python_version = "3.10")
    Condition
      Error in `greta_deps_spec()`:
      ! greta Does not yet support TFP > 0.23.0
      i See <https://github.com/greta-dev/greta/issues/675> for more information
      x The provided version was 0.24.0
      i The nearest valid version that is supported by greta is: 0.23.0
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "2.15.0", tfp_version = "0.6.0", python_version = "3.10")
    Condition
      Error in `greta_deps_spec()`:
      ! "TFP" version provided does not match supported versions
      The version 0.6.0 was not in 0.23.0, 0.22.1, 0.22.0, 0.21.0, 0.20.0, 0.19.0, 0.18.0, 0.17.0, 0.16.0, 0.15.0, 0.14.1, 0.14.0, 0.13.0, 0.12.2, 0.12.1, 0.12.0, 0.11.1, 0.11.0, ..., 0.9.0, and 0.8.0
      i The nearest valid version that is supported by greta is: 0.8.0
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "2.9.1", tfp_version = "0.23.0", python_version = "3.13")
    Condition
      Error in `greta_deps_spec()`:
      ! Python version must be between "3.3"-"3.11"
      x The version provided was "3.13".
      i Try: "3.11"

---

    Code
      greta_deps_spec(tf_version = "2.9.1", tfp_version = "0.23.0", python_version = "2.6")
    Condition
      Error in `greta_deps_spec()`:
      ! Python version must be between "3.3"-"3.11"
      x The version provided was "2.6".
      i Try: "3.3"

---

    Code
      greta_deps_spec(tf_version = "2.15.0", tfp_version = "0.23.0", python_version = "3.8")
    Condition
      Error in `greta_deps_spec()`:
      ! Provided `greta_deps_spec` does not match valid installation combinations.
      See below for a suggested config to use:
      `greta_deps_spec(tf_version = "2.15.0", tfp_version = "0.23.0", python_version = "3.11")`
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "2.15.0", tfp_version = "0.22.0", python_version = "3.10")
    Condition
      Error in `greta_deps_spec()`:
      ! Provided `greta_deps_spec` does not match valid installation combinations.
      See below for a suggested config to use:
      `greta_deps_spec(tf_version = "2.14.0", tfp_version = "0.22.0", python_version = "3.11")`
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "2.14.0", tfp_version = "0.21.0", python_version = "3.8")
    Condition
      Error in `greta_deps_spec()`:
      ! Provided `greta_deps_spec` does not match valid installation combinations.
      See below for a suggested config to use:
      `greta_deps_spec(tf_version = "2.13.0", tfp_version = "0.21.0", python_version = "3.11")`
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "2.13.0", tfp_version = "0.20.0", python_version = "3.8")
    Condition
      Error in `greta_deps_spec()`:
      ! Provided `greta_deps_spec` does not match valid installation combinations.
      See below for a suggested config to use:
      `greta_deps_spec(tf_version = "2.12.0", tfp_version = "0.20.0", python_version = "3.11")`
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "2.15.0", tfp_version = "0.17.0", python_version = "3.8")
    Condition
      Error in `greta_deps_spec()`:
      ! Provided `greta_deps_spec` does not match valid installation combinations.
      See below for a suggested config to use:
      `greta_deps_spec(tf_version = "2.9.1", tfp_version = "0.17.0", python_version = "3.10")`
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "2.17.0", tfp_version = "0.23.0", python_version = "3.8")
    Condition
      Error in `greta_deps_spec()`:
      ! greta Does not yet support TF > 2.15.0
      i See <https://github.com/greta-dev/greta/issues/675> for more information
      x The provided version was 2.17.0
      i The nearest valid version that is supported by greta is: 2.15.0
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

---

    Code
      greta_deps_spec(tf_version = "2.9.0", tfp_version = "0.17.0", python_version = "3.8")
    Condition
      Error in `greta_deps_spec()`:
      ! "TF" version provided does not match supported versions
      The version 2.9.0 was not in 2.15.0, 2.14.0, 2.14.0, 2.13.0, 2.12.0, 2.11.0, 2.10.0, 2.9.1, 2.8.0, 2.7.0, 2.6.0, 2.6.0, 2.5.0, 2.4.0, 2.4.0, 2.4.0, 2.3.0, 2.3.0, ..., 2.1.0, and 2.0.0
      i The nearest valid version that is supported by greta is: 2.8.0
      i Valid versions of TF, TFP, and Python are in `greta_deps_tf_tfp`
      i Inspect with:
      `View(greta_deps_tf_tfp)`

