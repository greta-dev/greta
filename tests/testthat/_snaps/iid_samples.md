# distributions without RNG error nicely

    Code
      compare_iid_samples(hypergeometric, rhyper, parameters = list(m = 11, n = 8, k = 5))
    Condition
      Error in `check_sampling_implemented()`:
      ! Sampling is not yet implemented for "hypergeometric" distributions

---

    Code
      compare_iid_samples(f, rtf, parameters = list(df1 = 4, df2 = 1, truncation = c(
        2, 3)))
    Condition
      Error in `dag$draw_sample()`:
      ! Sampling is not yet implemented for truncated "f" distributions

