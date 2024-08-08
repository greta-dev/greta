## code to prepare `DATASET` dataset goes here
library(rvest)
library(polite)
library(janitor)
library(tidyverse)

linux_mac_deps <- "https://www.tensorflow.org/install/source" |>
  bow() |>
  scrape()

windows_deps <- "https://www.tensorflow.org/install/source_windows" |>
  bow() |>
  scrape()

get_tidy_html_tables <- function(html_raw){
  html_raw |>
    html_table() |>
    map(clean_names)
}

bind_html_tables <- function(tidied_tables, os_hardware_names){
  tidied_tables |>
    setNames(os_hardware_names) |>
    bind_rows(
      .id = "os_hardware"
    ) |>
    separate_wider_delim(
      cols = "os_hardware",
      delim = "-",
      names = c("os", "hardware")
    )
}

tidy_tf_dep_tables <- function(html_raw, os_hardware_names){
  html_raw |>
    get_tidy_html_tables() |>
    bind_html_tables(os_hardware_names)
}

tf_deps_linux_mac <- linux_mac_deps |>
  tidy_tf_dep_tables(
    os_hardware_names = c(
      "linux-cpu",
      "linux-gpu",
      "mac-cpu",
      "mac-gpu"
    )
  )

tf_deps_windows <- windows_deps |>
  tidy_tf_dep_tables(
    os_hardware_names = c(
      "windows-cpu",
      "windows-gpu"
    )
  )

# check classes are the same
# waldo::compare(
#   map(tf_deps_windows, class),
#   map(tf_linux_mac_deps, class)
# )

tf_deps <- bind_rows(
  tf_deps_windows,
  tf_deps_linux_mac
)

tf_cpu_deps <- tf_deps |>
  filter(
    hardware == "cpu"
  ) |>
  select(os,
         version,
         python_version) |>
  mutate(
    version = str_remove_all(version, "tensorflow-")
  ) |>
  rename(
    tf_version = version
  ) |>
  mutate(
    tf_lt2 = map_int(tf_version, \(x) compareVersion(x, "2.0.0"))
  ) |>
  filter(
    tf_lt2 >= 0
  ) |>
  select(-tf_lt2) |>
  separate_wider_delim(
    cols = python_version,
    delim = "-",
    names = c("python_version_min", "python_version_max")
  )


# TFP versions are in https://github.com/greta-dev/greta/issues/638#issuecomment-2268372432

# This is the 0.24.0 release of TensorFlow Probability. It is tested and stable against TensorFlow 2.16.1 and JAX 0.4.25 (cannot use as TF 2.15 uses keras 3 which has breaking changes) (Mar 13, 2024)
#

tfp_to_tf_compatability <- tibble::tribble(
  ~tfp_version, ~tf_version,
  "tfp==0.24.0", "tf==2.16.1",
  "tfp==0.23.0", "tf==2.15.0",
  "tfp==0.22.1", "tf==2.14.0",
  "tfp==0.22.0", "tf==2.14.0",
  "tfp==0.21.0", "tf==2.13.0",
  "tfp==0.20.0", "tf==2.12.0",
  "tfp==0.19.0", "tf==2.11.0",
  "tfp==0.18.0", "tf==2.10.0",
  "tfp==0.17.0", "tf==2.9.1",
  "tfp==0.16.0", "tf==2.8.0",
  "tfp==0.15.0", "tf==2.7.0",
  "tfp==0.14.1", "tf==2.6.0",
  "tfp==0.14.0", "tf==2.6.0",
  "tfp==0.13.0", "tf==2.5.0",
  "tfp==0.12.2", "tf==2.4.0",
  "tfp==0.12.1", "tf==2.4.0",
  "tfp==0.12.0", "tf==2.4.0",
  "tfp==0.11.1", "tf==2.3.0",
  "tfp==0.11.0", "tf==2.3.0",
  "tfp==0.10.1", "tf==2.2.0",
  "tfp==0.9.0", "tf==2.1.0",
  "tfp==0.8.0", "tf==2.0.0"
) |>
  mutate(
    tfp_version = str_remove_all(tfp_version,"tfp=="),
    tf_version = str_remove_all(tf_version,"tf==")
  )

tfp_to_tf_compatability

extra_rows <- tibble(
  os = c("windows", "linux", "mac"),
  tf_version = rep("2.9.1", 3),
  python_version_min = rep("3.7", 3),
  python_version_max = rep("3.10", 3)
)



numeric_version(tf_cpu_deps$tf_version)

.deps_tf <- bind_rows(tf_cpu_deps, extra_rows) |>
  mutate(tf_version = numeric_version(tf_version)) |>
  arrange(os, desc(tf_version)) |>
  mutate(tf_version = as.character(tf_version))
.deps_tfp <- tfp_to_tf_compatability

remove_before_comma <- function(x){
  trimws(str_remove_all(x, ".*?,"))
}

greta_deps_tf_tfp <- .deps_tf |>
  left_join(.deps_tfp,
            by = "tf_version",
            relationship = "many-to-many") |>
  relocate(tfp_version,
           .after = tf_version) |>
  mutate(
    python_version_min =  remove_before_comma(python_version_min)
  ) |>
  mutate(
    across(
      contains("version"),
      numeric_version
    )
  ) |>
  relocate(
    tfp_version,
    .after = os
  ) |>
  arrange(os,
          desc(tfp_version)) |>
  drop_na() |>
  filter(
    tfp_version < "0.24.0"
  )

  usethis::use_data(
    .deps_tf,
    .deps_tfp,
    internal = TRUE,
    overwrite = TRUE
  )

  usethis::use_data(
    greta_deps_tf_tfp,
    overwrite = TRUE
  )

