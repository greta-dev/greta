# Finding out why Python would not load.
#
# greta cannot simply capture the reason. reticulate runs uv through system2()
# without redirecting its streams and keeps only the exit code, so uv's
# explanation -- "No solution found when resolving tool dependencies", a cache
# miss under UV_OFFLINE, a Python it cannot bootstrap -- goes to the terminal
# and is gone by the time greta knows anything failed. Nothing in reticulate
# stores it, and the condition greta catches says only "Installation of Python
# not found".
#
# So it is asked again, in a subprocess whose streams belong to us. That costs a
# second resolution attempt, on the failure path only, and it can in principle
# disagree with the original -- an intermittent network failure need not
# reproduce. Both are worth it for an answer that is otherwise unobtainable.

# One attempt per session. check_tf_version() runs on every greta array, so an
# unconfigured machine would otherwise spawn a subprocess per call.
diagnose_python_load <- function(
  args = greta_py_require_args(),
  timeout = 120
) {
  if (!is.null(greta_stash$python_load_diagnosis)) {
    return(greta_stash$python_load_diagnosis)
  }

  diagnosis <- tryCatch(
    run_python_load_probe(args = args, timeout = timeout),
    error = function(e) {
      paste("greta could not run the diagnostic:", conditionMessage(e))
    }
  )

  greta_stash$python_load_diagnosis <- diagnosis
  diagnosis
}

# Repeats the request greta makes at load time and returns everything the
# subprocess wrote to stderr, which is where both uv and reticulate report.
run_python_load_probe <- function(args, timeout) {
  probe <- callr::r_process$new(
    callr::r_process_options(
      func = function(packages, python_version) {
        reticulate::py_require(
          packages = packages,
          python_version = python_version
        )
        reticulate::py_config()
      },
      args = list(
        packages = args$packages,
        python_version = args$python_version
      ),
      stderr = "|",
      stdout = "|"
    )
  )

  on.exit(if (probe$is_alive()) probe$kill(), add = TRUE)
  finished <- probe$wait(timeout = timeout * 1000)

  if (probe$is_alive()) {
    return(
      cli::format_inline(
        "The diagnostic did not finish within {timeout} seconds, so the cause \\
        is still unknown."
      )
    )
  }

  probe$read_all_error_lines()
}
