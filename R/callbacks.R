# functions to be run whilst samplers are running in parallel

read_trace_log_file <- function (filename) {
  ans <- NULL
  if (file.exists(filename)) {
    ans <- read.table(filename)
  }
  ans
}

read_progress_bar_log_file <- function (filename) {
  ans <- ""
  if (file.exists(filename)) {
    ans <- suppressWarnings(readLines(filename)[-1])
  }
  ans
}

percentages <- function () {
  reads <- lapply(greta_stash$trace_log_files, read_trace_log_file)
  some_results <- !all(vapply(reads, is.null, FALSE))

  if (some_results) {

    chains <- seq_along(reads)
    # get progress info
    its <- vapply(reads,
                  function (x) {
                    if (is.null(x))  0L
                    else nrow(x) - 1L
                  },
                  0L)
    n_samples <- greta_stash$mcmc_info$n_samples
    percentages <- round(100 * its / n_samples)
    percentages_text <- sprintf("%i: %3i%%",
                                chains,
                                percentages)

    msg <- paste(percentages_text, collapse = "  ")
    cat("\r", msg)
    flush.console()

  }

}

progress_bars <- function () {
  reads <- lapply(greta_stash$progress_bar_log_files,
                  read_progress_bar_log_file)
  reads <- unlist(reads)
  some_results <- any(nchar(reads) > 0)

  if (some_results) {

    msg <- paste(reads, collapse = " ")

    message("\r", appendLF = FALSE)
    message(msg, appendLF = FALSE)
    flush.console()

  }

}

# register some
greta_stash$callbacks <- list(parallel_progress = progress_bars)

# the default callback should be the progress bars, having each sampler write
# its progress bar to a progress stream if running in parallel (using existing
# gist)

# other packages, such as greta.live, can then create new modifier functions
# that create and register callbacks with this mechanism. E.g.:
# greta.live::use_dashboard()
# which would add a callback to greta::.internals$misc$greta_stash$callbacks
# to update a shiny flex dashboard with convergence diagnostic plots,
# dynamic traces (in 1D or 2D), coloured-in summary stats
# (with crayon, bolder if they are more certain)

# then use ropenscilabs/ssh to scp progress information from the remote servers

