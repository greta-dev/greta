# functions to be run whilst samplers are running in parallel

read_trace_log_file <- function (filename) {
  ans <- NULL
  if (file.exists(filename)) {
    ans <- read.table(filename)
  }
  ans
}

read_progress_log_file <- function (filename, skip = 0) {
  ans <- ""
  if (file.exists(filename)) {
    ans <- suppressWarnings(readLines(filename))
    idx <- (skip + 1):length(ans)
    ans <- ans[idx]
  }
  ans
}

# generic rendering of progress text straight from a set of files
render_progress <- function (reads) {

  reads <- unlist(reads)
  reads[is.na(reads)] <- ""
  some_results <- any(nchar(reads) > 0)
  if (some_results) {

    msg <- paste(c("", reads, ""), collapse = "  |  ")

    message("\r", appendLF = FALSE)
    message(msg, appendLF = FALSE)
    flush.console()

  }
}

# callback functions
percentages <- function () {
  reads <- lapply(greta_stash$percentage_log_files,
                  read_progress_log_file)
  render_progress(reads)
}

progress_bars <- function () {
  reads <- lapply(greta_stash$progress_bar_log_files,
                  read_progress_log_file,
                  skip = 1)
  render_progress(reads)

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

