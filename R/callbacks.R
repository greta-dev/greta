# functions to be run whilst samplers are running in parallel

#' @importFrom utils read.table
read_trace_log_file <- function(filename) {
  ans <- NULL
  if (file.exists(filename)) {
    ans <- read.table(filename)
  }
  ans
}

read_progress_log_file <- function(filename, skip = 0) {
  ans <- ""
  if (file.exists(filename)) {
    ans <- suppressWarnings(readLines(filename))
    idx <- (skip + 1):length(ans)
    ans <- ans[idx]
  }
  ans
}

# generic rendering of progress text straight from a set of files
#' @importFrom utils flush.console
render_progress <- function(reads) {

  reads <- unlist(reads)
  reads[is.na(reads)] <- ""
  some_results <- any(nchar(reads) > 0)
  if (some_results) {

    # optionally add blanks to put lines at the edges
    if (length(reads) > 1)
      reads <- c("", reads, "")

    msg <- paste(reads, collapse = "  |  ")

    message("\r", appendLF = FALSE)
    message(msg, appendLF = FALSE)
    flush.console()

  }
}

# callback functions
percentages <- function() {
  reads <- lapply(greta_stash$percentage_log_files,
                  read_progress_log_file)
  render_progress(reads)
}

progress_bars <- function() {
  reads <- lapply(greta_stash$progress_bar_log_files,
                  read_progress_log_file,
                  skip = 1)
  render_progress(reads)
}

# register some
greta_stash$callbacks <- list(parallel_progress = progress_bars)
