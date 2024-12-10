# nice progress bars for MCMC sampling

#' @importFrom progress progress_bar

# create a custom progress bar for sampling.
# 'phase' must be either 'warmup' or 'sampling'
# 'iter' must be a length-two vector giving the total warmup and sampling
#   iterations respectively
# 'pb_update' gives the number of iterations between updates of the progress bar
create_progress_bar <- function(phase, iter, pb_update, width, ...) {

  # name for formatting
  name <- switch(phase,
    warmup = "  warmup",
    sampling = "sampling"
  )

  # total iterations for bat
  iter_this <- switch(phase,
    warmup = iter[1],
    sampling = iter[2]
  )

  # pad the frmat so that the width iterations counter is the same for both
  # warmup and sampling
  digit_diff <- nchar(max(iter)) - nchar(iter_this)
  count_pad <- paste0(rep(" ", 2 * digit_diff), collapse = "")

  # formatting
  format_text <- glue::glue(
    "  {name} :bar {count_pad}:iter/:total | eta: :eta :rejection"
  )

  pb <- progress::progress_bar$new(
    format = format_text,
    total = iter_this,
    width = width,
    incomplete = " ",
    clear = FALSE,
    current = "=",
    show_after = 0,
    force = TRUE,
    ...
  )

  # add the increment information and return
  pb_update <- round(pb_update)

  check_finite_positive_scalar_integer(pb_update)

  assign("pb_update", pb_update, envir = pb$.__enclos_env__)

  pb
}


# iterate a progress bar, giving information about the number of rejections due
# to numerical instability
# 'pb' is a progress_bar R6 object created by create_progress_bar
# 'it' is the current iteration
# 'rejects' is the total number of rejections so far due to numerical
#   instability
iterate_progress_bar <- function(pb, it, rejects, chains, file = NULL) {
  increment <- pb$.__enclos_env__$pb_update

  if (it %% increment == 0) {
    if (rejects > 0) {
      reject_perc <- 100 * rejects / (it * chains)
      if (reject_perc < 1) {
        reject_perc_string <- "<1"
      } else {
        reject_perc_string <- prettyNum(round(reject_perc))
      }
      # pad the end of the line to keep the update bar a consistent width
      pad_char <- pmax(0, 2 - nchar(reject_perc_string))
      pad <- paste0(rep(" ", pad_char), collapse = "")

      reject_text <- glue::glue("| {reject_perc_string}% bad{pad}")
    } else {
      reject_text <- "         "
    }

    total <- pb$.__enclos_env__$private$total
    iter_pretty <- prettyNum(it, width = nchar(total))

    amount <- ifelse(it > 0, increment, 0)

    # tick the progess bar and record the output message
    # (or print it if file = NULL)
    record(pb$tick(amount,
      tokens = list(
        iter = iter_pretty,
        rejection = reject_text
      )
    ),
    file = file
    )
  }
}

progress_bar_module <- module(
  create_progress_bar,
  iterate_progress_bar
)
