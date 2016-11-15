
flat_distribution <- R6Class (
  'flat_distribution',
  inherit = distribution,
  public = list(

    from_free = function (x) {
      upper <- self$parameters$upper$value()
      lower <- self$parameters$lower$value()
      plogis(x) * (upper - lower) + lower
    },

    to_free = function (y) {
      upper <- self$parameters$upper$value()
      lower <- self$parameters$lower$value()
      qlogis((y - lower) / (upper - lower))
    },

    tf_from_free = function (x, env) {

      # cannot allow the upper and lower values to be nodes
      # otherwise it would severely screw with the gradients
      upper <- self$parameters$upper$value()
      lower <- self$parameters$lower$value()

      (1 / (1 + tf$exp(-1 * x))) * (upper - lower) + lower

    },

    initialize = function (lower = -1e6, upper = 1e6, dim = 1) {

      if (!(is.numeric(lower) & is.numeric(upper) &
            is.finite(lower) & is.finite(upper) &
            length(lower) == 1 & length(upper) == 1)) {
        stop ('lower and upper must be finite scalars')
      }

      super$initialize('flat', dim)

      self$add_parameter(lower, 'lower')
      self$add_parameter(upper, 'upper')
    },

    log_density_function = function (value, parameters) 0,

    tf_log_density_function = function (value, parameters)
      tf$constant(0, dtype = tf$float32)

  )
)

free_distribution <- R6Class (
  'free_distribution',
  inherit = distribution,
  public = list(

    from_free = function (x) x,

    to_free = function (y) y,

    tf_from_free = function (x, env) x,

    initialize = function (dim = 1)
      super$initialize('free', dim),

    log_density_function = function (value, parameters) 0,

    tf_log_density_function = function (value, parameters)
      tf$constant(0, dtype = tf$float32)

  )
)

normal_distribution <- R6Class (
  'normal_distribution',
  inherit = distribution,
  public = list(

    from_free = function (x) x,
    to_free = function (y) y,

    tf_from_free = function (x, env) x,

    initialize = function (mu, sigma, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('normal', dim)
      self$add_parameter(mu, 'mu')
      self$add_parameter(sigma, 'sigma')
    },

    log_density_function = function (x) {
      # convert to tensorflow later
      dnorm(x,
            self$parameters$mu$value(),
            self$parameters$sigma$value(),
            log = TRUE)
    },

    tf_log_density_function = function (x, parameters) {

      x <- x
      mu <- parameters$mu
      var <- tf$square(parameters$sigma)

      -0.5 * tf$log(2 * pi) - 0.5 * tf$log(var) - 0.5 * tf$square(tf$sub(mu, x)) / var

    }

  )
)

lognormal_distribution <- R6Class (
  'lognormal_distribution',
  inherit = distribution,
  public = list(

    from_free = exp,
    to_free = log,
    tf_from_free = function (x, env) tf$exp(x),

    initialize = function (mu, sigma, dim = 1) {
      super$initialize('lognormal', dim)
      self$add_parameter(mu, 'mu')
      self$add_parameter(sigma, 'sigma')
    },

    log_density_function = function (x) {
      # convert to tensorflow later
      dlnorm(x,
             self$parameters$mu$value(),
             self$parameters$sigma$value(),
             log = TRUE)
    },

    tf_log_density_function = function (x, parameters) {

      mu <- parameters$mu
      var <- tf$square(parameters$sigma)
      lx <- tf$log(x)

      -0.5 * tf$log(2 * pi) - 0.5 * tf$log(var) - 0.5 * tf$square(tf$sub(mu, lx)) / var


    }
  )
)

# export constructors (add flat)
#' @export
normal <- normal_distribution$new
lognormal <- lognormal_distribution$new
flat <- flat_distribution$new
free <- free_distribution$new
