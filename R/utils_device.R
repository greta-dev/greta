is_device_available <- function(device) {
  device_text <- tensorflow::tf$config$list_physical_devices(device)
  is_empty_list <- length(device_text) == 0
  device_available <- ifelse(
    test = is_empty_list,
    yes = FALSE,
    no = TRUE
  )
  return(device_available)
}

is_gpu_available <- function() {
  is_device_available("GPU")
}

is_cpu_available <- function() {
  is_device_available("CPU")
}

are_cpu_and_gpu_available <- function() {
  is_gpu_available() && is_cpu_available()
}

default_device <- function() {
  both_available <- are_cpu_and_gpu_available()
  gpu_availabe <- is_gpu_available()
  cpu_availabe <- is_cpu_available()

  if (both_available) {
    return("GPU")
  }

  if (gpu_available & !cpu_available) {
    return("GPU")
  }

  if (!gpu_available & cpu_available) {
    return("CPU")
  }

  if (!gpu_available & !cpu_available) {
    msg <- cli::format_error("Neither GPU or CPU are available")
    stop(
      msg,
      call. = FALSE
    )
  }
}

set_device <- function(device) {
  visible_device <- tensorflow::tf$config$get_visible_devices(device)

  tensorflow::tf$config$set_visible_devices(visible_device)
}

gpu_only <- function(){
  "GPU"
}

cpu_only <- function(){
  "CPU"
}
