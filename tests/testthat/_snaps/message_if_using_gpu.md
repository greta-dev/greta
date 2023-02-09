# message_if_using_gpu gives the correct message for cpu or gpu use

    Code
      message_if_using_gpu(cpu_only())

---

    Code
      message_if_using_gpu(gpu_only())
    Message <simpleMessage>
      NOTE: When using GPU, the random number seed may not always be respected (results may not be fully reproducible).
      For more information, see details of the `compute_options` argument in `?calculate`.
      You can turn off this message with:
      `options(greta_gpu_message = FALSE)`

# message_if_using_gpu does not message when option set

    Code
      message_if_using_gpu(gpu_only())

# message_if_using_gpu does message when option set

    Code
      message_if_using_gpu(gpu_only())
    Message <simpleMessage>
      NOTE: When using GPU, the random number seed may not always be respected (results may not be fully reproducible).
      For more information, see details of the `compute_options` argument in `?calculate`.
      You can turn off this message with:
      `options(greta_gpu_message = FALSE)`

# calculate provides a message when GPU is set

    Code
      calc_x <- calculate(x, nsim = 1, compute_options = gpu_only())
    Message <simpleMessage>
      NOTE: When using GPU, the random number seed may not always be respected (results may not be fully reproducible).
      For more information, see details of the `compute_options` argument in `?calculate`.
      You can turn off this message with:
      `options(greta_gpu_message = FALSE)`

---

    Code
      calc_x <- calculate(x, nsim = 1, compute_options = cpu_only())

# calculate/mcmc does not message when option set

    Code
      calc_x <- calculate(x, nsim = 1, compute_options = gpu_only())

---

    Code
      mcmc_m <- mcmc(model = m, n_samples = 1, warmup = 0, compute_options = gpu_only(),
      verbose = FALSE)

# calculate/mcmc does message when option set

    Code
      calc_x <- calculate(x, nsim = 1, compute_options = gpu_only())
    Message <simpleMessage>
      NOTE: When using GPU, the random number seed may not always be respected (results may not be fully reproducible).
      For more information, see details of the `compute_options` argument in `?calculate`.
      You can turn off this message with:
      `options(greta_gpu_message = FALSE)`

---

    Code
      mcmc_m <- mcmc(model = m, n_samples = 1, warmup = 0, compute_options = gpu_only(),
      verbose = FALSE)
    Message <simpleMessage>
      NOTE: When using GPU, the random number seed may not always be respected (results may not be fully reproducible).
      For more information, see details of the `compute_options` argument in `?calculate`.
      You can turn off this message with:
      `options(greta_gpu_message = FALSE)`

# mcmc provides a message when GPU is set

    Code
      mcmc_gpu <- mcmc(model = m, n_samples = 1, warmup = 0, compute_options = gpu_only(),
      verbose = FALSE)
    Message <simpleMessage>
      NOTE: When using GPU, the random number seed may not always be respected (results may not be fully reproducible).
      For more information, see details of the `compute_options` argument in `?calculate`.
      You can turn off this message with:
      `options(greta_gpu_message = FALSE)`

---

    Code
      mcmc_cpu <- mcmc(model = m, n_samples = 1, warmup = 0, compute_options = cpu_only(),
      verbose = FALSE)

# mcmc prints out CPU and GPU text

    Code
      draws <- mcmc(m, n_samples = 5, warmup = 5, compute_options = cpu_only())
    Message <simpleMessage>
      running 4 chains simultaneously on up to 8 CPU cores
    Output
      
    Message <message>
      
          warmup                                              0/5 | eta:  ?s          
      
          warmup ============================================ 5/5 | eta:  0s          
      
      
        sampling                                              0/5 | eta:  ?s          
      
        sampling ============================================ 5/5 | eta:  0s          
      

---

    Code
      draws <- mcmc(m, n_samples = 5, warmup = 5, compute_options = gpu_only())
    Message <simpleMessage>
      NOTE: When using GPU, the random number seed may not always be respected (results may not be fully reproducible).
      For more information, see details of the `compute_options` argument in `?calculate`.
      You can turn off this message with:
      `options(greta_gpu_message = FALSE)`
      running 4 chains simultaneously on GPU
    Output
      
    Message <message>
      
          warmup                                              0/5 | eta:  ?s          
      
          warmup ============================================ 5/5 | eta:  0s          
      
      
        sampling                                              0/5 | eta:  ?s          
      
        sampling ============================================ 5/5 | eta:  0s          
      

