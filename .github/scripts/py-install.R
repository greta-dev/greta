
#### STEP 1: INSTALL PACKAGES WITH PYTHON DEPENDENCIES
install.packages("reticulate",dependencies = TRUE)

#### STEP 2: INSTALL & UPDATE MINICONDA SO R CAN FIND PYTHON
## install miniconda in default location if possible
condaInstall = try(reticulate::install_miniconda())
condaPath = try(reticulate::miniconda_path())
## if ERROR is due to a previous installation, then ignore the error.
## if install fails due to a space in your path, then uncomment
## the below two lines and run them.
## condaPath = file.path("/", "miniconda")
## reticulate::install_miniconda(path = condaPath,force = TRUE)}


#### STEP 4:  Update "r-reticulate" CONDA ENVIRONMENT
####          FOR TENSORFLOW
## Install the specific versions of modules
## for the TensorFlow installation via CONDA.
## these next lines may take a few minutes to execute
reticulate::conda_remove("r-reticulate")  #start clean
reticulate::py_config()  # initiate basic r-reticulate config -- ignore any error here
## install other packages and downgrade numpy
reticulate::conda_install(envname = "r-reticulate",
                          packages =
                            c(
                              "python=3.7",
                              "tensorflow=1.14",
                              "pyyaml",
                              "requests",
                              "Pillow",
                              "pip",
                              "numpy=1.16",
                              "h5py=2.8",
                              "tensorflow-probability=0.7"
                            ))
