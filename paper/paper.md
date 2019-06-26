---
title: 'greta: simple and scalable statistical modelling in R'
tags:
  - statistics
  - statistical modelling
  - bayesian statistics
  - mcmc
  - hamiltonian monte carlo
  - tensorflow
authors:
  - name: Nick Golding
    orcid: 0000-0001-8916-5570
    affiliation: 1
affiliations:
 - name: School of BioSciences, University of Melbourne
   index: 1
date: 26 June 2019
bibliography: paper.bib
---

# Summary

Statistical modelling useful throughout the sciences. Often a need to write custom models that cannot be fitted using off-the shelf statistical software (such as software for for fitting mixed effects models). Hence writing out the model in a modelling language and fitting them by MCMC or maximum likelihood. This lets the user focus on the statistical nature of the model, rather than implementation details and inference procedures. This has lead to the development of software including BUGS, JAGS and NIMBLE [@openbugs, @jags, @nimble]. In these software packages, users typically write out models in a domain-specific language which is then compiled into computational code (though see the Python packages PyMC [@pymc] and Edward [@edward]).

With increasing quantitites of data and increasing complexity and realism of the statistical models that users wish to buiold with these software, ther is a push for software that scales better with data size and model complexity. Therefore using Hamiltonian Monte Carlo rather than Gibbs samplers, and paying particular attention to computational efficiency (Stan) [@stan].

``greta`` is an R package for statistical modelling that has three core differences to commonly used statistical modelling software packages:

  1. ``greta`` models are written interactively in R code rather than in a compiled domain specific language.
  2. ``greta`` can be extended by other R packages; providing a fully-featured package management system for extensions.
  3. ``greta`` performs statistical inference using TensorFlow [@tf] enabling it to scale across modern high-performance computing systems.
  
``greta`` can be used to construct both Bayesian and non-Bayesian statistical models, and perform inference via MCMC or optimisation (for maximum likelihood or maximum *a posteriori* estimation). The default MCMC algorithm is Hamiltonian Monte Carlo, which is generally very efficient for Bayesian models with large numbers of parameters or highly-correlated posteriors.

The project website [https://greta-stats.org/]() hosts a *getting started* guide, worked examples of analyses using greta, a catalogue of example models, documentation, and a user forum.

# demonstration

< a simple model interacting with R objects, producing outputs and post-hoc posterior prediction >


# Implementation

R front end, extending existing R functions.
using R6 objects internally to build up a DAG
Using the DAG to construct a likelihood function using TensorFlow
Using TensorFlow [@tf] and TensorFlow Probability [@tfp] via reticulate and the tensorflow R API [@reticulate, @r_tf] functionality for the cor computational part of inference.
Whereas most MCMC software packages enable each MCMC chain to run on a separate CPU, greta can parallelise MCMC on a single chain across an arbitrary number of CPUs by parallelising 
By simply installing the appropriate version of TensorFlow, greta models can also be run on Graphics Processing Units (GPUs).
``greta`` is also integrated with the ``future`` [@future] R package for remote and parallel processing, providing a simple interface to run inference for each chain of MCMC on a separate, remote machines.

# extending greta

``greta`` is not only designed to be extensible, but makes a deliberately distinction between the API for *users* who construct statistical models using existing functionality, and *developers* who add new functionality. Rather than letting users directly modify the inference target within a model, new probability distributions and operations are created using a developer user interface, exposed via the `.internals` object. Once developed in this way, it becomes simple to distribute this new functionality to other users via an R package that extends ``greta``. Linking to the well established R package mechanism means that ``greta`` extensions automatically come with a  fully-featured package management system, with tooling for development and distribution via CRAN or code sharing platforms.

This developer API is under active development to make the process of extending greta simpler. Existing extensions are mostly in prototype form, for example: ? ? ?

Whilst anyone can write and distribute their own extension package, an aim of the greta project is to maintain a set of extension packages that meet software quality standards and are completely interoperable, in a similar way to the ``tidyverse`` [@tidyverse] of R packages for data manipulation. These packages will be hosted on both the project GitHub organisation at [https://github.com/greta-dev/]() and on CRAN.

# future work

### discrete parameters

``greta`` currently only handles models with exclusively continuous-valued parameters, since these models are compatible with the most commonly used optimisation routines and the efficient HMC sampler that is used by default. In the near future, ``greta`` will be extended to enable users to perform inference on models with discrete-valued parameters as required, in combination with the (typically less efficient) samplers with which these models are compatible.

### marginalisation

Many common statistical modelling approaches, such as hierarchical models, use unobserved *latent* variables whose posterior distributions must be integrated over in order to perform inference on parameters of interest. Whilst MCMC is a general-purpose method for marginalising these parameters, other methods are often better suited to the task in specific models. For example where those latent variables are discrete-valued and efficient samplers cannot be used, or when deterministic numerical approximations such as a Laplace approximation are more computationally-efficient. A simple user interface to specifying these marginalisation schemes within a ``greta`` model is planned. This will enable users to experiment with combinations of different inference approaches without the need delve into nuances of implementation.

# Acknowledgements

I'd like to acknowledge direct contributions from Simon Dirmeier, Adam Fleischhacker, Shirin Glander, Martin Ingram, Lee Hazel, Tiphaine Martin, Matt Mulvahill, Michael Quinn, David Smith, Paul Teetor, and Jian Yen, as well as Jeffrey Pullin and many others who have provided feedback and suggestions on greta and its extensions. ``greta`` was developed with support from both a McKenzie fellowship from the University of Melbourne, and a DECRA fellowship from the Australian Research Council (DE180100635).

# References
