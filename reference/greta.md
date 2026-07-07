# greta: simple and scalable statistical modelling in R

greta lets you write statistical models interactively in native R code,
then sample from them efficiently using Hamiltonian Monte Carlo.

The computational heavy lifting is done by TensorFlow, Google's
automatic differentiation library. So greta is particularly fast where
the model contains lots of linear algebra, and greta models can be run
across CPU clusters or on GPUs.

See the simple example below, and take a look at the [greta
website](https://greta-dev.github.io/greta/) for more information
including
[tutorials](https://greta-dev.github.io/greta/articles/get_started.html)
and
[examples](https://greta-dev.github.io/greta/articles/example_models.html).

## See also

Useful links:

- <https://greta-dev.github.io/greta/>

- <https://github.com/greta-dev/greta>

- Report bugs at <https://github.com/greta-dev/greta/issues>

## Author

**Maintainer**: Nicholas Tierney <nicholas.tierney@gmail.com>
([ORCID](https://orcid.org/0000-0003-1460-8722))

Authors:

- Nicholas Tierney <nicholas.tierney@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-1460-8722))

- Nick Golding <nick.golding.research@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-8916-5570)) \[copyright holder\]

Other contributors:

- Simon Dirmeier \[contributor\]

- Adam Fleischhacker \[contributor\]

- Shirin Glander \[contributor\]

- Martin Ingram \[contributor\]

- Lee Hazel \[contributor\]

- Lionel Hertzog \[contributor\]

- Tiphaine Martin \[contributor\]

- Matt Mulvahill \[contributor\]

- Michael Quinn \[contributor\]

- David Smith \[contributor\]

- Paul Teetor \[contributor\]

- Jian Yen \[contributor\]

## Examples

``` r
if (FALSE) { # \dontrun{
# a simple Bayesian regression model for the iris data

# priors
int <- normal(0, 5)
coef <- normal(0, 3)
sd <- lognormal(0, 3)

# likelihood
mean <- int + coef * iris$Petal.Length
distribution(iris$Sepal.Length) <- normal(mean, sd)

# build and sample
m <- model(int, coef, sd)
draws <- mcmc(m, n_samples = 100)
} # }
```
