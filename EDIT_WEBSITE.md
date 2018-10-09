# building and  contributing to the greta website

This document explains how [the greta website](https://greta-stats.org) is built, and how to edit website content or add an analysis case study or an example model. Before you submit a pull request, please [open a GitHub issue](https://github.com/greta-dev/greta/issues/new) describing the content you'd like to change or add, and discuss it with the package maintainers.

## building the website

*greta* uses a customised [pkgdown](http://pkgdown.r-lib.org/) setup to build the greta website at [https://greta-stats.org](). This setup does not create the [greta forum](https://forum.greta-stats.org), which uses [discourse](https://www.discourse.org/) running on [a cloud server](https://www.digitalocean.com/docs/one-clicks/discourse/).

Due to [a small bug in pkgdown](https://github.com/r-lib/pkgdown/pull/834), the build currently only works with fork of the main pkgdown repo. You can install that version with:

```
devtools::install_github("goldingn/pkgdown@rmd_home_template")
```

To rebuild the website, change your working directory to the top of the
*greta* repository, and use the pkgdown `build_site()` function from within R:

```
pkgdown::build_site()
```

This will update the `docs` directory. You then need to commit those changes to the `git` repository, and push to github.

The greta website is hosted by [netlify](https://www.netlify.com/), which will update the website after the files are pushed.

## customising pkgdown

The greta website build makes use of pkgdown's ability to customise templates, css, and the `_pkgdown.yml` configuration file, see the [`build_site()` documentation](http://pkgdown.r-lib.org/reference/build_site.html) for details.

The page templates are stored in `inst/pkgdown/templates`, and the custom css is at `inst/pkgdown/assets/pkgdown.css`. The `index.Rmd` file also contains a small amount of html code to align the example on the home page.

## updating existing pages

The main website pages can be edited by making changes to Rmarkdown documents and re-building the website:

To change ...       |  Edit ...
--------------------|------------------------
Home page example   |  `index.Rmd`
get started         |  `vignettes/get_started.Rmd`
contribute          |  `vignettes/webpages/contribute.Rmd`
software            |  `vignettes/webpages/software.Rmd`
technical details   |  `vignettes/webpages/technical_details.Rmd`
why greta           |  `vignettes/webpages/why_greta.Rmd`


## adding analysis case studies

To add an analysis case study, upload an Rmarkdown document to the `vignettes/analyses` folder, and edit 'menu' in the 'analyses' entry in the 'navbar' section of `_pkgdown.yml`, providing a short title and a relative path to the rendered output. E.g.:
```
- text: my analysis
  href: articles/analyses/my_analysis.html
```

## adding example models

Example models should show how to define greta arrays representing a general type of statistical model, using an example dataset. They should not contain calls to `model()`, or any plotting or inference on models. Each example model is in a self-contained Rmarkdown file.

To add an example model, create an R markdown file with the following components:

### title

Short, and s a level-3 header.

### brief introductory text

No more than a paragraph, please!

### data section

If needed, a level-4 header 'data' and a code chunk defining the data. The code chunk must have the chunk argument `highlight=FALSE`, and a chunk name that differs from the other example models and ends in `_data`. If the dataset is included with base R, this section can be omitted.

### greta code section

A level-4 header 'greta code' and an R code chunk defining the model in greta. The chunk name must that differ from the other example models and end in `_greta`.

### BUGS/JAGS code

If relevant, a level-4 header 'BUGS/JAGS code' and a (non-R) markdown code chunk giving the equivalent model in BUGS/JAGS code. The code block should be surrounded by an html `div` command defining a 'bugs' environment, which is used for code formatting.

### Stan code
If relevant, a level-4 header 'Stan code' and an R code chunk (with chunk argument `echo=FALSE`) with an R command to read in the equivalent model from the [Stan example models repository](https://github.com/stan-dev/example-models). The chunk name must that differ from the other example models and end in `_stan`. The code block should be surrounded by an html `div` command defining the Stan code formatting.

### 

Here's a template:
````
### my example model

introduce the model here

#### data
```{r my_example_data, highlight = FALSE}
```

#### greta code
```{r my_example_greta}
```

### BUGS/JAGS code
<div class="bugs">
```
```
</div>

### Stan code
<div class="stan">
```{r my_example_stan, echo = FALSE}
cat(readLines('https://raw.githubusercontent.com/stan-dev/example-models/master/ < path / to / model / code >.stan'), sep = '\n')
```
</div>
````

This should be saved as an .Rmd file with a meaningful name in the directory `inst/examples`. To add the example to the website, edit `vignettes/example_models.Rmd`, to add a horizontal rule and a code chunk in the relevant section, loading your example file as a child document. E.g.:

````
<hr>

```{r child='../inst/examples/my_example.Rmd'}
```
````

