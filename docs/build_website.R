# bespoke templating of pkgdown helpfiles and index page
write_topic <- function (data) {
  topic_template <- readLines('docs/_topic_template.txt')
  rmd <- whisker::whisker.render(topic_template, data)
  cat(rmd, file = paste0("docs/", data$name, ".Rmd"))
}
write_index <- function (data) {
  index_template <- readLines('docs/_index_template.txt')
  rmd <- whisker::whisker.render(index_template, data)
  cat(rmd, file = "docs/reference-index.Rmd")
}
# codeface and split argument names in a topic
split_args <- function (topic) {
  for (i in seq_along(topic$arguments)) {
    arg_names <- topic$arguments[[i]]$name
    arg_names <- gsub('&#8230;', '...', arg_names)
    arg_names <- gsub(', ', '`, `', arg_names)
    arg_names <- paste0('`', arg_names, '`')
    topic$arguments[[i]]$name <- arg_names
  }
  topic
}

# make sure docs and docs/figures exist
if (!dir.exists('docs'))
  dir.create('docs')
if (!dir.exists('docs/figures'))
  dir.create('docs/figures')

# copy css over
file.copy('vignettes/greta.css',
          'docs/greta.css',
          overwrite = TRUE)

# copy icons over
file.copy('logos/name_icon_on_purple.png',
          'docs/banner-icon.png',
          overwrite = TRUE)
file.copy('logos/name_icon_on_light.png',
          'docs/main-icon.png',
          overwrite = TRUE)
file.copy('logos/greta-header.png',
          'docs/greta-header.png',
          overwrite = TRUE)

# move man figures over
file.copy('man/figures/plotlegend.png',
          'docs/figures/plotlegend.png',
          overwrite = TRUE)

# copy vignettes (and examples) over
vignettes <- list.files('vignettes/',
                        pattern = '.Rmd',
                        full.names = TRUE)
sapply(vignettes,
       file.copy,
       'docs',
       recursive = TRUE)

file.copy('vignettes/examples',
          'docs',
          recursive = TRUE,
          overwrite = TRUE)

if (!dir.exists('docs/reference'))
  dir.create('docs/reference')

# build pages for helpfiles, put them in docs/reference
pkg <- pkgdown::as_pkgdown()
topics <- purrr::transpose(pkg$topics)
data_list <- lapply(topics, pkgdown:::data_reference_topic, pkg, examples = FALSE)
data_list <- lapply(data_list, split_args)
lapply(data_list, write_topic)

# build page for helpfile index
data_index <- pkgdown:::data_reference_index(pkg)
write_index(data_index)

# roll the whole site
rmarkdown::render_site('docs')
