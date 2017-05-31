# template rmd file to paste html contents into
topic_template <- function() readLines('docs/_topic_template.txt')

# template rmd file to paste html contents into
index_template <- function() readLines('docs/_index_template.txt')

# substitute in the data and write the rmd
write_topic <- function (data) {
  rmd <- whisker::whisker.render(topic_template(), data)
  cat(rmd, file = paste0("docs/", data$name, ".Rmd"))
}

write_index <- function (data) {
  rmd <- whisker::whisker.render(index_template(), data)
  cat(rmd, file = "docs/reference-index.Rmd")
}

# make sure docs exists
if (!dir.exists('docs'))
  dir.create('docs')

# copy css over
file.copy('vignettes/greta.css', 'docs/greta.css', overwrite = TRUE)

# copy banner icon over
file.copy('logos/name_icon_on_purple.png', 'docs/banner_icon.png', overwrite = TRUE)

# copy vignettes (and examples) over
vignettes <- list.files('vignettes/', pattern = '.Rmd', full.names = TRUE)
sapply(vignettes, file.copy, 'docs', recursive = TRUE)
file.copy('vignettes/examples', 'docs', recursive = TRUE)

if (!dir.exists('docs/reference'))
  dir.create('docs/reference')

# build pages for helpfiles, put them in docs/reference
pkg <- pkgdown::as_pkgdown()
topics <- purrr::transpose(pkg$topics)
data_list <- lapply(topics, pkgdown:::data_reference_topic, pkg, examples = FALSE)
lapply(data_list, write_topic)

# need to run this the same way
# pkgdown::build_reference_index()
data_index <- pkgdown:::data_reference_index(pkg)
write_index(data_index)
rmarkdown::render_site('docs')


# still need to get this to see the examples (render being run from the top level, not from docs)
