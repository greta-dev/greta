# functions for creating logos etc.

# get the coordinates and links to tile the logo 'repeats' times.
# x_start and y_start give the position of the first node
logo_shape <- function (x_start = 0, y_range = c(0, 1)) {

  # coordinates and links of the base shape
  coords <- data.frame(x = c(0, 0, 1, 1, 2, 2, 3),
                       y = c(0, 1, 0.5, 1.5, 0, 1, 0.5))

  links <- rbind(c(1, 3),
                 c(2, 3),
                 c(3, 6),
                 c(4, 6),
                 c(5, 7),
                 c(6, 7))

  scale <- abs(diff(y_range)) / 1.6
  coords$x <- coords$x * scale + x_start
  coords$y <- coords$y * scale + y_range[1]

  list(coords = coords, links =links)

}

plot_logo <- function (background = c('white', 'purple', 'light', 'lighter'),
                       pointsize = 4.5,
                       add = FALSE,
                       edge_width = 1,
                       ...) {

  background <- match.arg(background)

  data <- logo_shape(...)

  bg_col <- switch (background,
                    white = 'white',
                    light = greta:::greta_col('light'),
                    lighter = greta:::greta_col('lighter'),
                    purple = greta:::greta_col('main'))

  link_col <- switch (background,
                      white = greta:::greta_col('light'),
                      light = greta:::greta_col('dark'),
                      lighter = greta:::greta_col('dark'),
                      purple = greta:::greta_col('dark'))

  node_col <- switch (background,
                      white = greta:::greta_col('dark'),
                      light = greta:::greta_col('dark'),
                      lighter = greta:::greta_col('dark'),
                      purple = greta:::greta_col('dark'))

  if (!add) {

    old_mar <- par()$mar
    old_xpd <- par()$xpd
    old_bg <- par()$bg

    on.exit( par(mar = old_mar, xpd = old_xpd, bg = old_bg) )

    par(mar = rep(2, 4),
        xpd = NA,
        bg = bg_col)

    plot.new()
    plot.window(xlim = range(data$coords$x),
                ylim = range(data$coords$y),
                asp = 1)


  }

  # loop though from right to left, plotting points and edges to ensure
  # gaps on either side distances

  x_loc <- sort(unique(data$coords$x),
                decreasing = TRUE)

  for (loc in x_loc) {

    # find relevant nodes and edges
    idx_nodes <- which(data$coords$x %in% loc)
    idx_edges <- which(data$links[, 2] %in% idx_nodes)
    nodes <- data$coords[idx_nodes, , drop = FALSE]
    edges <- data$links[idx_edges, , drop = FALSE]

    # plot nodes with fat edge
    points(y ~ x,
           data = nodes,
           pch = 21,
           bg = node_col,
           col = bg_col,
           cex = pointsize - 1.5,
           lwd = pointsize * 4 * edge_width)

    # plot lines
    for (i in seq_len(nrow(edges))) {
      link <- edges[i, ]
      lines(x = data$coords$x[link],
            y = data$coords$y[link],
            lwd = pointsize * 2.3 * edge_width,
            col = link_col)
    }

    # plot nodes with thin edge
    points(y ~ x,
           data = nodes,
           pch = 21,
           bg = node_col,
           col = bg_col,
           cex = pointsize - 1.5,
           lwd = 0)

  }

}

# greta logo generation
# Muli fontface from: https://fonts.google.com/specimen/Muli

# plot a purple banner, with greta in white
# 'width' gives the width:height ratio
# 'margin' gives the proportion of the vertical height to use a border on each side
# the text is scaled to never exceed that border
#' @importFrom graphics par plot.new plot.window strheight strwidth text
banner <- function (background = c('purple', 'white', 'light', 'lighter'),
                    transparent_bg = FALSE,
                    width = 8, margin = 0.2,
                    font = c('Muli', 'sans'),
                    add_logo = TRUE, ...) {

  font <- match.arg(font)
  background <- match.arg(background)

  # warn if the banner isn't height-filled
  min_width <- 3.184175 + 2 * margin * (1 - 3.184175)
  if (width < min_width) {
    warning ('with a margin of ',
             margin,
             ' the minimum width to ensure the banner is height-filled is ',
             min_width)
  }

  bg_col <- switch (background,
                    white = 'white',
                    light = greta:::greta_col('light'),
                    lighter = greta:::greta_col('lighter'),
                    purple = greta:::greta_col('main'))

  text_col <- switch (background,
                      white = greta:::greta_col('dark'),
                      light = 'white',
                      lighter = greta:::greta_col('dark'),
                      purple = 'white')

  # cache the old graphics options
  old_bg <- par('bg')
  old_mar <- par('mar')
  old_family <- par('family')

  # switch to a purple background, no margins and Muli typeface
  par(bg = ifelse(transparent_bg, NA, bg_col),
      mar = rep(0, 4),
      family = font)

  # set up the device, to have the correct width
  plot.new()
  plot.window(xlim = c(0, width),
              ylim = c(0, 1),
              asp = 1)

  # scale the font, so that 'greta' fills the area (excluding self-imposed
  # margins) either vertically or horizontally
  max_height <- (1 - 2 * margin) / strheight('greta')
  max_width <- (width - 2 * margin) / strwidth('greta')
  fontsize <- min(max_height, max_width)

  # find the final string dimensions
  string_height <- fontsize * strheight('greta')
  string_width <- fontsize * strwidth('greta')

  # how far to indent 'greta'
  xpos <- margin

  # 'g' should be aligned to the left of the box
  text(x = xpos,
       y = 0.5,
       label = 'greta',
       col = text_col,
       cex = fontsize,
       pos = 4,
       offset = 0)

  if (add_logo) {

    plot_logo(background = background,
              add = TRUE,
              x_start = string_width + xpos * 3,
              y_range = 0.55 + string_height * 0.5 * c(-1, 1),
              ...)

  }

  par(bg = old_bg,
      mar = old_mar,
      family = old_family)

  invisible(NULL)

}

# same dimensions as banner, but with no text
blank_banner <- function (width = 8, margin = 0.2) {

  # cache the old graphics options
  old_bg <- par('bg')
  old_mar <- par('mar')

  # switch to a purple background with no margins
  par(bg = greta:::greta_col(),
      mar = rep(0, 4))

  # set up the device, to have the correct width
  plot.new()
  plot.window(xlim = c(0, width),
              ylim = c(0, 1),
              asp = 1)

  par(bg = old_bg,
      mar = old_mar)

  invisible(NULL)

}

# make and save an image of a triangular tesselation GMRF pattern in greta purple
tesselation_image <- function (ncol = 10, nrow = 10,
                               max_edge = 0.08,
                               jitter = 0.1,
                               thickness = 1,
                               line_col = greta:::greta_col('light'),
                               ramp_cols = NULL) {

  if (is.null(ramp_cols)) {
    cols <- c(greta:::greta_col('lighter'),
              greta:::greta_col('light'))
    ramp_cols <- colorRampPalette(cols)(2000)[-(1:1000)]
  }

  require (INLA)
  require (raster)
  require (greta)
  require (fields)

  # grid sizes for sampling the GRF and for the final image
  ncol_sim <- round(ncol / 10)
  nrow_sim <- round(nrow / 10)
  ratio <- ncol / nrow
  grid <- list(x = seq(0, 1, length.out = ncol_sim),
               y = seq(0, 1, length.out = nrow_sim))

  obj <- Exp.image.cov(grid = grid, theta = 0.1, setup = TRUE)

  r <- raster(sim.rf(obj))

  image <- raster(nrow = nrow, ncol = ncol)
  extent(image) <- c(0, ratio, 0, 1)
  extent(r) <- extent(image)


  pts <- expand.grid(seq(0, ratio, length.out = 10),
                     seq(0, 1, length.out = 10))
  pts <- pts + cbind(rnorm(100, 0, jitter),
                     rnorm(100, 0, jitter))

  # make an inla mesh
  sp <- as(extent(image), 'SpatialPolygons')
  mesh <- inla.mesh.2d(loc = pts,
                       boundary = inla.sp2segment(sp),
                       max.edge = max_edge,
                       offset = 0)

  # sample GRF at nodes
  z <- extract(r, mesh$loc[, 1:2])

  # get projection to raster
  image_coords<- xyFromCell(image, 1:ncell(image))
  A <- inla.spde.make.A(mesh, loc = image_coords)

  # instead of linear interpolation, average the three node values
  A2 <- A
  A2@x[A2@x > 0] <- 1/3
  image[] <- (A2 %*% z)[, 1]

  pm <- par("mar")
  on.exit(par(mar = pm))

  par(mar = rep(0, 4))
  image(image,
        col = ramp_cols,
        asp = 1,
        axes = FALSE,
        xlab = '',
        ylab = '')
  plot(mesh,
       add = TRUE,
       edge.color = line_col,
       lwd = thickness,
       draw.segments = FALSE)
  points(mesh$loc,
         pch = 16,
         cex = 0.5 * sqrt(thickness),
         col = line_col)
}
