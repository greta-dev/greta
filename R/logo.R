# greta logo generation
# Muli fontface from: https://fonts.google.com/specimen/Muli

# plot a purple banner, with greta in white
# 'width' gives the width:height ratio
# 'margin' gives the proportion of the vertical height to use a border on each side
# the text is scaled to never exceed that border
banner <- function (width = 8, margin = 0.2) {

  # warn in the banner isn't height-filled
  min_width <- 3.184175 + 2 * margin * (1 - 3.184175)
  if (width < min_width) {
    warning ('with a margin of ',
             margin,
             ' the minimum width to ensure the banner is height-filled is ',
             min_width)
  }

  # cache the old graphics options
  old_bg <- par('bg')
  old_mar <- par('mar')
  old_family <- par('family')

  # switch to a purple background, no margins and Muli typeface
  par(bg = 'darkorchid1',
      mar = rep(0, 4),
      family = 'Muli')

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
       col = 'white',
       cex = fontsize,
       pos = 4,
       offset = 0)

  par(bg = old_bg,
      mar = old_mar,
      xpd = old_xpd,
      family = old_family)

  invisible(NULL)

}

# same dimensions as banner, but with no text
blank_banner <- function (width = 8, margin = 0.2) {

  # cache the old graphics options
  old_bg <- par('bg')
  old_mar <- par('mar')

  # switch to a purple background with no margins
  par(bg = 'darkorchid1',
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
