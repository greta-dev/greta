library(hexSticker)
library(magick)
library(sysfonts)
font_add_google("Muli")
attach(greta::.internals$utils$colours)

# load various functions
source ("logos/logo_functions.R")

# make a hex-shaped mask
hexd <- data.frame(x = 1 + c(rep(-sqrt(3) / 2, 2), 0, rep(sqrt(3) / 2, 2), 0),
                   y = 1 + c(0.5, -0.5, -1, -0.5, 0.5, 1))

x_lim <- range(hexd$x)
y_lim <- range(hexd$y)
x_dim <- abs(diff(x_lim))
y_dim <- abs(diff(y_lim))
ex <- 4
pdf("logos/hex_mask.pdf", width = x_dim * ex, height = y_dim * ex)
par(pty = "s",
    xpd = NA,
    bg = "black",
    mar = rep(0, 4), oma = rep(0, 4))
plot.new()
plot.window(xlim = x_lim, ylim = y_lim)
polygon(hexd, col = "white")
dev.off()

# load the hex mask
mask <- image_read("logos/hex_mask.pdf")
mask <- image_transparent(mask, "black")
mask_dim <- as.numeric(image_info(mask)[1, 2:3])
# create a tesselation figure with the same footprint
# make a nice GRF tesselation for the header image
set.seed(2018-02-20)
dim <- mask_dim * 2
cols <- c(greta:::greta_col("lighter"),
          greta:::greta_col("main"))
ramp_cols <- colorRampPalette(cols)(2000)#[-(1:1000)]

png("logos/hex_bg.png",
    width = dim[1],
    height = dim[2],
    pointsize = 30)
tesselation_image(ncol = dim[1], nrow = dim[2],
                  max_edge = 0.1,
                  jitter = 0.05,
                  thickness = 2,
                  ramp_cols = ramp_cols,
                  line_col = greta_col("main"))
dev.off()


# crop and mask the pattern to a hexagon
bg <- image_read("logos/hex_bg.png")
geometry <- sprintf("%ix%i+%i+%i",
                    mask_dim[1],
                    mask_dim[2],
                    mask_dim[1] %/% 2,
                    mask_dim[2] %/% 2)
bg <- image_crop(bg, geometry)

hex_bg <- image_composite(bg, mask, "CopyOpacity")
image_write(hex_bg, path = "logos/hex_bg.pdf")

par(pty = "s",
    xpd = NA,
    bg = "black",
    mar = c(5, 4, 4, 2) + 0.1)

greta_hex <- sticker("logos/hex_bg.pdf",
                     s_x = 1,
                     s_y = 1,
                     s_width = 1.05,
                     package = "greta",
                     p_y = 1.1,
                     p_family = "Muli",
                     p_size = 15,
                     h_fill = greta_col("main"),
                     h_color = greta_col("main"),
                     filename = "logos/greta_hex.png")
library(ggplot2)
ggsave(greta_hex, width = 43.9, height = 50.8,
       filename = "logos/greta_hex.png",
       bg = "transparent",
       units = "mm",
       dpi = 600)

file.remove("logos/hex_bg.pdf")
file.remove("logos/hex_bg.png")
file.remove("logos/hex_mask.pdf")
