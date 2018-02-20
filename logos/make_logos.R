# load various functions
source ("logos/logo_functions.R")

# ~~~~~~~~~~~
# render logos and banners

# banner for top of documents, with name and logo
png('README_files/top_banner.png',
    height = 288, width = 4032,
    pointsize = 25)
banner(width = 14)
dev.off()

# thin blank banner for between document sections
png('README_files/banner.png',
    height = 4, width = 940,
    pointsize = 25)
blank_banner(14/0.2)
dev.off()

# thicker blank banner for end of document
png('README_files/bottom_banner.png',
    height = 8, width = 940,
    pointsize = 25)
blank_banner(14/0.5)
dev.off()

png('logos/icon_on_white.png',
    height = 1000, width = 1800,
    pointsize = 16)
plot_logo(pointsize = 24)
dev.off()

png('logos/icon_on_purple.png',
    height = 1000, width = 1800,
    pointsize = 16)
plot_logo('purple', pointsize = 24)
dev.off()

ptsz <- 80

png('logos/name_on_purple.png',
    height = 1000, width = 1800,
    pointsize = ptsz)
banner(transparent_bg = TRUE,
       width = 2.310505,
       add_logo = FALSE)
dev.off()

png('logos/name_on_white.png',
    height = 1000, width = 1800,
    pointsize = ptsz)
banner('white',
       transparent_bg = TRUE,
       width = 2.310505,
       add_logo = FALSE)
dev.off()

png('logos/name_icon_on_white.png',
    height = 1000, width = 3600,
    pointsize = ptsz)
banner('white',
       transparent_bg = TRUE,
       width = 4,
       add_logo = TRUE,
       edge_width = 2.7)
dev.off()

png('logos/name_icon_on_purple.png',
    height = 1000, width = 3600,
    pointsize = ptsz)
banner('purple',
       transparent_bg = TRUE,
       width = 4,
       add_logo = TRUE,
       edge_width = 2.7)
dev.off()

png('logos/name_icon_on_light.png',
    height = 1000, width = 3600,
    pointsize = ptsz)
banner('light',
       transparent_bg = TRUE,
       width = 4,
       add_logo = TRUE,
       edge_width = 2.7)
dev.off()

png('logos/name_icon_on_lighter.png',
    height = 1000, width = 3600,
    pointsize = ptsz)
banner('lighter',
       transparent_bg = TRUE,
       width = 4,
       add_logo = TRUE,
       edge_width = 2.7)
dev.off()

png('logos/gravatar.png',
    height = 3600, width = 3600,
    pointsize = ptsz)
banner('white',
       width = 2.310505,
       add_logo = FALSE)
dev.off()

# make a nice GRF tesselation for the header image
set.seed(1)
png("logos/greta-header.png",
    width = 2732,
    height = 1194,
    pointsize = 30)
tesselation_image(ncol = 2732, nrow = 1194)
dev.off()
