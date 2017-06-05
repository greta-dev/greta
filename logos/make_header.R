# make a nice GRF tesselation for the header image
rm(list = ls())
set.seed(1)

library(INLA)
library(raster)
library(greta)
library(fields)

# grid sizes for sampling the GRF and for the final image
ncol <- 2732
nrow <- 1194
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
pts <- pts + cbind(rnorm(100, 0, 0.1),
                   rnorm(100, 0, 0.1))

# make an inla mesh
sp <- as(extent(image), 'SpatialPolygons')
mesh <- inla.mesh.2d(loc = pts,
                     boundary = inla.sp2segment(sp),
                     max.edge = 0.08,
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

pal <- colorRampPalette(c(greta:::greta_col('lighter'),
                          greta:::greta_col('light')))

# make a pretty plot
png('logos/greta-header.png',
    width = ncol,
    height = nrow,
    pointsize = 30)

par(mar = rep(0, 4))
image(image,
      col = pal(2000)[-(1:1000)],
      asp = 1,
      axes = FALSE,
      xlab = '',
      ylab = '')
plot(mesh,
     add = TRUE,
     edge.color = greta:::greta_col('light'),
     lwd = 1,
     draw.segments = FALSE)
points(mesh$loc,
       pch = 16,
       cex = 0.5,
       col = greta:::greta_col('light'))

dev.off()
