library(GA)

dist <- function(x, y){
  (0.125246*0.3 - 0.802732*0.1 + 0.033585*x - 0.859439*y + 0.802732)*5
}

x <- y <- seq(0, 1, length = 50)
z <- outer(x, y, dist)


persp3D(x, y, z, zlab = "dist", xlab="quality", ylab="speed",
      theta = 30, phi = 15, col.palette = heat.colors)

#plot_ly(data, x = ~quality, y = ~speed, z = ~dist, type="scatter3d", mode = 'line')
