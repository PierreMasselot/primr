library(fields)

x <- matrix(runif(400), ncol = 2)
y <- 10 * x[,1] + 5 * x[,2] + 10 * (x[,1] > .8 & x[,2] > .5) + rnorm(100)

plot(x, col = tim.colors(10)[cut(y, 10)], pch = 16)

object <- peeling.sequence(x = x, y = y)

plot_box(object)
plot_box(object, ypalette = heat.colors(5), box.args = list(lwd = 3))
plot_box(object, support = c(.5, .4, .3), 
  box.args = list(border = terrain.colors(3)))
plot_box(object, select = 1)
plot_box(object, select = 2)
plot_box(object, select = 2, npeel = 1:3*10, 
  box.args = list(border = heat.colors(3)))

plot_trajectory(object)
plot_trajectory(object, type = "b", pch = 16, col = "red")
plot_trajectory(object, xtype = "nobs", col = "blue")
plot_trajectory(object, xtype = "nobs", ytype = "diff")
plot_trajectory(object, xtype = "nobs", ytype = "rel.diff", pch = 15, cex = 0.5)   