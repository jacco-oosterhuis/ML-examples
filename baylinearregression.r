# library(MASS)
library(gridExtra)
library(ggplot2)
library(ellipse)
library(plyr)
set.seed(50)
x <- matrix(runif(600, min = -1, max = 1))
f <- function(x, a1, a2){a1 + a2 * x}
noise <- rnorm(600, mean = 0, sd = 0.2)
target <- f(x, a1 = -0.3, a2 = 0.5) + noise

a <- cbind(rep(1, length(x)), x)
solve(t(a) %*%  a) %*% t(a) %*% target
# ginv(a) %*% target
alpha <- 2.0
beta <- 25
S0 <- c(1/alpha,1/alpha) * diag(2)
S0
mode <- c(0,0)
data.grid <- expand.grid(s.1 = seq(-2, 2, length.out=200), s.2 = seq(-2, 2, length.out=200))
q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = c(0,0), sigma = S0))

plot1 <- ggplot(q.samp, aes(x=s.1, y=s.2)) + 
    geom_raster(aes(fill = prob)) +
    coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)


precision <- c(alpha,alpha) + beta * (matrix(a[1,], nrow = 2, ncol = 2)) %*%  t(matrix(a[1,], nrow = 2, ncol = 2))
precision
mode <- ginv(precision) %*% (beta * (matrix(a[1,], nrow = 2, ncol = 2)) %*% matrix(target[1,], nrow = 2))
p.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mode, sigma = ginv(precision)))
plot2 <- ggplot(p.samp, aes(x=s.1, y=s.2)) + 
    geom_raster(aes(fill = prob)) +
    coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)
    
precision <- c(alpha,alpha) + beta * (t(a[1:2,]) %*%  (a[1:2,]))
mode <- ginv(precision) %*% (beta * (t(a[1:2,])) %*% target[1:2,])
p.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mode, sigma = ginv(precision)))
plot3 <- ggplot(p.samp, aes(x=s.1, y=s.2)) + 
    geom_raster(aes(fill = prob)) +
    coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)    
grid.arrange(plot1, plot2, plot3, nrow=2, ncol=2)
# Y = mvrnorm(1000, mu = mode, Sigma = ginv(precision))
# alpha_levels <- seq(0.5, 0.95, by = 0.05)
# contour_data <- ldply(alpha_levels, ellipse, x = ginv(precision), scale = c(1,1), centre = mode)
# sampled_data <- data.frame(X = Y[,1], Y = Y[,2])
# p <- ggplot(data = contour_data, aes(x, y)) + coord_cartesian(xlim= c(-2,2), ylim = c(-2,2))
# p + geom_path()
# precision
