library(sandwich)
library(MASS)
library(gridExtra)
library(ggplot2)
library(ellipse)
library(plyr)

set.seed(30)

n <- 2
x <- matrix(seq(0,1,length.out = 100), nrow=100, ncol = 9)
target <- sin(2 * pi * x)

noise <- rnorm(100, mean = 0, sd = 0.2)
target_orig = target
target <- target+ noise

# -------block 1
basis_u = seq(1,9)
basis_u = t(replicate(100, basis_u))

a <- cbind(rep(1, 100), exp(-((x- basis_u)/10) ** 2 / (2 * 0.1**2)))
# 
a

# solve(t(a) %*%  a) %*% t(a) %*% target

alpha <- 0.5
beta <- 25
S0 <- c(1/alpha,1/alpha) * diag(2)
mode <- c(0,0)


precision1w <- rep(alpha,10) + beta * (t(a[c(1,19,50,99),]) %*%  (a[c(1,19,50,99),]))
mode1w <- (beta * ginv(precision1w) %*%  (t(a[c(1,19,50,99),])) %*% target[c(1,19,50,99),])
mode1w <- mode1w[,1]
mode1w
variance1pred <- 1/beta + a %*% ginv(precision1w) %*% t(a)
# 
mode1pred <- mode1w %*% t(a)
# 
data.grid <- data.frame(cbind(x[,1], target_orig))
# q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mode1, sigma = variance1))
# mode1pred
# 
plot1 <- ggplot(data.frame(data.grid), aes(x = X1, y = X2)) + geom_line(color = "green") + geom_line(y = t(mode1pred), color = "darkred")
y_sample <- mvrnorm(n = 20, mu = mode1pred, Sigma = variance1pred)
x_sample <- sample(seq(1,100),20) 

y_sample <- y_sample[, c(x_sample)]
y_sample
y_sample <- apply(y_sample, MARGIN = 2, function(x) sample(x, replace=TRUE, size = 1))
y_sample
x_sample_new <- x_sample / 100
samples <- data.frame(cbind(x_sample_new, y_sample))
target_samples <- data.frame(cbind(x_sample_new, target[x_sample]))
plot1 <- plot1 + geom_point(data = samples, colour = "red", aes(x = x_sample_new, y = y_sample)) 
target[x_sample]
target_samples
plot1 + geom_point(data = target_samples, colour = "green", aes(x = x_sample_new, y = V2 ))


#     
# precision2 <- c(alpha,alpha) + beta * t(a[1:2,]) %*%  a[1:2,]
# variance2 <- 1/beta + a[1:2,] * precision2 * t(a[1:2,])
# mode2 <- ginv(precision2) %*% (beta * t(a[1:2,]) %*% t[1:2,])
# mode1
# mode2 <- t(mode2) %*% a[1:2,]
# q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mode2, sigma = variance2))
# q.samp

# plot2 <- ggplot(q.samp, aes(x=s.1, y=s.2)) + 
#     geom_raster(aes(fill = prob)) +
#     coord_fixed(xlim = c(0, 1), ylim = c(0, 1), ratio = 1)
# 
# grid.arrange(plot1, plot2, nrow=2, ncol=1)


# precision <- c(alpha,alpha) + beta * (matrix(a[1,], nrow = 2, ncol = 2)) %*%  t(matrix(a[1,], nrow = 2, ncol = 2))
# precision
# mode <- ginv(precision) %*% (beta * (matrix(a[1,], nrow = 2, ncol = 2)) %*% matrix(target[1,], nrow = 2))
# p.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mode, sigma = ginv(precision)))
# plot2 <- ggplot(p.samp, aes(x=s.1, y=s.2)) + 
#     geom_raster(aes(fill = prob)) +
#     coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)
#     
# precision <- c(alpha,alpha) + beta * (t(a[1:2,]) %*%  (a[1:2,]))
# mode <- ginv(precision) %*% (beta * (t(a[1:2,])) %*% target[1:2,])
# p.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mode, sigma = ginv(precision)))
# plot3 <- ggplot(p.samp, aes(x=s.1, y=s.2)) + 
#     geom_raster(aes(fill = prob)) +
#     coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)    
# grid.arrange(plot1, plot2, plot3, nrow=2, ncol=2)
