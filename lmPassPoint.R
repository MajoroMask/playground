# Generate some data that *do* pass through three points (up to random error).
x <- 1:24
f <- function(x) ((x-2)*(x-12) + (x-2)*(x-23) + (x-12)*(x-23)) / 100
y0 <-(x-2) * (x-12) * (x-23) * (1 + x - (x/24)^2) / 10^4  + f(x)
set.seed(17)
eps <- rnorm(length(y0), mean=0, 1/2)
y <- y0 + eps
data <- data.frame(x,y)

# Plot the data and the three special points.
plot(data)
points(cbind(c(2,12,23), f(c(2,12,23))), pch=19, col="Red", cex=1.5)

# For comparison, conduct unconstrained polynomial regression
data$x2 <- x^2
data$x3 <- x^3
data$x4 <- x^4

fit0 <- lm(y ~ x + x2 + x3 + x4, data=data)
lines(predict(fit0), lty=2, lwd=2)

# Conduct the constrained regressions
data$y1 <- y - f(x)
data$r <- (x-2)*(x-12)*(x-23)
data$z0 <- data$r
data$z1 <- data$r * x
data$z2 <- data$r * x^2

fit <- lm(y1 ~ z0 + z1 + z2 - 1, data=data)
lines(predict(fit) + f(x), col="red", lwd=2)
