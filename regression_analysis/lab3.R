df <- read.table(
  file = "~/Projects/HW_R/regression_analysis/data/c10.txt", 
  header=T, sep = '\t')

x <- df[, "X"]
y <- df[, "Y"]

plot(x, y)
curve(((x-15)^2) / x * 3 - 40, add=T, col="blue")

plot(x,log(y))
plot(log(x),y)
plot(log(x),log(y))

t  <- log(x)
t2 <- t^2
t3 <- t^3
t4 <- t^4

mdl <- lm(y ~ t4 + t3 + t2 + t)
summary(mdl)

library("car")
plot(y, mdl$fitted.values)
abline(a=0,b=1, add=T, col="blue")
hist(mdl$residuals, col="blue")
qqPlot(mdl$residuals)

