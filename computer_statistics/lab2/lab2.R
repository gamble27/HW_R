export RSTUDIO_WHICH_R=/usr/bin/R


# task 1 - read the data
df <-read.table(file = "~/Projects/HW_R/computer_statistics/lab2/distr10.txt",header=T, sep = '\t')
x <- data.matrix(df)

# create useless function
hist.task <- function(x, distrib){
  hist(x,freq = 0)
  curve(distrib(x), add = TRUE)
}

# some templates
legends <- c("N(1,1)", "LN(1,0.6)", "Exp(1)", "X^2 (n=1)")
p.distrs <- list(pnorm(sort(x), mean=1), 
                       plogis(sort(x), location = 1, scale = 0.6),
                       pexp(sort(x)),
                       pchisq(sort(x), df=1))
q.distrs <- list(qnorm(ppoints(x), mean=1), 
                 qlogis(ppoints(x), location = 1, scale = 0.6),
                 qexp(ppoints(x)),
                 qchisq(ppoints(x), df=1))

# task 2 - create histogram
hist(x,freq = 0)
curve(dnorm(x=x, mean=1), col="red", add=T) # normal
curve(dlogis(x, location = 1, scale = 0.6), col="blue", add=T) # lognormal
curve(dexp(x), col="green", add=T) # exp
curve(dchisq(x, df=1), col="orange", add=T) # chi squared
legend("topright", lty=1, cex=0.6,
       legend = legends, 
       col=c("red", "blue", "green", "orange"), 
       title = "Theoretical density")

# create two more useless functions
pp.task <- function(theor.distr, name){
  plot(theor.distr, (1:length(x))/length((x)),
         xlab = "Theoretical",
         ylab = "Empirical",
         main = paste(name, "PP-plot"))
  abline(0,1,col="red")
}
  
qq.task <- function(theor.distr, name){
  plot(theor.distr, sort(x),
       xlab = "Theoretical",
       ylab = "Empirical",
       main = paste(name, "QQ-plot"))
  abline(0,1,col="red")
}

# task 2 - create PP & QQ plots
for (i in 1:length(legends)){
  pp.task(p.distrs[[i]], legends[i])
  qq.task(q.distrs[[i]], legends[i])
}

# task 3 - QQ plots with intervals
library("car")
qqPlot(x-1)










