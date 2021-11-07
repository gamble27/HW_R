library(rgl)

#############
## task 10 ##
#############

seed = 2^5

#############
###   1   ###
#############

# pseudo-random generator
# corresponding to the 
# number of task
pseudo.random <- function(){
  m = 2^31
  a = 75831
  c0 = 3000
  
  seed <<- (a*seed + c0) %% m
  return(seed)
}

# park-miller generator
park.miller <- function(){
  a = 7^5
  m = 2^31-1
  
  seed <<- a*seed %% m
  return(seed)
}

#############
###   2   ###
#############

# automation
exercise.2 <- function(generator, name){
  # generate sequence
  seed = 2^5
  n=500
  elements = replicate(n, generator())
  # compare theoretical 
  # & empirical PDF
  plot(ecdf(elements), main=name)
  segments(x0=0, y=0, x1=max(elements), y1=1, col="red", lwd=2)
  
  # plot diagram
  hist(elements, main=name)
  
  # plot pairs
  plot(elements[1:(n-1)],elements[2:n], main=name)
  
  # plot three
  plot3d(elements[1:(n-2)], elements[2:(n-1)], elements[3:n], main=name, size=15)
}

# my generator
exercise.2(pseudo.random, "Task 10")

# park-miller generator
exercise.2(park.miller, "Park-Miller")

#############
###   3   ###
#############

# normal transformation
box.muller <- function(x,y){
  
  a <- (-2*log(x))^0.5 * sin(2*pi*y)
  b <- (-2*log(x))^0.5 * cos(2*pi*y)
  
  return(c(a,b))
}

# generate the mixture
n=50000
seed <- 2^5
template <- replicate(3*n, park.miller())
norm1 <- template[1:n] / (max(template))
norm2 <- template[(n+1):(2*n)] / max(template)
mixer <- template[(2*n+1):(3*n)] / max(template)
for (i in (1:(n/2))*2){
  v <- box.muller(norm1[i], norm1[n-i+1])
  norm1[i] <- v[1]
  norm1[n-i+1] <- v[2]
  
  v <- box.muller(norm2[i], norm2[n-i+1])
  norm2[i] <- v[1]
  norm2[n-i+1] <- v[2]
}
norm1 <- norm1 + 1
norm2 <- norm2*(0.5)^0.5 - 1
mixture <- 1:n
for (i in 1:n){
  if (mixer[i] < 0.5){
    mixture[i] <- norm1[i]
  } else {
    mixture[i] <- norm2[i]
  }
}

# plot the mixture
#plot3d(mixture[1:498], mixture[2:499], mixture[3:500], size=10)

#############
###   4   ###
#############

plot(ecdf(mixture), main="Mixture CDF")
x = seq(-3,6,.01)
truth = 0.5*pnorm(x,mean=1,sd=1) + 0.5*pnorm(x,mean=-1,sd=0.5^0.5)
lines(x,truth,col="red",lwd=1)
legend("bottomright", legend=c("empirical", "theoretical"),
       col=c("black", "red"), lty=1:2, cex=0.8)

plot(density(mixture), main="Mixture density estimate", lwd=2)
x = seq(-3,6,.01)
truth = 0.5*dnorm(x,1,1) + 0.5*dnorm(x,-1,0.5^0.5)
lines(x,truth,col="red", lty=2, lwd=2)
legend("topright", legend=c("empirical", "theoretical"),
       col=c("black", "red"), lty=1:2, cex=0.8)

#############
###   5   ###
#############
n=50000                                         
mixture = 1:n
for(i in 1:n){
  if(runif(1)<0.5){
    mixture[i] = rnorm(1,1,1)
  } else {
    mixture[i] = rnorm(1,-1,0.5^0.5)
  }
}

plot(density(mixture), main="Mixture density estimate", lwd=2)
x = seq(-3,6,.01)
truth = 0.5*dnorm(x,1,1) + 0.5*dnorm(x,-1,0.5^0.5)
lines(x,truth,col="red", lty=2, lwd=2)
legend("topright", legend=c("empirical", "theoretical"),
       col=c("black", "red"), lty=1:2, cex=0.8)

#############
###   6   ###
#############
fib <- 1:500
fib[1] <- 2^5
fib[2] <- 2^5 * 7^5 %% (2^31-1)

fib.generator <- function(i){
  m = 2^31-1
  fib[i] <<- (fib[i-1]+fib[i-2]) %% m
}

for (i in 3:500){
  fib.generator(i)
}

# compare theoretical 
# & empirical PDF
plot(ecdf(fib), main="Fibonacci")
segments(x0=0, y=0, x1=max(fib), y1=1, col="red", lwd=2)

# plot diagram
hist(fib, main="Fibonacci")

# plot pairs
plot(fib[1:(n-1)],fib[2:n], main="Fibonacci")

# plot three
plot3d(fib[1:(n-2)], fib[2:(n-1)], fib[3:n], main="Fibonacci", size=15)
  