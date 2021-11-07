sd = 1

# оцінка методу моментів 
EstMM <- function(X, sd=1){
  return(log(mean(X)) - (sd^2)/2)
}

# зміщення оцінки
get.bias <- function(est, n, mu=0){
  (n^0.5)*(mean(est)-mu)
}

# розсіюванню оцінки
get.dispersion <- function(est,n){
  n*var(est)
}

# довірчий інтервал
get.interval <- function(X, alpha=0.05, sd=1){
  estim <- EstMM(X, sd)
  dispersion <- exp(sd^2)-1
  delta <- qnorm(1-alpha/2)*(dispersion / length((X)))^0.5
  c(estim-delta,estim+delta)
}

# отримати оцінку для B вибірок об'єму n
get.est <- function(n, B=1000, mu=0, seed=42){
  set.seed(seed)
  estims <- 1:B
  for (i in 1:B){
    x <- exp(rnorm(n, mean=mu, sd=sd))
    estims[i] <- EstMM(x, sd=sd)
  }
  return(estims)
}

# отримати значущість довірчого інтервалу
# для B вибірок об'єму n
get.significance <- function(n, B=1000, mu=0, seed=42){
  set.seed(seed)
  res <- 1:B
  for (i in 1:B){
    x <- exp(rnorm(n, mean=mu, sd=sd))
    est <- EstMM(x,sd=sd)
    int <- get.interval(x)
    if ((mu>int[1]) && (mu<int[2])){
      res[i] <- 1
    } else {
      res[i] <- 0
    }
  }
  return(1-mean(res))
}

tasks.4.6 <- function(){
  ns <- c(50, 100, 500, 1000, 5000)
  for (n in ns){
    est <- get.est(n)
    m <- get.bias(est,n)
    d <- get.dispersion(est,n)
    s <- get.significance(n)
    
    title <- sprintf("Histogram of 1000 samples (vol=%i)", n)
    hist(n^0.5 * (est), freq=F, main=title)
    curve(dnorm(x,mean=m,sd=d^0.5), col="red",add=T)
    
    cat("For 1000 samples of volume",n,
        ":\nBias                 ",m,
         "\nDispersion           ",d,
         "\nInterval significance",s,
        "\n\n")
  }
}

tasks.4.6()
