sd = 1

# оцінка методу моментів 
EstMLE <- function(X){
  return(mean(log(X)))
}

# зміщення оцінки
get.bias <- function(est, n, mu=0){
  (n^0.5)*(mean(est)-mu)
}

# розсіювання оцінки
get.dispersion <- function(est,n){
  n*var(est)
}

# отримати оцінку для B вибірок об'єму n
get.est <- function(n, B=1000, mu=0){
  estims <- 1:B
  for (i in 1:B){
    x <- exp(rnorm(n, mean=mu, sd=sd))
    estims[i] <- EstMLE(x)
  }
  return(estims)
}

task <- function(){
  ns <- c(50, 100, 500, 1000)
  for (n in ns){
    est <- get.est(n)
    m <- get.bias(est,n)
    d <- get.dispersion(est,n)
    
    title <- sprintf("Histogram of 1000 samples (vol=%i)", n)
    hist(n^0.5 * (est), freq=F, main=title)
    curve(dnorm(x,mean=m,sd=d^0.5), col="red",add=T)
    
    cat("For 1000 samples of volume",n,
        ":\nBias          ",m,
         "\nDispersion    ",d,
        "\n\n")
  }
}

task()
