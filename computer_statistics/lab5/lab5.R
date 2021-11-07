# H0: Pois(3)
# H1: Pois(4)
l0 = 3
l1 = 4

# sample volume
n=45

# calculate log(likelihood ratio)
log.lr <- function(X){
  return(length(X)*(l0-l1)+log(l1/l0)*sum(X))
}

# calculate test threshold
get.threshold <- function(n, alpha=0.05, smpls=1e+4){
  l = 1:smpls
  for (i in 1:smpls){
    smpl = rpois(n, l0)
    l[i] = log.lr(smpl)
  }
  return(quantile(l, probs=1-alpha))
}

# calculate probability of getting type II error
get.pII <- function(n, c, smpls=1e+4){
  f = 1:smpls
  for (i in 1:smpls){
    smpl = rpois(n, l1)
    f[i] = ifelse(log.lr(smpl)<=c, 1, 0)
  }
  return(mean(f))
}

# calculate real significance level
get.pI <- function(n, c, smpls=1e+4){
  f = 1:smpls
  for (i in 1:smpls){
    smpl = rpois(n, l0)
    f[i] = ifelse(log.lr(smpl)>c, 1, 0)
  }
  return(mean(f))
}

c = get.threshold(n, smpls = 1e+8)
get.pII(n, c)
get.pI (n, c)

# get minimum sample size for pI, pII < 0.05
alpha = 0.05
n.min = n
for (i in 1:(n-1)){
  cn = get.threshold(n-i)
  if ((get.pI(n-i, cn, smpls = 1e+6) < alpha) && (get.pII(n-i, cn, smpls = 1e+6) < alpha)){
    n.min = n-i
  } else break
}
n.min

# lr histogram for H0, H1
smpls = 1e+4
lr0 = replicate(smpls, log.lr(rpois(n, lambda=l0)))
lr1 = replicate(smpls, log.lr(rpois(n, lambda=l1)))
xlims = c(min(c(min(lr0), min(lr1))), 
          max(c(max(lr0), max(lr1))))
hist(lr0, probability = T, 
     col = "blue", density = 10, xlim = xlims, 
     main = "Histogram of ln(LR(X))",
     xlab = "LR(X)")
hist(lr1, probability = T, col = "red",  density = 10, xlim = xlims, add = T)
legend("topright", legend = c("H0: Pois(3)", "H1: Pois(4)"),
       col = c("blue", "red"), lty =1, cex=0.55)
