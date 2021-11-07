df <- read.table(
  file = "~/Projects/HW_R/regression_analysis/data/house01.txt", 
  header=T, sep = '\t')

x = df$STOTAL
y = df$TOTALEXP
z = df$HEATIND
plot(x, y, col=ifelse(z==1,"green","cyan"))

cnd1 = which(x>1)
cnd2 = which(x<300)
cnd3 = which(y<30000)
cnd = intersect(cnd1,intersect(cnd2,cnd3))

x = x[cnd]
y = y[cnd]
z = z[cnd]
plot(x,y, col=ifelse(z==1,"green","cyan"))
abline(lm(y~x), col="red")
abline(lm(y.1~x.1), col="green4")
abline(lm(y.2~x.2), col="blue")
legend("topright", lty=1, cex=0.6,
       legend = c("Unsliced model","Slice 1 (z=1)","Slice 2 (z=2)"),
       col = c("red", "green4", "blue"),
       title = "Regression lines")

x.1 = x[z==1]
y.1 = y[z==1]
plot(x.1,y.1, col="green")
abline(lm(y.1~x.1), col="green4")

x.2 = x[z==2]
y.2 = y[z==2]
plot(x.2,y.2, col="cyan")
abline(lm(y.2~x.2), col="blue")

get.rss <- function(x,y){
  Lm1 <- lm(y~x)
  return(sum(Lm1$residuals^2))
}

m=length(x)-4
U.R = get.rss(x,y)
U = get.rss(x.1,y.1) + get.rss(x.2,y.2)
F.empirical <- m*(U.R-U)/(2*U)
alpha=0.05
F.theoretical <- qf(1-alpha, df1=2, df2=m)

summary(lm(y~x))
