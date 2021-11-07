###########################################################################
###############                part 1                ######################
###########################################################################

# read the data

filenames <- list.files(
  path="/home/olga/Projects/HW_R/computer_statistics_ms/lab1/data",
  full.names=TRUE
)
datalist <- lapply(filenames,
                   function(x){
                    x0 <- read.csv(file=x,header=F)[,c(1,6)];
                    colnames(x0) <- c("date",
                                      unlist(strsplit(x,"[_.]"))[5]);
                    x0
                  })
y <- Reduce(
  function(x,y) {
    merge(x,y,by="date")
  },
  datalist)

# modeling 0 to nn-10

# all regressors
Data <- y[-nrow(y),-1]
Data$apol <- y$apol[-1]
nn <- nrow(Data)
mdl10 <- lm(apol~.-apol, data=Data[0:(nn-10),])
summary(mdl10)

# without ba
mdl10_2 <- lm(apol~.-apol-ba, data=Data[0:(nn-10),])
summary(mdl10_2)

# modeling nn-60 to nn-10
Data <- y[-nrow(y),-1]
Data$apol <- y$apol[-1]
nn <- nrow(Data)
mdl50 <- lm(apol~.-apol, data=Data[(nn-60):(nn-10),])
summary(mdl50)

mdl50_2 <- lm(apol~.-apol, data=Data[(nn-60):(nn-10),-c(3,4,6,8,9,10)])
summary(mdl50_2)

###########################################################################
###############                part 2                ######################
###########################################################################

library(car)
influencePlot(mdl10_2)

influencePlot(mdl50)

U1 <- Data$apol[(nn-9):(nn)]-predict(mdl50,Data[(nn-9):(nn),])
U2 <- Data$apol[(nn-9):(nn)]-predict(mdl10_2,Data[(nn-9):(nn),])

hist (U1,freq = 0)
hist (U2,freq = 0)

plot(
  qnorm(ppoints(U2)), sort(U2),
  xlab = "Theoretical" ,
  ylab = "Empirical" ,
  main = "mdl50 QQ - plot"
)
abline (0 ,1,col = "red")

plot(U1,type="l",col="red",ylim=c(min(U1,U2),max(U1,U2)))
lines(U2,col="blue")
legend(
  "bottomright",
  legend = c("mdl50", "mdl10_2"),
  col = c("red", "blue"),
  lty=1:1
)

aU1 <- abs(U1)
aU2 <- abs(U2)
plot(aU1,type="l",col="red",ylim=c(0,max(aU1,aU2)))
lines(aU2,col="blue")
legend(
  "topright",
  legend = c("mdl50", "mdl10_2"),
  col = c("red", "blue"),
  lty=1:1
)
