library("MASS")

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
                    x0 <- read.csv(file=x,header=F)[,c(1,6)]
                    colnames(x0) <- c("date",
                                      unlist(strsplit(x,"[_.]"))[5])
                    x0
                  })
y <- Reduce(
  function(x,y) {
    merge(x,y,by="date")
  },
  datalist)

Data <- y[-nrow(y),-1]
Data$apol <- y$apol[-1]

nn <- nrow(Data)
train_full <- Data[1:(nn-20),]
train_last <- Data[(nn-70):(nn-20),]
test_set   <- Data[(nn-20):nn,]

###########################################################################
###############                part 2                ######################
###########################################################################

# utils

train <- 2:10
test  <- 1

path <- "/home/olga/Projects/HW_R/computer_statistics_ms/lab3/plots/"

create_img_path <- function(prefix, root, default_path=path) {
  return(paste0(default_path, prefix, root, ".png", sep=""))
  }

# ridge regression

perform_ridge <- function (dataset, name, lambdas=seq(0.001, 50, .01)) {
  # create model
  mdl <- lm.ridge(apol~.-apol, data=dataset,lambda = lambdas)

  # summary
  print(name)
  print(summary(mdl))

  png(file=create_img_path("lambdas_", name), width = 1000,height = 800)
  plot(mdl$lambda, mdl$GCV,
       type="l", xlab="mu", ylab="CV")
  i <- which.min(mdl$GCV)
  abline(v=mdl$lambda[i],col="red")
  dev.off()

  png(file=create_img_path("matplot_", name), width = 1000, height = 800)
  matplot(mdl$lambda, t(mdl$coef),
          type="l", col=1:8, lty=1:8,
          xlab="mu", ylab="coeficients")
  legend("topright", col=1:9,
         legend=colnames(Data)[1:8], lty=1:8)
  abline(a=0, b=0)
  abline(v=mdl$lambda[i], col="red")
  dev.off()

  # coefs
  coefs <- coef(mdl)[i,]
  print(coefs)

  # regularization parameter & CV value
  print(mdl$GCV[i])

  # prediction on test set
  pred <- as.matrix(cbind(const=1,test_set[,train])) %*% coefs
  U <- test_set[,test]-pred

  # print("MSE: ")
  # print(mean((pred - test_set[,test])^2))

  print("R squared: ")
  print(cor(pred, test_set[,test])^2)

  png(file=create_img_path("test_",name), width = 1000, height = 800)
  plot(pred, test_set[,test])
  dev.off()

  png(file=create_img_path("resid_",name), width = 1000, height = 800)
  plot(U, type="l", col="red", ylim=c(min(U),max(U)))
  dev.off()
}

perform_ridge(train_full, "full")
perform_ridge(train_last, "last")

###########################################################################
###############                part 3                ######################
###########################################################################

# compare

mdl2 <- lm(apol~.-apol, data = train_last)
mdl <- lm.ridge(apol~.-apol, data = train_last, lambda = seq(0.001, 50, .01))
pred2 <- predict(mdl2, test_set[,train])
i <- which.min(mdl$GCV)
coefs <- coef(mdl)[i,]
pred <- as.matrix(cbind(const=1,test_set[,train])) %*% coefs
U <- test_set[,test]-pred
U2 <- test_set[,test]-pred2
plot(U, type="l", col="red", ylim=c(min(U,U2),max(U,U2)))
lines(U2,col="blue")
legend("topleft", legend=c("lm", "ridge"),col=c("red", "blue"), lty=1:2, cex=0.8)
legend("topleft", legend=c("lm", "ridge"),col=c("red", "blue"), lty=1:1, cex=0.8)
legend("topleft", legend=c("lm", "ridge"),col=c("blue", "red"), lty=1:1, cex=0.8)
