library("pls")

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

Data <- y[-nrow(y),-1]
Data$apol <- y$apol[-1]

nn <- nrow(Data)
train_full <- Data[1:(nn-20),]
train_last <- Data[(nn-70):(nn-20),]
test_set   <- Data[(nn-20):nn,]

###########################################################################
###############                part 2                ######################
###########################################################################

# PCA

train <- 2:10
test  <- 1

perform_pca <- function (dataset, name) {
  pc <- princomp(dataset[,train],cor=T)

  print(name)
  print(summary(pc))
  print(loadings(pc))

  png(file=paste("/home/olga/Projects/HW_R/computer_statistics_ms/lab2/plots/pc_",name,".png"),width = 1000,height = 800)
  plot(pc)
  dev.off()
}

# PCR

perform_pcr <- function (dataset, name, n_components) {
  mdl <- pcr(apol~.-apol, data=dataset,scale=T,validation="CV", ncomp=n_components)

  print(name)
  print(summary(mdl))

  png(file=paste("/home/olga/Projects/HW_R/computer_statistics_ms/lab2/plots/valid_",name,".png"),width = 1000,height = 800)
  validationplot(mdl)
  dev.off()

  png(file=paste("/home/olga/Projects/HW_R/computer_statistics_ms/lab2/plots/pred_",name,".png"),width = 1000,height = 800)
  predplot(mdl)
  dev.off()

  png(file=paste("/home/olga/Projects/HW_R/computer_statistics_ms/lab2/plots/coef_",name,".png"),width = 1000,height = 800)
  coefplot(mdl)
  dev.off()

  pred <- predict(mdl,test_set[,train],ncomp=n_components)
  print("MSE: ")
  print(mean((pred - test_set[,test])^2))

  print("R squared: ")
  print(cor(pred, test_set[,test])^2)

  png(file=paste("/home/olga/Projects/HW_R/computer_statistics_ms/lab2/plots/test_",name,".png"),width = 1000,height = 800)
  plot(pred, test_set[,test])
  dev.off()
}



perform_pca(train_full, "full")
perform_pcr(train_full, "full", 6)

perform_pca(train_last, "last")
perform_pcr(train_last, "last", 7)
perform_pcr(train_last, "last", 6)
