# Title     : lab 5
# Objective : v10
# Created by: olga
# Created on: 24.05.21

# libararies
library(corrplot)
library(dplyr)

# read data
df <- read.csv2("/home/olga/Projects/HW_R/asymptotic_statistics/data/wine.csv", header=T)
df <- df[df$Site!=3,]
df$Site <- df$Site - 1
attach(df)

# split X, Y
Y <- df$Site
X <- select(df, Magnesium, phenols, Flavanoids, NF)

# split train & test sets
n.train <- round(length(df$Site)*0.85)
idx.train <- sample(seq_along(df$Site), size = n.train)

X.train <- X[idx.train,]
Y.train <- Y[idx.train]

X.test <- X[-idx.train,]
Y.test <- Y[-idx.train]

# plot correlation and scatter
labels <- c("Site", "Magnesium", "Phenols", "Flavanoids", "NF")
pairs(Y.train ~ X.train[,"Magnesium"] + X.train[,"phenols"] + X.train[,"Flavanoids"] + X.train[,"NF"],
      col = ifelse(Y.train == 0,'red','blue'), pch = 19,
      labels = labels)

pearson_corr  <- cor(df[idx.train,c("Site", "Magnesium", "phenols", "Flavanoids", "NF")], method="pearson")
corrplot(pearson_corr,
         method = "number",
         title="Pearson correlation",
         tl.srt = 45,
         tl.cex = .7, tl.col = "blue",
         # order = "hclust", addrect=3,
         mar=c(0,0,1,0))

# a bit of automatization
classif.result <- function(y, y.hat)
{
  cf.m <- table(y, y.hat)

  list(
    cf.table = cf.m,
    dat.precision = cf.m[1,1]/(cf.m[1,1] + cf.m[1,2]),
    dat.recall = cf.m[1,1]/(cf.m[1,1] + cf.m[2,1])
  )
}

summary.glm <- function (glm_res){
  print(summary(glm_res))

  print(confint(glm_res))

  print("Train")
  print(classif.result(Y.train, (fitted.values(glm_res) > 0.5)*1))

  print("Test")
  prediction <- predict.glm(glm_res, X.test, type="response") > 0.5
  print(classif.result(Y.test, prediction*1))
}

# train some models
summary.glm(glm(Y.train ~ Magnesium + phenols + Flavanoids + NF,
                data = X.train,
                family = binomial()))

summary.glm(glm(Y.train ~ Magnesium + phenols + NF,
                data = X.train,
                family = binomial()))

summary.glm(glm(Y.train ~ Magnesium + Flavanoids + NF,
                data = X.train,
                family = binomial()))