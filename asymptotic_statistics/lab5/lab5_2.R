library("corrplot")

classif.result <- function(y, y.hat)
{
  cf.m <- table(y, y.hat)
  
  list(
    cf.table = cf.m,
    dat.precision = cf.m[1,1]/(cf.m[1,1] + cf.m[1,2]),
    dat.recall = cf.m[1,1]/(cf.m[1,1] + cf.m[2,1])
  )
}

set.seed(0)

path.to.wine <- "C:/Users/dango/OneDrive/Documents/R/regr/wine.csv"
wine.data <- read.csv2(file=path.to.wine)
columns.to.use <- c("Site", "Alcogol", "Ash", "Proline", "Malic_acid")
wine.data <- wine.data[wine.data$Site != 3, columns.to.use]

# red - 1, blue - 0
X <- wine.data[,-1]
Y <- wine.data[,1] - 1

# Stratified sampling

N <- nrow(X)
n.0 <- 50

idx <- 1:N
idx.0 <- sample((1:N)[Y==0], size=n.0)
idx.1 <- sample((1:N)[Y==1], size=n.0)
idx.train <- c(idx.0, idx.1)

X.train <- X[idx.train, ]
Y.train <- Y[idx.train]

plot(X, col=ifelse(Y, 'red', 'blue'), cex=0.75)

corrplot(cor(X.train), method="color", addCoef.col = "black")
corrplot(cor(X.train, method="spearman"), 
         method="color", addCoef.col = "black")

# Model #1

print("Model #1")

model.full <- glm(Y.train ~ Alcogol + Ash + Proline + Malic_acid, data=X.train,
                  family="binomial")
print(summary(model.full))

print("Train")
print(classif.result(Y[idx.train], (fitted.values(model.full) > 0.5)*1))
print("Test")
Y.hat.full <- predict(model.full, X[-idx.train,], type="response") > 0.5
print(classif.result(Y[-idx.train], Y.hat.full*1))

# Model #2

print("Model #2")

model.red.1 <- glm(Y.train ~ Alcogol + Proline, data=X.train, 
                   family='binomial')

print(
  summary(model.red.1)
)

print("Train")
print(classif.result(Y[idx.train], (fitted.values(model.red.1) > 0.5)*1))
print("Test")
Y.hat.red.1 <- predict(model.red.1, X[-idx.train,], type="response") > 0.5
print(classif.result(Y[-idx.train], Y.hat.red.1*1))

print(confint(model.red.1))

# Model #3

print("Model #3")

model.red.2 <- glm(Y.train ~ Proline, data=X.train, 
                   family='binomial')

print(
  summary(model.red.2)
)

print("Train")
print(classif.result(Y[idx.train], (fitted.values(model.red.2) > 0.5)*1))
print("Test")
Y.hat.red.2 <- predict(model.red.2, X[-idx.train,], type="response") > 0.5
print(classif.result(Y[-idx.train], Y.hat.red.2*1))

print(confint(model.red.2))