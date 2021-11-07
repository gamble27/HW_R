# Title     : lab4
# Objective : v10
# Created by: olga
# Created on: 25.05.21

df <- read.table("/home/olga/Projects/HW_R/asymptotic_statistics/data/D10.txt", header = T)

x <- df[,"x"]
y <- df[,"y"]

# model 1
plot(x,y)
xtick<-seq(0, 10, by=0.5)
axis(side=1, at=xtick, labels = FALSE)
ytick<-seq(-100, 10, by=0.5)
axis(side=2, at=ytick, labels = FALSE)
grid()


b0 <- 3.14159
a0 <- log(2)/2

model<-nls(y~exp(a*x)*cos(b*x), start=list(a=a0, b=b0))
summary(model)

a<-coef(model)["a"]
b<-coef(model)["b"]
curve(exp(a*x)*cos(b*x), col="red", lwd=1.5, add=T)

confint(model, level=0.95)

u<-residuals(model)
pr_y<-fitted(model)
plot(y,pr_y,cex=0.5,xlab="Response",ylab="Prediction")
abline(0,1,col="red")
plot(pr_y,u,xlab="Prediction",ylab="Residuals")
qqnorm(u,col="blue",cex=0.2,main="Residuals normal QQ-diagram")
qqline(u,col="red")

# model 2
plot(x,y)
xtick<-seq(0, 10, by=0.5)
axis(side=1, at=xtick, labels = FALSE)
ytick<-seq(-100, 10, by=0.5)
axis(side=2, at=ytick, labels = FALSE)
grid()


b0 <- 3.14159
a0 <- log(2)/2

model<-nls(y~exp(a*x)*sin(b*x), start=list(a=a0, b=b0))
summary(model)

a<-coef(model)["a"]
b<-coef(model)["b"]
curve(exp(a*x)*sin(b*x), col="red", lwd=1.5, add=T)

confint(model, level=0.95)

u<-residuals(model)
pr_y<-fitted(model)
plot(y,pr_y,cex=0.5,xlab="Response",ylab="Prediction")
abline(0,1,col="red")
plot(pr_y,u,xlab="Prediction",ylab="Residuals")
qqnorm(u,col="blue",cex=0.2,main="Residuals normal QQ-diagram")
qqline(u,col="red")
