# Title     : lab 5
# Objective : anova
# Created by: olga
# Created on: 29.05.21

library(plotrix)
library(car)

df <- read.table("/home/olga/Projects/HW_R/regression_analysis/data/kaffee.txt", header=T)
df <- df[c("dauer","treue")]

boxplot(dauer~treue, data=df, outline = F)

res <- aov(dauer~treue, data=df)
summary(res)

alpha0 <- 0.05
alpha  <- 1 - (1 - alpha0)^(1/2)
l <- tapply(df$dauer, df$treue, length)
m <- tapply(df$dauer, df$treue, mean)
s <- tapply(df$dauer, df$treue, sd)
tf <- qt(1-alpha/2, l-1)
h <- s * tf / sqrt(l)

plotCI(1:2, y=m, uiw=h,
       xlab=" ", ylab="means",
       xlim=c(0.9,2.2), xaxt="n")
axis(1, at=1:2, labels=c("1","2"))

# чекнем нормальність щоб робити тест Левена
hist(df$dauer[df$treue==1])
hist(df$dauer[df$treue==2])
hist(log(df$dauer[df$treue==2]))
hist(log(df$dauer[df$treue==1]))

# ну значить зробимо тест для логарифму
df$log_dauer <- log(df$dauer)
leveneTest(df$log_dauer,df$treue)

# інтервали теж побудуємо для логарифму
alpha0 <- 0.05
alpha  <- 1 - (1 - alpha0)^(1/2)
l <- tapply(df$log_dauer, df$treue, length)
s <- tapply(df$log_dauer, df$treue, sd)
v <- tapply(df$log_dauer, df$treue, var)
xi.plus  <- qchisq(alpha/2, l-1)
xi.minus <- qchisq(1-alpha/2, l-1)
h.plus  <- s^2 * (l-1) / xi.plus
h.minus <- s^2 * (l-1) / xi.minus

plotCI(1:2, y=v, uiw=h.plus, liw = h.minus,
       xlab=" ", ylab="var",
       xlim=c(0.9,2.2), xaxt="n")
axis(1, at=1:2, labels=c("1","2"))

