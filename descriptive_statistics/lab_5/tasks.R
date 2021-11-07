# Title     : tasks
# Objective : Principal Components Analysis
# Created by: olga
# Created on: 10.11.19

library(rgl)

#  _       _       ____
# | | __ _| |__   | ___|
# | |/ _` | '_ \  |___ \
# | | (_| | |_) |  ___) |
# |_|\__,_|_.__/  |____/
#


# read file
df <- read.table("/home/olga/Projects/HW_R/lab_5_ds/F10s.txt", header=F)

# pairs scatter plot
png("pairs.png", width = 1000, height = 1000)
pairs(df, cex = 0.1, col="seagreen")
dev.off()

# principal components
principals <- princomp(df)
plot(principals)
print(summary(principals))

# plot some components in 2D
# to detect a structure
plot(principals$scores[,1:2], main = "Components 1 and 2", col="blue")
plot(principals$scores[,2:3], main = "Components 2 and 3", col="blue")

plot(principals$scores[,3:4], main = "Components 3 and 4", col="purple")
plot(principals$scores[, c(1,4)], main = "Components 1 and 4", col="purple")

# plot components in 3D
# plot3d(principals$scores[, 1:3], col="blue", size=20)  # pls make it fullscreen on 4k :]
# plot3d(principals$scores[, c(1,3,4)], col="blue", size=20)

#  _       _        __    ___
# | | __ _| |__    / /_  |__ \
# | |/ _` | '_ \  | '_ \   / /
# | | (_| | |_) | | (_) | |_|
# |_|\__,_|_.__/   \___/  (_)
#

# try this for clustering
df.dist<-dist(principals$scores[,4:5])
df.hclust<-hclust(df.dist,method ="single")
plot(as.dendrogram(df.hclust),leaflab="none")

groups2<-cutree(df.hclust,k=2)
plot(principals$scores[,c(4,5)],col=c("red","blue")[groups2])
plot3d(principals$scores[, 3:5], col=c("red","blue")[groups2], size=20)  # pls make it fullscreen on 4k :]

# so, lets check for 4,5 components
dist2 <- ((principals$scores[,4])^2 + (principals$scores[,5])^2)^(0.5)
hist(dist2) # here we will see clusters

# now we form clusters using histogram
clusters<-(dist2<3)+1

# finally visualize
plot3d(principals$scores[, 3:5], col=c("red","blue")[clusters], size=20)

