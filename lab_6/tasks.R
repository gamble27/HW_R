# Title     : tasks
# Objective : Cluster analysis continued
# Created by: olga
# Created on: 20.11.19

library(rgl)

# configure working directory
# setwd( dirname(sys.frame(1)$ofile) )

##############################################################
#            _                  _         _       _          #
#   _____  _| |_ _ __ __ _  ___| |_    __| | __ _| |_ __ _   #
#  / _ \ \/ / __| '__/ _` |/ __| __|  / _` |/ _` | __/ _` |  #
# |  __/>  <| |_| | | (_| | (__| |_  | (_| | (_| | || (_| |  #
#  \___/_/\_\\__|_|  \__,_|\___|\__|  \__,_|\__,_|\__\__,_|  #
#                                                            #
##############################################################

#             _                                         _      _
#  _ __  __ _| |_____   __ _ _ __ _ __ _ _ ___ _ __ _ _(_)__ _| |_ ___
# | '  \/ _` | / / -_) / _` | '_ \ '_ \ '_/ _ \ '_ \ '_| / _` |  _/ -_)
# |_|_|_\__,_|_\_\___| \__,_| .__/ .__/_| \___/ .__/_| |_\__,_|\__\___|
#                           |_|  |_|          |_|
#   __              _   _
#  / _|_  _ _ _  __| |_(_)___ _ _  ___
# |  _| || | ' \/ _|  _| / _ \ ' \(_-<
# |_|  \_,_|_||_\__|\__|_\___/_||_/__/

close_price <- function(path_to_dataset, start=1,stop=20){
    tables    <- list.files(path = path_to_dataset,full.names = TRUE)
    # companies <- list.files(path = "~/Projects/HW_R/lab_2_ds",full.names = FALSE)

    # we will only work with slice
    tables    <- tables   [start:stop]
    # companies <- companies[start:stop]

    # names(df) <- c("dat", "z", "opn", "mx", "mn", "clo", "vol")

    datalist = lapply(tables, function(x){
        x0<-read.csv(file=x,header=F)[,c(1,6)]; # the 6-th field is the desired clo
        colnames(x0)<-c("date", unlist(strsplit(x,"[_.]"))[5]);
        x0})
    clo <-Reduce(function(x,y) {merge(x,y,by="date")}, datalist)

    return(clo)
}

log_return <- function(path_to_dataset, start=1, stop=20){
    tables    <- list.files(path = path_to_dataset,full.names = TRUE)

    # we will only work with slice
    tables    <- tables   [start:stop]

    # names(df) <- c("dat", "z", "opn", "mx", "mn", "clo", "vol")

    datalist = lapply(tables, function(x){
        x0 <- read.csv(file=x,header=F)[,c(1,6)]; # the 6-th field is the desired clo
        col1_name <- unlist(strsplit(x,"[_.]"))[5]
        colnames(x0) <- c("date", col1_name);
        col1 <- log(x0[,col1_name][-nrow(x0)]/x0[,col1_name][-1])
        date_1 <- x0$date[2:length(x0$date)]
        x1 <- data.frame(date = date_1, col1_name = col1)
        colnames(x1) <- c("date", col1_name)
        x1})
    log_returnn <- Reduce(function(x,y) {merge(x,y,by="date")}, datalist)

    # return
    return(log_returnn)
}

#             _         _           _
#  _ __  __ _| |_____  | |_ __ _ __| |__
# | '  \/ _` | / / -_) |  _/ _` (_-< / /
# |_|_|_\__,_|_\_\___|  \__\__,_/__/_\_\
#
#     _      _         __
#  __| |__ _| |_ __ _ / _|_ _ __ _ _ __  ___ ___
# / _` / _` |  _/ _` |  _| '_/ _` | '  \/ -_|_-<
# \__,_\__,_|\__\__,_|_| |_| \__,_|_|_|_\___/__/

# csv files and company names
ds_path <- "~/Projects/HW_R/lab_2_ds"
df_cl <- close_price(ds_path, 30, 49)
# df_lr <- log_return(ds_path, 30, 49)

#########################################################################
#       _           _                                _           _      #
#   ___| |_   _ ___| |_ ___ _ __    __ _ _ __   __ _| |_   _ ___(_)___  #
#  / __| | | | / __| __/ _ \ '__|  / _` | '_ \ / _` | | | | / __| / __| #
# | (__| | |_| \__ \ ||  __/ |    | (_| | | | | (_| | | |_| \__ \ \__ \ #
#  \___|_|\__,_|___/\__\___|_|     \__,_|_| |_|\__,_|_|\__, |___/_|___/ #
#                                                      |___/            #
#                                                                       #
#########################################################################

# pick principals
cl_principals <- princomp(df_cl)
plot(cl_principals)
print(summary(cl_principals))

# form clusters
df_cl.dist <- dist(cl_principals$scores[,1:3])  # , method = "manhattan")
df_cl.hclust <- hclust(df_cl.dist)  # , method ="single")
plot(as.dendrogram(df_cl.hclust), leaflab="none")

# visualize
groups3 <- cutree(df_cl.hclust,k=3)
plot(cl_principals$scores[,c(1,3)],col=c("red","blue","seagreen")[groups3])
plot3d(cl_principals$scores[, 1:3], col=c("red","blue","seagreen")[groups3], size=20)  # pls make it fullscreen on 4k :]
