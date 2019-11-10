# Title     : tasks
# Objective : correlation analysis
# Created by: olga
# Created on: 08.11.19

# include libraries
library(corrplot)
library(qgraph)


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
df_lr <- log_return(ds_path, 30, 49)

#########################################################
#                          _       _   _                #
#                         | |     | | (_)               #
#   ___ ___  _ __ _ __ ___| | __ _| |_ _  ___  _ __     #
#  / __/ _ \| '__| '__/ _ \ |/ _` | __| |/ _ \| '_ \    #
# | (_| (_) | |  | | |  __/ | (_| | |_| | (_) | | | |   #
#  \___\___/|_|  |_|  \___|_|\__,_|\__|_|\___/|_| |_|   #
#                                                       #
#########################################################

#                     _      _   _                       _       _
#  __ ___ _ _ _ _ ___| |__ _| |_(_)___ _ _    _ __  __ _| |_ _ _(_)_ __
# / _/ _ \ '_| '_/ -_) / _` |  _| / _ \ ' \  | '  \/ _` |  _| '_| \ \ /
# \__\___/_| |_| \___|_\__,_|\__|_\___/_||_| |_|_|_\__,_|\__|_| |_/_\_\

# calculate correlation matrix
pearson_corr  <- cor(df_cl[, -1], method="pearson") # without date column
kendall_corr  <- cor(df_cl[, -1], method="kendall")
spearman_corr <- cor(df_cl[, -1], method="spearman")

# print in the numeric view
print(pearson_corr)
# print(kendall_corr)
# print(spearman_corr)

#                     _      _   _
#  __ ___ _ _ _ _ ___| |__ _| |_(_)___ _ _    _ __  __ _ _ __
# / _/ _ \ '_| '_/ -_) / _` |  _| / _ \ ' \  | '  \/ _` | '_ \
# \__\___/_| |_| \___|_\__,_|\__|_\___/_||_| |_|_|_\__,_| .__/
#                                                       |_|

# specify color palette
palette1 <- colorRampPalette(c("khaki4", "khaki1", "white", "darkviolet", "darkmagenta"))
palette2 <- colorRampPalette(c("cyan", "white", "blue"))

# mixed correlation map
corrplot.mixed(pearson_corr,
               title="Close price Pearson correlation map",
               diag="n",
               number.cex = .5, lower.col = "purple",
               tl.cex = .7, tl.col = "blue",
               order = "hclust", addrect=2,
               mar=c(0,0,1,0))
corrplot.mixed(kendall_corr,
               title="Close price Kendall correlation map",
               diag="n",
               number.cex = .5, lower.col = "purple",
               tl.cex = .7, tl.col = "blue",
               order = "hclust", addrect=2,
               upper.col = palette1(100),
               mar=c(0,0,1,0))
corrplot.mixed(spearman_corr,
               title="Close price Spearman correlation map",
               diag="n",
               number.cex = .5, lower.col = "purple",
               tl.cex = .7, tl.col = "blue",
               order = "hclust", addrect=2,
               upper.col = palette2(100),
               mar=c(0,0,1,0))

# correlation map with clusters
corrplot(pearson_corr,
        title="Close price Pearson clusters",
        tl.srt = 45,
        tl.cex = .7, tl.col = "blue",
        order = "hclust", addrect=3,
        mar=c(0,0,1,0))
corrplot(kendall_corr,
        title="Close price Kendall clusters",
        tl.srt = 45,
        tl.cex = .7, tl.col = "blue",
        order = "hclust", addrect=6,
        col = palette1(100),
        mar = c(0,0,1,0))
corrplot(spearman_corr,
        title="Close price Spearman clusters",
        tl.srt = 45,
        tl.cex = .7, tl.col = "blue",
        order = "hclust", addrect=4,
        col = palette2(100),
        mar = c(0,0,1,0))

########################################################
#                _   _                    _       _    #
#  ___  ___ __ _| |_| |_ ___ _ __   _ __ | | ___ | |_  #
# / __|/ __/ _` | __| __/ _ \ '__| | '_ \| |/ _ \| __| #
# \__ \ (_| (_| | |_| ||  __/ |    | |_) | | (_) | |_  #
# |___/\___\__,_|\__|\__\___|_|    | .__/|_|\___/ \__| #
#                                  |_|                 #
#                                                      #
########################################################

# from the same cluster
pairs(df_cl[,c("aph", "amt", "arg")], main="Same cluster", col="blue")

# from different clusters
pairs(df_cl[,c("anf", "amp", "amzn")], main="Different clusters", col="purple")

# log return scatter plots
pairs(df_lr[,c("aph", "amt", "arg")], main="Same cluster - log return", col="deeppink2")
pairs(df_lr[,c("anf", "amp", "amzn")], main="Different clusters - log return", col="olivedrab3")

############################################################
#                          _            _   _              #
#   ___ ___  _ __ _ __ ___| | ___  __ _| |_(_) ___  _ __   #
#  / __/ _ \| '__| '__/ _ \ |/ _ \/ _` | __| |/ _ \| '_ \  #
# | (_| (_) | |  | | |  __/ |  __/ (_| | |_| | (_) | | | | #
#  \___\___/|_|  |_|  \___|_|\___|\__,_|\__|_|\___/|_| |_| #
#                                                          #
#                        _                                 #
#   __ _ _ __ __ _ _ __ | |__                              #
#  / _` | '__/ _` | '_ \| '_ \                             #
# | (_| | | | (_| | |_) | | | |                            #
#  \__, |_|  \__,_| .__/|_| |_|                            #
#  |___/          |_|                                      #
#                                                          #
############################################################

qgraph(pearson_corr, layout="spring", threshold=0.5, labels=colnames(pearson_corr), title="Pearson correlation graph")
qgraph(kendall_corr, layout="spring", threshold=0.5, labels=colnames(pearson_corr), title="Kendall correlation graph")
qgraph(spearman_corr, layout="spring", threshold=0.5, labels=colnames(pearson_corr), title="Spearman correlation graph")
