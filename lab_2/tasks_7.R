export XMODIFIERS=
export GTK_IM_MODULE=
    
library(graphics)

#                   _           __                  _   _                 
#   _ __ ___   __ _| | _____   / _|_   _ _ __   ___| |_(_) ___  _ __  ___ 
#  | '_ ` _ \ / _` | |/ / _ \ | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
#  | | | | | | (_| |   <  __/ |  _| |_| | | | | (__| |_| | (_) | | | \__ \
#  |_| |_| |_|\__,_|_|\_\___| |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
#                                                                         
#  

log_max_min <- function(path_to_dataset){
  # read the data and set names for columns
  df = read.csv(path_to_dataset, header = FALSE)
  names(df) <- c("dat", "z", "opn", "mx", "mn", "clo", "vol")
  
  # calculate
  log_max_minn   <- log(df$mx / df$mn) # log max/min
  
  # return
  log_max_minn
}

log_opn_clo <- function(path_to_dataset){
  # read the data and set names for columns
  df = read.csv(path_to_dataset, header = FALSE)
  names(df) <- c("dat", "z", "opn", "mx", "mn", "clo", "vol")
  
  # calculate
  log_opn_close <- log(df$opn / df$clo) # log opn/clo
  
  # return
  log_opn_close
}

log_return <- function(path_to_dataset){
  # read the data and set names for columns
  df = read.csv(path_to_dataset, header = FALSE)
  names(df) <- c("dat", "z", "opn", "mx", "mn", "clo", "vol")
  
  # calculate
  log_returnn <- log(df$clo[-nrow(df)]/df$clo[-1]) # log return
  
  # return
  log_returnn
}

#             _                  _         _       _        
#    _____  _| |_ _ __ __ _  ___| |_    __| | __ _| |_ __ _ 
#   / _ \ \/ / __| '__/ _` |/ __| __|  / _` |/ _` | __/ _` |
#  |  __/>  <| |_| | | (_| | (__| |_  | (_| | (_| | || (_| |
#   \___/_/\_\\__|_|  \__,_|\___|\__|  \__,_|\__,_|\__\__,_|
#                                                           
#  

tables    <- list.files(path = "~/Projects/domashki-R/lab_2_ds",full.names = TRUE)
companies <- list.files(path = "~/Projects/domashki-R/lab_2_ds",full.names = FALSE)

tables <- tables[1:20]
compnies <- companies[1:20]
companies <- c()

for (i in (1:20)){
  companies <- append(companies,substr(compnies[i], 7, nchar(compnies[i])-4))
}

mx_mn <- c()
op_cl <- c()
lo_re <- c()

cmp1 <- c()
cmp2 <- c()

for (i in (1:length(companies))){
  #print(tables[i])
  tbl <- tables[i]
  
  mm <- log_max_min(tbl)
  lr <- log_return(tbl)
  
  mx_mn <- append(mx_mn, mm) 
  op_cl <- append(op_cl, log_opn_clo(tbl))
  lo_re <- append(lo_re, lr)
  
  hist(mx_mn, )
  
  for(j in (1:length(mm))){cmp1 <- append(cmp1,c(companies[i]))}
  for(j in (1:length(lr))){cmp2 <- append(cmp2,c(companies[i]))}
}

df_mm_oc = data.frame(
  max_min = mx_mn,
  opn_clo = op_cl,
  names = cmp1
)

df_lr = data.frame(
  log_ret = lo_re,
  names = cmp2
)

#   _                     _       _   
#  | |__   _____  ___ __ | | ___ | |_ 
#  | '_ \ / _ \ \/ / '_ \| |/ _ \| __|
#  | |_) | (_) >  <| |_) | | (_) | |_ 
#  |_.__/ \___/_/\_\ .__/|_|\___/ \__|
#                  |_|                
#  

boxplot(max_min~names, data=df_mm_oc)
boxplot(op_cl~names, data=df_mm_oc)
boxplot(log_ret~names, data=df_lr)

boxplot(max_min~names, data=df_mm_oc, outline = F)
boxplot(op_cl~names, data=df_mm_oc, outline = F)
boxplot(log_ret~names, data=df_lr, outline = F)

#   _     _     _                                  
#  | |__ (_)___| |_ ___   __ _ _ __ __ _ _ __ ___  
#  | '_ \| / __| __/ _ \ / _` | '__/ _` | '_ ` _ \ 
#  | | | | \__ \ || (_) | (_| | | | (_| | | | | | |
#  |_| |_|_|___/\__\___/ \__, |_|  \__,_|_| |_| |_|
#                        |___/                     
#  

hist(, probability=TRUE, add=TRUE)









