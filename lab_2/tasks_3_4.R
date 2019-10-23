export XMODIFIERS=
export GTK_IM_MODULE=
  
library(graphics)

#   _            _      _____ 
#  | |_ __ _ ___| | __ |___ / 
#  | __/ _` / __| |/ /   |_ \ 
#  | || (_| \__ \   <   ___) |
#   \__\__,_|___/_|\_\ |____/ 
#                             
#  

task_3 <- function(path_to_dataset){
  # read the data and set names for columns
  df = read.csv(path_to_dataset, header = FALSE)
  names(df) <- c("dat", "z", "opn", "mx", "mn", "clo", "vol")
  
  # make calculations for task 3
  log_return <- log(df$clo[-nrow(df)]/df$clo[-1]) # log return
  log_max_min   <- log(df$mx / df$mn) # log max/min
  log_opn_clo <- log(df$opn / df$clo) # log opn/clo
  
  # write results to new .csv file
  slice <- substr(path_to_dataset, 1, nchar(path_to_dataset)-4)
  # print(nchar(path_to_dataset)-4)
  
  df <- data.frame(log_max_min, log_opn_clo)
  names(df) <- c("log_max_min", "log_opn_clo")
  write.csv(df, file = paste(slice,"_task_3_mm_oc.csv",sep=""))
  
  df <- data.frame(log_return)
  names(df) <- c("log_return")
  write.csv(df, file = paste(slice,"_task_3_lr.csv",sep=""))
}

# datasets settings
directory <- "~/Projects/domashki-R/lab_2/"
tables = c("table_amzn.csv","table_ibm.csv","table_aapl.csv")  

# call task 3
for (name in tables){
  task_3(paste(directory,name, sep=""))
}

#   _            _      _  _      ___ ____  __  __ 
#  | |_ __ _ ___| | __ | || | _  |_ _| __ )|  \/  |
#  | __/ _` / __| |/ / | || |(_)  | ||  _ \| |\/| |
#  | || (_| \__ \   <  |__   _|   | || |_) | |  | |
#   \__\__,_|___/_|\_\    |_|(_) |___|____/|_|  |_|
#                                                  
#   

df_ibm_mm_oc <- read.csv("~/Projects/domashki-R/lab_2/table_ibm_task_3_mm_oc.csv", header = TRUE)
df_ibm_lr <- read.csv("~/Projects/domashki-R/lab_2/table_ibm_task_3_lr.csv", header = TRUE)

# make plot in R

# outline: normal
boxplot(df_ibm_mm_oc$log_max_min, df_ibm_mm_oc$log_opn_clo, df_ibm_lr$log_return, names = c("max/min", "opn/clo", "return"))
# outline: F
boxplot(df_ibm_mm_oc$log_max_min, df_ibm_mm_oc$log_opn_clo, df_ibm_lr$log_return, names = c("max/min", "opn/clo", "return"), outline = F)

# calculate metrics for plotting on scratch

# max/min
print("max/min:")
sprintf("med: %f", median(df_ibm_mm_oc$log_max_min))
sprintf("mean: %f", mean(df_ibm_mm_oc$log_max_min))
print("quartiles:")
print(quantile(df_ibm_mm_oc$log_max_min, c(.09, .25, .5, .75, .91)))
# opn/clo
print("opn/clo:")
sprintf("med: %f", median(df_ibm_mm_oc$log_opn_clo))
sprintf("mean: %f", mean(df_ibm_mm_oc$log_opn_clo))
print("quartiles:")
print(quantile(df_ibm_mm_oc$log_opn_clo, c(.09, .25, .5, .75, .91)))
# log return
print("log return:")
sprintf("med: %f", median(df_ibm_lr$log_return))
sprintf("mean: %f", mean(df_ibm_lr$log_return))
print("quartiles:")
print(quantile(df_ibm_lr$log_return, c(.09, .25, .5, .75, .91)))
