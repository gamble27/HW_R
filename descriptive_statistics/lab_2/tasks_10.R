# firms ace, acn, act
# lr

library(graphics)

# extraction function definition
log_max_min <- function(path_to_dataset){
  # read the data and set names for columns
  df = read.csv(path_to_dataset, header = FALSE)
  names(df) <- c("dat", "z", "opn", "mx", "mn", "clo", "vol")

  # calculate
  log_max_minn   <- log(df$mx / df$mn) # log max/min

  # return
  log_max_minn
}
# tables list
tables = c("/home/olga/Projects/HW_R/lab_2_ds/table_ace.csv",
           "/home/olga/Projects/HW_R/lab_2_ds/table_acn.csv",
           '/home/olga/Projects/HW_R/lab_2_ds/table_act.csv')

# collecting data
lr_ace = log_max_min(tables[1])
lr_acn = log_max_min(tables[2])
lr_act = log_max_min(tables[3])

# plotting results
hist(lr_ace, col=rgb(1,0,0,1/2), probability=TRUE, ylim=c(0,25))
hist(lr_acn, col=rgb(0,1,0,1/4), probability=TRUE, add = TRUE)
hist(lr_act, col=rgb(0,0,1,1/4), probability=TRUE, add = TRUE)

# add legend
legend("topright", c("ace", "acn", "act"), col=c(rgb(1,0,0,1/2), rgb(0,1,0,1/4), rgb(0,0,1,1/4)), lwd=10)
