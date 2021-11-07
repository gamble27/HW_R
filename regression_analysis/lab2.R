library(matlib)

# read the data
df = read.table(file = "~/Projects/HW_R/regression_analysis/data/project.txt",header=T, sep = '\t')
head(df)

# calculate sleep hrs
hrs = df$full_hours + df$minutes/60

# scatter plot
plot(hrs, df$avg_pulse)

# drop zeros
x1 = hrs[hrs>0]
y1 = df$avg_pulse[hrs>0]
y2 = df$min_pulse[hrs>0]
y3 = df$max_pulse[hrs>0]
  
# scatter plots
plot(x1,y1,main="AVG pulse vs sleep hrs")
plot(x1,y2,main="MIN pulse vs sleep hrs")
plot(x1,y3,main="MAX pulse vs sleep hrs")

# drop trainings and check scatter plots
cnd = which(df$training[hrs>0] == 0)

plot(x1[cnd],y1[cnd],main="AVG pulse vs sleep hrs (no training)")
plot(x1[cnd],y2[cnd],main="MIN pulse vs sleep hrs (no training)")
plot(x1[cnd],y3[cnd],main="MAX pulse vs sleep hrs (no training)")

# check scatter plots for training
cnd1 = which(df$training[hrs>0] > 0)

plot(x1[cnd1],y1[cnd1],main="AVG pulse vs sleep hrs (training)")
plot(x1[cnd1],y2[cnd1],main="MIN pulse vs sleep hrs (training)")
plot(x1[cnd1],y3[cnd1],main="MAX pulse vs sleep hrs (training)")

# some templates
confint.lect <- function(x, y, level = 0.95){
  model = lm(y~x)
  
  n_d = (length(x)-2)
  #sigma = c( ((y-sum(model$coefficients[1]*x))^2 / n_d)^0.5, # x
   #           ((y-sum(model$coefficients[2]*rep(1,length(x))))^2 / n_d)^0.5 # intercept
    #         )
  sigma = rep((sum(model$residuals^2)/n_d)^0.5,2)
  xx = as.matrix(data.frame(x,rep(1,length(x))))
  a = inv(t(xx)%*%xx)
  alpha = 1 - level
  t = qt(1-alpha/2, df=n_d)
  
  ci.int = t*sigma[2]*a[2,2]^0.5
  ci.x   = t*sigma[1]*a[1,1]^0.5
  
  data.frame(lower = c(model$coefficients[1]-ci.int,model$coefficients[2]-ci.x), 
             upper = c(model$coefficients[1]+ci.int,model$coefficients[2]+ci.x),
             row.names = c("(Intercept)", "x")
             )
}

get.total.summary <- function(x, y, lvl = 0.95){
  # model
  model = lm(y~x)
  
  # summary
  print(summary(model))
  
  # histogram of residuals and other plots
  hist(model$residuals)
  plot(model)
  
  # confidence intervals
  print(confint.lm(model, level = lvl))
  print(confint.lect(x, y, level = lvl))
  
  # headshot control test
  chisq.test(x,y)
}

# create linear model MAX pulse ~ sleep hrs
get.total.summary(x1,y3)
get.total.summary(x1[cnd], y3[cnd])
