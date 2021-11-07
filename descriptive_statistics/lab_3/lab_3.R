library('sp')
library('maptools')


# read world data
setwd("~/Projects/HW_R/lab_3/")
df <- read.csv("geoMap_fixed.csv")
names(df) <- c("index","country", "beer", "wine")

# map settings
data(wrld_simpl)
par(mai=c(0,0,0,0))
par(mar=c(0,0,0,0))

# base world map
plot(wrld_simpl,
     bg='azure2', 
     col='grey',
     border='black')

# plot all countries,
# coloured by preference
for (country_ in df$country){
    if (as.numeric(df[df$country == country_,]$beer) > as.numeric(df[df$country==country_,]$wine)){
    color <- "blue" # beer
    }else{
    color <- "red"  # wine
    }
    number = wrld_simpl[wrld_simpl$NAME == country_,]$ISO2
    plot(wrld_simpl[number,], col=c(color), add=T)
}
