## 1. Load Data
setwd("~/Dropbox/School/1/ST215/lab0/")
data = read.table("stateCoord.txt")
require("ggplot2")
data(USArrests)
USArrests$long = data$long
USArrests$lad = data$lad
rm(data)

## 2. 
rownames(USArrests)[USArrests$Rape>40 & UrbanPop<50]
