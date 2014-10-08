setwd("~/Dropbox/School/ST215/Lab/lab1/")
source("solution.R")
require(graphics)
library(splines)
library(MASS)
# Get data from solution of Lab1, and remove all all variable except 
# our dataset
l = ls()
l = l[l!="all"]
rm(list=l)
rm(l)

# 1. Same Gaussian Kernel, Different Bandwidth
# Ask question. How handle legend. Multiple plots
plot1 = ggplot(data=all, aes(x=humid_temp)) +
  geom_density(adjust = 1, color = "blue") +
  geom_density(adjust = 2, color = "green") +
  geom_density(adjust = 3, color = "yellow") +
  geom_density(adjust = 4, color = "orange") +
  geom_density(adjust = 5, color = "red") +
  ggtitle("Density Plot - Gaussian Kernel - Various BW") +
  xlab("Temperature") + ylab("Density") 


png(filename="density_bandwidth2.png")

bws = c(0,1, 0.25, 0.5, 1,2,4,6)
plot(density(all$humid_temp, bw =.1), xlab = "Temperature", 
        main = "Gaussian Density with Different Bandwidth")
for (i in 2:length(bws))
  lines(density(all$humid_temp, bw = bws[i]), col = i)
legend(1.5,4, legend = bws, col = seq(bws))
grid()
dev.off()

# 2. Different Kernel. 
png(filename="density_kernels01.png")
kernels = eval(formals(density.default)$kernel)
plot(density(all$humid_temp, bw = 0.1), xlab = "", 
  main = "Density of Temperature with different Kernels")
for (i in 2:length(kernels))
  lines(density(all$humid_temp,bw = 0.1, kernel = kernels[i]),col = i)
legend(1.5, .4, legend = kernels, col = seq(kernels), lty = 1,
    cex = .8, y.intersp = 1)
grid()
dev.off()

# 3. Pick a time of a day
epoch.To.Time = function(ep)
{
  e = ep %% 288
  hr = floor(ep/12)
  mn = 5*(e-12*hr)
  return(c(hr,mn))
}
n = 2
hr.mn = epoch.To.Time(n)
#png(filename="Temp_Humid_midnight.png")
# 3.1. Comparing bandwidth in LOESS plot
# First lower bandwidth
plot1 = ggplot(data = all[all$epoch %% 288 == n,], 
  aes(x = humidity, y =humid_temp)) + 
  geom_point(color = 'red')+ 
  stat_smooth(method = "loess", span = 0.5)+ 
  ggtitle(paste("Temperature vs. Humidity at ",
  toString(hr.mn[1]), ":", toString(hr.mn[2]), " LOESS Bandwidth 0.5")) +
  xlab("Humidity") + ylab("Temperature")
# Now with higher bandwidth
plot2 = ggplot(data = all[all$epoch %% 288 == n,],
  aes(x = humidity, y = humid_temp)) +
  geom_point(color = 'red')+
  stat_smooth(method = "loess", span = 2.0) +
  ggtitle(paste("Temperature vs. Humidity at ",
  toString(hr.mn[1]), ":", toString(hr.mn[2]), "LOESS Bandwidth 2.0")) +
  xlab("Humidity") + ylab("Temperature")
grid.arrange(plot1, plot2, nrow = 2)
#dev.off()  

# 3.2. Comparing different formula in LOESS. I.e. local linear
# fit or local quadratic fit
# Local Linear
plot1 = ggplot(data = all[all$epoch %% 288 == n,], 
               aes(x = humidity, y =humid_temp)) + 
  geom_point(color = 'red')+ 
  stat_smooth(method = "loess", formula = y ~ poly(x, 1), span = 2)+ 
  ggtitle(paste("Temperature vs. Humidity at ",
    toString(hr.mn[1]), ":", toString(hr.mn[2]), ". Linear Smoothing")) +
  xlab("Humidity") + ylab("Temperature")
plot2 = ggplot(data = all[all$epoch %% 288 == n,],
               aes(x = humidity, y = humid_temp)) +
  geom_point(color = 'red')+
  stat_smooth(method = "loess", formula = y ~ poly(x, 2), span = 2) +
  ggtitle(paste("Temperature vs. Humidity at ",
    toString(hr.mn[1]), ":", toString(hr.mn[2]), ". Quadratic Smoothing")) +
  xlab("Humidity") + ylab("Temperature")
grid.arrange(plot1, plot2, nrow = 2)

