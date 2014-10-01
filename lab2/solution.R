# inner_join(net, log, by = 'nodeid', 'epoch')
library(reshape2)
library(ggplot2)
library(lattice)
library(gridExtra)
library(dplyr)
library(reshape2)
library(xtable)

# Turn this option to TRUE to display all plots. 
# Turn to FALSE to skip all plots.
is.plot = FALSE
options(max.print=1000)

# 1. Load Data
setwd("~/Dropbox/School/ST215/Lab/lab1/data/")
log <- read.csv('sonoma-data-log.csv',header=T)
net <- read.csv('sonoma-data-net.csv',header=T)
all <- read.csv('sonoma-data-all.csv', header=T)
locs <- read.table('mote-location-data.txt', header=T)


# 2. Handling Outlier
# 2.1 Remove Uninformative Columns
 
head(log)
# Note that Result Time seems not to change
levels(log$result_time) # Confirm. Will remove this column
# As discussed in the lab, column parent, depth, and humid_adj 
# will be remove
colLog = names(log); 
colLog = colLog[c(2,3,5,7,8,10,11)]
log = log[,colLog]
rm(colLog)

head(net)
if (is.plot) ggplot(data=net, aes(x=voltage))+geom_density()
# We note that voltage here is not the battery voltage, which is around 3V.
# This is likely the voltage at the lab, where researcher receive the wireless 
# signal from the devices on the tree. This has little to do with our data.
colNet = names(net)
colNet = colNet[c(1,2,3,7,8,10,11)]
net = net[colNet]

# 2.2. Handling NA

# Count the total number of NA for each row, the summary the information.
table(rowSums(is.na(log)))
colSums(is.na(log))
# Notice that the na are always happen in pack of 4. 
# We check for column that have na, and that happens to be
# all the four data columns. Thus we can remove all of these 
# na row
naRow = rowSums(is.na(log)) == 0
log = log[naRow,]

table(rowSums(is.na(net)))
naRow = rowSums(is.na(net)) == 0
net = net[naRow,]

# Check that there is no NA left
sum(is.na(net))
sum(is.na(log))

# 2.3. Remove Error Data

# a. Checking density of epoch
if (is.plot) ggplot(data=log, aes(x=epoch))+geom_density()
if (is.plot) ggplot(data=net, aes(x=epoch))+geom_density()
# Nothing seems unusual from the density

# b. Check the nodeid
if (is.plot) ggplot(data=log, aes(x=nodeid))+geom_density()
if (is.plot) ggplot(data=net, aes(x=nodeid))+geom_density()

# We notice there is one place where nodeid is 65535.
# This is error as the nodeid should be between 1 and 200.
n = which(log$nodeid>300)
log[n,]
# Data seems off here because of low voltage. We'll delete this
log = log[-n,]

n = which(net$nodeid>300)
length(n)

# c. Check Voltage
if (is.plot) ggplot(data=log, aes(x=voltage))+geom_density()+
  ggtitle("Voltage Distribution")
n = which(log$voltage<2) 
length(n)
# Note that there are 30,000 observation where the voltage is below 2
# As mentioned in the paper, when voltage is < 2.4 the data is not realiable any more
# We will check the data at these point
if (is.plot) ggplot(data=log[n,], aes(x=voltage, y = humidity)) + geom_jitter() 
min(log$voltage)
table(log$voltage[log$voltage<1])
n1 = which(log$voltage<1.0)
head(log[n1,])
sum(log$nodeid==135)
table(log$nodeid)
# We note that there are 7 nodeids where all the observation has exactly the
# same voltage of 0.580567. They are 128  134  135  141  142  143  145 
plot1 = ggplot(data=log[n1,], aes(x=epoch, y = humid_temp)) + 
  geom_line(aes(group = nodeid, color = factor(nodeid))) + xlim(0,12000) + 
  ylim(0,40) + ggtitle("Temperature of NodeID with wrong voltage (0.58)")
plot2 = ggplot(data=log[n1,], aes(x=epoch, y = humidity)) + 
  geom_line(aes(group = nodeid, color = factor(nodeid))) + xlim(0,12000) + 
  ylim(0,110) + ggtitle("Humidity of NodeID with wrong voltage (0.58)")
plot3 = ggplot(data=log[(log$nodeid %% 10)==7,], aes(x=epoch, y= humid_temp)) +
  geom_line(aes(group = nodeid, color = factor(nodeid))) + xlim(0,12000) + 
  ylim(0,40) + ggtitle("Temperature of some NodeID with correct voltage")
plot4 = ggplot(data=log[(log$nodeid %% 10)==7,], aes(x=epoch, y= humidity)) +
  geom_line(aes(group = nodeid, color = factor(nodeid))) + xlim(0,12000) + 
  ylim(0,110) + ggtitle("Humidity of some NodeID with correct voltage")
if (is.plot) grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)
# There seems to be nothing wrong with these node that report voltage of 0.580567
# Now we check other observation with low voltage but not 0.580567

n = which(log$voltage<2.4 & log$voltage > 1)
length(n)
plot1 = ggplot(data=log[n,],aes(x=voltage,y=humid_temp))+
  geom_point()+ggtitle("Temperature of observations with low voltage")
plot2 = ggplot(data=log[n,],aes(x=voltage,y=humidity))+
  geom_point()+ggtitle("Humidity of observations with low voltage")+ylim(-100,100)
# We put ylim here because there are 11 observations with too low humidity. 
if (is.plot) grid.arrange(plot1, plot2, nrow = 2, ncol = 1)
# Remove those observation with voltage < 2.00
n = which(log$voltage<2.0 & log$voltage > 1)
length(n)
log = log[-n,]

n = which(log$voltage<2.5 & log$voltage>1)
plot1 = ggplot(data=log[n,],aes(x=voltage,y=humid_temp))+geom_point()
plot2 = ggplot(data=log[n,],aes(x=voltage,y=humidity))+geom_point()
if (is.plot) grid.arrange(plot1, plot2, nrow = 2, ncol = 1)
# Things seem normal for those observation with low voltage now.
# We see this gross outlier in humidity. Check it out



# d. Check Temperature and Humidity
n = which(log$humidity < -0)
log[n,]
log = log[-n,]

if (is.plot) hist(log$humid_temp)
if (is.plot) hist(net$humid_temp)
n = which(net$humid_temp  > 60)
length(n)
if (is.plot) ggplot(data=net[n,], aes(x=humid_temp, y = humidity)) + geom_point()
table(net$nodeid[n])

nodeids.with.error.temp = unique(net$nodeid[n])
n = net$nodeid %in% nodeids.with.error.temp
plot1 = ggplot(data=net[n,],aes(x=epoch,y=humid_temp)) + 
  geom_point(aes(group = nodeid, color = factor(nodeid))) +
  ggtitle("Temperature of nodeid with wrong temperature")
plot2 = ggplot(data=net[n,],aes(x=epoch,y=humidity)) + 
  geom_point(aes(group = nodeid, color = factor(nodeid))) +
  ggtitle("Humidity of nodeid with wrong temperature")
if (is.plot) grid.arrange(plot1, plot2, nrow=2, ncol=1)
# It seems the sensor stops for these nodeID stops working properly 
# after a while. Remove nodeid 3, 78, and 141. Since there are not 
# that many observations
net = net[net$nodeid != 3,]
net = net[net$nodeid != 78,]
net = net[net$nodeid != 145,]
# Deal with nodeid 123
if (is.plot) plot(net$humid_temp[net$nodeid==123][900:1100])
# Keep the first 1000 obs. Delete observation 1000 and after
n = net$nodeid == 123
n[n][1:1000] = FALSE
net = net[!n,]
# Deal with nodeid 141. Keep 3500 first obs, delete obs after.
n = net$nodeid == 141
n[n][1:3500] = FALSE
net = net[!n,]

plot1 = ggplot(data=net, aes(x=epoch, y = humid_temp)) + 
  geom_line(aes(group = nodeid, color = factor(nodeid))) + 
  ggtitle("Temperature as a function of time (epoch). Net data") + 
  ylim(5,35) + xlim(0,13000)
plot2 = ggplot(data=log, aes(x=epoch, y = humid_temp)) + 
  geom_line(aes(group = nodeid, color = factor(nodeid))) + 
  ggtitle("Temperature as a function of time (epoch). Log data") +
  ylim(5,35) + xlim(0,13000)
plot3 = ggplot(data=net, aes(x=epoch, y = humidity)) + 
  geom_line(aes(group = nodeid, color = factor(nodeid))) + 
  ggtitle("Humidity as a function of time (epoch). Net data") + 
  ylim(10,110) + xlim(0,13000)
plot4 = ggplot(data=log, aes(x=epoch, y = humidity)) + 
  geom_line(aes(group = nodeid, color = factor(nodeid))) + 
  ggtitle("Humidity as a function of time (epoch). Log data") +
  ylim(10,110) + xlim(0,13000)
if (is.plot) grid.arrange(plot1, plot3, plot2, plot4, nrow = 2, ncol = 2)

# Things seem good for humidity and temperature now

# e. Incident PAR and Reflected PAR
if (is.plot) ggplot(data=log, aes(x=epoch, y = hamatop)) + geom_line(aes(group = nodeid, color = factor(nodeid)))
if (is.plot) ggplot(data=net, aes(x=epoch, y = hamatop)) + geom_line(aes(group = nodeid, color = factor(nodeid)))
# Since these two variable measure roughly the sunline, we should expect
# the measurement at night to be zero. However from the plot there are some
# nodeid where the measurement is not zero at night. We will check out these.
table(net$nodeid)
if (is.plot) xyplot(hamatop ~ epoch | factor(nodeid), data = net, type = "l")
# Nodeid 198 has only 4 observation. Nodeid 135 seems wrong. 
net[net$nodeid==135,]
# Other measurement seems to be ok. But since there are only a few hundred obs with 
# this nodeid. We'll just remove it
net = net[net$nodeid != 135,]
log = log[log$nodeid != 135,]

# f. Remove nodeid with few data
# 854 is 3 days worth of data
good.node = as.numeric(names(which(table(net$nodeid)>864)))
net = net[net$nodeid %in% good.node,]
# Now log data

# We will remove those nodeid with less than 864 observations
good.node = as.numeric(names(which(table(log$nodeid)>864)))
log = log[log$nodeid %in% good.node,]
if (is.plot) xyplot(hamatop ~ epoch | factor(nodeid), data = log, type = "l")
if (is.plot) xyplot(hamabot ~ epoch | factor(nodeid), data = log, type = "l")
if (is.plot) xyplot(hamabot ~ epoch | factor(nodeid), data = net, type = "l")

# 2.4. Check for unique epoch and nodeid

net = tbl_df(net)
net = group_by(net, epoch, nodeid)
# Create a count that check for unique up w.r.t combination of epoch and nodeid
net = mutate(net, count = n())
# Get standard dev of obs when they share epoch and nodeid
net = mutate(net, range.humid = max(humidity)-min(humidity))
table(net$range.humid)
net = mutate(net, range.temp = max(humid_temp)-min(humid_temp))
table(net$range.temp)
net[net$range.temp>1,]
net = net[net$range.temp<1,]

# Most of them are duplicate. We'll remove them
net = summarize(net, humidity = mean(humidity), 
    humid_temp = mean(humid_temp), hamatop = mean(hamatop), 
    hamabot = mean(hamabot))

log = tbl_df(log)
log = group_by(log, epoch, nodeid)
# Create a count that check for unique up w.r.t combination of epoch and nodeid
log = mutate(log, count = n())
# Get standard dev of obs when they share epoch and nodeid
log = mutate(log, range = max(hamabot)-min(hamabot))
table(log$range)
log[log$range>0,]
# The hamatop and hamabot columns don't agree

# Most of them are duplicate. We'll remove them
log = summarize(log, humidity = mean(humidity), 
                humid_temp = mean(humid_temp), hamatop = max(hamatop), 
                hamabot = max(hamabot))
log = ungroup(log)
net = ungroup(net)

# 2.5 Join Data
inner_join(log,net,by=c("epoch","nodeid"))
# We check that for those observation that share the same "epoch"
# and "nodeid" between log and net data. They are almost always
# the same. The final data set will be just appended log 
# and net together

all = rbind(log,anti_join(net,log,by = c("epoch","nodeid")))
all = arrange(all, nodeid, epoch)
if (is.plot) xyplot(humid_temp ~ epoch | factor(nodeid), data = all, type = "l")
if (is.plot) xyplot(humidity ~ epoch | factor(nodeid), data = all, type = "l")
if (is.plot) xyplot(hamatop ~ epoch | factor(nodeid), data = all, type = "l")
if (is.plot) xyplot(hamabot ~ epoch | factor(nodeid), data = all, type = "l")
if (is.plot) plot(humidity ~ epoch, data = all[all$nodeid==138,])

############################################################################
## 3. Explore Data

# Histogram
plot1 = ggplot(data=all, aes(x=humidity)) + 
  geom_histogram() + ggtitle("Histogram of Humidity") + 
  xlab("Humidity")
plot2 = ggplot(data=all, aes(x=humid_temp)) + 
  geom_histogram() + ggtitle("Histogram of Temperature") +
  xlab("Temperature")
plot3 = ggplot(data=all, aes(x=hamatop)) + 
  geom_histogram() + ggtitle("Histogram of Incident PAR, excluding zero") + 
  scale_x_log10() + xlab("Incident PAR")
plot4 = ggplot(data=all, aes(x=hamabot)) + 
  geom_histogram() + ggtitle("Histogram of Reflected PAR, excluding zero") + 
  scale_x_log10() + xlab("Reflected PAR")
if (is.plot) grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

# Humidity vs. Temperature
if (is.plot) {
  ggplot(data = all[sample(nrow(all),10000),], aes(x = humid_temp, y = humidity)) + 
    geom_jitter(color="red") + 
    geom_smooth(method = "loess",span = .75) + 
    ggtitle("Scatter plot of Humidity vs. Temperature and LOESS bound") +
    xlab("Temperature")
}


if (is.plot) {
  ggplot(data = all[sample(nrow(all),10000),], aes(x = hamabot, y = hamatop)) + 
    geom_jitter(color="red") + 
    ggtitle("Scatter plot of Incident PAR vs. Reflected PAR and LOESS bound") +
    xlab("Reflected PAR") + ylab("Incident PAR")
}
# 4. Findings
# 4.1. Density of PAR/53.4

plot1 = ggplot(data = all[all$hamatop>0000 & all$hamatop<4000,], aes(x = hamatop/53.4)) + 
  geom_density() + ggtitle("Density Plot of Incident PAR First Part") + 
  xlab("Incident PAR / 53.4 (Restricted to interval (0, 4000])") + 
  ylab("Density")
plot2 = ggplot(data = all[all$hamatop>4000 & all$hamatop<8000,], aes(x = hamatop/53.4)) + 
  geom_density() + ggtitle("Density Plot of Incident PAR Second Part") + 
  xlab("Incident PAR / 53.4 (Restricted to interval [4000, 8000])") + 
  ylab("Density")
plot3 = ggplot(data = all[all$hamabot>0000 & all$hamabot<4000,], aes(x = hamabot/53.4)) + 
  geom_density() + ggtitle("Density Plot of Reflected PAR First Part") + 
  xlab("Reflected PAR / 53.4 (Restricted to interval (0, 4000])") + 
  ylab("Density")
plot4 = ggplot(data = all[all$hamabot>4000 & all$hamabot<8000,], aes(x = hamabot/53.4)) + 
  geom_density() + ggtitle("Density Plot of Reflected PAR Second Part") + 
  xlab("Reflected PAR / 53.4 (Restricted to interval [0, 4000])") + 
  ylab("Density")
if (is.plot) grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)


MAD = function(data,t) {
  mean(abs(data/t-round(data/t)))
}

# Perform a grid search
grid_values = seq(500,550,length=51)
iPAR_full = lapply(grid_values, (function (t) MAD(all$hamatop[all$hamatop != 0], t)))
rPAR_full = lapply(grid_values, (function (t) MAD(all$hamabot[all$hamabot != 0], t)))

iPAR_restricted = lapply(grid_values, (function (t) MAD(all$hamatop[all$hamatop>0 & all$hamatop<4000], t)))
rPAR_restricted = lapply(grid_values, (function (t) MAD(all$hamabot[all$hamabot>0 & all$hamabot<4000], t))) 

iPAR_full = as.numeric(iPAR_full); rPAR_full = as.numeric(rPAR_full); 
iPAR_restricted = as.numeric(iPAR_restricted); rPAR_restricted = as.numeric(rPAR_restricted)
df = data.frame(grid_values = grid_values,iPAR_full = iPAR_full,rPAR_full = rPAR_full,
                iPAR_restricted = iPAR_restricted,rPAR_restricted = rPAR_restricted)
x = grid_values
plot1 = ggplot(data = df, aes(x,iPAR_full)) + geom_point() + 
  ggtitle("Incident PAR - All Nonzero") + 
  xlab("Theta - Change of Unit Measurement") +
  ylab("MAD")

plot2 = ggplot(data = df, aes(x,rPAR_full)) + geom_point() + 
  ggtitle("Reflected PAR - All Nonzero") + 
  xlab("Theta - Change of Unit Measurement") +
  ylab("MAD")

plot3 = ggplot(data = df, aes(x,iPAR_restricted)) + geom_point() + 
  ggtitle("Incident PAR - Restricted (0,4000)") + 
  xlab("Theta - Change of Unit Measurement") +
  ylab("MAD")

plot4 = ggplot(data = df, aes(x,rPAR_restricted)) + geom_point() + 
  ggtitle("Reflected PAR - Restricted to (0,4000)") + 
  xlab("Theta - Change of Unit Measurement") +
  ylab("MAD")
if (is.plot) grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

# 4.2. 
# Get the index for those nodeid belong to edge tree, and those node id
# belong to interior tree. 

edge.tree.ids = locs$ID[locs$Tree == "edge"]
inte.tree.ids = locs$ID[locs$Tree == "interior"]
edge.tree = all$nodeid %in% edge.tree.ids
inte.tree = all$nodeid %in% inte.tree.ids

# Create a Height column in all. We restrict to 2000 epoch, because
# many nodeids die after this
all$height = locs$Height[match(all$nodeid, locs$ID)]
all1 = group_by(all[all$epoch<2000,], nodeid)

brief = summarize(all1, m.humid = mean(humidity), m.temp = mean(humid_temp),
                  m.ipar = mean(hamatop), m.rpar = mean(hamabot),
                  height = mean(height))
brief$tree[brief$nodeid %in% edge.tree.ids] = "Edge"
brief$tree[brief$nodeid %in% inte.tree.ids] = "Interior"
brief$tree[brief$nodeid == 138] = NA
plot1 = ggplot(data = brief) + 
  geom_line(aes(x = height, y = m.humid, group = tree, color = tree)) + 
  coord_flip() + ylab("Mean Humitidy")
plot2 = ggplot(data = brief) + 
  geom_line(aes(x = height, y = m.temp, group = tree, color = tree))  + 
  coord_flip() + ylab("Mean Temperature")
plot3 = ggplot(data = brief) + 
  geom_line(aes(x = height, y = m.ipar, group = tree, color = tree))  + 
  coord_flip() + ylab("Mean Incident PAR")
plot4 = ggplot(data = brief) + 
  geom_line(aes(x = height, y = m.rpar, group = tree, color = tree))  + 
  coord_flip() + ylab("Mean Reflected PAR")
if (is.plot) grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

# 4.3. Third Findings

all$dist = locs$Dist[match(all$nodeid, locs$ID)]
plot1 = ggplot(data = all[inte.tree,], 
  aes(x = height, y = humid_temp, group = nodeid, color = factor(dist))) + 
  geom_boxplot(outlier.shape = NA) + coord_flip() + 
  ggtitle("Boxplot for Interior Tree") + ylab("Temperature")

plot2 = ggplot(data = all[edge.tree,], 
  aes(x = height, y = humid_temp, group = nodeid, color = factor(dist))) + 
  geom_boxplot(outlier.shape = NA) + coord_flip() + 
  ggtitle("Boxplot for Edge Tree") + ylab("Temperature")
if (is.plot) grid.arrange(plot1, plot2, ncol = 2)
