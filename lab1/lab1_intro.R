# In-class preliminary exploration of the redwood data for lab1.

library(ggplot2)
library(dplyr)
library(reshape2)

# Turn off my usual default of warnings as error.
options(warn=0)

# Load the data.
#setwd(file.path(Sys.getenv("GIT_REPO_LOC"),
#                "classes/STAT215A_Fall2013/gsi_lab1"))
setwd("~/Dropbox/School/1/ST215/Lab/lab1/data/")
log <- read.csv('sonoma-data-log.csv',header=T)
net <- read.csv('sonoma-data-net.csv',header=T)
all <- read.csv('sonoma-data-all.csv', header=T)
locs <- read.table('mote-location-data.txt')

# First inspect the rough properties of the data.
# Let's work with the 'log' data first.

# How much memory are we using to store this stuff?
object.size(log)
nrow(log)
summary(log)
head(log)


#####################
# Make a few scatter plots to explore the data.

# First look at humidity vs temperature.
patient <- FALSE
if (patient) {
  # This takes a long time.
  ggplot(log) + geom_point(aes(x=humidity, y=humid_temp))
}

log.indices <- sample(nrow(log), 10000, replace=F)
ggplot(log[log.indices, ]) + geom_point(aes(x=humidity, y=humid_temp))

# hamatop vs hamabot
ggplot(log[log.indices, ]) + geom_point(aes(x=hamatop, y=hamabot))

# hamatop vs temperature, humidity as color.
ggplot(log[log.indices, ]) + geom_point(aes(x=hamatop, y=humid_temp, color=humidity))

# Take some class time to explore.


#####################
# Next, pick a few nodes, and inspect their time series.
group_by(log, nodeid) %>% summarize(n())
ggplot(filter(log, nodeid == 105)) + geom_point(aes(x=epoch, y=humid_temp))

# A little fancier.
Normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) 
}
node.melt <- melt(filter(log, nodeid == 105) %>%
                  select(epoch, voltage, humidity, humid_temp) %>%
                  mutate(voltage = Normalize(voltage),
                         humidity = Normalize(humidity),
                         humid_temp = Normalize(humid_temp)),
                  id="epoch")
ggplot(node.melt) + geom_point(aes(x=epoch, y=value)) + facet_grid(~ variable)

# Take some class time to explore.


#####################
# Finally, let's try combining the datasets.
# What do we have to think about?

# First, do the two datasets have the same nodeid and epoch combinations?

# How could we test this?

# Exercise in class: for each nodeid / epoch combo in net, find the corresponding row in log
# if there is one, otherwise return NA.



# An answer is below ->
























#########
# How many are bad?
sum(!is.numeric(net$nodeid))
sum(!is.numeric(net$epoch))
sum(!is.numeric(log$nodeid))
sum(!is.numeric(log$epoch))

# How many are repeated?
net.multiplicity <- group_by(net, nodeid, epoch) %>% summarise(n=n()) %>%
                    ungroup() %>% group_by(n) %>% summarise(total=n())
# Sanity check.
nrow(net)
sum(net.multiplicity$total * net.multiplicity$n)

net$row.id <- 1:nrow(net)
log$row.id <- 1:nrow(log)

# How many overall matches are there?
node.epoch.matches <- inner_join(select(net, nodeid, epoch, row.id),
                                 select(log, nodeid, epoch, row.id),
                                 by=c("nodeid", "epoch")))
# Why doesn't count_distinct work?
nrow(unique(node.epoch.matches[c("nodeid", "epoch")])) / nrow(net)
nrow(unique(node.epoch.matches[c("nodeid", "epoch")])) / nrow(log)

# How many are in net which are not in log?
net.node.epoch.matches <- left_join(select(net, nodeid, epoch, row.id),
                                    select(log, nodeid, epoch, row.id),
                                    by=c("nodeid", "epoch"))
nrow(unique(net.node.epoch.matches[c("nodeid", "epoch")])) / nrow(net)

# By the way, this is pretty fast, compared with merge.
system.time(
  node.epoch.matches <- inner_join(select(net, nodeid, epoch, row.id),
                                   select(log, nodeid, epoch, row.id),
                                   by=c("nodeid", "epoch")))
system.time(
  merged <- merge(net[c("nodeid", "epoch")], log[c("nodeid", "epoch")],
                by=c("nodeid", "epoch")))


