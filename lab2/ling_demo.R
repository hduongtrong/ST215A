# Identify cluster. Think about whether what you do make sense.
# PCA, cluster, mixture. Talk about randomness, what is the random processes
# is that generate this data. Think about stability. Rubust. 

# map_ex: example how to use map
# Do you scale and center
# Use different way R calculate PCA to see if they result in the same
# "irlba" calculate eigenvalue one by one. 
# pipeR

setwd("~/Dropbox/School/ST215/Lab/lab2/")
#setwd(file.path(Sys.getenv("GIT_REPO_LOC"), "classes/STAT215A_Fall2013/gsi_lab2/lab"))

options(warn=0)
library(maps)
library(ggplot2)
library(dplyr)

lingData <- read.table('lingData.txt', header = T)
lingLocation <- read.table('lingLocation.txt', header = T)
load("question_data.RData")

# lingData has a column for each question, and lingLocation has a column
# for each question x answer.  Sorry the columns in lingLocation are not usefully named,
# but it's not too tricky to figure out which is which.
# Note that you still need to clean this data (check for NA's, missing location data, etc.)
names(lingData)
names(lingLocation)
state.df <- map_data("state")

blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

############
# Make a plot similar to the website for the second person plural answers.
# You may want to join these data sets more efficiently than this.
plural.second.person <- filter(lingData, Q050 %in% c(1, 2, 9), long > -125)
answers.q50 <- all.ans[['50']]

# Make the column to join on.  They must be the same type.
answers.q50$Q050 <- rownames(answers.q50)
plural.second.person$Q050 <- as.character(plural.second.person$Q050)
plural.second.person <- inner_join(plural.second.person, answers.q50, by="Q050")

# Plot!
ggplot(data=NULL) +
  geom_point(data=plural.second.person, aes(x=long, y=lat, color=ans), size=3, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme



###############
# Plot the lingLocation data (which lives on a grid).  Note that this doesn't look great with
# state outlines.  You can probably do better!
ggplot(data=NULL) +
  geom_tile(data=filter(lingLocation, Longitude > -125),
            aes(x=Longitude, y=Latitude, color=log10(V12), fill=log10(V12))) +
  geom_polygon(data=state.df, colour = "gray", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme
