# Basic examples of dplyr and reshape2.
# For more information see:
# Tidy Data, by Hadley Wickham
# http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# <webpage for reshape2?>


library(dplyr)
library(ggplot2)
library(reshape2)

# setwd(file.path(Sys.getenv("GIT_REPO_LOC"), "classes/STAT215A_Fall2013/gsi_lab1"))
setwd("~/Dropbox/School/ST215/Lab/lab1/data/")
mote.locations <- read.delim("mote-location-data.txt", sep="")
head(mote.locations)

############################
# A quick tour of dplyr.

# This isn't strictly necessary, but it makes the data frame print more nicely.
mote.locations <- tbl_df(mote.locations)

# Here's how to limit rows:
filter(mote.locations, Direc == "S")

# Here's how to limit columns:
select(filter(mote.locations, Direc == "S"), Direc, ID)

# Here's how to chain commands.  The return of the
# previous value becomes the first argument of the next.
# The result of this is the same as above.
filter(mote.locations, Direc == "S") %>%
  select(Direc, ID)

# Summarize and group by:
print(direc.summary <- group_by(mote.locations, Direc) %>%
      summarize(count=n(), mean.height=mean(Height)))

# Arrange:
arrange(direc.summary, mean.height)

# Mutate.  Note that this works because direc.summary is not
# grouped anymore.  (You can use ungroup() to remove groupings.)
mutate(direc.summary, centered.mean.height = mean.height - mean(mean.height))

# Use a join.

# Within a certain direction, is the edge or interior higher on average?
direc.tree <- (select(mote.locations, Height, Direc, Tree) %>%
               group_by(Direc, Tree) %>%
               summarize(mean.height=mean(Height)))
direc.tree.join <- inner_join(filter(direc.tree, Tree == "edge"),
                              filter(direc.tree, Tree == "interior"),
                              by="Direc")
ggplot(direc.tree.join) +
  geom_point(aes(x=mean.height.x, y=mean.height.y), size=4) +
  geom_abline(aes(slope=1, intercept=0), color="purple", lwd=2) +
  xlab("Edge") + ylab("Interior")

################################################################
# An extremely brief look at reshape and melt.

# Here's a better way using reshape:
direc.tree.cast <- dcast(direc.tree,
                         Direc ~ Tree,
                         value.var="mean.height")
direc.tree.cast <- filter(direc.tree.cast, !is.na(edge), !is.na(interior))
ggplot(direc.tree.cast) +
  geom_point(aes(x=edge, y=interior), size=4) +
  geom_abline(aes(slope=1, intercept=0), color="purple", lwd=2)

# Here's an even better way using reshape:
direc.tree.cast2 <- dcast(mote.locations,
                          Direc ~ Tree,
                          value.var="Height",
                          fun.aggregate=mean)
direc.tree.cast2 <- filter(direc.tree.cast2, !is.na(edge), !is.na(interior))

# Here's how to undo the "cast" operation:
melt(direc.tree.cast2, id.vars="Direc")

##########################################
# A quick look at R date / time functions.

current.time <- Sys.time()

# Times can be stored two ways in R, as POSIXct or POSIXlt.
# ?POSIXct and ?POSIXlt give the same page which tells you many things.
# The default for Sys.time is POSIXct.
class(current.time)

# The LT class is list-like.  It's hard to store in data frames but
# is a little more human-useable.
current.time.lt <- as.POSIXlt(current.time)
unclass(current.time.lt)
current.time.lt$hour
current.time.lt$min
current.time.lt$sec

# A standardized string:
as.character(current.time)

# Seconds since 1970, UTC:
as.numeric(current.time)

# The two main functions you need to move between times and character
# representations are strftime (time -> string) and strptime (string -> time).

# Time -> string.  Think about your time zones!
strftime(current.time, "The date is %Y-%m-%d and the time is %H:%M:%S")
strftime(current.time, "In Greenwich, the date is %Y-%m-%d and the time is %H:%M:%S",
         tz="GMT")

# String -> time.  Also think about your time zones here!
birthday <- strptime("2014-05-10", "%Y-%m-%d")
class(birthday)
unclass(birthday)
birthday$year
strftime(birthday, "%H:%M:%S")









