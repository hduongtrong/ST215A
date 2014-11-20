library(gridExtra)
library(dplyr)
library(ggplot2)
working.directory <- file.path(Sys.getenv("GIT_REPO_LOC"),
                               "STAT215A_Fall2013/gsi_lab5")

# If this is true, some slightly slow sanity checks will also be run.
run.sanity.checks <- FALSE

# I'll set the seed so that I can re-generate exactly the same files if
# need be.
set.seed(42)

images.list <- list()
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
for (file.num in 1:3) {
  cat("Loading image", file.num, "\n")
  images.list[[file.num]] <- read.delim(file.path(working.directory,
                                                  sprintf("image%d.txt", file.num)),
                                   header=FALSE, sep="")
  names(images.list[[file.num]]) <- collabs
  images.list[[file.num]]$image <- file.num
}

images <- do.call(rbind, images.list)
rm(images.list)

# Look to confirm that the x and y ranges are similar, and then define
# the limits for the whole image.
ranges <- group_by(images, image) %>% summarize(min.x=min(x), max.x=max(x),
                                                min.y=min(y), max.y=max(y))
x.min <- min(ranges$min.x)
y.min <- min(ranges$min.y)

# Break the rows into boxes.
box.size <- 10
images$x.box <- with(images, floor((x - x.min) / box.size))
images$y.box <- with(images, floor((y - y.min) / box.size))

# Look to make sure I did it right.  The first graph should have vertical
# stripes, and the second should have horizontal stripes.
if (run.sanity.checks) {
  ggplot(images) + geom_point(aes(x=x, y=y, color=factor(x.box))) + facet_grid(~ image)
  ggplot(images) + geom_point(aes(x=x, y=y, color=factor(y.box))) + facet_grid(~ image)
}

boxes <- group_by(images, x.box, y.box, image) %>% summarize()

if (run.sanity.checks) {
  # Make sure each box occurs exactly three times.
  boxes %>% summarize(n=n()) %>% ungroup() %>% group_by(n) %>% summarize(n())
}

# Shuffle the images within each box and merge with the original frame.
boxes <- group_by(boxes, x.box, y.box) %>% mutate(new.image=sample(1:3))
images <- inner_join(images, boxes, by=c("x.box", "y.box", "image"))

if (run.sanity.checks) {
  # Make sure there's still only one pixel at each point per image.
  group_by(images, new.image, x, y) %>% summarize(n=n()) %>%
    ungroup() %>% group_by(n) %>% summarize(count=n())
  
  # Take a look at the result for one image.
  image.choice <- 1
  grid.arrange(
    ggplot(filter(images, image == image.choice)) + geom_point(aes(x=x, y=y, color=AF)),
    ggplot(filter(images, new.image == image.choice)) + geom_point(aes(x=x, y=y, color=AF)),
    nrow=1
  )
}

# Save the new images in a format similar to the old.
for (file.num in 1:3) {
  cat("Writing image", file.num, "\n")
  write.table(filter(images, new.image == file.num)[collabs],
              file=file.path(working.directory, sprintf("new_image%d.txt", file.num)),
              quote=FALSE, row.names=FALSE, col.names=FALSE)
}







