setwd("~/Dropbox/School/ST215/Lab/lab4/image_data/")
options(max.print = 1000)

#########################################################################
### 1. Load Data
#########################################################################
# Get the data for three images
image1 <- read.table('image1.txt', header=F)
image2 <- read.table('image2.txt', header=F)
image3 <- read.table('image3.txt', header=F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

# Function to sample data
getTrainTest = function(list.images, train.percentage = 0.60)
{
  set.seed(1); n.images = length(list.images) 
  n.row = nrow(list.images[[1]])
  train.index = sample(n.row, n.row*train.percentage)
  train = list.images[[1]][ train.index, ]
  test =  list.images[[1]][-train.index, ]
  i = 2;
  while (i <= n.images)
  {
    set.seed(i)
    n.row = nrow(list.images[[i]])
    train.index = sample(n.row, n.row*train.percentage)
    train = rbind(train, list.images[[i]][ train.index, ])
    test  = rbind(test,  list.images[[i]][-train.index, ])
    i = i + 1
  }
  return(list(train, test))
}
l = getTrainTest(list(image1, image2))
train = l[[1]]; test = l[[2]]; rm(l);
