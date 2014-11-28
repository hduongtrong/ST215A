# This file loads and looks at some of the data sets for the
# STAT215A Fall 2014 final project.

library(corrplot)
library(ggplot2)
library(reshape2)
library(rgl)

RotateImageVector <- function(image.vec) {
  # Rotate a raw image vector into the
  # correct orientation for viewing.
  
  # There are this many rows and columns,
  # and transpose and re-ordering gives it the correct
  # orientation.
  kRows <- 128
  return(t(matrix(image.vec, nrow = kRows)[kRows:1,]))
}


ReadImage <- function(number) {
  # Read a single line from fit_stim.csv into
  # a matrix that can be plotted as an image.
  #
  # Args:
  #  - number: Which image number to load, starting
  #            with 1.
  #
  # Returns:
  #  - A matrix containing the raw image pixels.
  
  # The first line contains column names, so we always
  # skip at least one line.
  img <- scan("fit_stim.csv",
              skip = number, nlines = 1, sep=",")

  return(RotateImageVector(img))
  
}


ReadRealBasisFunction <- function(number) {
  # Read a single column from real_wav.csv into
  # a matrix that can be plotted as an image.  This
  # uses a bash command and may not work for windows users. 
  #
  # Args:
  #  - number: Which basis function to load, starting
  #            with 1.
  #
  # Returns:
  #  - A matrix containing the basis function.  NB:
  #    I am not 100% sure that the rotation is the same
  #    for the basis function as for the images.
  
  # Use bash to read a single column into a text file.
  # Note that this may not work for windows users.
  temp.file <- tempfile()
  system(paste("cat real_wav.csv | cut -d, -f ",
               number, " > ",
               temp.file))
  basis.data <- read.csv(temp.file, header=T)[[1]]
  return(RotateImageVector(basis.data))
}



#####################
# Load the data.
setwd(file.path(Sys.getenv("GIT_REPO_LOC"),
                  "STAT215A_Fall2013/gsi_final_project"))
load("fMRIdata.RData")
ls()

# Load in a raw image.
img1 <- ReadImage(1)
image(img1, col=gray((1:500) / 501))

# Load in a raw basis function.
wav1 <- ReadRealBasisFunction(150)
image(wav1)

# Plot the physical locations of the voxels.
voxel.locs <- data.frame(loc_dat)
rm(loc_dat)
voxel.locs$random.cluster <- sample(1:4, nrow(voxel.locs), replace=TRUE)
rgl.spheres(voxel.locs$X1, voxel.locs$X2, voxel.locs$X3,
            color=voxel.locs$random.cluster, radius=0.3)

# Take a look at the distribution of responses.
resp.dat <- data.frame(resp_dat)
names(resp.dat)  <- paste("voxel", 1:ncol(resp.dat), sep="")
rm(resp_dat)
resp.melt <- melt(resp.dat)
ggplot(resp.melt) +
  geom_density(aes(x=value)) +
  facet_grid(variable ~ .)
corrplot(cor(resp.dat))

# Look at the first image's feature distribution.
fit.feat <- data.frame(fit_feat)
rm(fit_feat)
qplot(x=as.numeric(fit.feat[1, ]), geom="density")


# Look at the validation set.
dim(val_feat)




