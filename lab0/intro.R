# This file contains examples of some common data manipulation tasks in R.
# It may be useful as a reference, especially for those who are new to R.

# This is the directory on my local computer where I have this .R file saved.
# After setting the working directory here, relative file paths
# will use this directory as the base.
setwd("~/Documents/classes/STAT215A_GSI/Stat215A-2014/Lab_Sessions/20140902")

# These commands only need to be run once, when you first install R.
# After that, you can simply load them with library()
install.packages("ggplot2")
install.packages("ddplyr")
install.packages("reshape2")

# Load the libraries each time like this.
library(ggplot2)
library(ddplyr)
library(reshape2)

# Here are some basic manipulations of numeric vectors.
x <- 1:5
length(x)
c(x, x)
as.character(x)
x * 2

# Vectors can be of other types, like character or boolean.
x.string <- as.character(x)
paste(x.string, "clearly_I_am_a_string_now", sep=",")
x.string == "3"

# Matrices are constructed columnwise be default.
matrix(c(x, x), ncol=2)

# Some basic matrix operations.
y <- matrix(1:8, nrow=2, ncol=4)
y
t(y)
t(y) %*% y
y %*% diag(4)
rbind(y, y)
cbind(y, y)

# Get dimensions.
nrow(y)
ncol(y)
dim(y)

# Inversion and eigenvalues:
z <- diag(4) + matrix(0.1, nrow=4, ncol=4)
solve(z, matrix(1:4, ncol=1))
z.chol <- chol(z)
t(z.chol) %*% z.chol
crossprod(z.chol)
eigen(z)

# Matrices can be indexed a few different ways.
example.mat <- matrix(1:12, ncol=3)
example.mat[1, 2] # What you'd expect
example.mat[5] # As if it were a columnwise vector.
example.mat[1, ] # First row
example.mat[, 1] # First column
example.mat[-1, -1] # All but the first column and row

# If you assign row and column names, you can use them
# like numbers.
colnames(example.mat) <- c("badger", "fox", "bat")
example.mat[3, "badger"]

rownames(example.mat) <- letters[1:4]
example.mat["b", "badger"]

# Indexing with numeric vectors selects sub-matrices.
example.mat[c(1, 2, 4), c(2, 3)]
example.mat[1:nrow(example.mat), 2]

# Indexing a matrix by a two-column matrix selects
# particular elements.
example.mat.indices <- matrix(c(1, 4, 2, 3), ncol=2) 
example.mat[example.mat.indices]

# Matrices are different from numeric vectors.  Sometimes R
# ignores the difference, sometimes it does not.  This is
# one of the most obnoxious attributes of R, and you must be
# on your guard against unexpected problems.

# This works:
solve(z, 1:4)
matrix(1:4, nrow=1) %*% solve(z, 1:4)
z.bad <- z[,1]
z[2, 1]

# This fails!  R has (horrifically) automatically turned z into a
# vector when you used a 1-d index.
z.bad[2, 1]

# This works -- drop=FALSE keeps you from losing the matrix type.
# If you refer to matrices with variable indices, if there is any
# possibility of this happening, you should always used drop=FALSE
# in order to prevent hard-to-detect bugs.
z.ok <- z[, 1, drop=FALSE]
z.ok[2, 1]

# Lists are an important data type in R.  They can contain unstructured data.
my.list <- list()
my.list[["raw.mat"]] <- z
my.list[["eigen.result"]] <- eigen(z)
my.list[["x.as.string"]] <- x.string
my.list[["list.in.list"]] <- list("a"=1, "b"=2)

# Elements of lists can be referred to by name or number.
my.list[[1]]
my.list$raw.mat
my.list[["raw.mat"]]
list.item <- "raw.mat"
my.list[[list.item]]

# Be careful with the numbers, though.  Assigning an entry to NULL
# erases that entry and changes the rest of the numbers.
my.list[[2]]
my.list[[1]] <- NULL
my.list[[2]]

# You can chain together these indices however you want.
my.list[["eigen.result"]]$vectors[1, 1:4]

# names() will tell you what's in a list.
names(my.list)

# A strange thing:
# Double brackets gives the original data type, single brackets
# gives a list containing only that item.
class(my.list[["x.as.string"]])
class(my.list["x.as.string"])

# lapply applies the same function to each element of a vector.
# Protip: this can be used to load in a large number of files.
MakeMatrix <- function(n) {
  return(matrix(n, nrow=n, ncol=n))
}
mat.list <- lapply(1:5, MakeMatrix)
mat.list[3]

# Usually, the basic unit of data analysis is the data frame.  These
# two beaver datasets are loaded by default as part of the
# "datasets" package.  head() shows the first few rows.
# For information about this (and any) R command, 
# you can type "?head" at the prompt.
head(beaver1)
head(beaver2)

# You can also simply look at the first few rows by selecting 
# them by hand.
beaver1[1:5, ]

# class() shows the type of a variable.  A common data type
# in R is the data frame, which is like a matrix where each
# column can be its own data type.  The beaver datsets have this
# data type.
class(beaver1)

# Data frame columns have names.  These columns can be referred to
# several ways.  These are equivalent, and all return numeric vectors.
# The first is readable if you know in advance the column names.
# The second is useful if you want to refer to a column by a name stored
# in a character variable.
# The third is useful if you want to refer to a column by number.

# Method 1:
beaver1$day

# Method 2:
beaver1[["day"]]
beaver1[, "day"]

# Method 3.  Note that referring to columns by number can be brittle
# and unreadable, and should be avoided.
beaver1[[1]]
beaver1[, 1]

# These are another way to get at a column, but they return data frames
# with a single column, not numeric vectors.
class(beaver1["day"])
class(beaver1[1])

# Here is how to get the names of a data frame.
names(beaver1)

# As observed above, you can also refer to certain rows of a data frame.
# This can be with row names, boolean vectors, or numeric vectors.
# The subset() command is also useful.

# Numeric indices:
beaver1[c(80, 81, 83, 85, 86), ]

# Row names (which are strings that, in this case, happen to
# coincide with the row numbers):
beaver1[c("80", "81", "83", "85", "86"), ]

# Boolean indexing:
beaver1[beaver1$temp > 37.2, ]

# Subset:
subset(beaver1, temp > 37.2)

