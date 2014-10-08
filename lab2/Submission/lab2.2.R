library(ggplot2)
library(maps)
library(lattice)
library(microbenchmark)
library("irlba")
options(max.print=1000)

#######################################
### 1. Load Data
#######################################
setwd("/Users/hd/Dropbox/School/ST215/Lab/lab2/")
question = load("question_data.RData")
data = read.table("lingData.txt", header = TRUE)
data = data[!rowSums(is.na(data)),]
data = data[data$long > -130,] # Exclude Alaska and Hawaii
gc()

########################################
### 2. Investigate two survey questions
########################################
# Load County and State Data
state_df <- map_data("state")
#county_df <- map_data("county")
#names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
#county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
#county_df$state_name <- NULL

# DC is not in state.abb, we add it in as there are a few people from DC
#county_df$state[is.na(county_df$state)] = "DC"
#state.abb2 = c(state.abb, "DC")
#data = data[data$STATE %in% state.abb2,] # Remove wrong state name

#data$county = tolower(data$CITY)
################################### 
# Plotting with map of US
n = 73
p <- ggplot(data = NULL, aes(long, lat)) +
  geom_point(data = data, aes(color = factor(Q073), alpha = 0.5, size = 0.5)) #+ 
  scale_color_discrete(name = "Answer", 
                       breaks = 0:length(all.ans[[n]]$ans),
		   labels = c("No Answer", as.character(all.ans[[n]]$ans))) +  
  geom_polygon(data = state_df, colour = "white", fill = NA, aes(group = group)) +
  ggtitle(quest.use[[n,2]]) +
  xlab("Longtitude") + ylab("Latitude") 
print(p)


## Use iplot
iplot(data$long, data$lat)
imosaic(data$Q050, data$Q105)

#######################################
### 3. Encode Data into Binary Form
#######################################
# Get the number of unique answer for each question

# n[length(n)] = 468 columns needed, agrees with the Lab2 statement

# Preallocate Data
nn = numeric(67) # 67 questions
cn = names(data)[5:71] # Columns names of questions
for (i in 1:length(cn))
{
  nn[i] = nrow(unique(data[cn[i]])) - 1 # - 1 for the zero value
}
nn = cumsum(nn); nn = c(0,nn)
save.binary = function(filename, nn = nn, cn = cn)
{
  bdata = matrix(nrow=nrow(data),ncol=nn[length(n)])
  for (i in 1:length(cn))
  {
    bdata[,(nn[i]+1):nn[i+1]] = 
      model.matrix(formula(paste("~ factor(",cn[i],")")), data = data)[,-1]
  }
  save(bdata, file=filename)
}
# If not have binary data yet uncomment the above line to generate it
# save.binary("b_data.rda")
load("b_data.rda")

########################################
### 4. Dimension Reduction by MDS
########################################
# Please run MDS.R to obtain the eigen values and eigen vectors


save.mds = function(n = 20000) # n : number of observations to include 
{
  load("b_data.rda")
  m= 468 
  D=1-exp(-dist(bdata[1:n,],method="manhattan")/134)

  H=diag(rep(1,n))-matrix(1/n,n,n)
  D=(-0.5)*H%*%as.matrix(D)%*%H/(n/2)
  rm(H); gc();

  pcas_few <- irlba(D, nu = 3, nv = 3)
  write.table(pcas_few$d, file = "eivalue2.csv")
  write.table(pcas_few$u, file = "eivector_left2.csv")
  write.table(pcas_few$v, file = "eivector_right2.csv")
  evector = read.csv("eivector_left2.csv")
  evalue = read.csv("eivalue2.csv")
}
# If not have eigen-vector and eigen-value of distance matrix yet,
# please uncomment and run the function below
# save.mds()
n = 20000
evector = read.csv("eivector_left2.csv", sep = " ")
evalue = read.csv("eivalue2.csv", sep = " ")[[1]]
evector$V1 = evector$V1*sqrt(evalue[1])
evector$V2 = evector$V2*sqrt(evalue[2])
evector$V3 = evector$V3*sqrt(evalue[3])
evector$lat = data$lat[1:n]
evector$long = data$long[1:n]

iplot(evector$long, evector$lat)
iplot(evector$V3, evector$V2)
ggplot(data = evector, aes(x = V1, y = V3)) + geom_point(alpha = 0.5, size = 0.5)
cloud(V2 ~ V1 + V2, data = evector)

# Get the matrix that denote the contribution of each column to Principal Component
a = t(as.matrix(evector)[,1:3])%*%bdata[1:20000,]
question.strength = function(x, idx)
{
  diff(c(0,cumsum(abs(x)))[idx+1])
}
q.strength1 = question.strength(a[1,],nn) # Strength of each question to PC1
q.strength2 = question.strength(a[2,],nn) # to PC2
q.strength3 = question.strength(a[3,],nn) # to PC3
plot(quest.use[['qnum']],q.strength1)

plot1 = ggplot(data=NULL, aes(x = quest.use[['qnum']], y = q.strength1)) + 
  geom_point() + ggtitle("Effect of each question on First Principal Component") +
  xlab("Question Number") + ylab("Total Absolute Weight") + 
  ylim(0,11)
plot2 = ggplot(data=NULL, aes(x = quest.use[['qnum']], y = q.strength2)) + 
  geom_point() + ggtitle("Effect of each question on Second Principal Component") +
  xlab("Question Number") + ylab("Total Absolute Weight") + 
  ylim(0,11)
plot3 = ggplot(data=NULL, aes(x = quest.use[['qnum']], y = q.strength3)) + 
  geom_point() + ggtitle("Effect of each question on Third Principal Component") +
  xlab("Question Number") + ylab("Total Absolute Weight") + 
  ylim(0,11)
grid.arrange(plot1, plot2, plot3, ncol = 1, nrow = 3)

# Cluster
set.seed()
model = kmeans(evector[c("V2","V3")], centers = 5)
# plot(evector[["V2"]], evector[["V3"]], col = model$cluster)
# xlab("Third PC"); ylab("Second PC")
evector$cluster = model$cluster
plot5 = ggplot(data=evector, aes(x = V3, y = V2)) + 
  geom_point(aes(color = factor(cluster)), size = 0.5, alpha = 0.5) +
  xlab("Third PC") + ylab("Second PC") + ggtitle("K-mean 5 clusters") + 
  theme_bw() + theme(
    plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,panel.border = element_blank()
   ,legend.position = "none"
   ,axis.line=element_blank()
   ,axis.text.x=element_blank()
   ,axis.text.y=element_blank()
   ,axis.ticks=element_blank()
   ,axis.title.x=element_blank()
   ,axis.title.y=element_blank()
  )

grid.arrange(plot2, plot3, plot4, plot5, ncol = 2, nrow = 2, main = "Different Number of Cluster")


plot1 = ggplot(data=evector, aes(x = long, y = lat)) + 
  geom_point(aes(color = factor(cluster)), size = 0.7, alpha = 0.5) +
  geom_polygon(data = state_df, colour = "grey", fill = NA, aes(group = group), alpha = 0.5, size = 0.4) +
  ggtitle("K-mean Five Clusters") +
  theme_bw() + theme(
    plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,panel.border = element_blank()
   ,legend.position = "none"
   ,axis.line=element_blank()
   ,axis.text.x=element_blank()
   ,axis.text.y=element_blank()
   ,axis.ticks=element_blank()
   ,axis.title.x=element_blank()
   ,axis.title.y=element_blank()
  )
grid.arrange(plot1, plot2, ncol = 2, nrow = 1)