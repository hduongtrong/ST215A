library(microbenchmark)
library("irlba")


load("b_data.rda")
n= 20000
m= 468
D=1-exp(-dist(bdata[1:n,],method="manhattan")/134)

H=diag(rep(1,n))-matrix(1/n,n,n)
D=(-0.5)*H%*%as.matrix(D)%*%H/(n/2)
rm(H); gc();

pcas_few <- irlba(D, nu = 3, nv = 3)
write.table(pcas_few$d, file = "eivalue2.csv")
write.table(pcas_few$u, file = "eivector_left2.csv")
write.table(pcas_few$v, file = "eivector_right2.csv")
