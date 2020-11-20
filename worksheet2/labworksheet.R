setwd("/home/rishiddh/R/PhD_course_work/CES/KI_QuantativeEcology/worksheet")

a<- c(male="red", female="green", juvenile="blue") ## assign names to elements in a vector
b<- seq(from=5, to = 12) ## assign the sequence 5:12 to b
p<- rep(c(1,3), times=10) ## repeat the sequence c(1,3) ten times

b[c(2,4)] ## values of the second and fourth elements
d<-b[-1] ## assign all of b except first element to d
a["male"]

### Practice1 ###
j<- runif(20)
j[c(2,12)]
length(j[j<0.5])
###

trait.x<- rnorm(n=20, mean=1, sd=0.5)
mean(trait.x)

dat.m<- matrix(c(1,2,3,20,21,22), nrow = 2, ncol = 3, byrow = T)
rownames(dat.m)<- c("R1","R2")
colnames(dat.m)<- c("C1", "C2","C3")

dat.m[1,1]
dat.m[-1,] ## all elements except in row 1
dat.m[2,] ## all elements in row 2
dat.m[,2] ## all elements in column 2
dat.m["R1","C3"]
positions<- matrix(ncol = 2, byrow = T, c(1,1,2,3))
positions
dat.m[positions]

dat<- data.frame(mass=rnorm(n=16, mean=10, sd=1), sex=rep(c("m", "f"), times=8), call.rate=rnorm(16, 15, 2))
str(dat)
names(dat)
dat$mass
with(dat, mass)

dat[1,1]
dat[,1]
dat[1,]
dat[,"mass"]

subset(dat, sex=="m")
dat[dat$sex=="m",]

dat[dat$sex=="m","mass"]
subset(dat, mass<mean(mass))

### Practice2 ###
mean(dat[dat$sex=="f","call.rate"])


##### Power Analysis #####

p1 <- 0.1 ## prob. Of ‘success’ for group 1
p2 <- 0.2 ## prob. of ‘success’ for group 2
nvec <- seq(20, 400, by=20) ## vector of sample sizes
reps <- 1000 ## number of iterations
power.out <- numeric(length(nvec)) ## vector to store power estimates
names(power.out) <- nvec
for (i in 1:length(nvec)){## cycle over sample sizes
  out1 <- rbinom(reps, nvec[i], p1)## gives a vector of ## "successes", one for each replicate
  out2 <- rbinom(reps, nvec[i], p2)
  out <- cbind(out1, out2)## bind together successes for ## each group
  pvals <- numeric(reps)## vector to store p values for each iteration
  for (r in 1:nrow(out)) {## cycle over replicates
    pvals[r] <- prop.test(out[r,], c(nvec[i], nvec[i]))$p.value
  }
  power.out[i] <- length(pvals[pvals <= 0.05])/reps## calculate power for each sample size
  #print(power.out[i])## optional -- to keep track of progress
}
plot(nvec, power.out)

print(pvals)