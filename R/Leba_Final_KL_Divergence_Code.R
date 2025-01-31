library(raster)
library(rgdal)
library(tripack)
library(manipulate)
library(clhs)
library(entropy)

#data set (spatial data covariates)
df<- All_Covariates
str(df)


####################################################################

#Quantiles of the population (this is for test 3)
# Number of bins
nb<- 25

n_c = ncol(df)-2  ### ADDED

#quantile matrix (of the covariate data)
q.mat<- matrix(NA, nrow=(nb+1), ncol= n_c) ### CHANGED NCOL
j=1
for (i in 3:ncol(df)){ #note the index start here
  #get a quantile matrix together of the covariates
  ran1<- max(df[,i]) - min(df[,i])
  step1<- ran1/nb
  q.mat[,j]<- seq(min(df[,i]), to = max(df[,i]), by =step1)
  j<- j+1
}
q.mat



#covariate data hypercube (this is for test 4)
## This takes a while to do so only do it once if you can

cov.mat<- matrix(1, nrow=nb, ncol=n_c) ### CHANGED NCOL
for (i in 1:nrow(df)){ # the number of pixels
  cntj<- 1
  for (j in 3:ncol(df)){ #for each column
    dd<- df[i,j]  
    for (k in 1:nb){  #for each quantile
      kl<- q.mat[k, cntj]
      ku<- q.mat[k+1, cntj]
      if (dd >= kl & dd <= ku){cov.mat[k, cntj]<- cov.mat[k, cntj] + 1}
    }
    cntj<- cntj+1
  }
}

cov.mat
####################################################################





#######################################################################
#How many samples do we need?
#beginning of algorithm

#initial settings
cseq<- seq(1,500,1) # cLHC sample size
its<-10  # number internal iterations with each sample size number
mat.seq<- matrix(NA,ncol=2,nrow=length(cseq)) #empty matix for outputs

# Initialize a list to store samples
samples_list <- vector("list", length(cseq))

for (w in 1:length(cseq)){ # for every sample number configuration....
  print(w) ### ADDED TO SHOW PROGRESS
  s.size=cseq[w]  # sample size
  mat.f<- matrix(NA,ncol=10,nrow=its ) # placement for iteration outputs
  
  #internal loop
  for (j in 1:its){ #Note that this takes quite a while to run to completion
    repeat{
      ss <- clhs(df[,3:n_c], size = s.size, progress = T, iter = 1000) # Do a conditioned latin hypercube sample ### CHANGED n FROM 6 TO n_c ### CHANGED ITER FROM 100000 TO 1000
      s.df<- df[ss,]
      if (sum(duplicated(s.df) | duplicated(s.df[nrow(s.df):1, ])[nrow(s.df):1]) < 2)
      {break}}
    
    # Save the sample in the list
    if (is.null(samples_list[[w]])) {
      samples_list[[w]] <- list()
    }
    samples_list[[w]][[j]] <- s.df
    
    #print((samples_list[[17]][[1]]))
#write.csv(samples_list[[187]][[1]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(1).csv', col.names = TRUE, row.names = FALSE, sep = ",")
#write.csv(samples_list[[187]][[2]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(2).csv', col.names = TRUE, row.names = FALSE, sep = ",")
#write.csv(samples_list[[187]][[3]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(3).csv', col.names = TRUE, row.names = FALSE, sep = ",")
#write.csv(samples_list[[187]][[4]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(4).csv', col.names = TRUE, row.names = FALSE, sep = ",")
#write.csv(samples_list[[187]][[5]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(5).csv', col.names = TRUE, row.names = FALSE, sep = ",")
#write.csv(samples_list[[187]][[6]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(6).csv', col.names = TRUE, row.names = FALSE, sep = ",")
#write.csv(samples_list[[187]][[7]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(7).csv', col.names = TRUE, row.names = FALSE, sep = ",")
#write.csv(samples_list[[187]][[8]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(8).csv', col.names = TRUE, row.names = FALSE, sep = ",")
#write.csv(samples_list[[187]][[9]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(9).csv', col.names = TRUE, row.names = FALSE, sep = ",")
#write.csv(samples_list[[187]][[10]], file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/Sample List2/sampled_points(10).csv', col.names = TRUE, row.names = FALSE, sep = ",")
    
    ## Fourth test: Kullback-Leibler (KL) divergence
    ####Compare whole study area covariate space with the slected sample
    #sample data hypercube (essentially the same script as for the grid data but just doing it on the sample data)
    h.mat<- matrix(1, nrow=nb, ncol=n_c) ### CHANGED NCOL FROM 4 TO N_C
    
    for (ii in 1:nrow(s.df)){ # the number of observations
      cntj<- 1
      for (jj in 3:ncol(s.df)){ #for each column
        dd<- s.df[ii,jj]  
        for (kk in 1:nb){  #for each quantile
          kl<- q.mat[kk, cntj]
          ku<- q.mat[kk+1, cntj]
          if (dd >= kl & dd <= ku){h.mat[kk, cntj]<- h.mat[kk, cntj] + 1}
        }
        cntj<- cntj+1}
    }
    
    #h.mat
    #Kullback-Leibler (KL) divergence
    klo.1<- KL.empirical(c(cov.mat[,1]), c(h.mat[,1])) #1
    klo.2<- KL.empirical(c(cov.mat[,2]), c(h.mat[,2])) #2
    klo.3<- KL.empirical(c(cov.mat[,3]), c(h.mat[,3])) #3
    klo.4<- KL.empirical(c(cov.mat[,4]), c(h.mat[,4])) #4
    klo.5<- KL.empirical(c(cov.mat[,5]), c(h.mat[,5])) #5
    klo.6<- KL.empirical(c(cov.mat[,6]), c(h.mat[,6])) #6
    klo.7<- KL.empirical(c(cov.mat[,7]), c(h.mat[,7])) #7
    klo.8<- KL.empirical(c(cov.mat[,8]), c(h.mat[,8])) #8
    klo.9<- KL.empirical(c(cov.mat[,9]), c(h.mat[,9])) #9
    klo.10<- KL.empirical(c(cov.mat[,10]), c(h.mat[,10])) #10
    klo.11<- KL.empirical(c(cov.mat[,11]), c(h.mat[,11])) #11
    klo.12<- KL.empirical(c(cov.mat[,12]), c(h.mat[,12])) #12
    klo.13<- KL.empirical(c(cov.mat[,13]), c(h.mat[,13])) #13
    klo.14<- KL.empirical(c(cov.mat[,14]), c(h.mat[,14])) #14
    klo.15<- KL.empirical(c(cov.mat[,15]), c(h.mat[,15])) #15
    klo.16<- KL.empirical(c(cov.mat[,16]), c(h.mat[,16])) #16
    klo.17<- KL.empirical(c(cov.mat[,17]), c(h.mat[,17])) #17
    klo.18<- KL.empirical(c(cov.mat[,18]), c(h.mat[,18])) #18
    klo.19<- KL.empirical(c(cov.mat[,19]), c(h.mat[,19])) #19
    klo.20<- KL.empirical(c(cov.mat[,20]), c(h.mat[,20])) #20
    klo.21<- KL.empirical(c(cov.mat[,21]), c(h.mat[,21])) #21
    klo.22<- KL.empirical(c(cov.mat[,22]), c(h.mat[,22])) #22
    klo.23<- KL.empirical(c(cov.mat[,23]), c(h.mat[,23])) #23
    klo.24<- KL.empirical(c(cov.mat[,24]), c(h.mat[,24])) #24
    klo.25<- KL.empirical(c(cov.mat[,25]), c(h.mat[,25])) #25
    klo.26<- KL.empirical(c(cov.mat[,26]), c(h.mat[,26])) #26
    klo.27<- KL.empirical(c(cov.mat[,27]), c(h.mat[,27])) #27
    klo.28<- KL.empirical(c(cov.mat[,28]), c(h.mat[,28])) #28
    klo.29<- KL.empirical(c(cov.mat[,29]), c(h.mat[,29])) #29
    klo.30<- KL.empirical(c(cov.mat[,30]), c(h.mat[,30])) #30
    klo.31<- KL.empirical(c(cov.mat[,31]), c(h.mat[,31])) #31
    klo.32<- KL.empirical(c(cov.mat[,32]), c(h.mat[,32])) #32
    klo.33<- KL.empirical(c(cov.mat[,33]), c(h.mat[,33])) #33
    klo.34<- KL.empirical(c(cov.mat[,34]), c(h.mat[,34])) #34
    klo<- mean(c(klo.1,klo.2,klo.3,klo.4,klo.5,klo.6,klo.7,klo.8,klo.9,klo.10,klo.11,klo.12,klo.13,klo.14,klo.15,klo.16,klo.17,klo.18,klo.19,klo.20,klo.21,klo.22,klo.23,klo.24,klo.25,klo.26,klo.27,klo.28,klo.29,klo.30,klo.31,klo.32,klo.33,klo.34))    
    mat.f[j,10]<- klo  # value of 0 means no divergence
  }
  
  
  #arrange outputs
  mat.seq[w,1]<-mean(mat.f[,10])
  mat.seq[w,2]<-sd(mat.f[,10])} ## END of LOOP

dat.seq<- as.data.frame(cbind(cseq,mat.seq))
names(dat.seq)<- c("samp_nos","mean_KL","sd_KL")
##########################################################
# Calculate normalized KL values
min_KL <- min(dat.seq$mean_KL)
max_KL <- max(dat.seq$mean_KL)
dat.seq$normalized_KL <- 1 - (dat.seq$mean_KL - min_KL) / (max_KL - min_KL)

# Print the normalized KL values
print(dat.seq)
write.csv(dat.seq, file = 'C:/Users/moleb/Desktop/PhD/Objective_2/Cosmos Data/meanKL&NormalizedKL500.csv', col.names = TRUE, row.names = FALSE, sep = ",")

#######################################################  
#plot some outputs
#plot(cseq,mat.seq[,1], xlab="number of samples", ylab= "similarity between covariates (entire field) with covariates (sample)",main="Population and sample similarity")
#plot(cseq,mat.seq[,2],xlab="number of samples", ylab= "standard deviation similarity between covariates (entire field) with covariates (sample)",main="Population and sample similarity (sd)")
#plot(cseq,mat.seq[,3])
#plot(cseq,mat.seq[,4])
#plot(cseq,mat.seq[,5],xlab="number of samples", ylab= "percentage of total covariate variance of population account for in sample",main="Population and sample similarity")
#plot(cseq,mat.seq[,6],xlab="number of samples", ylab= "standard deviation of percentage of total covariate variance of population account for in sample",main="Population and sample similarity")
plot(cseq,mat.seq[,1],xlab="number of samples", ylab= "KL divergence")
#plot(cseq,mat.seq[,8],xlab="number of samples", ylab= "standard deviation of percentage of total covariate variance of population account for in sample",main="Population and sample similarity")
write.table(dat.seq, "Nav_datseq_clHC.txt", col.names=T, row.names=FALSE, sep=",")  # Save output to text file
##########################################################



##########################################################
# make an exponetial decay function (of the KL divergence)
x<- dat.seq$samp_nos
y = 1-(dat.seq$mean_KL-min(dat.seq$mean_KL))/(max(dat.seq$mean_KL)-min(dat.seq$mean_KL)) ##(dat.seq$mean_PIP-min(dat.seq$mean_PIP))/(max(dat.seq$mean_PIP)-min(dat.seq$mean_PIP)) #PIP
### CHANGED Y: FROM NORMALIZED PIP TO NORMALIZED KL

#Parametise Exponential decay function
plot(x, y, xlab="sample number", ylab= "1 - KL similarity")          # Initial plot of the data ## CHANGED LABEL FROM PC TO KL
start <- list()     # Initialize an empty list for the starting values

#fit 1
manipulate(
  {
    plot(x, y)
    k <- kk; b0 <- b00; b1 <- b10
    curve(k*exp(-b1*x) + b0, add=TRUE)
    start <<- list(k=k, b0=b0, b1=b1)
  },
  kk=slider(0, 5, step = 0.01,  initial = 2),
  b10=slider(0, 1, step = 0.000001, initial = 0.01),
  b00=slider(0,1 , step=0.000001,initial= 0.01))

fit1 <- nls(y ~ k*exp(-b1*x) + b0, start = start)
summary(fit1)
lines(x, fitted(fit1), col="red")
#############################################################################
#Apply fit
xx<- seq(1,500,1)
lines(xx, predict(fit1,list(x=xx)))

jj<- predict(fit1,list(x=xx))
normalized = 1- (jj-min(jj))/(max(jj)-min(jj))

x<- xx
y<- 1-normalized ### CHANGED: Y=NORM TO Y=1-NORM


plot(x, y, xlab="sample number", ylab= "normalised KL", type="l", lwd=2)  # Initial plot of the data ### CHANGE PIP TO KL

### NEEDS TO BE CHANGED:
x1<- c(-1, 500); y1<- c(0.2, 0.2)
lines(x1,y1, lwd=2, col="red")

x2<- c(100, 100); y2<- c(0, 1)
lines(x2,y2, lwd=2, col="red")
