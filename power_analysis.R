# Analysis of simulated data following the hypothesized DGP of rationalization 
# of party choice through missreporting of ideological positions
# Author: Fridolin Linder

rm(list=ls())
library(ggplot2)

### Function simulating the data generating process for simple case:
### Only self position is biased and party position is assumed to be 
### unbiased (see proposal manuscript for definition of unbiasedness)
#
## Arguments:
# N: number of samples
# md: mean of distribution from which the bias parameter is drawn
# p1,p2: fixed 'objective' positions of parties 1 and 2
# v: standard deviation of voters perception error for the party positions
# 
## The DGP wotks as follows
# Voters Ideal points drawn from ~truncN(.5,.3,0,1)
# Their perception of the two parties' positions is drawn from N(p1,v),N(p2,v)
# Their prefered party is selected by minimizing the distance between self and 
# perceived party position
# Subjects in the treatment condition report biased selfpositions where the individual
# bias is calculated by drawing a bias parameter d from truncN(md,.05,0,1), then
# rs_i = ts_i + d_i*(pp_i-ts_i), where, rs: reported self, ts: true self, 
# pp: perceived party and i = 1,...,N are the subjects
#
# 

genData = function(N,md=0.5,p1=0.2,p2=0.8,v=0.1){
  require(truncnorm)
  ts = rtruncnorm(N,0,1,0.5,0.3) # true self position
  pp = cbind(rtruncnorm(N,0,1,p1,v),rtruncnorm(N,0,1,p2,v)) # perc part pos
  dtf = apply(pp-ts,1,function(x) x[which.min(abs(x))]) # distance to favorite
  fp = apply(abs(pp-ts),1,which.min) # favorite party
  d = c(rnorm(N/2,md,0.05),rep(0,N/2)) # Bias par 0 for control group
  rs = ts + d*dtf # reported self = true self + bias
  rp = apply(cbind(pp,fp),1,function(x) x[x[3]])
  data.frame('rep_self'=rs,'rep_party'=rp,'fav_party'=fp
             ,'group'=c(rep(1,N/2),rep(0,N/2)))
}

# Demo
N = 1e4
X = genData(N)
dist = abs(X$rep_self-X$rep_party)
hist(dist[X$group==0],breaks=30,col='grey')
hist(dist[X$group==1],breaks=30,col='grey')
mean(dist[X$group==1])/mean(dist[X$group==0])


### Power analysis by simulation

# Assumed effect size (in terms of md)
md = as.list(seq(0.05,1,by=0.05))
N = 100
B = 1000

# Function to generate data and calcualte difference in mean abs distance
diff = function(md,N,ratio=T){
  X = genData(N,md=md)
  dist = abs(X$rep_self-X$rep_party)
  if(!ratio) mean(dist[X$group==0]) - mean(dist[X$group==1])
    else mean(dist[X$group==1])/mean(dist[X$group==0])
}

# Sample difference
sampDiff = function(B,N,md) sapply(rep(md,B),diff,N=N)

# Get dist'ns for different md vals
distns = lapply(md,sampDiff1,B=B,N=N)
