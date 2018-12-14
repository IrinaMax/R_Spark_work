#https://www.r-bloggers.com/ab-testing-in-r-%E2%80%93-part-1/

site1 = c(.40, 500) # pink
site2 = c(.30, 550) # black

abtestfunc <- function(ad1, ad2){
  sterror1 = sqrt( ad1[1] * (1-ad1[1]) / ad1[2] )
  sterror2 = sqrt( ad2[1] * (1-ad2[1]) / ad2[2] )
  minmax1 = c((ad1[1] - 1.28*sterror1) * 100, (ad1[1] + 1.28*sterror1) * 100)
  minmax2 = c((ad2[1] - 1.28*sterror2) * 100, (ad2[1] + 1.28*sterror2) * 100)
  print( round(minmax1,2) )
  print( round(minmax2,2) )
}

abtestfunc(site1, site2)

# my code confidence interval 95% 
sample1 = c(.28, 5000) # sample 1 with 18% and 5000 people
sample2 = c(.23, 7400) # sample 2 with 15% but 5400 people

abtestfunc <- function(ad1, ad2){
  sqRs1 = sqrt( ad1[1] * (1-ad1[1]) / ad1[2] )
  sqRs2 = sqrt( ad2[1] * (1-ad2[1]) / ad2[2] )
  minmax1 = c((ad1[1] - 1.96*sqRs1) * 100, (ad1[1] + 1.96*sqRs1) * 100)
  minmax2 = c((ad2[1] - 1.96*sqRs2) * 100, (ad2[1] + 1.96*sqRs2) * 100)
  print( round(minmax1,2) )
  print( round(minmax2,2) )
}

abtestfunc(sample1, sample2)


# http://www.win-vector.com/blog/2014/05/a-clear-picture-of-power-and-significance-in-ab-tests/
# https://www.r-bloggers.com/a-clear-picture-of-power-and-significance-in-ab-tests/
library(gtools)
library(ggplot2)

# q>p, compute the probability of a 
# p-rate process measuring as q-rate 
# or better in n steps
pSignificanceError <- function(p,q,n) {
  pbinom(ceiling(q*n)-1,prob=p,size=n,lower.tail=FALSE)
}

# q>p, compute the proability of a
# q-rate process measuring as p-rate 
# or lower in n steps
pPowerError <- function(p,q,n) {
  pbinom(floor(p*n),prob=q,size=n,lower.tail=TRUE)
}

designExperiment <- function(pA,pB,pError,pAUpper=pB,pBLower=pA) {
  aSoln <- binsearch(
    function(k) {
      pSignificanceError(pA,pAUpper,k) - pError},
    range=c(100,1000000))
  nA <- max(aSoln$where)
  print(paste('nA',nA))
  
  bSoln <- binsearch(
    function(k) {
      pPowerError(pBLower,pB,k) - pError},
    range=c(100,1000000))
  nB <- max(bSoln$where)
  print(paste('nB',nB))
  
  
  low = floor(min(pA*nA,pB*nB))
  high = ceiling(max(pA*nA,pB*nB))
  width = high-low
  countRange <- (low-width):(high+width)
  
  dA <- data.frame(count=countRange)
  dA$group <- paste('A: sample size=',nA,sep='')
  dA$density <- dbinom(dA$count,prob=pA,size=nA)
  dA$rate <- dA$count/nA
  dA$error <- dA$rate>=pAUpper
  dB <- data.frame(count=countRange)
  dB$group <- paste('B: sample size=',nB,sep='')
  dB$density <- dbinom(dB$count,prob=pB,size=nB)
  dB$rate <- dB$count/nB
  dB$error <- dB$rate<=pBLower
  d <- rbind(dA,dB)
  
  plot = ggplot(data=d,aes(x=rate,y=density)) +
    geom_line() +
    geom_ribbon(data=subset(d,error),
                aes(ymin=0,ymax=density),fill='red') + 
    facet_wrap(~group,ncol=1,scales='free_y') +
    geom_vline(xintercept=pAUpper,linetype=2) +
    geom_vline(xintercept=pBLower,linetype=2)
  list(nA=nA,nB=nB,plot=plot)
}


r1 <- designExperiment(pA=0.005,pB=0.006,pError=0.01)
print(r1$plot)

r2 <- designExperiment(pA=0.005,pB=0.006,pError=0.01,pAUpper=0.0055,pBLower=0.0055)
print(r2$plot)

r3 <- designExperiment(pA=0.005,pB=0.006,pError=0.005,pAUpper=0.0055,pBLower=0.0055)

