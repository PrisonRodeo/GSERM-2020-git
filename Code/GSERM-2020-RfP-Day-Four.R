##############################################
# Code GSERM "Regression for Publishing"
# (June 2020)
#
# Day Four
#
# File created 6/17/2020
#
# File last modified 6/17/2020
######################################
# setwd() first:
#
# setwd("~/Dropbox (Personal)/GSERM/Materials 2020") # or whatever...

library(RCurl)    # <-- install all these as necessary
library(stargazer)# <--
library(MASS)     # <--
library(mfx)      # <--
library(car)      # <-- 
library(rms)      # <-- 
library(plm)      # <-- 
library(lmtest)   # <-- 
library(dplyr)    # <-- 
library(oddsratio)# <--
library(mlogit)   # <-- 
library(VGAM)     # <-- 
library(aod)      # <-- 

##########################################
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

##############################################
# Discrete-Choice Models
##############################################

# Models for Ordinal Responses

# Ordered sim:

set.seed(7222009)
X<-runif(1000,0,10)
Ystar<-0 + 1*X + rnorm(1000)
Y1<-Ystar
Y1[Ystar<2.5]<-1
Y1[Ystar>=2.5 & Ystar<5]<-2
Y1[Ystar>=5 & Ystar<7.5]<-3
Y1[Ystar>=7.5]<-4
table(Y1)

summary(lm(Ystar~X))
summary(lm(Y1~X))

pdf("OrdinalOneR.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(X,Ystar,pch=20,xlab="X",ylab="Y*")
abline(lm(Ystar~X),lwd=3,col="red")
abline(h=c(2.5,5,7.5),lty=2)
plot(X,Y1,pch=20,xlab="X",ylab="Y1")
abline(lm(Y1~X),lwd=3,col="red")
dev.off()

Y2<-Ystar
Y2[Ystar<2]<-1
Y2[Ystar>=2 & Ystar<8]<-2
Y2[Ystar>=8 & Ystar<9]<-3
Y2[Ystar>9]<-4
table(Y2)

summary(lm(Y2~X))

pdf("OrdinalTwoR.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(X,Ystar,pch=20,xlab="X",ylab="Y*")
abline(lm(Ystar~X),lwd=3,col="red")
abline(h=c(2,8,9),lty=2)
plot(X,Y2,pch=20,xlab="X",ylab="Y2")
abline(lm(Y2~X),lwd=3,col="red")
dev.off()

# Best Example Ever...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/Beer.csv")
beer<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(beer)

beer.logit<-polr(as.factor(quality)~price+calories+craftbeer
                 +bitter+malty,data=beer)
summary(beer.logit)

beer.probit<-polr(as.factor(quality)~price+calories+craftbeer+
                    bitter+malty,data=beer,method="probit")
summary(beer.probit)

# Profile-likelihood-based CIs:

CIs.logit <- confint(beer.logit)

# Compare to normal CIs:

CIs.alt <- cbind(beer.logit$coefficients-1.96*sqrt(diag(vcov(beer.logit)))[1:5],
                 beer.logit$coefficients+1.96*sqrt(diag(vcov(beer.logit)))[1:5])

CIs.logit
CIs.alt

# Odds Ratios

olreg.or <- function(model) { 
  coeffs <- coef(summary(beer.logit)) 
  lci <- exp(coeffs[ ,1] - 1.96 * coeffs[ ,2]) 
  or <- exp(coeffs[ ,1]) 
  uci <- exp(coeffs[ ,1] + 1.96 * coeffs[ ,2]) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

olreg.or(beer.logit)

# Predicted probs

calories<-seq(60,200,1)
price<-mean(beer$price)
craftbeer<-median(beer$craftbeer)
bitter<-mean(beer$bitter)
malty<-mean(beer$malty)
beersim<-cbind(calories,price,craftbeer,bitter,malty)
beer.hat<-predict(beer.logit,beersim,type='probs')

pdf("ROrdinalProbs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(c(60,200), c(0,1), type='n', xlab="Calories", ylab='Fitted 
  Probability')
lines(60:200, beer.hat[1:141, 1], lty=1, lwd=3)
lines(60:200, beer.hat[1:141, 2], lty=2, lwd=3)
lines(60:200, beer.hat[1:141, 3], lty=3, lwd=3)
lines(60:200, beer.hat[1:141, 4], lty=4, lwd=3)
dev.off()

# Cumulative probs:

xaxis<-c(60,60:200,200)
yaxis1<-c(0,beer.hat[,1],0)
yaxis2<-c(0,beer.hat[,2]+beer.hat[,1],0)
yaxis3<-c(0,beer.hat[,3]+beer.hat[,2]+beer.hat[,1],0)
yaxis4<-c(0,beer.hat[,4]+beer.hat[,3]+beer.hat[,2]+beer.hat[,1],0)

pdf("ROrdinalCumProbs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(c(60,200), c(0,1), type='n', xlab="Calories", 
     ylab="Cumulative Probability")
polygon(xaxis,yaxis4,col="white")
polygon(xaxis,yaxis3,col="grey80")
polygon(xaxis,yaxis2,col="grey50")
polygon(xaxis,yaxis1,col="grey10")
dev.off()

# Variants: Series of Binary Regressions:

beer$goodplus<-as.factor(beer$quality>1)
beer.good<-glm(goodplus~price+calories+craftbeer+bitter+
                 malty,family="binomial",data=beer)
summary(beer.good)

beer$VGplus<-as.factor(beer$quality>2)
beer.VG<-glm(VGplus~price+calories+craftbeer+bitter+malty,
             family="binomial",data=beer)
summary(beer.VG)


##############################################
# Models for Unordered Responses

# MNL, etc.

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/Election1992small.csv")
nes92<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(nes92)

nes92.mlogit<-vglm(presvote~partyid, multinomial, nes92)
summary(nes92.mlogit)

Bush.nes92.mlogit<-vglm(formula=presvote~partyid, 
                        family=multinomial(refLevel=1),data=nes92) 
summary(Bush.nes92.mlogit)

Clinton.nes92.mlogit<-vglm(formula=presvote~partyid,
                           family=multinomial(refLevel=2),data=nes92)
summary(Clinton.nes92.mlogit)

# CL

colnames(nes92)<-c("caseid","presvote","partyid","FT.Bush","FT.Clinton","FT.Perot")
nes92$PVote<-factor(nes92$presvote,labels=c("Bush","Clinton","Perot"))
head(nes92)

nes92CL<-mlogit.data(nes92,shape="wide",choice="PVote",varying=4:6)
head(nes92CL,6)

# CL regression:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# Interpretation:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/Election1992.csv")
BigNES92<-read.csv(text=temp, header=TRUE)
rm(temp)


NES.MNL<-vglm(presvote~partyid+age+white+female,data=BigNES92,
              multinomial(refLevel=1))
summaryvglm(NES.MNL)


wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(5,6))
wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(1,3,5,7,9))

# Hats

PickBush<-ifelse(fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,2] 
                 & fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,3], 1,0)
PickWJC<-ifelse(fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,3], 2, 0)
PickHRP<-ifelse(fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,2], 3, 0)
OutHat<-PickBush+PickWJC+PickHRP
table(BigNES92$presvote,OutHat)

# Odds ratios:

mnl.or <- function(model) { 
  coeffs <- c(t(coef(NES.MNL))) 
  lci <- exp(coeffs - 1.96 * diag(vcov(NES.MNL))^0.5) 
  or <- exp(coeffs) 
  uci <- exp(coeffs + 1.96* diag(vcov(NES.MNL))^0.5) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

mnl.or(NES.MNL)

# In-sample predictions:

hats<-as.data.frame(fitted.values(NES.MNL))
names(hats)<-c("Bush","Clinton","Perot")
attach(hats)

pdf("InSampleRScatterplotMatrix.pdf",8,7)
spm(~Bush+Clinton+Perot,pch=20,plot.points=TRUE,
    diagonal="histogram",col=c("black","grey"))
dev.off()

pdf("InSampleMNLPredProbsR.pdf",8,6)
par(mfrow=c(1,3))
plot(BigNES92$partyid,Bush,xlab="Party ID")
plot(BigNES92$partyid,Clinton,xlab="Party ID")
plot(BigNES92$partyid,Perot,xlab="Party ID")
par(mfrow=c(1,1))
dev.off()

# CL example:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# In-sample predictions:

CLhats<-predict(nes92.clogit,nes92CL)

pdf("InSampleCLHatsR.pdf",7,6)
plot(nes92$FT.Bush,CLhats[,1],pch=19,
     col=rgb(100,0,0,100,maxColorValue=255),
     xlab="Feeling Thermometer",
     ylab="Predicted Probability")
points(nes92$FT.Clinton+runif(nrow(CLhats),-1,1),
       CLhats[,2],pch=4,col=rgb(0,0,100,100,maxColorValue=255))
points(nes92$FT.Perot+runif(nrow(CLhats),-1,1),
       CLhats[,3],pch=17,col=rgb(0,100,0,50,maxColorValue=255))
lines(lowess(nes92$FT.Bush,CLhats[,1]),lwd=2,col="red")
lines(lowess(nes92$FT.Clinton,CLhats[,2]),lwd=2,col="blue")
lines(lowess(nes92$FT.Perot,CLhats[,3]),lwd=2,col="darkgreen")
legend("topleft",bty="n",c("Bush","Clinton","Perot"),
       col=c("red","blue","darkgreen"),pch=c(19,4,17))
dev.off()


##############################################
# Event Count Models
##############################################

# Various Poisson histograms

set.seed(7222009)
N<-1000
LP05<-rpois(N,0.5)
LP1<-rpois(N,1)
LP5<-rpois(N,5)
LP10<-rpois(N,10)

pdf("PoissonHistogramsR.pdf",7,6)
par(mfrow=c(2,2))
hist(LP05,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 0.5")
hist(LP1,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 1.0")
hist(LP5,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 5")
hist(LP10,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 10")
dev.off()

# Get SCOTUS nullifications data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/nulls.csv")
Nulls<-read.csv(text=temp, header=TRUE)
rm(temp)

# Histogram:

pdf("NullsHist.pdf",6,5)
par(mar=c(4,4,2,2))
with(Nulls, 
     hist(nulls,main="",xlab="Number of Nullifications",
          col="grey"))
dev.off()

# Poisson regression:

nulls.poisson<-glm(nulls~tenure+unified,family="poisson",
                   data=Nulls)
summary(nulls.poisson)

# IRRs:

nulls.poisson.IRR<-poissonirr(nulls~tenure+unified,
                              data=Nulls)
nulls.poisson.IRR

# Predictions:

tenure<-seq(0,20,1)
unified<-1
simdata<-as.data.frame(cbind(tenure,unified))
nullhats<-predict(nulls.poisson,newdata=simdata,se.fit=TRUE)

# NOTE: These are XBs, not predicted counts.
# Transforming:

nullhats$Yhat<-exp(nullhats$fit)
nullhats$UB<-exp(nullhats$fit + 1.96*(nullhats$se.fit))
nullhats$LB<-exp(nullhats$fit - 1.96*(nullhats$se.fit))

# Plot...

pdf("NullsOutOfSampleHatsR.pdf",6,5)
plot(simdata$tenure,nullhats$Yhat,t="l",lwd=3,ylim=c(0,5),ylab=
       "Predicted Count", xlab="Mean Tenure")
lines(simdata$tenure,nullhats$UB,lwd=2,lty=2)
lines(simdata$tenure,nullhats$LB,lwd=2,lty=2)
dev.off()

# Predicted probabilities...

counts <- c(0,1,2,3) # insert count values for which you
                     # want predicted probabilities

pdf("PoissonPredictedProbabilities.pdf",7,6)
par(mar=c(4,4,2,2))
plot(simdata$tenure,dpois(counts[1],nullhats$Yhat),t="l",
     lwd=0,ylim=c(0,1),ylab="Predicted Probability",
     xlab="Mean Tenure")
for(i in 1:length(counts)){  # looping over elements of "count"
  lines(simdata$tenure,dpois(counts[i],nullhats$Yhat),lwd=3,lty=i,
        col=i)
legend("topright",bty="n",legend=c(paste0("Y=",counts[1]),
                                   paste0("Y=",counts[2]),
                                   paste0("Y=",counts[3]),
                                   paste0("Y=",counts[4])),
       col=seq(1:length(counts)),lty=seq(1:length(counts)),
       lwd=3)
 }
dev.off()

# Offsets with dyadic data...Aggregated counts
# of conflicts between the countries in each
# dyad, 1950-1985...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/offsetIR.csv")
IR<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(IR)

cor(IR,use="complete.obs")

IR.fit1<-glm(disputes~allies+openness,data=IR,family="poisson")
summary(IR.fit1)

IR.fit2<-glm(disputes~allies+openness,data=IR,family="poisson",
             offset=log(Ndyads))
summary(IR.fit2)

IR.fit3<-glm(disputes~allies+openness+log(Ndyads),data=IR,
             family="poisson")
summary(IR.fit3)

# z-test:
2*pnorm((0.811-1)/.071)

# Wald test:
wald.test(b=coef(IR.fit3),Sigma=vcov(IR.fit3),Terms=4,H0=1)

