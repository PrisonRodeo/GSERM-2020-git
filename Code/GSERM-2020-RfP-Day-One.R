##############################################
# Code GSERM "Regression for Publishing"
# (June 2020)
#
# File created 6/14/2020
#
# File last modified 6/14/2020
######################################
# setwd() first:
#
# setwd("~/Dropbox (Personal)/GSERM/Materials 2020") # or whatever...

library(RCurl) # <-- install as necessary
library(car)

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

###########################################
# Session 1: OLS review
###########################################

library(RCurl)

# Read data from the github repo:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/CountryData2000.csv")
IR2000<-read.csv(text=temp, header=TRUE)
rm(temp)

# I offer no warranty that any of this is the most 
# efficient way to do what I'm doing... all graphics
# are using base R graphics functionality, with
# the exception of the scatterplot matrices, which use
# the -car- package.

# Figure 1

pdf("IMbyLE.pdf",7,7) # make a PDF
par(cex=1.2) # make the things bigger
par(mar=c(4,4,2,2)) # plot margins
with(IR2000, plot(lifeexpectancy,infantmortalityperK, # draw the plot
                  pch="",xlab="Life Expectancy at Birth",
                  ylab="Infant Mortality Per 1000 Live Births"))
par(cex=0.8) # make the things smaller
# Add names:
with(IR2000, text(lifeexpectancy,infantmortalityperK,label=WBcode))
# add a linear-fit line
with(IR2000, abline(lm(infantmortalityperK~lifeexpectancy),lwd=3))
dev.off() # turn off the PDF-maker


# Figure 2 ("residual" plot)

small<-data.frame(infantmortalityperK,lifeexpectancy,WBcode)
small<-small[is.na(small$lifeexpectancy)==FALSE,]
small<-small[is.na(small$infantmortalityperK)==FALSE,]

IMLE.fit<-lm(small$infantmortalityperK~small$lifeexpectancy)
IMLE.res<-resid(IMLE.fit)

pdf("IMbyLEresids.pdf",7,7) # make a PDF
par(cex=1.2)
par(mar=c(4,4,2,2))
with(IR2000, plot(small$lifeexpectancy,IMLE.res,pch="",
                  xlab="Life Expectancy at Birth",
                  ylab="Residuals (Observed minus Expected)"))
par(cex=0.8)
with(IR2000, text(small$lifeexpectancy,IMLE.res,
                  label=small$WBcode))
abline(h=0,lwd=3)
dev.off() # turn off the PDF-maker

# Figure 3

pdf("IMbyFertility.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2) #symbol size
with(IR2000, plot(fertility,infantmortalityperK,pch="",xlab="Fertility Rate (Births Per Woman)",
                  ylab="Infant Mortality Per 1000 Live Births"))
par(cex=0.8)
with(IR2000, text(fertility,infantmortalityperK,label=WBcode))
with(IR2000, abline(lm(infantmortalityperK~fertility),lwd=3))
dev.off()

# Figure 4

pdf("IMbyGDP.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2)
with(IR2000, plot(NEWrgdpch,infantmortalityperK,pch="",xlab="Real GDP per capita",
                  ylab="Infant Mortality Per 1000 Live Births"))
par(cex=0.8)
with(IR2000, text(NEWrgdpch,infantmortalityperK,label=WBcode))
with(IR2000, abline(lm(infantmortalityperK~NEWrgdpch),lwd=3))
with(IR2000, lines(lowess(infantmortalityperK~NEWrgdpch,span=0.1,iter=1),
                   lwd=3,col="red",lty=2))
dev.off()

# Figure 5

pdf("lnIMbylnGDP.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2)
with(IR2000, plot(log(NEWrgdpch),log(infantmortalityperK),pch="",xlab="Logged Real GDP per 
       Capita",ylab="Logged Infant Mortality Per 1000 Live Births"))
par(cex=0.8)
with(IR2000, text(log(NEWrgdpch),log(infantmortalityperK),label=WBcode))
with(IR2000, abline(lm(log(infantmortalityperK)~log(NEWrgdpch)),lwd=3))
with(IR2000, lines(lowess(log(infantmortalityperK)~log(NEWrgdpch)),
                   lwd=3,lty=2,col="red"))
dev.off()

# Figure 6

pdf("IMbyPOLITY.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2)
with(IR2000, plot(polity2,infantmortalityperK,pch="",xlab="POLITY IV score",ylab="Infant 
       Mortality Per 1000 Live Births"))
par(cex=0.8)
with(IR2000, text(polity2,infantmortalityperK,label=WBcode))
with(IR2000, abline(lm(infantmortalityperK~polity2),lwd=3))
with(IR2000, lines(lowess(infantmortalityperK~polity2),lwd=3,
                   lty=2,col="red"))
dev.off()

# Figure 7

IR2000$Rich<-as.factor(IR2000$NEWrgdpch>median(IR2000$NEWrgdpch,na.rm=TRUE))

pdf("IMbyPOLITYandGDP.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2)
scatterplot(infantmortalityperK~polity2|Rich,IR2000,cex=1.5,
            cex.axis=1.5,cex.lab=1.3,boxplots="",xlab="POLITY IV", 
            ylab="Infant Mortality per 1000 Live Births",
            pch=c(16,17),legend.coords="topleft",grid=FALSE,
            reg.line=FALSE,lwd=c(3,3),col=c(1,2),lty=c(3,4))
dev.off()

############################
# Now the IM / DPT example:

# Read in data (from web):

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the data
rm(url)

# Summary statistics

# install.packages("psych") <- Install psych package, if necessary
library(psych)
with(Data, describe(infantmortalityperK))
with(Data, describe(DPTpct))

# Regression:

IMDPT<-lm(infantmortalityperK~DPTpct,data=Data,na.action=na.exclude)
summary.lm(IMDPT)

# ANOVA

anova(IMDPT)

# Scatterplot:

pdf("IMDPT.pdf",6,6) # <- create PDF
with(Data, plot(DPTpct,infantmortalityperK,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Infant Mortality (Deaths per 1000 Births)"))
with(Data, text(DPTpct,infantmortalityperK,labels=WBcode))
with(Data, abline(v=mean(DPTpct,na.rm=TRUE),lty=2))
with(Data, abline(h=mean(infantmortalityperK,na.rm=TRUE),lty=2))
abline(IMDPT,lwd=3)
dev.off()

# Residuals (u):

Data$IMDPTres <- with(Data, residuals(IMDPT))
describe(Data$IMDPTres)

# Residual density plot:

pdf("IMDPTResidualsDensity.pdf",6,6)
with(Data, plot(density(IMDPTres,na.rm=TRUE),
                main="Density Plot: Regression Residuals",
                xlab="Residual Value"))
abline(v=0,lty=2,lwd=2)
dev.off()

# Fitted Values:

Data$IMDPThat<-fitted.values(IMDPT)
describe(Data$IMDPThat)

# Densities plot:

pdf("IMDPTFittedDensity.pdf",6,6)
with(Data, plot(density(IMDPThat,na.rm=TRUE),
                main="Density Plot: Actual and Fitted Values",
                xlab="Values of Y"))
with(Data, lines(density(infantmortalityperK,na.rm=TRUE),
                 lty=2,col="red"))
with(Data, abline(v=mean(infantmortalityperK,na.rm=TRUE),
                  lty=2,lwd=2))
dev.off()

# Correlations:

with(Data, cor(infantmortalityperK,DPTpct,use="complete.obs"))
with(Data, cor(IMDPTres,infantmortalityperK,use="complete.obs"))
with(Data, cor(IMDPTres,DPTpct,use="complete.obs"))
with(Data, cor(IMDPThat,infantmortalityperK,use="complete.obs"))
with(Data, cor(IMDPThat,DPTpct,use="complete.obs"))
with(Data, cor(IMDPTres,IMDPThat,use="complete.obs"))

# Plotting residuals vs. X:

pdf("IMDPTResiduals.pdf",6,6)
with(Data, plot(DPTpct,IMDPTres,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Residuals (Y - Y-hat)"))
with(Data, text(DPTpct,IMDPTres,labels=WBcode))
abline(h=0,lwd=2)
dev.off()

# Squared residuals vs. X:

pdf("IMDPTSquaredResiduals.pdf",6,6)
with(Data, plot(DPTpct,IMDPTres^2,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Residuals (Y - Y-hat)"))
with(Data, text(DPTpct,IMDPTres^2,labels=WBcode))
dev.off() 


###########################################
# Session 2: Tricks and tables, etc.
###########################################

# Simple regression:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/africa2001.csv")
africa<-read.csv(text=temp, header=TRUE)
rm(temp)
summary(africa)

fit<-with(africa, lm(adrate~muslperc))
summary(fit)

# First figure

SEs<-predict(fit,interval="confidence")
Sort<-order(africa$muslperc)

pdf("SRTFig1.pdf",6,5)
plot(africa$muslperc, africa$adrate, 
     xlab="Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate",pch=16) 
abline(fit,lwd=3)
lines(sort(africa$muslperc),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslperc),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Add 10:

africa$muslplusten<-africa$muslperc+10
fit2<-with(africa, lm(adrate~muslplusten,data=africa))
summary(fit2)

SEs<-predict(fit2,interval="confidence")

pdf("SRTFig2.pdf",6,5)
plot(africa$muslplusten, africa$adrate, 
     xlab="Muslim Percentage of the Population + 10", 
     ylab="HIV Prevalence Rate",pch=16)
abline(fit2,lwd=3)
lines(sort(africa$muslplusten),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslplusten),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Multiply Y times -314:

africa$screwyrate<-africa$adrate*(-314)
fit3<-with(africa, lm(screwyrate~muslperc))
summary(fit3)

SEs<-predict(fit3,interval="confidence")

pdf("SRTFig3.pdf",6,5)
plot(africa$muslperc, africa$screwyrate, 
     xlab="Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate times -314",pch=16,
     ylim=c(-13000,1000))
abline(fit3,lwd=3)
lines(sort(africa$muslperc),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslperc),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Reversing the scales:

africa$nonmuslimpct <- 100 - africa$muslperc
africa$noninfected <- 100 - africa$adrate
fit4<-lm(noninfected~nonmuslimpct,data=africa)
summary(fit4)

SEs<-predict(fit4,interval="confidence")
Sort2 <- order(africa$nonmuslimpct)

pdf("SRTFig4.pdf",6,5)
plot(africa$nonmuslimpct, africa$noninfected, 
     xlab="Non-Muslim Percentage of the Population", 
     ylab="HIV Non-Prevalence Rate",pch=16,
     ylim=c(60,111))
abline(fit4,lwd=3)
lines(sort(africa$nonmuslimpct),SEs[Sort2,2],col="red",lwd=2,lty=2)
lines(sort(africa$nonmuslimpct),SEs[Sort2,3],col="red",lwd=2,lty=2)
dev.off()

# Centering X:

africa$muslcenter<-africa$muslperc - mean(africa$muslperc, na.rm=TRUE)
fit5<-lm(adrate~muslcenter,data=africa)
summary(fit5)

SEs<-predict(fit5,interval="confidence")

pdf("SRTFig5.pdf",6,5)
plot(africa$muslcenter, africa$adrate, 
     xlab="Centered Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate",pch=16,
     ylim=c(-10,40))
abline(fit5,lwd=3)
lines(sort(africa$muslcenter),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslcenter),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Rescaling X for interpretability:

fit6<-lm(adrate~population,data=africa)
summary(fit6)

africa$popmil<-africa$population / 1000000
fit7<-lm(adrate~popmil,data=africa)
summary(fit7)

# Dichotomous X and t-tests:

fit8<-lm(adrate~subsaharan,data=africa)
summary(fit8)

with(africa,
     t.test(adrate~subsaharan, var.equal=TRUE))

################
# Reporting:

fit<-lm(adrate~muslperc, data=africa)
summary.lm(fit)

# Easy LaTeX table using *stargazer*:

library(stargazer) # <-- install if necessary
stargazer(fit,
          type="latex",
          title="OLS Regression Model of HIV/AIDS Rates in Africa, 2001",
          dep.var.caption="",
          dep.var.labels="Model I",
          digits=2,
          no.space=TRUE,
          intercept.top=TRUE,intercept.bottom=FALSE,
          covariate.labels=c("(Constant)","
                             Muslim Percentage of the Population"))
