##############################################
# Code GSERM "Regression for Publishing"
# (June 2020)
#
# Day Five
#
# File created 6/18/2020
#
# File last modified 6/18/2020
######################################
# setwd() first:
#
# setwd("~/Dropbox (Personal)/GSERM/Materials 2020") # or whatever...

library(RCurl)    # <-- install all these as necessary
library(rlang)    # <--
library(RColorBrewer)
library(colorspace)
library(foreign)
library(psych)
library(lme4)
library(plm)
library(gtools)
library(boot)
library(plyr)
library(dplyr)
library(lubridate)
library(texreg)
library(statmod)
library(pscl)

# Function for displaying within & between variation:

xtsum <- function(data, varname, unit) {
  varname <- enquo(varname)
  loc.unit <- enquo(unit)
  ores <- data %>% summarise(ovr.mean=mean(!! varname, na.rm=TRUE), ovr.sd=sd(!! varname, na.rm=TRUE), ovr.min = min(!! varname, na.rm=TRUE), ovr.max=max(!! varname, na.rm=TRUE), ovr.N=sum(as.numeric((!is.na(!! varname)))))
  bmeans <- data %>% group_by(!! loc.unit) %>% summarise(meanx=mean(!! varname, na.rm=T), t.count=sum(as.numeric(!is.na(!! varname))))
  bres <- bmeans %>% ungroup() %>% summarise(between.sd = sd(meanx, na.rm=TRUE), between.min = min(meanx, na.rm=TRUE), between.max=max(meanx, na.rm=TRUE), Units=sum(as.numeric(!is.na(t.count))), t.bar=mean(t.count, na.rm=TRUE))
  wdat <- data %>% group_by(!! loc.unit) %>% mutate(W.x = scale(!! varname, scale=FALSE))
  wres <- wdat %>% ungroup() %>% summarise(within.sd=sd(W.x, na.rm=TRUE), within.min=min(W.x, na.rm=TRUE), within.max=max(W.x, na.rm=TRUE))
  return(list(ores=ores,bres=bres,wres=wres))
}

##########################################
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

##########################################
# Between & within variation...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/COVID.csv")
COVID<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(COVID)

COVIDs<-subset(COVID,select=c(location,date,new_cases))

summary(COVIDs)

# Overall:

describe(COVIDs$new_cases)

# Between:

CountryMeans <- ddply(COVIDs,.(location),summarise,
                      newcases = mean(new_cases,na.rm=TRUE))
with(CountryMeans, describe(newcases))

# Within:

COVIDs <- ddply(COVIDs, .(location), mutate,
               CaseMean = mean(new_cases,na.rm=TRUE))
COVIDs$within <- with(COVIDs, new_cases-CaseMean)
with(COVIDs, describe(within))

# Now with the function:

xtsum(COVIDs,new_cases,location)



######################################
# Fixed & random effects, etc.:

##########
# FEs and REs:

RefsURL<-"https://raw.githubusercontent.com/PrisonRodeo/GSERM-2020-git/master/Data/Refugees.csv"
temp<-getURL(RefsURL)
Refugees<-read.csv(textConnection(temp))
rm(temp, RefsURL)

summary(Refugees)

# FE:

RefFE<-plm(ln_ref_flow~pop_diff+distance+regimedif+
             wardiff, data=Refugees, effect="individual",
           model="within")

# BE:

RefBE<-plm(ln_ref_flow~pop_diff+distance+regimedif+
             wardiff, data=Refugees, effect="individual",
           model="between")

# RE: 

RefRE<-lmer(ln_ref_flow~pop_diff+distance+regimedif+
              wardiff+(1|dirdyadID), data=Refugees)

# RE, again:

AltRefRE<-plm(ln_ref_flow~pop_diff+distance+regimedif+
                wardiff, data=Refugees, effect="individual",
              model="random")

# Hausman test (ugh):

phtest(RefFE, AltRefRE)
