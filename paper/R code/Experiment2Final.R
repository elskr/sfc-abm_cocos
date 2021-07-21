setwd("C:/Users/elise/Desktop/New folder/benchmark/paper/BenchmarkModel_SimilationResults/Launch_Simulations/BenchmarkModel_Baseline_Sensitivity")

library(mFilter)
library(poweRlaw)
library(tseries)
library(propagate)
library(tidyverse)
library(plotrix)  
library(mgcv)
library(itsadug)
library(data.table)
library(xtable)

#The folder where data are stored
folder<-"data_experiment2"

#COMMENT IF MERGED .CSV FILES ALREADY CREATED#######
#source("MergeMonteCarloSim.R")
#generateMergedCSV(folder)
#generateSums(folder)
####################################################


#IMPORT DATA FROM .csv FILES
gdp<-read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
cPrices<-read.csv(paste(folder,"/Merged_cAvPrice.csv",sep=""))
kPrices<-read.csv(paste(folder,"/Merged_kAvPrice.csv",sep=""))
unemp<-read.csv(paste(folder,"/Merged_unemployment.csv",sep=""))
inv<-read.csv(paste(folder,"/Merged_cFirmsRealInvestment.csv",sep=""))
nominvAggregate<-read.csv(paste(folder,"/Merged_nominalInvestment.csv",sep=""))
cons<-read.csv(paste(folder,"/Merged_cFirmsRealSales.csv",sep=""))
nomcons<-read.csv(paste(folder,"/Merged_cFirmsNominalSales.csv",sep=""))
banksCoCos=read.csv(paste(folder,"/Merged_banksCoCoBonds.csv", sep=""))

#LOOK FOR CRASHED SIMULATIONS BASED ON UNEMPLOYMENT RATE (ONLY PUBLIC EMPLOYMENT REMAINS)
toBeRemoved=c()
for (i in 2:length(unemp[1,])){
  if (any(unemp[,i]>0.8)){
    toBeRemoved=c(toBeRemoved,i)
  }
}

#FIND THE NAME OF THE SIMULATIONS CRASHED
simCrashed=colnames(unemp)
simCrashed=simCrashed[toBeRemoved]
#REMOVE CRASHED SIMULATIONS
if (!is.null(toBeRemoved)){
  unemp=unemp[,-toBeRemoved]
  gdp=gdp[,-toBeRemoved]
  cPrices=cPrices[,-toBeRemoved]
  kPrices=kPrices[,-toBeRemoved]
}


#IDENTIFY THE COLUMNS WHERE AGGREGATE INVESTMENT&CONSUMPTION FOR EACH SIMULATION HAS BEEN COMPUTED
indexSum<-grep("SUM",colnames(inv))
cn<-colnames(inv)
nbExp<-length(grep("Run1SUM",cn[indexSum]))
nbRuns<-length(grep(paste("Exp",nbExp,"Run",sep=""),cn[indexSum]))


#CREATE DATASET FOR AGGREGATE INVESTMENT AND CONSUMPTION
invAggregate=inv[,c(1,indexSum)]
consAggregate=cons[,c(1,indexSum)]
nomconsAggregate=nomcons[,c(1,indexSum)]

#IDENTIFY THE CRASHED SIMULATIONS BASED ON THE CRASHED SIMULATION NAMES
if(!is.null(toBeRemoved)){
  indexCrashed=c()
  for (i in 1:length(simCrashed)){
    indexCrashed=c(indexCrashed,grep(simCrashed[i], colnames(invAggregate)))
  }
  
  #REMOVE CRASHED SIMULATIONS
  invAggregate=invAggregate[,-indexCrashed]
  consAggregate=consAggregate[,-indexCrashed]
  nomconsAggregate=nomconsAggregate[,-indexCrashed]
}
invUnitsAggregate=invAggregate
#GET REAL VALUE OF INVESTMENT GOODS
for(i in 1:nbExp){
  for(j in 1:nbRuns){
    if (!(paste("Exp",i,"Run",j,sep="")%in% simCrashed)){
      invAggregate[,grep(paste("Exp",i,"Run",j,"SUM",sep=""),colnames(invAggregate))]=invAggregate[,grep(paste("Exp",i,"Run",j,"SUM",sep=""),colnames(invAggregate))]*kPrices[,grep(paste("Exp",i,"Run",j,"$",sep=""),colnames(kPrices))]/cPrices[,grep(paste("Exp",i,"Run",j,"$",sep=""),colnames(cPrices))]
    }
  }
}

#IDENTIFY THE COLUMNS WHERE AGGREGATE COCOS FOR EACH SIMULATION HAS BEEN COMPUTED
indexSum<-grep("SUM",colnames(banksCoCos))
cn<-colnames(banksCoCos)
nbExp<-length(grep("Run1SUM",cn[indexSum]))
nbRuns<-length(grep(paste("Exp",nbExp,"Run",sep=""),cn[indexSum]))

#CREATE DATASET FOR AGGREGATE COCO
cocosAggregate=banksCoCos[,c(1,indexSum)]

#IDENTIFY THE CRASHED SIMULATIONS BASED ON THE CRASHED SIMULATION NAMES
if(!is.null(toBeRemoved)){
  indexCrashed=c()
  for (i in 1:length(simCrashed)){
    indexCrashed=c(indexCrashed,grep(simCrashed[i], colnames(cocosAggregate)))
  }
  
  #REMOVE CRASHED SIMULATIONS
  cocosAggregate=cocosAggregate[,-indexCrashed]
}


#WE FIX THE TIME DIMENSION AT A POINT DISTANT ENOUGH (HERE, T=300)
#WE ALSO DROP THE COLUMN FOR TIME
#WE PREPARE ALL THE DATA SUBSETS FOR A MERGE
time_point = 300

spline_gdp <- as.data.frame(t(gdp[time_point,-1]))
colnames(spline_gdp) <-c("GDP")
spline_gdp <- rownames_to_column (spline_gdp, var = "Exp_Run")

spline_nominvAggregate <- as.data.frame(t(nominvAggregate[time_point,-1]))
colnames(spline_nominvAggregate) <-c("Investment")
spline_nominvAggregate <- rownames_to_column (spline_nominvAggregate, var = "Exp_Run")

spline_nomconsAggregate <- as.data.frame(t(nomconsAggregate[time_point,-1]))
colnames(spline_nomconsAggregate) <-c("Consumption")
spline_nomconsAggregate <- rownames_to_column (spline_nomconsAggregate, var = "Exp_Run")
spline_nomconsAggregate <- spline_nomconsAggregate %>% mutate_all(~gsub("SUM", "", .))
spline_nomconsAggregate <- spline_nomconsAggregate[order(spline_nomconsAggregate$Exp_Run),]

spline_cocosAggregate <- as.data.frame(t(cocosAggregate[time_point,-1]))
colnames(spline_cocosAggregate) <-c("CoCos")
spline_cocosAggregate <- rownames_to_column (spline_cocosAggregate, var = "Exp_Run")
spline_cocosAggregate <- spline_cocosAggregate %>% mutate_all(~gsub("SUM", "", .))
spline_cocosAggregate <- spline_cocosAggregate[order(spline_cocosAggregate$Exp_Run),]

#WE MERGE THE SUBSETS TOGETHER

subtotal1 <- merge(spline_gdp, spline_cocosAggregate, by="Exp_Run")
subtotal2 <- merge(subtotal1, spline_nomconsAggregate, by="Exp_Run")
spline_finalDataSet <- merge(subtotal2, spline_nominvAggregate, by="Exp_Run")

#WE ADD MANUALLY A THRESHHOLD OF ACTIVATION VARIABLE

spline_finalDataSet$threshold <- 0

spline_finalDataSet$threshold[1:24] <- 0.59
spline_finalDataSet$threshold[25:49] <- 0.60
spline_finalDataSet$threshold[50:74] <- 0.61
spline_finalDataSet$threshold[75:99] <- 0.62
spline_finalDataSet$threshold[100:124] <- 0.63
spline_finalDataSet$threshold[125:149] <- 0.64
spline_finalDataSet$threshold[150:174] <- 0.65
spline_finalDataSet$threshold[175:199] <- 0.66
spline_finalDataSet$threshold[200:224] <- 0.67
spline_finalDataSet$threshold[225:249] <- 0.68
spline_finalDataSet$threshold[250:274] <- 0.69
spline_finalDataSet$threshold[275:299] <- 0.70

spline_finalDataSet$test = as.factor(paste(spline_finalDataSet$threshold)) 


#THE AGGREGATE VARIABLES HAD A STRING FORMAT WE DON'T WANT, SO WE CONVERT THEM TO NUMERIC VALUES

spline_finalDataSet[,c(4:8)] <- sapply(spline_finalDataSet[,c(4:8)], as.numeric)

#WE CHECK THE FINAL DATASET

str(spline_finalDataSet)

#LINEAR REGRESSIONS

lm_mod1 <- lm(nomconsAggregate ~ factor(threshold) + cocosAggregate, data = spline_finalDataSet )
lm_mod2 <- lm(gdp ~ factor(threshold) + cocosAggregate, data = spline_finalDataSet )
lm_mod3 <- lm(nominvAggregate ~ factor(threshold) + cocosAggregate, data = spline_finalDataSet )

summary(lm_mod1)
summary(lm_mod2)
summary(lm_mod3)

#LINEAR REGRESSIONS WITH QUADRATIC TERMS

quad_mod1 <- lm(nomconsAggregate ~ factor(test) + cocosAggregate + I(cocosAggregate^2), data = spline_finalDataSet )
quad_mod2 <- lm(gdp ~ factor(test) + cocosAggregate + I(cocosAggregate^2), data = spline_finalDataSet )
quad_mod3 <- lm(nominvAggregate ~ factor(test) + cocosAggregate + I(cocosAggregate^2), data = spline_finalDataSet )

summary(quad_mod1)
summary(quad_mod2)
summary(quad_mod3)

#NON-LINEAR PENALIZED SPLINE REGRESSIONS


gamm_modA <- gamm( nomconsAggregate ~  s(cocosAggregate),data = spline_finalDataSet, random=list(threshold=~1), correlation = corARMA( p = 1, q = 0 ) )
summary(gamm_modA$gam)
gamtabs(gamm_modA$gam)
itsadug::plot_smooth(gamm_modA$gam, view="cocosAggregate", rug=F)

gamm_modB <- gamm (nominvAggregate ~  s(cocosAggregate),data = spline_finalDataSet, random=list(threshold=~1), correlation = corARMA( p = 1, q = 0 ))
summary(gamm_modB$gam)
gamtabs(gamm_modB$gam)
itsadug::plot_smooth(gamm_modB$gam, view="cocosAggregate", rug=F)

gamm_modC <- gamm (gdp ~  s(cocosAggregate),data = spline_finalDataSet, random=list(threshold=~1), correlation = corARMA( p = 1, q = 0 ))
summary(gamm_modC$gam)
gamtabs(gamm_modC$gam)
itsadug::plot_smooth(gamm_modC$gam, view="cocosAggregate", rug=F)

