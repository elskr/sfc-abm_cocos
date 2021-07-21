setwd("C:/Users/elise/Desktop/New folder/benchmark/paper/BenchmarkModel_SimilationResults/Launch_Simulations/BenchmarkModel_Baseline_Sensitivity")

library("xlsx")
library("dplyr")
library("tidyverse")
library("mFilter")
library("poweRlaw")
library("tseries")
library("propagate")
library()

folder<-"data_experiment1"

#COMMENT IF MERGED .CSV FILES ALREADY CREATED#######
#source("MergeMonteCarloSim.R")
#generateMergedCSV(folder)
#generateSums(folder)
####################################################

#IMPORT DATA FROM .csv FILES
banksBankruptcies=read.csv(paste(folder,"/Merged_banksBankrupcty.csv",sep=""))
banksBailoutCost=read.csv(paste(folder,"/Merged_banksBailoutCost.csv",sep=""))
banksNbCoCoTriggered=read.csv(paste(folder,"/Merged_banksCoCoTriggered.csv", sep=""))
banksCapitalRatio=read.csv(paste(folder,"/Merged_banksCapitalRatio.csv", sep=""))
banksLiquidityRatio=read.csv(paste(folder,"/Merged_banksLiquidityRatio.csv", sep=""))
inv<-read.csv(paste(folder,"/Merged_cFirmsRealInvestment.csv",sep=""))
nominvAggregate<-read.csv(paste(folder,"/Merged_nominalInvestment.csv",sep=""))
cons<-read.csv(paste(folder,"/Merged_cFirmsRealSales.csv",sep=""))
nomcons<-read.csv(paste(folder,"/Merged_cFirmsNominalSales.csv",sep=""))
unemp<-read.csv(paste(folder,"/Merged_unemployment.csv",sep=""))
netIncome<-read.csv(paste(folder,"/Merged_hhAvNetIncome.csv",sep=""))
gdp<-read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
cPrices<-read.csv(paste(folder,"/Merged_cAvPrice.csv",sep=""))
kPrices<-read.csv(paste(folder,"/Merged_kAvPrice.csv",sep=""))

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
  netIncome=netIncome[,-toBeRemoved]
  banksBankruptcies=banksBankruptcies[,-toBeRemoved]
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

#GENERATE DATA FRAME FOR STATISTICAL TESTS

test0 <- rep(NA,45)

test1 <- as.data.frame(colSums(banksBankruptcies))
test1 = test1[-1,]

test2 <-c(sum(banksBailoutCost$Exp1Run1SUM), sum(banksBailoutCost$Exp1Run2SUM), sum(banksBailoutCost$Exp1Run3SUM), sum(banksBailoutCost$Exp1Run4SUM), sum(banksBailoutCost$Exp1Run5SUM), sum(banksBailoutCost$Exp2Run1SUM), sum(banksBailoutCost$Exp2Run2SUM), sum(banksBailoutCost$Exp2Run3SUM), sum(banksBailoutCost$Exp2Run4SUM), sum(banksBailoutCost$Exp2Run5SUM), sum(banksBailoutCost$Exp3Run1SUM), sum(banksBailoutCost$Exp3Run2SUM), sum(banksBailoutCost$Exp3Run3SUM), sum(banksBailoutCost$Exp3Run4SUM), sum(banksBailoutCost$Exp3Run5SUM), sum(banksBailoutCost$Exp4Run1SUM), sum(banksBailoutCost$Exp4Run2SUM), sum(banksBailoutCost$Exp4Run3SUM), sum(banksBailoutCost$Exp4Run4SUM), sum(banksBailoutCost$Exp4Run5SUM), sum(banksBailoutCost$Exp5Run1SUM), sum(banksBailoutCost$Exp5Run2SUM), sum(banksBailoutCost$Exp5Run3SUM), sum(banksBailoutCost$Exp5Run4SUM), sum(banksBailoutCost$Exp5Run5SUM), sum(banksBailoutCost$Exp6Run1SUM), sum(banksBailoutCost$Exp6Run2SUM), sum(banksBailoutCost$Exp6Run3SUM), sum(banksBailoutCost$Exp6Run4SUM), sum(banksBailoutCost$Exp6Run5SUM), sum(banksBailoutCost$Exp7Run1SUM), sum(banksBailoutCost$Exp7Run2SUM), sum(banksBailoutCost$Exp7Run3SUM), sum(banksBailoutCost$Exp7Run4SUM), sum(banksBailoutCost$Exp7Run5SUM), sum(banksBailoutCost$Exp8Run1SUM), sum(banksBailoutCost$Exp8Run2SUM), sum(banksBailoutCost$Exp8Run3SUM), sum(banksBailoutCost$Exp8Run4SUM), sum(banksBailoutCost$Exp8Run5SUM), sum(banksBailoutCost$Exp9Run1SUM), sum(banksBailoutCost$Exp9Run2SUM), sum(banksBailoutCost$Exp9Run3SUM), sum(banksBailoutCost$Exp9Run4SUM), sum(banksBailoutCost$Exp9Run5SUM))

test3 <-c(sum(banksCapitalRatio$Exp1Run1SUM/4000), sum(banksCapitalRatio$Exp1Run2SUM/4000), sum(banksCapitalRatio$Exp1Run3SUM/4000), sum(banksCapitalRatio$Exp1Run4SUM/4000), sum(banksCapitalRatio$Exp1Run5SUM/4000), sum(banksCapitalRatio$Exp2Run1SUM/4000), sum(banksCapitalRatio$Exp2Run2SUM/4000), sum(banksCapitalRatio$Exp2Run3SUM/4000), sum(banksCapitalRatio$Exp2Run4SUM/4000), sum(banksCapitalRatio$Exp2Run5SUM/4000), sum(banksCapitalRatio$Exp3Run1SUM/4000), sum(banksCapitalRatio$Exp3Run2SUM/4000), sum(banksCapitalRatio$Exp3Run3SUM/4000), sum(banksCapitalRatio$Exp3Run4SUM/4000), sum(banksCapitalRatio$Exp3Run5SUM/4000), sum(banksCapitalRatio$Exp4Run1SUM/4000), sum(banksCapitalRatio$Exp4Run2SUM/4000), sum(banksCapitalRatio$Exp4Run3SUM/4000), sum(banksCapitalRatio$Exp4Run4SUM/4000), sum(banksCapitalRatio$Exp4Run5SUM/4000), sum(banksCapitalRatio$Exp5Run1SUM/4000), sum(banksCapitalRatio$Exp5Run2SUM/4000), sum(banksCapitalRatio$Exp5Run3SUM/4000), sum(banksCapitalRatio$Exp5Run4SUM/4000), sum(banksCapitalRatio$Exp5Run5SUM/4000), sum(banksCapitalRatio$Exp6Run1SUM/4000), sum(banksCapitalRatio$Exp6Run2SUM/4000), sum(banksCapitalRatio$Exp6Run3SUM/4000), sum(banksCapitalRatio$Exp6Run4SUM/4000), sum(banksCapitalRatio$Exp6Run5SUM/4000), sum(banksCapitalRatio$Exp7Run1SUM/4000), sum(banksCapitalRatio$Exp7Run2SUM/4000), sum(banksCapitalRatio$Exp7Run3SUM/4000), sum(banksCapitalRatio$Exp7Run4SUM/4000), sum(banksCapitalRatio$Exp7Run5SUM/4000), sum(banksCapitalRatio$Exp8Run1SUM/4000), sum(banksCapitalRatio$Exp8Run2SUM/4000), sum(banksCapitalRatio$Exp8Run3SUM/4000), sum(banksCapitalRatio$Exp8Run4SUM/4000), sum(banksCapitalRatio$Exp8Run5SUM/4000), sum(banksCapitalRatio$Exp9Run1SUM/4000), sum(banksCapitalRatio$Exp9Run2SUM/4000), sum(banksCapitalRatio$Exp9Run3SUM/4000), sum(banksCapitalRatio$Exp9Run4SUM/4000), sum(banksCapitalRatio$Exp9Run5SUM/4000))

test4 <-c(sum(nomconsAggregate$Exp1Run1SUM/400), sum(nomconsAggregate$Exp1Run2SUM/400), sum(nomconsAggregate$Exp1Run3SUM/400), sum(nomconsAggregate$Exp1Run4SUM/400), sum(nomconsAggregate$Exp1Run5SUM/400), sum(nomconsAggregate$Exp2Run1SUM/400), sum(nomconsAggregate$Exp2Run2SUM/400), sum(nomconsAggregate$Exp2Run3SUM/4000), sum(nomconsAggregate$Exp2Run4SUM/400), sum(nomconsAggregate$Exp2Run5SUM/400), sum(nomconsAggregate$Exp3Run1SUM/400), sum(nomconsAggregate$Exp3Run2SUM/400), sum(nomconsAggregate$Exp3Run3SUM/400), sum(nomconsAggregate$Exp3Run4SUM/400), sum(nomconsAggregate$Exp3Run5SUM/400), sum(nomconsAggregate$Exp4Run1SUM/400), sum(nomconsAggregate$Exp4Run2SUM/400), sum(nomconsAggregate$Exp4Run3SUM/400), sum(nomconsAggregate$Exp4Run4SUM/400), sum(nomconsAggregate$Exp4Run5SUM/400), sum(nomconsAggregate$Exp5Run1SUM/400), sum(nomconsAggregate$Exp5Run2SUM/400), sum(nomconsAggregate$Exp5Run3SUM/400), sum(nomconsAggregate$Exp5Run4SUM/400), sum(nomconsAggregate$Exp5Run5SUM/400), sum(nomconsAggregate$Exp6Run1SUM/400), sum(nomconsAggregate$Exp6Run2SUM/400), sum(nomconsAggregate$Exp6Run3SUM/400), sum(nomconsAggregate$Exp6Run4SUM/400), sum(nomconsAggregate$Exp6Run5SUM/400), sum(nomconsAggregate$Exp7Run1SUM/400), sum(nomconsAggregate$Exp7Run2SUM/400), sum(nomconsAggregate$Exp7Run3SUM/400), sum(nomconsAggregate$Exp7Run4SUM/400), sum(nomconsAggregate$Exp7Run5SUM/400), sum(nomconsAggregate$Exp8Run1SUM/400), sum(nomconsAggregate$Exp8Run2SUM/400), sum(nomconsAggregate$Exp8Run3SUM/400), sum(nomconsAggregate$Exp8Run4SUM/400), sum(nomconsAggregate$Exp8Run5SUM/400), sum(nomconsAggregate$Exp9Run1SUM/400), sum(nomconsAggregate$Exp9Run2SUM/400), sum(nomconsAggregate$Exp9Run3SUM/400), sum(nomconsAggregate$Exp9Run4SUM/400), sum(nomconsAggregate$Exp9Run5SUM/400))

test5 <-c(sum(nominvAggregate$Exp1Run1/400), sum(nominvAggregate$Exp1Run2/400), sum(nominvAggregate$Exp1Run3/400), sum(nominvAggregate$Exp1Run4/400), sum(nominvAggregate$Exp1Run5/400), sum(nominvAggregate$Exp2Run1/400), sum(nominvAggregate$Exp2Run2/400), sum(nominvAggregate$Exp2Run3/400), sum(nominvAggregate$Exp2Run4/400), sum(nominvAggregate$Exp2Run5/400), sum(nominvAggregate$Exp3Run1/400), sum(nominvAggregate$Exp3Run2/400), sum(nominvAggregate$Exp3Run3/400), sum(nominvAggregate$Exp3Run4/400), sum(nominvAggregate$Exp3Run5/400), sum(nominvAggregate$Exp4Run1/400), sum(nominvAggregate$Exp4Run2/400), sum(nominvAggregate$Exp4Run3/400), sum(nominvAggregate$Exp4Run4/400), sum(nominvAggregate$Exp4Run5/400), sum(nominvAggregate$Exp5Run1/400), sum(nominvAggregate$Exp5Run2/400), sum(nominvAggregate$Exp5Run3/400), sum(nominvAggregate$Exp5Run4/400), sum(nominvAggregate$Exp5Run5/400), sum(nominvAggregate$Exp6Run1/400), sum(nominvAggregate$Exp6Run2/400), sum(nominvAggregate$Exp6Run3/400), sum(nominvAggregate$Exp6Run4/400), sum(nominvAggregate$Exp6Run5/400), sum(nominvAggregate$Exp7Run1/400), sum(nominvAggregate$Exp7Run2/400), sum(nominvAggregate$Exp7Run3/400), sum(nominvAggregate$Exp7Run4/400), sum(nominvAggregate$Exp7Run5/400), sum(nominvAggregate$Exp8Run1/400), sum(nominvAggregate$Exp8Run2/400), sum(nominvAggregate$Exp8Run3/400), sum(nominvAggregate$Exp8Run4/400), sum(nominvAggregate$Exp8Run5/400), sum(nominvAggregate$Exp9Run1/400), sum(nominvAggregate$Exp9Run2/400), sum(nominvAggregate$Exp9Run3/400), sum(nominvAggregate$Exp9Run4/400), sum(nominvAggregate$Exp9Run5/400))

test6 <-c(sum(gdp$Exp1Run1/400), sum(gdp$Exp1Run2/400), sum(gdp$Exp1Run3/400), sum(gdp$Exp1Run4/400), sum(gdp$Exp1Run5/400), sum(gdp$Exp2Run1/400), sum(gdp$Exp2Run2/400), sum(gdp$Exp2Run3/400), sum(gdp$Exp2Run4/400), sum(gdp$Exp2Run5/400), sum(gdp$Exp3Run1/400), sum(gdp$Exp3Run2/400), sum(gdp$Exp3Run3/400), sum(gdp$Exp3Run4/400), sum(gdp$Exp3Run5/400), sum(gdp$Exp4Run1/400), sum(gdp$Exp4Run2/400), sum(gdp$Exp4Run3/400), sum(gdp$Exp4Run4/400), sum(gdp$Exp4Run5/400), sum(gdp$Exp5Run1/400), sum(gdp$Exp5Run2/400), sum(gdp$Exp5Run3/400), sum(gdp$Exp5Run4/400), sum(gdp$Exp5Run5/400), sum(gdp$Exp6Run1/400), sum(gdp$Exp6Run2/400), sum(gdp$Exp6Run3/400), sum(gdp$Exp6Run4/400), sum(gdp$Exp6Run5/400), sum(gdp$Exp7Run1/400), sum(gdp$Exp7Run2/400), sum(gdp$Exp7Run3/400), sum(gdp$Exp7Run4/400), sum(gdp$Exp7Run5/400), sum(gdp$Exp8Run1/400), sum(gdp$Exp8Run2/400), sum(gdp$Exp8Run3/400), sum(gdp$Exp8Run4/400), sum(gdp$Exp8Run5/400), sum(gdp$Exp9Run1/400), sum(gdp$Exp9Run2/400), sum(gdp$Exp9Run3/400), sum(gdp$Exp9Run4/400), sum(gdp$Exp9Run5/400))

df <- data.frame('run'=test0,'b_bankruptcies'=test1, 'b_bailoutcost'=test2, 'b_avrgcapitalratio'=test3, 'avrgaggnomcons'=test4, 'avrgaggnominv'=test5, 'avrggdp'=test6)

df$run[1:5] <- 1
df$run[6:10] <- 2
df$run[11:15] <- 3
df$run[16:20] <- 4
df$run[21:25] <- 5
df$run[26:30] <- 6
df$run[31:35] <- 7
df$run[36:40] <- 8
df$run[41:45] <- 9

#DESCRIPTIVE STATS FOR EACH VARIABLE TO GET AN IDEA ABOUT WHAT'S GOING ON
group_by(df, run) %>%
  summarise(
    count = n(),
    mean = mean(b_bankruptcies, na.rm = TRUE),
    std = sd(b_bankruptcies, na.rm = TRUE),
    IQR = IQR(b_bankruptcies, na.rm = TRUE)
  )

group_by(df, run) %>%
  summarise(
    count = n(),
    median = median(b_bailoutcost, na.rm = TRUE),
    std = sd(b_bailoutcost, na.rm = TRUE),
    IQR = IQR(b_bailoutcost, na.rm = TRUE)
  )

group_by(df, run) %>%
  summarise(
    count = n(),
    median = median(b_avrgcapitalratio, na.rm = TRUE),
    std = sd(b_avrgcapitalratio, na.rm = TRUE),
    IQR = IQR(b_avrgcapitalratio, na.rm = TRUE)
  )

group_by(df, run) %>%
  summarise(
    count = n(),
    median = median(avrgaggnomcons, na.rm = TRUE),
    std = sd(avrgaggnomcons, na.rm = TRUE),
    IQR = IQR(avrgaggnomcons, na.rm = TRUE)
  )

group_by(df, run) %>%
  summarise(
    count = n(),
    median = median(avrgaggnominv, na.rm = TRUE),
    std = sd(avrgaggnominv, na.rm = TRUE),
    IQR = IQR(avrgaggnominv, na.rm = TRUE)
  )

group_by(df, run) %>%
  summarise(
    count = n(),
    median = median(avrggdp, na.rm = TRUE),
    std = sd(avrggdp, na.rm = TRUE),
    IQR = IQR(avrggdp, na.rm = TRUE)
  )


###BASELINE RESULTS###

##MANN-WHITNEY TESTS##

#WE SUBSET THE DATAFRAME 
#(only the runs 1 and 2 which correspond to the cases presented in the paper)
baseline <- subset(df, run %in% c(1, 2))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = baseline, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = baseline, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = baseline, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = baseline, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = baseline, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = baseline, exact = FALSE, alternative = "less")
#P-VALUES vector
v_baseline <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)




##GRAPHS##

###APPLY THE HP FILTER ON GDP
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
gdphp_cycle<-gdp
gdphp_trend<-gdp
#APPLY THE FILTER
for(i in 2:ncol(gdp)){
  gdp.hp<-hpfilter(as.ts(gdp[,i],frequency=4),freq=1600,type="lambda")
  gdphp_cycle[,i]<-gdp.hp$cycle
  gdphp_trend[,i]<-gdp.hp$trend
}
#COMPUTE AVERAGE TREND AND STANDARD DEVIATION
gdptrend.average<-apply(gdphp_trend[,-1],1,mean) 
gdptrend.sd<-apply(gdphp_trend[,-1],1,sd)


###APPLY THE HP FILTER TO AGGREGATE INVESTMENT AND CONSUMPTION (NOMINAL)
#DECLARE VARIABLES WHERE STORING TREND AND CYCLE COMPONENTS
invhp_cycle<-nominvAggregate
invhp_trend<-nominvAggregate
conshp_cycle<-nomconsAggregate
conshp_trend<-nomconsAggregate
#APPLY THE HP FILTER
for(i in 2:ncol(nominvAggregate)){
  inv.hp<-hpfilter(as.ts(nominvAggregate[,i],frequency=4),freq=1600,type="lambda")
  cons.hp<-hpfilter(as.ts(nomconsAggregate[,i],frequency=4),freq=1600,type="lambda")
  invhp_cycle[,i]<-inv.hp$cycle
  invhp_trend[,i]<-inv.hp$trend
  conshp_cycle[,i]<-cons.hp$cycle
  conshp_trend[,i]<-cons.hp$trend
}
#COMPUTE AVERAGE TREND AND SD
invtrend.average<-matrix(NA,ncol=(nbExp+1),nrow=nrow(invhp_trend))
invtrend.average[,1]<-invhp_trend[,1]
invtrend.sd<-matrix(NA,ncol=(nbExp+1),nrow=nrow(invhp_trend))
invtrend.sd[,1]<-invhp_trend[,1]
constrend.average<-matrix(NA,ncol=(nbExp+1),nrow=nrow(conshp_trend))
constrend.average[,1]<-conshp_trend[,1]
constrend.sd<-matrix(NA,ncol=(nbExp+1),nrow=nrow(conshp_trend))
constrend.sd[,1]<-conshp_trend[,1]
for(i in 1:nbExp){
  invtrend.average[,i+1]<-apply(invhp_trend[,grep(paste("Exp",i,"Run",sep=""),colnames(invhp_trend))],1,mean)
  invtrend.sd[,i+1]<-apply(invhp_trend[,grep(paste("Exp",i,"Run",sep=""),colnames(invhp_trend))],1,sd)
  constrend.average[,i+1]<-apply(conshp_trend[,grep(paste("Exp",i,"Run",sep=""),colnames(invhp_trend))],1,mean)
  constrend.sd[,i+1]<-apply(conshp_trend[,grep(paste("Exp",i,"Run",sep=""),colnames(invhp_trend))],1,sd)
}



###APPLY THE HP FILTER ON GDP
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
gdphp_cycle<-gdp
gdphp_trend<-gdp

for(i in 2:length(gdp[1,])){
  gdp.hp<-hpfilter(as.ts(gdp[,i],frequency=4),freq=1600,type="lambda")
  gdphp_cycle[,i]<-gdp.hp$cycle
  gdphp_trend[,i]<-gdp.hp$trend
}
gdptrend.average<-matrix(NA,ncol=(nbExp+1),nrow=nrow(gdphp_trend))
gdptrend.average[,1]<-gdphp_trend[,1]
gdptrend.sd<-matrix(NA,ncol=(nbExp+1),nrow=nrow(gdphp_trend))
gdptrend.sd[,1]<-gdphp_trend[,1]
for(i in 1:nbExp){
  gdptrend.average[,i+1]<-apply(gdphp_trend[,grep(paste("Exp",i, "Run",sep=""),colnames(gdphp_trend))],1,mean)
  gdptrend.sd[,i+1]<-apply(gdphp_trend[,grep(paste("Exp",i,"Run",sep=""),colnames(gdphp_trend))],1,sd)
}




layout(matrix(c(1,3,5,2,4,6), 2, 3, byrow = TRUE))

#par(mfrow = c(1, 1))
matplot(invtrend.average[,1],log(invtrend.average[,2:(nbExp+1)]),type="l",main="Nominal Investment (log)",
        ylab="",xlab="",lwd=2,col=paste("grey",seq(0,90,round(90/nbExp,digit=0))),lty=1)
dev.copy2eps(file=paste(folder,"/LogInvSensitivity.ps",sep=""))
#dev.off()

#par(mfrow = c(1, 1))
matplot(invtrend.average[,1],log(invtrend.average[,2:(nbExp+1)]),type="l",main="Nominal Investment (log)",
        ylab="",xlab="",xlim=c(350,400),ylim=c(10.4,10.9),lwd=2,col=paste("grey",seq(0,90,round(90/nbExp,digit=0))),lty=1)
dev.copy2eps(file=paste(folder,"/LogInvSensitivity.ps",sep=""))
#dev.off()

#par(mfrow = c(1, 1))
matplot(constrend.average[,1],log(constrend.average[,2:(nbExp+1)]),type="l",main="Nominal Consumption (log)",
        ylab="",xlab="",lwd=2,col=paste("grey",seq(0,90,round(90/nbExp,digit=0))),lty=1)
dev.copy2eps(file=paste(folder,"/LogConsSensitivity.ps",sep=""))
#dev.off()

#par(mfrow = c(1, 1))
matplot(constrend.average[,1],log(constrend.average[,2:(nbExp+1)]),type="l",main="Nominal Consumption (log)",
        ylab="",xlab="",xlim=c(350,400),ylim=c(11.7,12.2),lwd=2,col=paste("grey",seq(0,90,round(90/nbExp,digit=0))),lty=1)
dev.copy2eps(file=paste(folder,"/LogConsSensitivity.ps",sep=""))
#dev.off()

#par(mfrow = c(1, 1))
matplot(gdptrend.average[,1],log(gdptrend.average[,2:(nbExp+1)]),type="l",main="GDP (log)",
        ylab="",xlab="",lwd=2,col=paste("grey",seq(0,90,round(90/nbExp,digit=0))),lty=1)
dev.copy2eps(file=paste(folder,"/LogGDPSensitivity.ps",sep=""))
#dev.off()

#par(mfrow = c(1, 1))
matplot(gdptrend.average[,1],log(gdptrend.average[,2:(nbExp+1)]),type="l",main="GDP (log)",
        ylab="",xlab="",xlim=c(350,400),ylim=c(12.1,12.6),lwd=2,col=paste("grey",seq(0,90,round(90/nbExp,digit=0))),lty=1)
dev.copy2eps(file=paste(folder,"/LogGDPSensitivity.ps",sep=""))
#dev.off()





###ROBUSTNESS TESTS FOR MANN WHITNEY TESTS###
##each robustness test requires a new subset of the dataframe (without CoCos VS one with CoCos for different values of activation threshold)


robustness1 <- subset(df, run %in% c(1, 3))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness1, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness1, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness1, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness1, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness1, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness1, exact = FALSE, alternative = "less")
#P-VALUES vector
res2$p.value
res3$p.value
res4$p.value
res1$p.value
res5$p.value
res6$p.value

v_robustness1 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)


robustness2 <- subset(df, run %in% c(1, 4))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness2, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness2, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness2, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness2, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness2, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness2, exact = FALSE, alternative = "less")
#P-VALUES vector
v_robustness2 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)


robustness3 <- subset(df, run %in% c(1,5))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness3, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness3, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness3, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness3, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness3, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness3, exact = FALSE, alternative = "less")
#P-VALUES vector
v_robustness3 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)


robustness4 <- subset(df, run %in% c(1,6))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness4, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness4, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness4, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness4, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness4, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness4, exact = FALSE, alternative = "less")
#P-VALUES vector
v_robustness4 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)


robustness5 <- subset(df, run %in% c(1,7))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness5, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness5, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness5, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness5, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness5, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness5, exact = FALSE, alternative = "less")
#P-VALUES vector
v_robustness5 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)


robustness6 <- subset(df, run %in% c(1,8))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness6, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness6, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness6, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness6, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness6, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness6, exact = FALSE, alternative = "less")
#P-VALUES vector
v_robustness6 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)


robustness7 <- subset(df, run %in% c(1,9))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness7, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness7, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness7, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness7, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness7, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness7, exact = FALSE, alternative = "less")
#P-VALUES vector
v_robustness7 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)


##SUMMARY TABLE OF MANN WHITNEY ROBUSTNESS CHECKS

summary <- data.frame('baseline'=v_baseline,'robustness1'=v_robustness1, 'robustness2'=v_robustness2, 'robustness3'=v_robustness3, 'robustness4'=v_robustness4, 'robustness5'=v_robustness5, 'robustness6'=v_robustness6, 'robustness7'=v_robustness7)
write.xlsx(summary, "Mann-Whitney_summary.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)