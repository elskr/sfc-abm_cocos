#SET THE WORKING DIRECTORY WHERE THE RESULTS ARE STORED
setwd("C:\Users\Documents\GitHub\sfc-abm_cocos\paper\Simulation Results")

#IMPORT LIBRARIES
library("xlsx")
library("dplyr")
library("tidyverse")
library("mFilter")
library("poweRlaw")
library("tseries")
library("propagate")

#The folder where data are stored
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
#for (i in 2:length(unemp[1,])){
#  if (any(unemp[,i]>0.8)){
#    toBeRemoved=c(toBeRemoved,i)
#  }
#}

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

test0 <- rep(NA,300)

test1 <- as.data.frame(colSums(banksBankruptcies))
test1 = test1[-1,]

test2 <-c(sum(banksBailoutCost$Exp1Run1SUM), sum(banksBailoutCost$Exp1Run2SUM), sum(banksBailoutCost$Exp1Run3SUM), sum(banksBailoutCost$Exp1Run4SUM), sum(banksBailoutCost$Exp1Run5SUM),
            sum(banksBailoutCost$Exp1Run6SUM), sum(banksBailoutCost$Exp1Run7SUM), sum(banksBailoutCost$Exp1Run8SUM), sum(banksBailoutCost$Exp1Run9SUM), sum(banksBailoutCost$Exp1Run10SUM),
            sum(banksBailoutCost$Exp1Run11SUM), sum(banksBailoutCost$Exp1Run12SUM), sum(banksBailoutCost$Exp1Run13SUM), sum(banksBailoutCost$Exp1Run14SUM), sum(banksBailoutCost$Exp1Run15SUM),
            sum(banksBailoutCost$Exp1Run16SUM), sum(banksBailoutCost$Exp1Run17SUM), sum(banksBailoutCost$Exp1Run18SUM), sum(banksBailoutCost$Exp1Run19SUM), sum(banksBailoutCost$Exp1Run20SUM),
            sum(banksBailoutCost$Exp1Run21SUM), sum(banksBailoutCost$Exp1Run22SUM), sum(banksBailoutCost$Exp1Run23SUM), sum(banksBailoutCost$Exp1Run24SUM), sum(banksBailoutCost$Exp1Run25SUM),
            sum(banksBailoutCost$Exp2Run1SUM), sum(banksBailoutCost$Exp2Run2SUM), sum(banksBailoutCost$Exp2Run3SUM), sum(banksBailoutCost$Exp2Run4SUM), sum(banksBailoutCost$Exp2Run5SUM),
            sum(banksBailoutCost$Exp2Run6SUM), sum(banksBailoutCost$Exp2Run7SUM), sum(banksBailoutCost$Exp2Run8SUM), sum(banksBailoutCost$Exp2Run9SUM), sum(banksBailoutCost$Exp2Run10SUM),
            sum(banksBailoutCost$Exp2Run11SUM), sum(banksBailoutCost$Exp2Run12SUM), sum(banksBailoutCost$Exp2Run13SUM), sum(banksBailoutCost$Exp2Run14SUM), sum(banksBailoutCost$Exp2Run15SUM),
            sum(banksBailoutCost$Exp2Run16SUM), sum(banksBailoutCost$Exp2Run17SUM), sum(banksBailoutCost$Exp2Run18SUM), sum(banksBailoutCost$Exp2Run19SUM), sum(banksBailoutCost$Exp2Run20SUM),
            sum(banksBailoutCost$Exp2Run21SUM), sum(banksBailoutCost$Exp2Run22SUM), sum(banksBailoutCost$Exp2Run23SUM), sum(banksBailoutCost$Exp2Run24SUM), sum(banksBailoutCost$Exp2Run25SUM),
            sum(banksBailoutCost$Exp3Run1SUM), sum(banksBailoutCost$Exp3Run2SUM), sum(banksBailoutCost$Exp3Run3SUM), sum(banksBailoutCost$Exp3Run4SUM), sum(banksBailoutCost$Exp3Run5SUM),
            sum(banksBailoutCost$Exp3Run6SUM), sum(banksBailoutCost$Exp3Run7SUM), sum(banksBailoutCost$Exp3Run8SUM), sum(banksBailoutCost$Exp3Run9SUM), sum(banksBailoutCost$Exp3Run10SUM),
            sum(banksBailoutCost$Exp3Run11SUM), sum(banksBailoutCost$Exp3Run12SUM), sum(banksBailoutCost$Exp3Run13SUM), sum(banksBailoutCost$Exp3Run14SUM), sum(banksBailoutCost$Exp3Run15SUM),
            sum(banksBailoutCost$Exp3Run16SUM), sum(banksBailoutCost$Exp3Run17SUM), sum(banksBailoutCost$Exp3Run18SUM), sum(banksBailoutCost$Exp3Run19SUM), sum(banksBailoutCost$Exp3Run20SUM),
            sum(banksBailoutCost$Exp3Run21SUM), sum(banksBailoutCost$Exp3Run22SUM), sum(banksBailoutCost$Exp3Run23SUM), sum(banksBailoutCost$Exp3Run24SUM), sum(banksBailoutCost$Exp3Run25SUM),
            sum(banksBailoutCost$Exp4Run1SUM), sum(banksBailoutCost$Exp4Run2SUM), sum(banksBailoutCost$Exp4Run3SUM), sum(banksBailoutCost$Exp4Run4SUM), sum(banksBailoutCost$Exp4Run5SUM),
            sum(banksBailoutCost$Exp4Run6SUM), sum(banksBailoutCost$Exp4Run7SUM), sum(banksBailoutCost$Exp4Run8SUM), sum(banksBailoutCost$Exp4Run9SUM), sum(banksBailoutCost$Exp4Run10SUM),
            sum(banksBailoutCost$Exp4Run11SUM), sum(banksBailoutCost$Exp4Run12SUM), sum(banksBailoutCost$Exp4Run13SUM), sum(banksBailoutCost$Exp4Run14SUM), sum(banksBailoutCost$Exp4Run15SUM),
            sum(banksBailoutCost$Exp4Run16SUM), sum(banksBailoutCost$Exp4Run17SUM), sum(banksBailoutCost$Exp4Run18SUM), sum(banksBailoutCost$Exp4Run19SUM), sum(banksBailoutCost$Exp4Run20SUM),
            sum(banksBailoutCost$Exp4Run21SUM), sum(banksBailoutCost$Exp4Run22SUM), sum(banksBailoutCost$Exp4Run23SUM), sum(banksBailoutCost$Exp4Run24SUM), sum(banksBailoutCost$Exp4Run25SUM),
            sum(banksBailoutCost$Exp5Run1SUM), sum(banksBailoutCost$Exp5Run2SUM), sum(banksBailoutCost$Exp5Run3SUM), sum(banksBailoutCost$Exp5Run4SUM), sum(banksBailoutCost$Exp5Run5SUM),
            sum(banksBailoutCost$Exp5Run6SUM), sum(banksBailoutCost$Exp5Run7SUM), sum(banksBailoutCost$Exp5Run8SUM), sum(banksBailoutCost$Exp5Run9SUM), sum(banksBailoutCost$Exp5Run10SUM),
            sum(banksBailoutCost$Exp5Run11SUM), sum(banksBailoutCost$Exp5Run12SUM), sum(banksBailoutCost$Exp5Run13SUM), sum(banksBailoutCost$Exp5Run14SUM), sum(banksBailoutCost$Exp5Run15SUM),
            sum(banksBailoutCost$Exp5Run16SUM), sum(banksBailoutCost$Exp5Run17SUM), sum(banksBailoutCost$Exp5Run18SUM), sum(banksBailoutCost$Exp5Run19SUM), sum(banksBailoutCost$Exp5Run20SUM),
            sum(banksBailoutCost$Exp5Run21SUM), sum(banksBailoutCost$Exp5Run22SUM), sum(banksBailoutCost$Exp5Run23SUM), sum(banksBailoutCost$Exp5Run24SUM), sum(banksBailoutCost$Exp5Run25SUM),
            sum(banksBailoutCost$Exp6Run1SUM), sum(banksBailoutCost$Exp6Run2SUM), sum(banksBailoutCost$Exp6Run3SUM), sum(banksBailoutCost$Exp6Run4SUM), sum(banksBailoutCost$Exp6Run5SUM),
            sum(banksBailoutCost$Exp6Run6SUM), sum(banksBailoutCost$Exp6Run7SUM), sum(banksBailoutCost$Exp6Run8SUM), sum(banksBailoutCost$Exp6Run9SUM), sum(banksBailoutCost$Exp6Run10SUM),
            sum(banksBailoutCost$Exp6Run11SUM), sum(banksBailoutCost$Exp6Run12SUM), sum(banksBailoutCost$Exp6Run13SUM), sum(banksBailoutCost$Exp6Run14SUM), sum(banksBailoutCost$Exp6Run15SUM),
            sum(banksBailoutCost$Exp6Run16SUM), sum(banksBailoutCost$Exp6Run17SUM), sum(banksBailoutCost$Exp6Run18SUM), sum(banksBailoutCost$Exp6Run19SUM), sum(banksBailoutCost$Exp6Run20SUM),
            sum(banksBailoutCost$Exp6Run21SUM), sum(banksBailoutCost$Exp6Run22SUM), sum(banksBailoutCost$Exp6Run23SUM), sum(banksBailoutCost$Exp6Run24SUM), sum(banksBailoutCost$Exp6Run25SUM),
            sum(banksBailoutCost$Exp7Run1SUM), sum(banksBailoutCost$Exp7Run2SUM), sum(banksBailoutCost$Exp7Run3SUM), sum(banksBailoutCost$Exp7Run4SUM), sum(banksBailoutCost$Exp7Run5SUM),
            sum(banksBailoutCost$Exp7Run6SUM), sum(banksBailoutCost$Exp7Run7SUM), sum(banksBailoutCost$Exp7Run8SUM), sum(banksBailoutCost$Exp7Run9SUM), sum(banksBailoutCost$Exp7Run10SUM),
            sum(banksBailoutCost$Exp7Run11SUM), sum(banksBailoutCost$Exp7Run12SUM), sum(banksBailoutCost$Exp7Run13SUM), sum(banksBailoutCost$Exp7Run14SUM), sum(banksBailoutCost$Exp7Run15SUM),
            sum(banksBailoutCost$Exp7Run16SUM), sum(banksBailoutCost$Exp7Run17SUM), sum(banksBailoutCost$Exp7Run18SUM), sum(banksBailoutCost$Exp7Run19SUM), sum(banksBailoutCost$Exp7Run20SUM),
            sum(banksBailoutCost$Exp7Run21SUM), sum(banksBailoutCost$Exp7Run22SUM), sum(banksBailoutCost$Exp7Run23SUM), sum(banksBailoutCost$Exp7Run24SUM), sum(banksBailoutCost$Exp7Run25SUM),
            sum(banksBailoutCost$Exp8Run1SUM), sum(banksBailoutCost$Exp8Run2SUM), sum(banksBailoutCost$Exp8Run3SUM), sum(banksBailoutCost$Exp8Run4SUM), sum(banksBailoutCost$Exp8Run5SUM),
            sum(banksBailoutCost$Exp8Run6SUM), sum(banksBailoutCost$Exp8Run7SUM), sum(banksBailoutCost$Exp8Run8SUM), sum(banksBailoutCost$Exp8Run9SUM), sum(banksBailoutCost$Exp8Run10SUM),
            sum(banksBailoutCost$Exp8Run11SUM), sum(banksBailoutCost$Exp8Run12SUM), sum(banksBailoutCost$Exp8Run13SUM), sum(banksBailoutCost$Exp8Run14SUM), sum(banksBailoutCost$Exp8Run15SUM),
            sum(banksBailoutCost$Exp8Run16SUM), sum(banksBailoutCost$Exp8Run17SUM), sum(banksBailoutCost$Exp8Run18SUM), sum(banksBailoutCost$Exp8Run19SUM), sum(banksBailoutCost$Exp8Run20SUM),
            sum(banksBailoutCost$Exp8Run21SUM), sum(banksBailoutCost$Exp8Run22SUM), sum(banksBailoutCost$Exp8Run23SUM), sum(banksBailoutCost$Exp8Run24SUM), sum(banksBailoutCost$Exp8Run25SUM),
            sum(banksBailoutCost$Exp9Run1SUM), sum(banksBailoutCost$Exp9Run2SUM), sum(banksBailoutCost$Exp9Run3SUM), sum(banksBailoutCost$Exp9Run4SUM), sum(banksBailoutCost$Exp9Run5SUM),
            sum(banksBailoutCost$Exp9Run6SUM), sum(banksBailoutCost$Exp9Run7SUM), sum(banksBailoutCost$Exp9Run8SUM), sum(banksBailoutCost$Exp9Run9SUM), sum(banksBailoutCost$Exp9Run10SUM),
            sum(banksBailoutCost$Exp9Run11SUM), sum(banksBailoutCost$Exp9Run12SUM), sum(banksBailoutCost$Exp9Run13SUM), sum(banksBailoutCost$Exp9Run14SUM), sum(banksBailoutCost$Exp9Run15SUM),
            sum(banksBailoutCost$Exp9Run16SUM), sum(banksBailoutCost$Exp9Run17SUM), sum(banksBailoutCost$Exp9Run18SUM), sum(banksBailoutCost$Exp9Run19SUM), sum(banksBailoutCost$Exp9Run20SUM),
            sum(banksBailoutCost$Exp9Run21SUM), sum(banksBailoutCost$Exp9Run22SUM), sum(banksBailoutCost$Exp9Run23SUM), sum(banksBailoutCost$Exp9Run24SUM), sum(banksBailoutCost$Exp9Run25SUM),
            sum(banksBailoutCost$Exp10Run1SUM), sum(banksBailoutCost$Exp10Run2SUM), sum(banksBailoutCost$Exp10Run3SUM), sum(banksBailoutCost$Exp10Run4SUM), sum(banksBailoutCost$Exp10Run5SUM),
            sum(banksBailoutCost$Exp10Run6SUM), sum(banksBailoutCost$Exp10Run7SUM), sum(banksBailoutCost$Exp10Run8SUM), sum(banksBailoutCost$Exp10Run9SUM), sum(banksBailoutCost$Exp10Run10SUM),
            sum(banksBailoutCost$Exp10Run11SUM), sum(banksBailoutCost$Exp10Run12SUM), sum(banksBailoutCost$Exp10Run13SUM), sum(banksBailoutCost$Exp10Run14SUM), sum(banksBailoutCost$Exp10Run15SUM),
            sum(banksBailoutCost$Exp10Run16SUM), sum(banksBailoutCost$Exp10Run17SUM), sum(banksBailoutCost$Exp10Run18SUM), sum(banksBailoutCost$Exp10Run19SUM), sum(banksBailoutCost$Exp10Run20SUM),
            sum(banksBailoutCost$Exp10Run21SUM), sum(banksBailoutCost$Exp10Run22SUM), sum(banksBailoutCost$Exp10Run23SUM), sum(banksBailoutCost$Exp10Run24SUM), sum(banksBailoutCost$Exp10Run25SUM),
            sum(banksBailoutCost$Exp11Run1SUM), sum(banksBailoutCost$Exp11Run2SUM), sum(banksBailoutCost$Exp11Run3SUM), sum(banksBailoutCost$Exp11Run4SUM), sum(banksBailoutCost$Exp11Run5SUM),
            sum(banksBailoutCost$Exp11Run6SUM), sum(banksBailoutCost$Exp11Run7SUM), sum(banksBailoutCost$Exp11Run8SUM), sum(banksBailoutCost$Exp11Run9SUM), sum(banksBailoutCost$Exp11Run10SUM),
            sum(banksBailoutCost$Exp11Run11SUM), sum(banksBailoutCost$Exp11Run12SUM), sum(banksBailoutCost$Exp11Run13SUM), sum(banksBailoutCost$Exp11Run14SUM), sum(banksBailoutCost$Exp11Run15SUM),
            sum(banksBailoutCost$Exp11Run16SUM), sum(banksBailoutCost$Exp11Run17SUM), sum(banksBailoutCost$Exp11Run18SUM), sum(banksBailoutCost$Exp11Run19SUM), sum(banksBailoutCost$Exp11Run20SUM),
            sum(banksBailoutCost$Exp11Run21SUM), sum(banksBailoutCost$Exp11Run22SUM), sum(banksBailoutCost$Exp11Run23SUM), sum(banksBailoutCost$Exp11Run24SUM), sum(banksBailoutCost$Exp11Run25SUM),
            sum(banksBailoutCost$Exp12Run1SUM), sum(banksBailoutCost$Exp12Run2SUM), sum(banksBailoutCost$Exp12Run3SUM), sum(banksBailoutCost$Exp12Run4SUM), sum(banksBailoutCost$Exp12Run5SUM),
            sum(banksBailoutCost$Exp12Run6SUM), sum(banksBailoutCost$Exp12Run7SUM), sum(banksBailoutCost$Exp12Run8SUM), sum(banksBailoutCost$Exp12Run9SUM), sum(banksBailoutCost$Exp12Run10SUM),
            sum(banksBailoutCost$Exp12Run11SUM), sum(banksBailoutCost$Exp12Run12SUM), sum(banksBailoutCost$Exp12Run13SUM), sum(banksBailoutCost$Exp12Run14SUM), sum(banksBailoutCost$Exp12Run15SUM),
            sum(banksBailoutCost$Exp12Run16SUM), sum(banksBailoutCost$Exp12Run17SUM), sum(banksBailoutCost$Exp12Run18SUM), sum(banksBailoutCost$Exp12Run19SUM), sum(banksBailoutCost$Exp12Run20SUM),
            sum(banksBailoutCost$Exp12Run21SUM), sum(banksBailoutCost$Exp12Run22SUM), sum(banksBailoutCost$Exp12Run23SUM), sum(banksBailoutCost$Exp12Run24SUM), sum(banksBailoutCost$Exp12Run25SUM))

  
test3 <-c(sum(banksCapitalRatio$Exp1Run1SUM/4000), sum(banksCapitalRatio$Exp1Run2SUM/4000), sum(banksCapitalRatio$Exp1Run3SUM/4000), sum(banksCapitalRatio$Exp1Run4SUM/4000), sum(banksCapitalRatio$Exp1Run5SUM/4000),
          sum(banksCapitalRatio$Exp1Run6SUM/4000), sum(banksCapitalRatio$Exp1Run7SUM/4000), sum(banksCapitalRatio$Exp1Run8SUM/4000), sum(banksCapitalRatio$Exp1Run9SUM/4000), sum(banksCapitalRatio$Exp1Run10SUM/4000),
          sum(banksCapitalRatio$Exp1Run11SUM/4000), sum(banksCapitalRatio$Exp1Run12SUM/4000), sum(banksCapitalRatio$Exp1Run13SUM/4000), sum(banksCapitalRatio$Exp1Run14SUM/4000), sum(banksCapitalRatio$Exp1Run15SUM/4000),
          sum(banksCapitalRatio$Exp1Run16SUM/4000), sum(banksCapitalRatio$Exp1Run17SUM/4000), sum(banksCapitalRatio$Exp1Run18SUM/4000), sum(banksCapitalRatio$Exp1Run19SUM/4000), sum(banksCapitalRatio$Exp1Run20SUM/4000),
          sum(banksCapitalRatio$Exp1Run21SUM/4000), sum(banksCapitalRatio$Exp1Run22SUM/4000), sum(banksCapitalRatio$Exp1Run23SUM/4000), sum(banksCapitalRatio$Exp1Run24SUM/4000), sum(banksCapitalRatio$Exp1Run25SUM/4000),
          sum(banksCapitalRatio$Exp2Run1SUM/4000), sum(banksCapitalRatio$Exp2Run2SUM/4000), sum(banksCapitalRatio$Exp2Run3SUM/4000), sum(banksCapitalRatio$Exp2Run4SUM/4000), sum(banksCapitalRatio$Exp2Run5SUM/4000),
          sum(banksCapitalRatio$Exp2Run6SUM/4000), sum(banksCapitalRatio$Exp2Run7SUM/4000), sum(banksCapitalRatio$Exp2Run8SUM/4000), sum(banksCapitalRatio$Exp2Run9SUM/4000), sum(banksCapitalRatio$Exp2Run10SUM/4000),
          sum(banksCapitalRatio$Exp2Run11SUM/4000), sum(banksCapitalRatio$Exp2Run12SUM/4000), sum(banksCapitalRatio$Exp2Run13SUM/4000), sum(banksCapitalRatio$Exp2Run14SUM/4000), sum(banksCapitalRatio$Exp2Run15SUM/4000),
          sum(banksCapitalRatio$Exp2Run16SUM/4000), sum(banksCapitalRatio$Exp2Run17SUM/4000), sum(banksCapitalRatio$Exp2Run18SUM/4000), sum(banksCapitalRatio$Exp2Run19SUM/4000), sum(banksCapitalRatio$Exp2Run20SUM/4000),
          sum(banksCapitalRatio$Exp2Run21SUM/4000), sum(banksCapitalRatio$Exp2Run22SUM/4000), sum(banksCapitalRatio$Exp2Run23SUM/4000), sum(banksCapitalRatio$Exp2Run24SUM/4000), sum(banksCapitalRatio$Exp2Run25SUM/4000),
          sum(banksCapitalRatio$Exp3Run1SUM/4000), sum(banksCapitalRatio$Exp3Run2SUM/4000), sum(banksCapitalRatio$Exp3Run3SUM/4000), sum(banksCapitalRatio$Exp3Run4SUM/4000), sum(banksCapitalRatio$Exp3Run5SUM/4000),
          sum(banksCapitalRatio$Exp3Run6SUM/4000), sum(banksCapitalRatio$Exp3Run7SUM/4000), sum(banksCapitalRatio$Exp3Run8SUM/4000), sum(banksCapitalRatio$Exp3Run9SUM/4000), sum(banksCapitalRatio$Exp3Run10SUM/4000),
          sum(banksCapitalRatio$Exp3Run11SUM/4000), sum(banksCapitalRatio$Exp3Run12SUM/4000), sum(banksCapitalRatio$Exp3Run13SUM/4000), sum(banksCapitalRatio$Exp3Run14SUM/4000), sum(banksCapitalRatio$Exp3Run15SUM/4000),
          sum(banksCapitalRatio$Exp3Run16SUM/4000), sum(banksCapitalRatio$Exp3Run17SUM/4000), sum(banksCapitalRatio$Exp3Run18SUM/4000), sum(banksCapitalRatio$Exp3Run19SUM/4000), sum(banksCapitalRatio$Exp3Run20SUM/4000),
          sum(banksCapitalRatio$Exp3Run21SUM/4000), sum(banksCapitalRatio$Exp3Run22SUM/4000), sum(banksCapitalRatio$Exp3Run23SUM/4000), sum(banksCapitalRatio$Exp3Run24SUM/4000), sum(banksCapitalRatio$Exp3Run25SUM/4000),
          sum(banksCapitalRatio$Exp4Run1SUM/4000), sum(banksCapitalRatio$Exp4Run2SUM/4000), sum(banksCapitalRatio$Exp4Run3SUM/4000), sum(banksCapitalRatio$Exp4Run4SUM/4000), sum(banksCapitalRatio$Exp4Run5SUM/4000),
          sum(banksCapitalRatio$Exp4Run6SUM/4000), sum(banksCapitalRatio$Exp4Run7SUM/4000), sum(banksCapitalRatio$Exp4Run8SUM/4000), sum(banksCapitalRatio$Exp4Run9SUM/4000), sum(banksCapitalRatio$Exp4Run10SUM/4000),
          sum(banksCapitalRatio$Exp4Run11SUM/4000), sum(banksCapitalRatio$Exp4Run12SUM/4000), sum(banksCapitalRatio$Exp4Run13SUM/4000), sum(banksCapitalRatio$Exp4Run14SUM/4000), sum(banksCapitalRatio$Exp4Run15SUM/4000),
          sum(banksCapitalRatio$Exp4Run16SUM/4000), sum(banksCapitalRatio$Exp4Run17SUM/4000), sum(banksCapitalRatio$Exp4Run18SUM/4000), sum(banksCapitalRatio$Exp4Run19SUM/4000), sum(banksCapitalRatio$Exp4Run20SUM/4000),
          sum(banksCapitalRatio$Exp4Run21SUM/4000), sum(banksCapitalRatio$Exp4Run22SUM/4000), sum(banksCapitalRatio$Exp4Run23SUM/4000), sum(banksCapitalRatio$Exp4Run24SUM/4000), sum(banksCapitalRatio$Exp4Run25SUM/4000),
          sum(banksCapitalRatio$Exp5Run1SUM/4000), sum(banksCapitalRatio$Exp5Run2SUM/4000), sum(banksCapitalRatio$Exp5Run3SUM/4000), sum(banksCapitalRatio$Exp5Run4SUM/4000), sum(banksCapitalRatio$Exp5Run5SUM/4000),
          sum(banksCapitalRatio$Exp5Run6SUM/4000), sum(banksCapitalRatio$Exp5Run7SUM/4000), sum(banksCapitalRatio$Exp5Run8SUM/4000), sum(banksCapitalRatio$Exp5Run9SUM/4000), sum(banksCapitalRatio$Exp5Run10SUM/4000),
          sum(banksCapitalRatio$Exp5Run11SUM/4000), sum(banksCapitalRatio$Exp5Run12SUM/4000), sum(banksCapitalRatio$Exp5Run13SUM/4000), sum(banksCapitalRatio$Exp5Run14SUM/4000), sum(banksCapitalRatio$Exp5Run15SUM/4000),
          sum(banksCapitalRatio$Exp5Run16SUM/4000), sum(banksCapitalRatio$Exp5Run17SUM/4000), sum(banksCapitalRatio$Exp5Run18SUM/4000), sum(banksCapitalRatio$Exp5Run19SUM/4000), sum(banksCapitalRatio$Exp5Run20SUM/4000),
          sum(banksCapitalRatio$Exp5Run21SUM/4000), sum(banksCapitalRatio$Exp5Run22SUM/4000), sum(banksCapitalRatio$Exp5Run23SUM/4000), sum(banksCapitalRatio$Exp5Run24SUM/4000), sum(banksCapitalRatio$Exp5Run25SUM/4000),
          sum(banksCapitalRatio$Exp6Run1SUM/4000), sum(banksCapitalRatio$Exp6Run2SUM/4000), sum(banksCapitalRatio$Exp6Run3SUM/4000), sum(banksCapitalRatio$Exp6Run4SUM/4000), sum(banksCapitalRatio$Exp6Run5SUM/4000),
          sum(banksCapitalRatio$Exp6Run6SUM/4000), sum(banksCapitalRatio$Exp6Run7SUM/4000), sum(banksCapitalRatio$Exp6Run8SUM/4000), sum(banksCapitalRatio$Exp6Run9SUM/4000), sum(banksCapitalRatio$Exp6Run10SUM/4000),
          sum(banksCapitalRatio$Exp6Run11SUM/4000), sum(banksCapitalRatio$Exp6Run12SUM/4000), sum(banksCapitalRatio$Exp6Run13SUM/4000), sum(banksCapitalRatio$Exp6Run14SUM/4000), sum(banksCapitalRatio$Exp6Run15SUM/4000),
          sum(banksCapitalRatio$Exp6Run16SUM/4000), sum(banksCapitalRatio$Exp6Run17SUM/4000), sum(banksCapitalRatio$Exp6Run18SUM/4000), sum(banksCapitalRatio$Exp6Run19SUM/4000), sum(banksCapitalRatio$Exp6Run20SUM/4000),
          sum(banksCapitalRatio$Exp6Run21SUM/4000), sum(banksCapitalRatio$Exp6Run22SUM/4000), sum(banksCapitalRatio$Exp6Run23SUM/4000), sum(banksCapitalRatio$Exp6Run24SUM/4000), sum(banksCapitalRatio$Exp6Run25SUM/4000),
          sum(banksCapitalRatio$Exp7Run1SUM/4000), sum(banksCapitalRatio$Exp7Run2SUM/4000), sum(banksCapitalRatio$Exp7Run3SUM/4000), sum(banksCapitalRatio$Exp7Run4SUM/4000), sum(banksCapitalRatio$Exp7Run5SUM/4000),
          sum(banksCapitalRatio$Exp7Run6SUM/4000), sum(banksCapitalRatio$Exp7Run7SUM/4000), sum(banksCapitalRatio$Exp7Run8SUM/4000), sum(banksCapitalRatio$Exp7Run9SUM/4000), sum(banksCapitalRatio$Exp7Run10SUM/4000),
          sum(banksCapitalRatio$Exp7Run11SUM/4000), sum(banksCapitalRatio$Exp7Run12SUM/4000), sum(banksCapitalRatio$Exp7Run13SUM/4000), sum(banksCapitalRatio$Exp7Run14SUM/4000), sum(banksCapitalRatio$Exp7Run15SUM/4000),
          sum(banksCapitalRatio$Exp7Run16SUM/4000), sum(banksCapitalRatio$Exp7Run17SUM/4000), sum(banksCapitalRatio$Exp7Run18SUM/4000), sum(banksCapitalRatio$Exp7Run19SUM/4000), sum(banksCapitalRatio$Exp7Run20SUM/4000),
          sum(banksCapitalRatio$Exp7Run21SUM/4000), sum(banksCapitalRatio$Exp7Run22SUM/4000), sum(banksCapitalRatio$Exp7Run23SUM/4000), sum(banksCapitalRatio$Exp7Run24SUM/4000), sum(banksCapitalRatio$Exp7Run25SUM/4000),
          sum(banksCapitalRatio$Exp8Run1SUM/4000), sum(banksCapitalRatio$Exp8Run2SUM/4000), sum(banksCapitalRatio$Exp8Run3SUM/4000), sum(banksCapitalRatio$Exp8Run4SUM/4000), sum(banksCapitalRatio$Exp8Run5SUM/4000),
          sum(banksCapitalRatio$Exp8Run6SUM/4000), sum(banksCapitalRatio$Exp8Run7SUM/4000), sum(banksCapitalRatio$Exp8Run8SUM/4000), sum(banksCapitalRatio$Exp8Run9SUM/4000), sum(banksCapitalRatio$Exp8Run10SUM/4000),
          sum(banksCapitalRatio$Exp8Run11SUM/4000), sum(banksCapitalRatio$Exp8Run12SUM/4000), sum(banksCapitalRatio$Exp8Run13SUM/4000), sum(banksCapitalRatio$Exp8Run14SUM/4000), sum(banksCapitalRatio$Exp8Run15SUM/4000),
          sum(banksCapitalRatio$Exp8Run16SUM/4000), sum(banksCapitalRatio$Exp8Run17SUM/4000), sum(banksCapitalRatio$Exp8Run18SUM/4000), sum(banksCapitalRatio$Exp8Run19SUM/4000), sum(banksCapitalRatio$Exp8Run20SUM/4000),
          sum(banksCapitalRatio$Exp8Run21SUM/4000), sum(banksCapitalRatio$Exp8Run22SUM/4000), sum(banksCapitalRatio$Exp8Run23SUM/4000), sum(banksCapitalRatio$Exp8Run24SUM/4000), sum(banksCapitalRatio$Exp8Run25SUM/4000),
          sum(banksCapitalRatio$Exp9Run1SUM/4000), sum(banksCapitalRatio$Exp9Run2SUM/4000), sum(banksCapitalRatio$Exp9Run3SUM/4000), sum(banksCapitalRatio$Exp9Run4SUM/4000), sum(banksCapitalRatio$Exp9Run5SUM/4000),
          sum(banksCapitalRatio$Exp9Run6SUM/4000), sum(banksCapitalRatio$Exp9Run7SUM/4000), sum(banksCapitalRatio$Exp9Run8SUM/4000), sum(banksCapitalRatio$Exp9Run9SUM/4000), sum(banksCapitalRatio$Exp9Run10SUM/4000),
          sum(banksCapitalRatio$Exp9Run11SUM/4000), sum(banksCapitalRatio$Exp9Run12SUM/4000), sum(banksCapitalRatio$Exp9Run13SUM/4000), sum(banksCapitalRatio$Exp9Run14SUM/4000), sum(banksCapitalRatio$Exp9Run15SUM/4000),
          sum(banksCapitalRatio$Exp9Run16SUM/4000), sum(banksCapitalRatio$Exp9Run17SUM/4000), sum(banksCapitalRatio$Exp9Run18SUM/4000), sum(banksCapitalRatio$Exp9Run19SUM/4000), sum(banksCapitalRatio$Exp9Run20SUM/4000),
          sum(banksCapitalRatio$Exp9Run21SUM/4000), sum(banksCapitalRatio$Exp9Run22SUM/4000), sum(banksCapitalRatio$Exp9Run23SUM/4000), sum(banksCapitalRatio$Exp9Run24SUM/4000), sum(banksCapitalRatio$Exp9Run25SUM/4000),
          sum(banksCapitalRatio$Exp10Run1SUM/4000), sum(banksCapitalRatio$Exp10Run2SUM/4000), sum(banksCapitalRatio$Exp10Run3SUM/4000), sum(banksCapitalRatio$Exp10Run4SUM/4000), sum(banksCapitalRatio$Exp10Run5SUM/4000),
          sum(banksCapitalRatio$Exp10Run6SUM/4000), sum(banksCapitalRatio$Exp10Run7SUM/4000), sum(banksCapitalRatio$Exp10Run8SUM/4000), sum(banksCapitalRatio$Exp10Run9SUM/4000), sum(banksCapitalRatio$Exp10Run10SUM/4000),
          sum(banksCapitalRatio$Exp10Run11SUM/4000), sum(banksCapitalRatio$Exp10Run12SUM/4000), sum(banksCapitalRatio$Exp10Run13SUM/4000), sum(banksCapitalRatio$Exp10Run14SUM/4000), sum(banksCapitalRatio$Exp10Run15SUM/4000),
          sum(banksCapitalRatio$Exp10Run16SUM/4000), sum(banksCapitalRatio$Exp10Run17SUM/4000), sum(banksCapitalRatio$Exp10Run18SUM/4000), sum(banksCapitalRatio$Exp10Run19SUM/4000), sum(banksCapitalRatio$Exp10Run20SUM/4000),
          sum(banksCapitalRatio$Exp10Run21SUM/4000), sum(banksCapitalRatio$Exp10Run22SUM/4000), sum(banksCapitalRatio$Exp10Run23SUM/4000), sum(banksCapitalRatio$Exp10Run24SUM/4000), sum(banksCapitalRatio$Exp10Run25SUM/4000),
          sum(banksCapitalRatio$Exp11Run1SUM/4000), sum(banksCapitalRatio$Exp11Run2SUM/4000), sum(banksCapitalRatio$Exp11Run3SUM/4000), sum(banksCapitalRatio$Exp11Run4SUM/4000), sum(banksCapitalRatio$Exp11Run5SUM/4000),
          sum(banksCapitalRatio$Exp11Run6SUM/4000), sum(banksCapitalRatio$Exp11Run7SUM/4000), sum(banksCapitalRatio$Exp11Run8SUM/4000), sum(banksCapitalRatio$Exp11Run9SUM/4000), sum(banksCapitalRatio$Exp11Run10SUM/4000),
          sum(banksCapitalRatio$Exp11Run11SUM/4000), sum(banksCapitalRatio$Exp11Run12SUM/4000), sum(banksCapitalRatio$Exp11Run13SUM/4000), sum(banksCapitalRatio$Exp11Run14SUM/4000), sum(banksCapitalRatio$Exp11Run15SUM/4000),
          sum(banksCapitalRatio$Exp11Run16SUM/4000), sum(banksCapitalRatio$Exp11Run17SUM/4000), sum(banksCapitalRatio$Exp11Run18SUM/4000), sum(banksCapitalRatio$Exp11Run19SUM/4000), sum(banksCapitalRatio$Exp11Run20SUM/4000),
          sum(banksCapitalRatio$Exp11Run21SUM/4000), sum(banksCapitalRatio$Exp11Run22SUM/4000), sum(banksCapitalRatio$Exp11Run23SUM/4000), sum(banksCapitalRatio$Exp11Run24SUM/4000), sum(banksCapitalRatio$Exp11Run25SUM/4000),
          sum(banksCapitalRatio$Exp12Run1SUM/4000), sum(banksCapitalRatio$Exp12Run2SUM/4000), sum(banksCapitalRatio$Exp12Run3SUM/4000), sum(banksCapitalRatio$Exp12Run4SUM/4000), sum(banksCapitalRatio$Exp12Run5SUM/4000),
          sum(banksCapitalRatio$Exp12Run6SUM/4000), sum(banksCapitalRatio$Exp12Run7SUM/4000), sum(banksCapitalRatio$Exp12Run8SUM/4000), sum(banksCapitalRatio$Exp12Run9SUM/4000), sum(banksCapitalRatio$Exp12Run10SUM/4000),
          sum(banksCapitalRatio$Exp12Run11SUM/4000), sum(banksCapitalRatio$Exp12Run12SUM/4000), sum(banksCapitalRatio$Exp12Run13SUM/4000), sum(banksCapitalRatio$Exp12Run14SUM/4000), sum(banksCapitalRatio$Exp12Run15SUM/4000),
          sum(banksCapitalRatio$Exp12Run16SUM/4000), sum(banksCapitalRatio$Exp12Run17SUM/4000), sum(banksCapitalRatio$Exp12Run18SUM/4000), sum(banksCapitalRatio$Exp12Run19SUM/4000), sum(banksCapitalRatio$Exp12Run20SUM/4000),
          sum(banksCapitalRatio$Exp12Run21SUM/4000), sum(banksCapitalRatio$Exp12Run22SUM/4000), sum(banksCapitalRatio$Exp12Run23SUM/4000), sum(banksCapitalRatio$Exp12Run24SUM/4000), sum(banksCapitalRatio$Exp12Run25SUM/4000))

  
test4 <-c(sum(nomconsAggregate$Exp1Run1SUM/400), sum(nomconsAggregate$Exp1Run2SUM/400), sum(nomconsAggregate$Exp1Run3SUM/400), sum(nomconsAggregate$Exp1Run4SUM/400), sum(nomconsAggregate$Exp1Run5SUM/400),
          sum(nomconsAggregate$Exp1Run6SUM/400), sum(nomconsAggregate$Exp1Run7SUM/400), sum(nomconsAggregate$Exp1Run8SUM/400), sum(nomconsAggregate$Exp1Run9SUM/400), sum(nomconsAggregate$Exp1Run10SUM/400),
          sum(nomconsAggregate$Exp1Run11SUM/400), sum(nomconsAggregate$Exp1Run12SUM/400), sum(nomconsAggregate$Exp1Run13SUM/400), sum(nomconsAggregate$Exp1Run14SUM/400), sum(nomconsAggregate$Exp1Run15SUM/400),
          sum(nomconsAggregate$Exp1Run16SUM/400), sum(nomconsAggregate$Exp1Run17SUM/400), sum(nomconsAggregate$Exp1Run18SUM/400), sum(nomconsAggregate$Exp1Run19SUM/400), sum(nomconsAggregate$Exp1Run20SUM/400),
          sum(nomconsAggregate$Exp1Run21SUM/400), sum(nomconsAggregate$Exp1Run22SUM/400), sum(nomconsAggregate$Exp1Run23SUM/400), sum(nomconsAggregate$Exp1Run24SUM/400), sum(nomconsAggregate$Exp1Run25SUM/400),
          sum(nomconsAggregate$Exp2Run1SUM/400), sum(nomconsAggregate$Exp2Run2SUM/400), sum(nomconsAggregate$Exp2Run3SUM/400), sum(nomconsAggregate$Exp2Run4SUM/400), sum(nomconsAggregate$Exp2Run5SUM/400),
          sum(nomconsAggregate$Exp2Run6SUM/400), sum(nomconsAggregate$Exp2Run7SUM/400), sum(nomconsAggregate$Exp2Run8SUM/400), sum(nomconsAggregate$Exp2Run9SUM/400), sum(nomconsAggregate$Exp2Run10SUM/400),
          sum(nomconsAggregate$Exp2Run11SUM/400), sum(nomconsAggregate$Exp2Run12SUM/400), sum(nomconsAggregate$Exp2Run13SUM/400), sum(nomconsAggregate$Exp2Run14SUM/400), sum(nomconsAggregate$Exp2Run15SUM/400),
          sum(nomconsAggregate$Exp2Run16SUM/400), sum(nomconsAggregate$Exp2Run17SUM/400), sum(nomconsAggregate$Exp2Run18SUM/400), sum(nomconsAggregate$Exp2Run19SUM/400), sum(nomconsAggregate$Exp2Run20SUM/400),
          sum(nomconsAggregate$Exp2Run21SUM/400), sum(nomconsAggregate$Exp2Run22SUM/400), sum(nomconsAggregate$Exp2Run23SUM/400), sum(nomconsAggregate$Exp2Run24SUM/400), sum(nomconsAggregate$Exp2Run25SUM/400),
          sum(nomconsAggregate$Exp3Run1SUM/400), sum(nomconsAggregate$Exp3Run2SUM/400), sum(nomconsAggregate$Exp3Run3SUM/400), sum(nomconsAggregate$Exp3Run4SUM/400), sum(nomconsAggregate$Exp3Run5SUM/400),
          sum(nomconsAggregate$Exp3Run6SUM/400), sum(nomconsAggregate$Exp3Run7SUM/400), sum(nomconsAggregate$Exp3Run8SUM/400), sum(nomconsAggregate$Exp3Run9SUM/400), sum(nomconsAggregate$Exp3Run10SUM/400),
          sum(nomconsAggregate$Exp3Run11SUM/400), sum(nomconsAggregate$Exp3Run12SUM/400), sum(nomconsAggregate$Exp3Run13SUM/400), sum(nomconsAggregate$Exp3Run14SUM/400), sum(nomconsAggregate$Exp3Run15SUM/400),
          sum(nomconsAggregate$Exp3Run16SUM/400), sum(nomconsAggregate$Exp3Run17SUM/400), sum(nomconsAggregate$Exp3Run18SUM/400), sum(nomconsAggregate$Exp3Run19SUM/400), sum(nomconsAggregate$Exp3Run20SUM/400),
          sum(nomconsAggregate$Exp3Run21SUM/400), sum(nomconsAggregate$Exp3Run22SUM/400), sum(nomconsAggregate$Exp3Run23SUM/400), sum(nomconsAggregate$Exp3Run24SUM/400), sum(nomconsAggregate$Exp3Run25SUM/400),
          sum(nomconsAggregate$Exp4Run1SUM/400), sum(nomconsAggregate$Exp4Run2SUM/400), sum(nomconsAggregate$Exp4Run3SUM/400), sum(nomconsAggregate$Exp4Run4SUM/400), sum(nomconsAggregate$Exp4Run5SUM/400),
          sum(nomconsAggregate$Exp4Run6SUM/400), sum(nomconsAggregate$Exp4Run7SUM/400), sum(nomconsAggregate$Exp4Run8SUM/400), sum(nomconsAggregate$Exp4Run9SUM/400), sum(nomconsAggregate$Exp4Run10SUM/400),
          sum(nomconsAggregate$Exp4Run11SUM/400), sum(nomconsAggregate$Exp4Run12SUM/400), sum(nomconsAggregate$Exp4Run13SUM/400), sum(nomconsAggregate$Exp4Run14SUM/400), sum(nomconsAggregate$Exp4Run15SUM/400),
          sum(nomconsAggregate$Exp4Run16SUM/400), sum(nomconsAggregate$Exp4Run17SUM/400), sum(nomconsAggregate$Exp4Run18SUM/400), sum(nomconsAggregate$Exp4Run19SUM/400), sum(nomconsAggregate$Exp4Run20SUM/400),
          sum(nomconsAggregate$Exp4Run21SUM/400), sum(nomconsAggregate$Exp4Run22SUM/400), sum(nomconsAggregate$Exp4Run23SUM/400), sum(nomconsAggregate$Exp4Run24SUM/400), sum(nomconsAggregate$Exp4Run25SUM/400),
          sum(nomconsAggregate$Exp5Run1SUM/400), sum(nomconsAggregate$Exp5Run2SUM/400), sum(nomconsAggregate$Exp5Run3SUM/400), sum(nomconsAggregate$Exp5Run4SUM/400), sum(nomconsAggregate$Exp5Run5SUM/400),
          sum(nomconsAggregate$Exp5Run6SUM/400), sum(nomconsAggregate$Exp5Run7SUM/400), sum(nomconsAggregate$Exp5Run8SUM/400), sum(nomconsAggregate$Exp5Run9SUM/400), sum(nomconsAggregate$Exp5Run10SUM/400),
          sum(nomconsAggregate$Exp5Run11SUM/400), sum(nomconsAggregate$Exp5Run12SUM/400), sum(nomconsAggregate$Exp5Run13SUM/400), sum(nomconsAggregate$Exp5Run14SUM/400), sum(nomconsAggregate$Exp5Run15SUM/400),
          sum(nomconsAggregate$Exp5Run16SUM/400), sum(nomconsAggregate$Exp5Run17SUM/400), sum(nomconsAggregate$Exp5Run18SUM/400), sum(nomconsAggregate$Exp5Run19SUM/400), sum(nomconsAggregate$Exp5Run20SUM/400),
          sum(nomconsAggregate$Exp5Run21SUM/400), sum(nomconsAggregate$Exp5Run22SUM/400), sum(nomconsAggregate$Exp5Run23SUM/400), sum(nomconsAggregate$Exp5Run24SUM/400), sum(nomconsAggregate$Exp5Run25SUM/400),
          sum(nomconsAggregate$Exp6Run1SUM/400), sum(nomconsAggregate$Exp6Run2SUM/400), sum(nomconsAggregate$Exp6Run3SUM/400), sum(nomconsAggregate$Exp6Run4SUM/400), sum(nomconsAggregate$Exp6Run5SUM/400),
          sum(nomconsAggregate$Exp6Run6SUM/400), sum(nomconsAggregate$Exp6Run7SUM/400), sum(nomconsAggregate$Exp6Run8SUM/400), sum(nomconsAggregate$Exp6Run9SUM/400), sum(nomconsAggregate$Exp6Run10SUM/400),
          sum(nomconsAggregate$Exp6Run11SUM/400), sum(nomconsAggregate$Exp6Run12SUM/400), sum(nomconsAggregate$Exp6Run13SUM/400), sum(nomconsAggregate$Exp6Run14SUM/400), sum(nomconsAggregate$Exp6Run15SUM/400),
          sum(nomconsAggregate$Exp6Run16SUM/400), sum(nomconsAggregate$Exp6Run17SUM/400), sum(nomconsAggregate$Exp6Run18SUM/400), sum(nomconsAggregate$Exp6Run19SUM/400), sum(nomconsAggregate$Exp6Run20SUM/400),
          sum(nomconsAggregate$Exp6Run21SUM/400), sum(nomconsAggregate$Exp6Run22SUM/400), sum(nomconsAggregate$Exp6Run23SUM/400), sum(nomconsAggregate$Exp6Run24SUM/400), sum(nomconsAggregate$Exp6Run25SUM/400),
          sum(nomconsAggregate$Exp7Run1SUM/400), sum(nomconsAggregate$Exp7Run2SUM/400), sum(nomconsAggregate$Exp7Run3SUM/400), sum(nomconsAggregate$Exp7Run4SUM/400), sum(nomconsAggregate$Exp7Run5SUM/400),
          sum(nomconsAggregate$Exp7Run6SUM/400), sum(nomconsAggregate$Exp7Run7SUM/400), sum(nomconsAggregate$Exp7Run8SUM/400), sum(nomconsAggregate$Exp7Run9SUM/400), sum(nomconsAggregate$Exp7Run10SUM/400),
          sum(nomconsAggregate$Exp7Run11SUM/400), sum(nomconsAggregate$Exp7Run12SUM/400), sum(nomconsAggregate$Exp7Run13SUM/400), sum(nomconsAggregate$Exp7Run14SUM/400), sum(nomconsAggregate$Exp7Run15SUM/400),
          sum(nomconsAggregate$Exp7Run16SUM/400), sum(nomconsAggregate$Exp7Run17SUM/400), sum(nomconsAggregate$Exp7Run18SUM/400), sum(nomconsAggregate$Exp7Run19SUM/400), sum(nomconsAggregate$Exp7Run20SUM/400),
          sum(nomconsAggregate$Exp7Run21SUM/400), sum(nomconsAggregate$Exp7Run22SUM/400), sum(nomconsAggregate$Exp7Run23SUM/400), sum(nomconsAggregate$Exp7Run24SUM/400), sum(nomconsAggregate$Exp7Run25SUM/400),
          sum(nomconsAggregate$Exp8Run1SUM/400), sum(nomconsAggregate$Exp8Run2SUM/400), sum(nomconsAggregate$Exp8Run3SUM/400), sum(nomconsAggregate$Exp8Run4SUM/400), sum(nomconsAggregate$Exp8Run5SUM/400),
          sum(nomconsAggregate$Exp8Run6SUM/400), sum(nomconsAggregate$Exp8Run7SUM/400), sum(nomconsAggregate$Exp8Run8SUM/400), sum(nomconsAggregate$Exp8Run9SUM/400), sum(nomconsAggregate$Exp8Run10SUM/400),
          sum(nomconsAggregate$Exp8Run11SUM/400), sum(nomconsAggregate$Exp8Run12SUM/400), sum(nomconsAggregate$Exp8Run13SUM/400), sum(nomconsAggregate$Exp8Run14SUM/400), sum(nomconsAggregate$Exp8Run15SUM/400),
          sum(nomconsAggregate$Exp8Run16SUM/400), sum(nomconsAggregate$Exp8Run17SUM/400), sum(nomconsAggregate$Exp8Run18SUM/400), sum(nomconsAggregate$Exp8Run19SUM/400), sum(nomconsAggregate$Exp8Run20SUM/400),
          sum(nomconsAggregate$Exp8Run21SUM/400), sum(nomconsAggregate$Exp8Run22SUM/400), sum(nomconsAggregate$Exp8Run23SUM/400), sum(nomconsAggregate$Exp8Run24SUM/400), sum(nomconsAggregate$Exp8Run25SUM/400),
          sum(nomconsAggregate$Exp9Run1SUM/400), sum(nomconsAggregate$Exp9Run2SUM/400), sum(nomconsAggregate$Exp9Run3SUM/400), sum(nomconsAggregate$Exp9Run4SUM/400), sum(nomconsAggregate$Exp9Run5SUM/400),
          sum(nomconsAggregate$Exp9Run6SUM/400), sum(nomconsAggregate$Exp9Run7SUM/400), sum(nomconsAggregate$Exp9Run8SUM/400), sum(nomconsAggregate$Exp9Run9SUM/400), sum(nomconsAggregate$Exp9Run10SUM/400),
          sum(nomconsAggregate$Exp9Run11SUM/400), sum(nomconsAggregate$Exp9Run12SUM/400), sum(nomconsAggregate$Exp9Run13SUM/400), sum(nomconsAggregate$Exp9Run14SUM/400), sum(nomconsAggregate$Exp9Run15SUM/400),
          sum(nomconsAggregate$Exp9Run16SUM/400), sum(nomconsAggregate$Exp9Run17SUM/400), sum(nomconsAggregate$Exp9Run18SUM/400), sum(nomconsAggregate$Exp9Run19SUM/400), sum(nomconsAggregate$Exp9Run20SUM/400),
          sum(nomconsAggregate$Exp9Run21SUM/400), sum(nomconsAggregate$Exp9Run22SUM/400), sum(nomconsAggregate$Exp9Run23SUM/400), sum(nomconsAggregate$Exp9Run24SUM/400), sum(nomconsAggregate$Exp9Run25SUM/400),
          sum(nomconsAggregate$Exp10Run1SUM/400), sum(nomconsAggregate$Exp10Run2SUM/400), sum(nomconsAggregate$Exp10Run3SUM/400), sum(nomconsAggregate$Exp10Run4SUM/400), sum(nomconsAggregate$Exp10Run5SUM/400),
          sum(nomconsAggregate$Exp10Run6SUM/400), sum(nomconsAggregate$Exp10Run7SUM/400), sum(nomconsAggregate$Exp10Run8SUM/400), sum(nomconsAggregate$Exp10Run9SUM/400), sum(nomconsAggregate$Exp10Run10SUM/400),
          sum(nomconsAggregate$Exp10Run11SUM/400), sum(nomconsAggregate$Exp10Run12SUM/400), sum(nomconsAggregate$Exp10Run13SUM/400), sum(nomconsAggregate$Exp10Run14SUM/400), sum(nomconsAggregate$Exp10Run15SUM/400),
          sum(nomconsAggregate$Exp10Run16SUM/400), sum(nomconsAggregate$Exp10Run17SUM/400), sum(nomconsAggregate$Exp10Run18SUM/400), sum(nomconsAggregate$Exp10Run19SUM/400), sum(nomconsAggregate$Exp10Run20SUM/400),
          sum(nomconsAggregate$Exp10Run21SUM/400), sum(nomconsAggregate$Exp10Run22SUM/400), sum(nomconsAggregate$Exp10Run23SUM/400), sum(nomconsAggregate$Exp10Run24SUM/400), sum(nomconsAggregate$Exp10Run25SUM/400),
          sum(nomconsAggregate$Exp11Run1SUM/400), sum(nomconsAggregate$Exp11Run2SUM/400), sum(nomconsAggregate$Exp11Run3SUM/400), sum(nomconsAggregate$Exp11Run4SUM/400), sum(nomconsAggregate$Exp11Run5SUM/400),
          sum(nomconsAggregate$Exp11Run6SUM/400), sum(nomconsAggregate$Exp11Run7SUM/400), sum(nomconsAggregate$Exp11Run8SUM/400), sum(nomconsAggregate$Exp11Run9SUM/400), sum(nomconsAggregate$Exp11Run10SUM/400),
          sum(nomconsAggregate$Exp11Run11SUM/400), sum(nomconsAggregate$Exp11Run12SUM/400), sum(nomconsAggregate$Exp11Run13SUM/400), sum(nomconsAggregate$Exp11Run14SUM/400), sum(nomconsAggregate$Exp11Run15SUM/400),
          sum(nomconsAggregate$Exp11Run16SUM/400), sum(nomconsAggregate$Exp11Run17SUM/400), sum(nomconsAggregate$Exp11Run18SUM/400), sum(nomconsAggregate$Exp11Run19SUM/400), sum(nomconsAggregate$Exp11Run20SUM/400),
          sum(nomconsAggregate$Exp11Run21SUM/400), sum(nomconsAggregate$Exp11Run22SUM/400), sum(nomconsAggregate$Exp11Run23SUM/400), sum(nomconsAggregate$Exp11Run24SUM/400), sum(nomconsAggregate$Exp11Run25SUM/400),
          sum(nomconsAggregate$Exp12Run1SUM/400), sum(nomconsAggregate$Exp12Run2SUM/400), sum(nomconsAggregate$Exp12Run3SUM/400), sum(nomconsAggregate$Exp12Run4SUM/400), sum(nomconsAggregate$Exp12Run5SUM/400),
          sum(nomconsAggregate$Exp12Run6SUM/400), sum(nomconsAggregate$Exp12Run7SUM/400), sum(nomconsAggregate$Exp12Run8SUM/400), sum(nomconsAggregate$Exp12Run9SUM/400), sum(nomconsAggregate$Exp12Run10SUM/400),
          sum(nomconsAggregate$Exp12Run11SUM/400), sum(nomconsAggregate$Exp12Run12SUM/400), sum(nomconsAggregate$Exp12Run13SUM/400), sum(nomconsAggregate$Exp12Run14SUM/400), sum(nomconsAggregate$Exp12Run15SUM/400),
          sum(nomconsAggregate$Exp12Run16SUM/400), sum(nomconsAggregate$Exp12Run17SUM/400), sum(nomconsAggregate$Exp12Run18SUM/400), sum(nomconsAggregate$Exp12Run19SUM/400), sum(nomconsAggregate$Exp12Run20SUM/400),
          sum(nomconsAggregate$Exp12Run21SUM/400), sum(nomconsAggregate$Exp12Run22SUM/400), sum(nomconsAggregate$Exp12Run23SUM/400), sum(nomconsAggregate$Exp12Run24SUM/400), sum(nomconsAggregate$Exp12Run25SUM/400))

  
test5 <-c(sum(nominvAggregate$Exp1Run1/400), sum(nominvAggregate$Exp1Run2/400), sum(nominvAggregate$Exp1Run3/400), sum(nominvAggregate$Exp1Run4/400), sum(nominvAggregate$Exp1Run5/400),
          sum(nominvAggregate$Exp1Run6/400), sum(nominvAggregate$Exp1Run7/400), sum(nominvAggregate$Exp1Run8/400), sum(nominvAggregate$Exp1Run9/400), sum(nominvAggregate$Exp1Run10/400),
          sum(nominvAggregate$Exp1Run11/400), sum(nominvAggregate$Exp1Run12/400), sum(nominvAggregate$Exp1Run13/400), sum(nominvAggregate$Exp1Run14/400), sum(nominvAggregate$Exp1Run15/400),
          sum(nominvAggregate$Exp1Run16/400), sum(nominvAggregate$Exp1Run17/400), sum(nominvAggregate$Exp1Run18/400), sum(nominvAggregate$Exp1Run19/400), sum(nominvAggregate$Exp1Run20/400),
          sum(nominvAggregate$Exp1Run21/400), sum(nominvAggregate$Exp1Run22/400), sum(nominvAggregate$Exp1Run23/400), sum(nominvAggregate$Exp1Run24/400), sum(nominvAggregate$Exp1Run25/400),
          sum(nominvAggregate$Exp2Run1/400), sum(nominvAggregate$Exp2Run2/400), sum(nominvAggregate$Exp2Run3/400), sum(nominvAggregate$Exp2Run4/400), sum(nominvAggregate$Exp2Run5/400),
          sum(nominvAggregate$Exp2Run6/400), sum(nominvAggregate$Exp2Run7/400), sum(nominvAggregate$Exp2Run8/400), sum(nominvAggregate$Exp2Run9/400), sum(nominvAggregate$Exp2Run10/400),
          sum(nominvAggregate$Exp2Run11/400), sum(nominvAggregate$Exp2Run12/400), sum(nominvAggregate$Exp2Run13/400), sum(nominvAggregate$Exp2Run14/400), sum(nominvAggregate$Exp2Run15/400),
          sum(nominvAggregate$Exp2Run16/400), sum(nominvAggregate$Exp2Run17/400), sum(nominvAggregate$Exp2Run18/400), sum(nominvAggregate$Exp2Run19/400), sum(nominvAggregate$Exp2Run20/400),
          sum(nominvAggregate$Exp2Run21/400), sum(nominvAggregate$Exp2Run22/400), sum(nominvAggregate$Exp2Run23/400), sum(nominvAggregate$Exp2Run24/400), sum(nominvAggregate$Exp2Run25/400),
          sum(nominvAggregate$Exp3Run1/400), sum(nominvAggregate$Exp3Run2/400), sum(nominvAggregate$Exp3Run3/400), sum(nominvAggregate$Exp3Run4/400), sum(nominvAggregate$Exp3Run5/400),
          sum(nominvAggregate$Exp3Run6/400), sum(nominvAggregate$Exp3Run7/400), sum(nominvAggregate$Exp3Run8/400), sum(nominvAggregate$Exp3Run9/400), sum(nominvAggregate$Exp3Run10/400),
          sum(nominvAggregate$Exp3Run11/400), sum(nominvAggregate$Exp3Run12/400), sum(nominvAggregate$Exp3Run13/400), sum(nominvAggregate$Exp3Run14/400), sum(nominvAggregate$Exp3Run15/400),
          sum(nominvAggregate$Exp3Run16/400), sum(nominvAggregate$Exp3Run17/400), sum(nominvAggregate$Exp3Run18/400), sum(nominvAggregate$Exp3Run19/400), sum(nominvAggregate$Exp3Run20/400),
          sum(nominvAggregate$Exp3Run21/400), sum(nominvAggregate$Exp3Run22/400), sum(nominvAggregate$Exp3Run23/400), sum(nominvAggregate$Exp3Run24/400), sum(nominvAggregate$Exp3Run25/400),
          sum(nominvAggregate$Exp4Run1/400), sum(nominvAggregate$Exp4Run2/400), sum(nominvAggregate$Exp4Run3/400), sum(nominvAggregate$Exp4Run4/400), sum(nominvAggregate$Exp4Run5/400),
          sum(nominvAggregate$Exp4Run6/400), sum(nominvAggregate$Exp4Run7/400), sum(nominvAggregate$Exp4Run8/400), sum(nominvAggregate$Exp4Run9/400), sum(nominvAggregate$Exp4Run10/400),
          sum(nominvAggregate$Exp4Run11/400), sum(nominvAggregate$Exp4Run12/400), sum(nominvAggregate$Exp4Run13/400), sum(nominvAggregate$Exp4Run14/400), sum(nominvAggregate$Exp4Run15/400),
          sum(nominvAggregate$Exp4Run16/400), sum(nominvAggregate$Exp4Run17/400), sum(nominvAggregate$Exp4Run18/400), sum(nominvAggregate$Exp4Run19/400), sum(nominvAggregate$Exp4Run20/400),
          sum(nominvAggregate$Exp4Run21/400), sum(nominvAggregate$Exp4Run22/400), sum(nominvAggregate$Exp4Run23/400), sum(nominvAggregate$Exp4Run24/400), sum(nominvAggregate$Exp4Run25/400),
          sum(nominvAggregate$Exp5Run1/400), sum(nominvAggregate$Exp5Run2/400), sum(nominvAggregate$Exp5Run3/400), sum(nominvAggregate$Exp5Run4/400), sum(nominvAggregate$Exp5Run5/400),
          sum(nominvAggregate$Exp5Run6/400), sum(nominvAggregate$Exp5Run7/400), sum(nominvAggregate$Exp5Run8/400), sum(nominvAggregate$Exp5Run9/400), sum(nominvAggregate$Exp5Run10/400),
          sum(nominvAggregate$Exp5Run11/400), sum(nominvAggregate$Exp5Run12/400), sum(nominvAggregate$Exp5Run13/400), sum(nominvAggregate$Exp5Run14/400), sum(nominvAggregate$Exp5Run15/400),
          sum(nominvAggregate$Exp5Run16/400), sum(nominvAggregate$Exp5Run17/400), sum(nominvAggregate$Exp5Run18/400), sum(nominvAggregate$Exp5Run19/400), sum(nominvAggregate$Exp5Run20/400),
          sum(nominvAggregate$Exp5Run21/400), sum(nominvAggregate$Exp5Run22/400), sum(nominvAggregate$Exp5Run23/400), sum(nominvAggregate$Exp5Run24/400), sum(nominvAggregate$Exp5Run25/400),
          sum(nominvAggregate$Exp6Run1/400), sum(nominvAggregate$Exp6Run2/400), sum(nominvAggregate$Exp6Run3/400), sum(nominvAggregate$Exp6Run4/400), sum(nominvAggregate$Exp6Run5/400),
          sum(nominvAggregate$Exp6Run6/400), sum(nominvAggregate$Exp6Run7/400), sum(nominvAggregate$Exp6Run8/400), sum(nominvAggregate$Exp6Run9/400), sum(nominvAggregate$Exp6Run10/400),
          sum(nominvAggregate$Exp6Run11/400), sum(nominvAggregate$Exp6Run12/400), sum(nominvAggregate$Exp6Run13/400), sum(nominvAggregate$Exp6Run14/400), sum(nominvAggregate$Exp6Run15/400),
          sum(nominvAggregate$Exp6Run16/400), sum(nominvAggregate$Exp6Run17/400), sum(nominvAggregate$Exp6Run18/400), sum(nominvAggregate$Exp6Run19/400), sum(nominvAggregate$Exp6Run20/400),
          sum(nominvAggregate$Exp6Run21/400), sum(nominvAggregate$Exp6Run22/400), sum(nominvAggregate$Exp6Run23/400), sum(nominvAggregate$Exp6Run24/400), sum(nominvAggregate$Exp6Run25/400),
          sum(nominvAggregate$Exp7Run1/400), sum(nominvAggregate$Exp7Run2/400), sum(nominvAggregate$Exp7Run3/400), sum(nominvAggregate$Exp7Run4/400), sum(nominvAggregate$Exp7Run5/400),
          sum(nominvAggregate$Exp7Run6/400), sum(nominvAggregate$Exp7Run7/400), sum(nominvAggregate$Exp7Run8/400), sum(nominvAggregate$Exp7Run9/400), sum(nominvAggregate$Exp7Run10/400),
          sum(nominvAggregate$Exp7Run11/400), sum(nominvAggregate$Exp7Run12/400), sum(nominvAggregate$Exp7Run13/400), sum(nominvAggregate$Exp7Run14/400), sum(nominvAggregate$Exp7Run15/400),
          sum(nominvAggregate$Exp7Run16/400), sum(nominvAggregate$Exp7Run17/400), sum(nominvAggregate$Exp7Run18/400), sum(nominvAggregate$Exp7Run19/400), sum(nominvAggregate$Exp7Run20/400),
          sum(nominvAggregate$Exp7Run21/400), sum(nominvAggregate$Exp7Run22/400), sum(nominvAggregate$Exp7Run23/400), sum(nominvAggregate$Exp7Run24/400), sum(nominvAggregate$Exp7Run25/400),
          sum(nominvAggregate$Exp8Run1/400), sum(nominvAggregate$Exp8Run2/400), sum(nominvAggregate$Exp8Run3/400), sum(nominvAggregate$Exp8Run4/400), sum(nominvAggregate$Exp8Run5/400),
          sum(nominvAggregate$Exp8Run6/400), sum(nominvAggregate$Exp8Run7/400), sum(nominvAggregate$Exp8Run8/400), sum(nominvAggregate$Exp8Run9/400), sum(nominvAggregate$Exp8Run10/400),
          sum(nominvAggregate$Exp8Run11/400), sum(nominvAggregate$Exp8Run12/400), sum(nominvAggregate$Exp8Run13/400), sum(nominvAggregate$Exp8Run14/400), sum(nominvAggregate$Exp8Run15/400),
          sum(nominvAggregate$Exp8Run16/400), sum(nominvAggregate$Exp8Run17/400), sum(nominvAggregate$Exp8Run18/400), sum(nominvAggregate$Exp8Run19/400), sum(nominvAggregate$Exp8Run20/400),
          sum(nominvAggregate$Exp8Run21/400), sum(nominvAggregate$Exp8Run22/400), sum(nominvAggregate$Exp8Run23/400), sum(nominvAggregate$Exp8Run24/400), sum(nominvAggregate$Exp8Run25/400),
          sum(nominvAggregate$Exp9Run1/400), sum(nominvAggregate$Exp9Run2/400), sum(nominvAggregate$Exp9Run3/400), sum(nominvAggregate$Exp9Run4/400), sum(nominvAggregate$Exp9Run5/400),
          sum(nominvAggregate$Exp9Run6/400), sum(nominvAggregate$Exp9Run7/400), sum(nominvAggregate$Exp9Run8/400), sum(nominvAggregate$Exp9Run9/400), sum(nominvAggregate$Exp9Run10/400),
          sum(nominvAggregate$Exp9Run11/400), sum(nominvAggregate$Exp9Run12/400), sum(nominvAggregate$Exp9Run13/400), sum(nominvAggregate$Exp9Run14/400), sum(nominvAggregate$Exp9Run15/400),
          sum(nominvAggregate$Exp9Run16/400), sum(nominvAggregate$Exp9Run17/400), sum(nominvAggregate$Exp9Run18/400), sum(nominvAggregate$Exp9Run19/400), sum(nominvAggregate$Exp9Run20/400),
          sum(nominvAggregate$Exp9Run21/400), sum(nominvAggregate$Exp9Run22/400), sum(nominvAggregate$Exp9Run23/400), sum(nominvAggregate$Exp9Run24/400), sum(nominvAggregate$Exp9Run25/400),
          sum(nominvAggregate$Exp10Run1/400), sum(nominvAggregate$Exp10Run2/400), sum(nominvAggregate$Exp10Run3/400), sum(nominvAggregate$Exp10Run4/400), sum(nominvAggregate$Exp10Run5/400),
          sum(nominvAggregate$Exp10Run6/400), sum(nominvAggregate$Exp10Run7/400), sum(nominvAggregate$Exp10Run8/400), sum(nominvAggregate$Exp10Run9/400), sum(nominvAggregate$Exp10Run10/400),
          sum(nominvAggregate$Exp10Run11/400), sum(nominvAggregate$Exp10Run12/400), sum(nominvAggregate$Exp10Run13/400), sum(nominvAggregate$Exp10Run14/400), sum(nominvAggregate$Exp10Run15/400),
          sum(nominvAggregate$Exp10Run16/400), sum(nominvAggregate$Exp10Run17/400), sum(nominvAggregate$Exp10Run18/400), sum(nominvAggregate$Exp10Run19/400), sum(nominvAggregate$Exp10Run20/400),
          sum(nominvAggregate$Exp10Run21/400), sum(nominvAggregate$Exp10Run22/400), sum(nominvAggregate$Exp10Run23/400), sum(nominvAggregate$Exp10Run24/400), sum(nominvAggregate$Exp10Run25/400),
          sum(nominvAggregate$Exp11Run1/400), sum(nominvAggregate$Exp11Run2/400), sum(nominvAggregate$Exp11Run3/400), sum(nominvAggregate$Exp11Run4/400), sum(nominvAggregate$Exp11Run5/400),
          sum(nominvAggregate$Exp11Run6/400), sum(nominvAggregate$Exp11Run7/400), sum(nominvAggregate$Exp11Run8/400), sum(nominvAggregate$Exp11Run9/400), sum(nominvAggregate$Exp11Run10/400),
          sum(nominvAggregate$Exp11Run11/400), sum(nominvAggregate$Exp11Run12/400), sum(nominvAggregate$Exp11Run13/400), sum(nominvAggregate$Exp11Run14/400), sum(nominvAggregate$Exp11Run15/400),
          sum(nominvAggregate$Exp11Run16/400), sum(nominvAggregate$Exp11Run17/400), sum(nominvAggregate$Exp11Run18/400), sum(nominvAggregate$Exp11Run19/400), sum(nominvAggregate$Exp11Run20/400),
          sum(nominvAggregate$Exp11Run21/400), sum(nominvAggregate$Exp11Run22/400), sum(nominvAggregate$Exp11Run23/400), sum(nominvAggregate$Exp11Run24/400), sum(nominvAggregate$Exp11Run25/400),
          sum(nominvAggregate$Exp12Run1/400), sum(nominvAggregate$Exp12Run2/400), sum(nominvAggregate$Exp12Run3/400), sum(nominvAggregate$Exp12Run4/400), sum(nominvAggregate$Exp12Run5/400),
          sum(nominvAggregate$Exp12Run6/400), sum(nominvAggregate$Exp12Run7/400), sum(nominvAggregate$Exp12Run8/400), sum(nominvAggregate$Exp12Run9/400), sum(nominvAggregate$Exp12Run10/400),
          sum(nominvAggregate$Exp12Run11/400), sum(nominvAggregate$Exp12Run12/400), sum(nominvAggregate$Exp12Run13/400), sum(nominvAggregate$Exp12Run14/400), sum(nominvAggregate$Exp12Run15/400),
          sum(nominvAggregate$Exp12Run16/400), sum(nominvAggregate$Exp12Run17/400), sum(nominvAggregate$Exp12Run18/400), sum(nominvAggregate$Exp12Run19/400), sum(nominvAggregate$Exp12Run20/400),
          sum(nominvAggregate$Exp12Run21/400), sum(nominvAggregate$Exp12Run22/400), sum(nominvAggregate$Exp12Run23/400), sum(nominvAggregate$Exp12Run24/400), sum(nominvAggregate$Exp12Run25/400))

  
test6 <-c(sum(gdp$Exp1Run1/400), sum(gdp$Exp1Run2/400), sum(gdp$Exp1Run3/400), sum(gdp$Exp1Run4/400), sum(gdp$Exp1Run5/400),
          sum(gdp$Exp1Run6/400), sum(gdp$Exp1Run7/400), sum(gdp$Exp1Run8/400), sum(gdp$Exp1Run9/400), sum(gdp$Exp1Run10/400),
          sum(gdp$Exp1Run11/400), sum(gdp$Exp1Run12/400), sum(gdp$Exp1Run13/400), sum(gdp$Exp1Run14/400), sum(gdp$Exp1Run15/400),
          sum(gdp$Exp1Run16/400), sum(gdp$Exp1Run17/400), sum(gdp$Exp1Run18/400), sum(gdp$Exp1Run19/400), sum(gdp$Exp1Run20/400),
          sum(gdp$Exp1Run21/400), sum(gdp$Exp1Run22/400), sum(gdp$Exp1Run23/400), sum(gdp$Exp1Run24/400), sum(gdp$Exp1Run25/400),
          sum(gdp$Exp2Run1/400), sum(gdp$Exp2Run2/400), sum(gdp$Exp2Run3/400), sum(gdp$Exp2Run4/400), sum(gdp$Exp2Run5/400),
          sum(gdp$Exp2Run6/400), sum(gdp$Exp2Run7/400), sum(gdp$Exp2Run8/400), sum(gdp$Exp2Run9/400), sum(gdp$Exp2Run10/400),
          sum(gdp$Exp2Run11/400), sum(gdp$Exp2Run12/400), sum(gdp$Exp2Run13/400), sum(gdp$Exp2Run14/400), sum(gdp$Exp2Run15/400),
          sum(gdp$Exp2Run16/400), sum(gdp$Exp2Run17/400), sum(gdp$Exp2Run18/400), sum(gdp$Exp2Run19/400), sum(gdp$Exp2Run20/400),
          sum(gdp$Exp2Run21/400), sum(gdp$Exp2Run22/400), sum(gdp$Exp2Run23/400), sum(gdp$Exp2Run24/400), sum(gdp$Exp2Run25/400),
          sum(gdp$Exp3Run1/400), sum(gdp$Exp3Run2/400), sum(gdp$Exp3Run3/400), sum(gdp$Exp3Run4/400), sum(gdp$Exp3Run5/400),
          sum(gdp$Exp3Run6/400), sum(gdp$Exp3Run7/400), sum(gdp$Exp3Run8/400), sum(gdp$Exp3Run9/400), sum(gdp$Exp3Run10/400),
          sum(gdp$Exp3Run11/400), sum(gdp$Exp3Run12/400), sum(gdp$Exp3Run13/400), sum(gdp$Exp3Run14/400), sum(gdp$Exp3Run15/400),
          sum(gdp$Exp3Run16/400), sum(gdp$Exp3Run17/400), sum(gdp$Exp3Run18/400), sum(gdp$Exp3Run19/400), sum(gdp$Exp3Run20/400),
          sum(gdp$Exp3Run21/400), sum(gdp$Exp3Run22/400), sum(gdp$Exp3Run23/400), sum(gdp$Exp3Run24/400), sum(gdp$Exp3Run25/400),
          sum(gdp$Exp4Run1/400), sum(gdp$Exp4Run2/400), sum(gdp$Exp4Run3/400), sum(gdp$Exp4Run4/400), sum(gdp$Exp4Run5/400),
          sum(gdp$Exp4Run6/400), sum(gdp$Exp4Run7/400), sum(gdp$Exp4Run8/400), sum(gdp$Exp4Run9/400), sum(gdp$Exp4Run10/400),
          sum(gdp$Exp4Run11/400), sum(gdp$Exp4Run12/400), sum(gdp$Exp4Run13/400), sum(gdp$Exp4Run14/400), sum(gdp$Exp4Run15/400),
          sum(gdp$Exp4Run16/400), sum(gdp$Exp4Run17/400), sum(gdp$Exp4Run18/400), sum(gdp$Exp4Run19/400), sum(gdp$Exp4Run20/400),
          sum(gdp$Exp4Run21/400), sum(gdp$Exp4Run22/400), sum(gdp$Exp4Run23/400), sum(gdp$Exp4Run24/400), sum(gdp$Exp4Run25/400),
          sum(gdp$Exp5Run1/400), sum(gdp$Exp5Run2/400), sum(gdp$Exp5Run3/400), sum(gdp$Exp5Run4/400), sum(gdp$Exp5Run5/400),
          sum(gdp$Exp5Run6/400), sum(gdp$Exp5Run7/400), sum(gdp$Exp5Run8/400), sum(gdp$Exp5Run9/400), sum(gdp$Exp5Run10/400),
          sum(gdp$Exp5Run11/400), sum(gdp$Exp5Run12/400), sum(gdp$Exp5Run13/400), sum(gdp$Exp5Run14/400), sum(gdp$Exp5Run15/400),
          sum(gdp$Exp5Run16/400), sum(gdp$Exp5Run17/400), sum(gdp$Exp5Run18/400), sum(gdp$Exp5Run19/400), sum(gdp$Exp5Run20/400),
          sum(gdp$Exp5Run21/400), sum(gdp$Exp5Run22/400), sum(gdp$Exp5Run23/400), sum(gdp$Exp5Run24/400), sum(gdp$Exp5Run25/400),
          sum(gdp$Exp6Run1/400), sum(gdp$Exp6Run2/400), sum(gdp$Exp6Run3/400), sum(gdp$Exp6Run4/400), sum(gdp$Exp6Run5/400),
          sum(gdp$Exp6Run6/400), sum(gdp$Exp6Run7/400), sum(gdp$Exp6Run8/400), sum(gdp$Exp6Run9/400), sum(gdp$Exp6Run10/400),
          sum(gdp$Exp6Run11/400), sum(gdp$Exp6Run12/400), sum(gdp$Exp6Run13/400), sum(gdp$Exp6Run14/400), sum(gdp$Exp6Run15/400),
          sum(gdp$Exp6Run16/400), sum(gdp$Exp6Run17/400), sum(gdp$Exp6Run18/400), sum(gdp$Exp6Run19/400), sum(gdp$Exp6Run20/400),
          sum(gdp$Exp6Run21/400), sum(gdp$Exp6Run22/400), sum(gdp$Exp6Run23/400), sum(gdp$Exp6Run24/400), sum(gdp$Exp6Run25/400),
          sum(gdp$Exp7Run1/400), sum(gdp$Exp7Run2/400), sum(gdp$Exp7Run3/400), sum(gdp$Exp7Run4/400), sum(gdp$Exp7Run5/400),
          sum(gdp$Exp7Run6/400), sum(gdp$Exp7Run7/400), sum(gdp$Exp7Run8/400), sum(gdp$Exp7Run9/400), sum(gdp$Exp7Run10/400),
          sum(gdp$Exp7Run11/400), sum(gdp$Exp7Run12/400), sum(gdp$Exp7Run13/400), sum(gdp$Exp7Run14/400), sum(gdp$Exp7Run15/400),
          sum(gdp$Exp7Run16/400), sum(gdp$Exp7Run17/400), sum(gdp$Exp7Run18/400), sum(gdp$Exp7Run19/400), sum(gdp$Exp7Run20/400),
          sum(gdp$Exp7Run21/400), sum(gdp$Exp7Run22/400), sum(gdp$Exp7Run23/400), sum(gdp$Exp7Run24/400), sum(gdp$Exp7Run25/400),
          sum(gdp$Exp8Run1/400), sum(gdp$Exp8Run2/400), sum(gdp$Exp8Run3/400), sum(gdp$Exp8Run4/400), sum(gdp$Exp8Run5/400),
          sum(gdp$Exp8Run6/400), sum(gdp$Exp8Run7/400), sum(gdp$Exp8Run8/400), sum(gdp$Exp8Run9/400), sum(gdp$Exp8Run10/400),
          sum(gdp$Exp8Run11/400), sum(gdp$Exp8Run12/400), sum(gdp$Exp8Run13/400), sum(gdp$Exp8Run14/400), sum(gdp$Exp8Run15/400),
          sum(gdp$Exp8Run16/400), sum(gdp$Exp8Run17/400), sum(gdp$Exp8Run18/400), sum(gdp$Exp8Run19/400), sum(gdp$Exp8Run20/400),
          sum(gdp$Exp8Run21/400), sum(gdp$Exp8Run22/400), sum(gdp$Exp8Run23/400), sum(gdp$Exp8Run24/400), sum(gdp$Exp8Run25/400),
          sum(gdp$Exp9Run1/400), sum(gdp$Exp9Run2/400), sum(gdp$Exp9Run3/400), sum(gdp$Exp9Run4/400), sum(gdp$Exp9Run5/400),
          sum(gdp$Exp9Run6/400), sum(gdp$Exp9Run7/400), sum(gdp$Exp9Run8/400), sum(gdp$Exp9Run9/400), sum(gdp$Exp9Run10/400),
          sum(gdp$Exp9Run11/400), sum(gdp$Exp9Run12/400), sum(gdp$Exp9Run13/400), sum(gdp$Exp9Run14/400), sum(gdp$Exp9Run15/400),
          sum(gdp$Exp9Run16/400), sum(gdp$Exp9Run17/400), sum(gdp$Exp9Run18/400), sum(gdp$Exp9Run19/400), sum(gdp$Exp9Run20/400),
          sum(gdp$Exp9Run21/400), sum(gdp$Exp9Run22/400), sum(gdp$Exp9Run23/400), sum(gdp$Exp9Run24/400), sum(gdp$Exp9Run25/400),
          sum(gdp$Exp10Run1/400), sum(gdp$Exp10Run2/400), sum(gdp$Exp10Run3/400), sum(gdp$Exp10Run4/400), sum(gdp$Exp10Run5/400),
          sum(gdp$Exp10Run6/400), sum(gdp$Exp10Run7/400), sum(gdp$Exp10Run8/400), sum(gdp$Exp10Run9/400), sum(gdp$Exp10Run10/400),
          sum(gdp$Exp10Run11/400), sum(gdp$Exp10Run12/400), sum(gdp$Exp10Run13/400), sum(gdp$Exp10Run14/400), sum(gdp$Exp10Run15/400),
          sum(gdp$Exp10Run16/400), sum(gdp$Exp10Run17/400), sum(gdp$Exp10Run18/400), sum(gdp$Exp10Run19/400), sum(gdp$Exp10Run20/400),
          sum(gdp$Exp10Run21/400), sum(gdp$Exp10Run22/400), sum(gdp$Exp10Run23/400), sum(gdp$Exp10Run24/400), sum(gdp$Exp10Run25/400),
          sum(gdp$Exp11Run1/400), sum(gdp$Exp11Run2/400), sum(gdp$Exp11Run3/400), sum(gdp$Exp11Run4/400), sum(gdp$Exp11Run5/400),
          sum(gdp$Exp11Run6/400), sum(gdp$Exp11Run7/400), sum(gdp$Exp11Run8/400), sum(gdp$Exp11Run9/400), sum(gdp$Exp11Run10/400),
          sum(gdp$Exp11Run11/400), sum(gdp$Exp11Run12/400), sum(gdp$Exp11Run13/400), sum(gdp$Exp11Run14/400), sum(gdp$Exp11Run15/400),
          sum(gdp$Exp11Run16/400), sum(gdp$Exp11Run17/400), sum(gdp$Exp11Run18/400), sum(gdp$Exp11Run19/400), sum(gdp$Exp11Run20/400),
          sum(gdp$Exp11Run21/400), sum(gdp$Exp11Run22/400), sum(gdp$Exp11Run23/400), sum(gdp$Exp11Run24/400), sum(gdp$Exp11Run25/400),
          sum(gdp$Exp12Run1/400), sum(gdp$Exp12Run2/400), sum(gdp$Exp12Run3/400), sum(gdp$Exp12Run4/400), sum(gdp$Exp12Run5/400),
          sum(gdp$Exp12Run6/400), sum(gdp$Exp12Run7/400), sum(gdp$Exp12Run8/400), sum(gdp$Exp12Run9/400), sum(gdp$Exp12Run10/400),
          sum(gdp$Exp12Run11/400), sum(gdp$Exp12Run12/400), sum(gdp$Exp12Run13/400), sum(gdp$Exp12Run14/400), sum(gdp$Exp12Run15/400),
          sum(gdp$Exp12Run16/400), sum(gdp$Exp12Run17/400), sum(gdp$Exp12Run18/400), sum(gdp$Exp12Run19/400), sum(gdp$Exp12Run20/400),
          sum(gdp$Exp12Run21/400), sum(gdp$Exp12Run22/400), sum(gdp$Exp12Run23/400), sum(gdp$Exp12Run24/400), sum(gdp$Exp12Run25/400))

  
  
  
df <- data.frame('run'=test0,'b_bankruptcies'=test1, 'b_bailoutcost'=test2, 'b_avrgcapitalratio'=test3, 'avrgaggnomcons'=test4, 'avrgaggnominv'=test5, 'avrggdp'=test6)

df$run[1:25] <- 1
df$run[26:50] <- 2
df$run[51:75] <- 3
df$run[76:100] <- 4
df$run[101:125] <- 5
df$run[126:150] <- 6
df$run[151:175] <- 7
df$run[176:200] <- 8
df$run[201:225] <- 9
df$run[226:250] <- 10
df$run[251:275] <- 11
df$run[276:300] <- 12

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


robustness8 <- subset(df, run %in% c(1,10))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness8, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness8, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness8, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness8, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness8, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness8, exact = FALSE, alternative = "less")
#P-VALUES vector
v_robustness8 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)


robustness9 <- subset(df, run %in% c(1,11))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness9, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness9, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness9, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness9, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness9, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness9, exact = FALSE, alternative = "less")
#P-VALUES vector
v_robustness9 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)


robustness10 <- subset(df, run %in% c(1,12))
#MANN-WHITNEY TESTS (H0 : EQUALITY OF MEAN)
res1 <- wilcox.test(avrgaggnomcons ~ run, data = robustness10, exact = FALSE)
#MANN-WHITNEY TESTS (H0 : GREATER OR LOWER)
res2 <- wilcox.test(b_bankruptcies ~ run, data = robustness10, exact = FALSE, alternative = "greater")
res3 <- wilcox.test(b_bailoutcost ~ run, data = robustness10, exact = FALSE, alternative = "greater")
res4 <- wilcox.test(b_avrgcapitalratio ~ run, data = robustness10, exact = FALSE, alternative = "less")
res5 <- wilcox.test(avrgaggnominv ~ run, data = robustness10, exact = FALSE, alternative = "less")
res6 <- wilcox.test(avrggdp ~ run, data = robustness10, exact = FALSE, alternative = "less")
#P-VALUES vector
v_robustness10 <- c(res2$p.value,res3$p.value,res4$p.value,res1$p.value,res5$p.value,res6$p.value)



##SUMMARY TABLE OF MANN WHITNEY ROBUSTNESS CHECKS

summary <- data.frame('baseline'=v_baseline,'robustness1'=v_robustness1, 'robustness2'=v_robustness2, 'robustness3'=v_robustness3, 'robustness4'=v_robustness4, 'robustness5'=v_robustness5, 'robustness6'=v_robustness6, 'robustness7'=v_robustness7, 'robustness8'=v_robustness8, 'robustness9'=v_robustness9, 'robustness10'=v_robustness10)
write.xlsx(summary, "Mann-Whitney_summary.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)