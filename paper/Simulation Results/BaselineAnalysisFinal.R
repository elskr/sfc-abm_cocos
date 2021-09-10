#SET THE WORKING DIRECTORY WHERE THE RESULTS ARE STORED
setwd("C:\Users\Documents\GitHub\sfc-abm_cocos\paper\Simulation Results")

#IMPORT LIBRARIES
library(mFilter)
library(poweRlaw)
library(tseries)
library(propagate)

#The folder where data are stored
folder<-"data_baseline"

#COMMENT IF MERGED .CSV FILES ALREADY CREATED#######
#source("MergeMonteCarloSim.R")
#generateMergedCSV(folder)
#generateSums(folder)
####################################################


#IMPORT DATA FROM .csv FILES
gdp<-read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
cPrices<-read.csv(paste(folder,"/Merged_cAvPrice.csv",sep=""))
kPrices<-read.csv(paste(folder,"/Merged_kAvPrice.csv",sep=""))
netIncome<-read.csv(paste(folder,"/Merged_hhAvNetIncome.csv",sep=""))
unemp<-read.csv(paste(folder,"/Merged_unemployment.csv",sep=""))
inv<-read.csv(paste(folder,"/Merged_cFirmsRealInvestment.csv",sep=""))
cons<-read.csv(paste(folder,"/Merged_cFirmsRealSales.csv",sep=""))
credGap<-read.csv(paste(folder,"/Merged_cAggConsCredit.csv",sep=""))
cFirmsBS<-read.csv(paste(folder,"/Merged_aggCFBS.csv",sep=""))
hhBS=read.csv(paste(folder,"/Merged_aggHHBS.csv",sep=""))
kFirmsBS=read.csv(paste(folder,"/Merged_aggKFBS.csv",sep=""))
banksBS<-read.csv(paste(folder,"/Merged_aggBBS.csv",sep=""))
kSales=read.csv(paste(folder,"/Merged_kFirmsRealSales.csv",sep=""))
banksCredit=read.csv(paste(folder,"/Merged_banksTotalCredit.csv",sep=""))
banksCreditDegree<-read.csv(paste(folder,"/Merged_banksCreditDegreeDistribution.csv",sep=""))
hhWages=read.csv(paste(folder,"/Merged_hhAvWage.csv",sep=""))
cFirmsBankruptcy=read.csv(paste(folder,"/Merged_cFirmsBankrupcty.csv",sep=""))
kFirmsBankruptcy=read.csv(paste(folder,"/Merged_kFirmsBankrupcty.csv",sep=""))
banksBankruptcies=read.csv(paste(folder,"/Merged_banksBankrupcty.csv",sep=""))

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
  cPrices=cPrices[,-toBeRemoved]
  kPrices=kPrices[,-toBeRemoved]
  credGap=credGap[,-toBeRemoved]
  hhWages=hhWages[,-toBeRemoved]
  cFirmsBankruptcy=cFirmsBankruptcy[,-toBeRemoved]
  kFirmsBankruptcy=kFirmsBankruptcy[,-toBeRemoved]
  banksBankruptcies=banksBankruptcies[,-toBeRemoved]
}
#IDENTIFY THE COLUMNS WHERE AGGREGATE INVESTMENT&CONSUMPTION FOR EACH SIMULATION HAS BEEN COMPUTED
cn<-colnames(inv)
indexSum<-grep("SUM",colnames(inv))
nbRuns0<-length(grep(paste("Run",sep=""),cn[indexSum]))
#CREATE DATASET FOR AGGREGATE INVESTMENT AND CONSUMPTION
invAggregate=inv[,c(1,indexSum)]
consAggregate=cons[,c(1,indexSum)]
#IDENTIFY THE CRASHED SIMULATIONS BASED ON THE CRASHED SIMULATION NAMES
if (!is.null(toBeRemoved)){
  indexCrashed=c()
  for (i in 1:length(simCrashed)){
    indexCrashed=c(indexCrashed,grep(simCrashed[i], colnames(invAggregate)))
  }

  #REMOVE CRASHED SIMULATIONS
  invAggregate=invAggregate[,-indexCrashed]
  consAggregate=consAggregate[,-indexCrashed]
}
invUnitsAggregate=invAggregate
#GET REAL VALUE OF INVESTMENT GOODS
for (i in 1:(nbRuns0)){
  if (!(paste("Exp1Run",i,sep="")%in% simCrashed)){
    invAggregate[,grep(paste("Run",i,"SUM",sep=""),colnames(invAggregate))]=invAggregate[,grep(paste("Run",i,"SUM",sep=""),colnames(invAggregate))]*kPrices[,grep(paste("Run",i,"$",sep=""),colnames(kPrices))]/cPrices[,grep(paste("Run",i,"$",sep=""),colnames(cPrices))]
  }
}

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

###APPLY THE HP FILTER TO PRICES RATES OF GROWTH AND HHS NET INCOME (N° OF SIMULATION PERIODS-1 OBSERVATIONS)
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
cphp_cycle<-cPrices[-1,]
cphp_trend<-cPrices[-1,]
kphp_cycle<-kPrices[-1,]
kphp_trend<-kPrices[-1,]
nihp_cycle<-netIncome[-1,]
nihp_trend<-netIncome[-1,]

for(i in 2:ncol(cPrices)){
  cPr<-as.ts(cPrices[,i],frequency=4)
  kPr<-as.ts(kPrices[,i],frequency=4)
  nInc<-as.ts(netIncome[,i],frequency=4)
  #COMPUTE RATES OF GROWTH OF PRICES AND NET INCOME
  cPrGr<-100*(cPr[-1]-cPr[-length(cPr)])/cPr[-length(cPr)]
  kPrGr<-100*(kPr[-1]-kPr[-length(kPr)])/kPr[-length(kPr)]
  nIncGr<-100*(nInc[-1]-nInc[-length(nInc)])/nInc[-length(nInc)]
  #APPLY THE FILTER
  cp.hp<-hpfilter(cPrGr,freq=1600,type="lambda")
  kp.hp<-hpfilter(kPrGr,freq=1600,type="lambda")
  ni.hp<-hpfilter(nIncGr,freq=1600,type="lambda")
  cphp_cycle[,i]<-cp.hp$cycle
  cphp_trend[,i]<-cp.hp$trend
  kphp_cycle[,i]<-kp.hp$cycle
  kphp_trend[,i]<-kp.hp$trend
  nihp_cycle[,i]<-ni.hp$cycle
  nihp_trend[,i]<-ni.hp$trend
}

#COMPUTE AVERAGE TREND AND STANDARD DEVIATION
cptrend.average<-apply(cphp_trend[,-1],1,mean)
cptrend.sd<-apply(cphp_trend[,-1],1,sd)
kptrend.average<-apply(kphp_trend[,-1],1,mean)
kptrend.sd<-apply(kphp_trend[,-1],1,sd)
nitrend.average<-apply(nihp_trend[,-1],1,mean)
nitrend.sd<-apply(nihp_trend[,-1],1,sd)

###APPLY THE HP FILTER TO CREDIT CONSTRAINT
#DELCARE THE VARIABLES WHERE STORING TREND AND CYCLE COMPONENTS
credGaphp_cycle<-credGap
credGaphp_trend<-credGap
#APPLY THE FILTER
for(i in 2:ncol(credGap)){
  credgapts.hp<-hpfilter(as.ts(credGap[,i],frequency=4),freq=1600,type="lambda")
  credGaphp_cycle[,i]<-credgapts.hp$cycle
  credGaphp_trend[,i]<-credgapts.hp$trend
}
#COMPUTE AVERAGE TREND AND STANDARD DEVIATION
credGaptrend.average<-apply(credGaphp_trend[,-1],1,mean)
credGaptrend.sd<-apply(credGaphp_trend[,-1],1,sd)


###APPLY THE HP FILTER TO UNEMPLOYMENT
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
unemphp_cycle<-unemp
unemphp_trend<-unemp

#APPLY THE FILTER
for(i in 2:ncol(unemp)){
  unemp.hp<-hpfilter(as.ts(unemp[,i],frequency=4),freq=1600,type="lambda")
  unemphp_cycle[,i]<-unemp.hp$cycle
  unemphp_trend[,i]<-unemp.hp$trend
}

#COMPUTE AVERAGE TREND AND STANDARD DEVIATION
unemptrend.average<-apply(unemphp_trend[,-1],1,mean)
unemptrend.sd<-apply(unemphp_trend[,-1],1,sd)

###APPLY THE HP FILTER TO AGGREGATE INVESTMENT AND CONSUMPTION (REAL)
#DECLARE VARIABLES WHERE STORING TREND AND CYCLE COMPONENTS
invhp_cycle<-invAggregate
invhp_trend<-invAggregate
invUnitshp_cycle=invUnitsAggregate
invUnitshp_trend=invUnitsAggregate
conshp_cycle<-consAggregate
conshp_trend<-consAggregate

#APPLY THE HP FILTER
for(i in 2:ncol(invAggregate)){
  inv.hp<-hpfilter(as.ts(invAggregate[,i],frequency=4),freq=1600,type="lambda")
  invUnits.hp=hpfilter(as.ts(invUnitsAggregate[,i],frequency=4),freq=1600,type="lambda")
  cons.hp<-hpfilter(as.ts(consAggregate[,i],frequency=4),freq=1600,type="lambda")
  invhp_cycle[,i]<-inv.hp$cycle
  invhp_trend[,i]<-inv.hp$trend
  invUnitshp_cycle[,i]<-invUnits.hp$cycle
  invUnitshp_trend[,i]<-invUnits.hp$trend
  conshp_cycle[,i]<-cons.hp$cycle
  conshp_trend[,i]<-cons.hp$trend
}

#COMPUTE THE AVERAGE TREND AND STANDARD DEVIATION
invtrend.average<-apply(invhp_trend[,-1],1,mean)
invtrend.sd<-apply(invhp_trend[,-1],1,sd)
invUnitstrend.average<-apply(invUnitshp_trend[,-1],1,mean)
invUnitstrend.sd<-apply(invUnitshp_trend[,-1],1,sd)
constrend.average<-apply(conshp_trend[,-1],1,mean)
constrend.sd<-apply(conshp_trend[,-1],1,sd)


###APPLY THE HP FILTER TO AGGREGATE LOGS OF INVESTMENT AND CONSUMPTION (REAL)
#DECLARE VARIABLES WHERE STORING TREND AND CYCLE COMPONENTS
linvhp_cycle<-invAggregate
linvhp_trend<-invAggregate
linvUnitshp_cycle=invUnitsAggregate
linvUnitshp_trend=invUnitsAggregate
lconshp_cycle<-consAggregate
lconshp_trend<-consAggregate

#APPLY THE HP FILTER
for(i in 2:ncol(invAggregate)){
  linv.hp<-hpfilter(as.ts(log(invAggregate[,i]),frequency=4),freq=1600,type="lambda")
  lcons.hp<-hpfilter(as.ts(log(consAggregate[,i]),frequency=4),freq=1600,type="lambda")
  linvUnits.hp=hpfilter(as.ts(log(invUnitsAggregate[,i]),frequency=4),freq=1600,type="lambda")
  linvhp_cycle[,i]<-linv.hp$cycle
  linvhp_trend[,i]<-linv.hp$trend
  linvUnitshp_cycle[,i]<-linvUnits.hp$cycle
  linvUnitshp_trend[,i]<-linvUnits.hp$trend
  lconshp_cycle[,i]<-lcons.hp$cycle
  lconshp_trend[,i]<-lcons.hp$trend
}

#COMPUTE THE AVERAGE TREND AND STANDARD DEVIATION
linvtrend.average<-apply(linvhp_trend[,-1],1,mean)
linvtrend.sd<-apply(linvhp_trend[,-1],1,sd)
linvUnitstrend.average=apply(linvUnitshp_trend[,-1],1,mean)
linvUnitstrend.sd<-apply(linvUnitshp_trend[,-1],1,sd)
lconstrend.average<-apply(lconshp_trend[,-1],1,mean)
lconstrend.sd<-apply(lconshp_trend[,-1],1,sd)

#COMPUTE REAL GDP
rgdp<-gdp
#rgdp[,2:ncol(rgdp)]=gdp[,2:ncol(rgdp)]/cPrices[,2:ncol(rgdp)]
#or alternatively (if you want just focus on real output)
for (i in 1:(nbRuns0)){
  if (!(paste("Exp1Run",i,sep="")%in% simCrashed)){
    rgdp[,grep(paste("Run",i,"$",sep=""),colnames(rgdp))]=invAggregate[,grep(paste("Run",i,"SUM",sep=""),colnames(invAggregate))]+consAggregate[,grep(paste("Run",i,"SUM",sep=""),colnames(consAggregate))]
  }
}


#APPLY THE HP FILTER TO REAL GDP
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
rgdphp_cycle<-rgdp
rgdphp_trend<-rgdp

#APPLY THE HP FILTER
for(i in 2:ncol(rgdp)){
  rgdp.hp<-hpfilter(as.ts(rgdp[,i],frequency=4),freq=1600,type="lambda")
  rgdphp_cycle[,i]<-rgdp.hp$cycle
  rgdphp_trend[,i]<-rgdp.hp$trend
}

rgdptrend.average<-apply(rgdphp_trend[,-1],1,mean)
rgdptrend.sd<-apply(rgdphp_trend[,-1],1,sd)

#APPLY THE HP FILTER TO LOG OF REAL GDP
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
lrgdphp_cycle<-rgdp
lrgdphp_trend<-rgdp

#APPLY THE HP FILTER
for(i in 2:ncol(rgdp)){
  lrgdp.hp<-hpfilter(as.ts(log(rgdp[,i]),frequency=4),freq=1600,type="lambda")
  lrgdphp_cycle[,i]<-lrgdp.hp$cycle
  lrgdphp_trend[,i]<-lrgdp.hp$trend
}

###COMPUTE CORRELATIONS
#DECLARE VARIABLE WHERE STORING CORRELATIONS
corr<-as.data.frame(matrix(NA,ncol=169,nrow=ncol(rgdphp_trend)-1))
colnames(corr)<-c("RunExp",
                  paste("realgdp",paste("lag",seq(0,20),sep=""),sep=""),
                  paste("realinv",paste("lag",seq(0,20),sep=""),sep=""),
                  paste("unemp",paste("lag",seq(0,20),sep=""),sep=""),
                  paste("realcons",paste("lag",seq(0,20),sep=""),sep=""),
                  paste("realgdp_gdp",paste("lag",seq(-10,10),sep=""),sep=""),
                  paste("realgdp_inv",paste("lag",seq(-10,10),sep=""),sep=""),
                  paste("realgdp_unemp",paste("lag",seq(-10,10),sep=""),sep=""),
                  paste("realgdp_cons",paste("lag",seq(-10,10),sep=""),sep="")
)

j<-1
for(i in 1:nbRuns0){
  if (!(paste("Exp1Run",i,sep="")%in% simCrashed)){
  #COERCE TO TIME SERIES 
  rgdp_ts<-lrgdphp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(lrgdphp_cycle))]
  inv_ts<-linvhp_cycle[,grep(paste("Run",i,"SUM",sep=""),colnames(linvhp_cycle))]
  unemp_ts<-unemphp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(unemphp_cycle))]
  cons_ts<-lconshp_cycle[,grep(paste("Run",i,"SUM",sep=""),colnames(lconshp_cycle))]
  #COMPUTE AUTOCORRELATIONS
  simrgdp.acf<-acf(rgdp_ts,lag.max=20,plot=F,type="correlation")
  simrinv.acf<-acf(inv_ts,lag.max=20,plot=F,type="correlation")
  simunemp.acf<-acf(unemp_ts,lag.max=20,plot=F,type="correlation")
  simrcons.acf<-acf(cons_ts,lag.max=20,plot=F,type="correlation")
  #COMPUTE CROSS CORRELATIONS
  simrgdp.ccf<-ccf(rgdp_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
  simrinv.ccf<-ccf(inv_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
  simunemp.ccf<-ccf(unemp_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
  simrcons.ccf<-ccf(cons_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
  #STORE RESULTS
  corr[j,1]<-paste("Exp1Run",i,sep="")
  corr[j,2:169]<-c(simrgdp.acf$acf,simrinv.acf$acf,simunemp.acf$acf,simrcons.acf$acf,
                   simrgdp.ccf$acf,simrinv.ccf$acf,simunemp.ccf$acf,simrcons.ccf$acf)
  j<-j+1
  }
}

#COMPUTE AVERAGE (ACROSS MONTE CARLO SIM) CORRELATIONS AND STANDARD DEVIATIONS
corr[ncol(rgdp),1]<-paste("Exp1Av",sep="")
corr[ncol(rgdp),2:169]<-c(apply(corr[1:(ncol(rgdp)-1),2:169],2,mean))
corr[ncol(rgdp)+1,1]<-paste("Exp1Var",sep="")
corr[ncol(rgdp)+1,2:169]<-c(apply(corr[1:(ncol(rgdp)-1),2:169],2,var))

###########OTHER BASELINE PLOTS###############

#WE CALCULATE THE SD OF THE NORMALIZED SERIES
sd_rgdp<-mean(c(apply(rgdphp_cycle[,2:ncol(rgdphp_cycle)]/rgdphp_trend[,2:ncol(rgdphp_cycle)],2,sd)))*100
sd_cons<-mean(c(apply(conshp_cycle[,2:ncol(rgdphp_cycle)]/conshp_trend[,2:ncol(rgdphp_cycle)],2,sd)))*100
sd_inv<-mean(c(apply(invhp_cycle[,2:ncol(rgdphp_cycle)]/invhp_trend[,2:ncol(rgdphp_cycle)],2,sd)))*100
sd_unemp<-mean(c(apply(unemphp_cycle[,2:ncol(rgdphp_cycle)]/unemphp_trend[,2:ncol(rgdphp_cycle)],2,sd)))*100

#AND THE RELATIVE (TO RGDP) SD
relativeSD_cons=sd_cons/sd_rgdp
relativeSD_inv=sd_inv/sd_rgdp
relativeSD_unemp=sd_unemp/sd_rgdp

#NOMINAL GDP
par(mfrow = c(1, 1))
plot(gdptrend.average,type="l",main="Nominal GDP",ylab="",xlab="",lwd=2)
lines(gdptrend.average+gdptrend.sd,lwd=2,lty=2)
lines(gdptrend.average-gdptrend.sd,lwd=2,lty=2)
dev.copy2eps(file=paste(folder,"/nominalGDP.ps",sep=""))
#dev.off()

#NOMINAL GDP (LOG)
par(mfrow = c(1, 1))
plot(log(gdptrend.average),type="l",main="Nominal GDP (Log)",ylab="",xlab="",lwd=2)
lines(log(gdptrend.average+gdptrend.sd),lwd=2,lty=2)
lines(log(gdptrend.average-gdptrend.sd),lwd=2,lty=2)
dev.copy2eps(file=paste(folder,"/LognominalGDP.ps",sep=""))
#dev.off()

#REAL GDP 
par(mfrow = c(1, 1))
plot(rgdptrend.average,type="l",main="Real GDP",ylab="",xlab="",lwd=2,ylim=range(c(rgdptrend.average+rgdptrend.sd,rgdptrend.average-rgdptrend.sd)))
lines(rgdptrend.average+rgdptrend.sd,lwd=2,lty=2)
lines(rgdptrend.average-rgdptrend.sd,lwd=2,lty=2)
dev.copy2eps(file=paste(folder,"/realGDP.ps",sep=""))
#dev.off()

#CONSUMPTION PRICES, CAPITAL PRICES, AND NET INCOME GROWTH
par(mfrow = c(1, 1))
plot(cptrend.average,type="l",main="Growth rates (%)",ylab="",xlab="",lwd=2,
     ylim=range(c(cptrend.average+cptrend.sd,cptrend.average-cptrend.sd,
                  kptrend.average+kptrend.sd,kptrend.average-kptrend.sd,
                  nitrend.average+nitrend.sd,nitrend.average-nitrend.sd),na.rm=T))
lines(cptrend.average+cptrend.sd,lwd=2,lty=3,col="grey70")
lines(cptrend.average-cptrend.sd,lwd=2,lty=3,col="grey70")
lines(cptrend.average,lwd=3,lty=1,col="grey70")
lines(kptrend.average,lwd=3,lty=1,col=1)
lines(kptrend.average+kptrend.sd,lwd=2,lty=3,col=1)
lines(kptrend.average-kptrend.sd,lwd=2,lty=3,col=1)
lines(nitrend.average,lwd=3,lty=1,col="grey30")
lines(nitrend.average+nitrend.sd,lwd=2,lty=3,col="grey30")
lines(nitrend.average-nitrend.sd,lwd=2,lty=3,col="grey30")
legend(cex = 1.0,"topright",lty=1,legend=c("Consumption price","Capital price","Net income"),col = c("grey70", "black", "grey30"),lwd=3,bty='n')
dev.copy2eps(file=paste(folder,"/growthRates.ps",sep=""))
#dev.off()

#UNEMPLOYMENT RATE
par(mfrow = c(1, 1))
plot(unemptrend.average,type="l",main="Unemployment rate",ylab="",xlab="",lwd=2,ylim=range(c(unemptrend.average+unemptrend.sd,unemptrend.average-unemptrend.sd)))
lines(unemptrend.average+unemptrend.sd,lwd=2,lty=2)
lines(unemptrend.average-unemptrend.sd,lwd=2,lty=2)
dev.copy2eps(file=paste(folder,"/unemployment.ps",sep=""))
#dev.off()

#REAL INVESTMENT AND REAL CONSUMPTION
par(mfrow = c(1, 1))
plot(invtrend.average,type="l",main="Real Consumption and Investment",ylab="",xlab="",lwd=2,
     ylim=range(c(invtrend.average+invtrend.sd,invtrend.average-invtrend.sd),na.rm=T))
lines(invtrend.average+invtrend.sd,lwd=2,lty=1,col="grey")
lines(invtrend.average-invtrend.sd,lwd=2,lty=1,col="grey")
legend(cex = 1.0,"right",legend=c("Real Investment (left)","Real Consumption (right)"),lwd=2,bty='n')
par(new=TRUE)
plot(constrend.average,lwd=2,lty=2,col=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=range(c(constrend.average+constrend.sd,constrend.average-constrend.sd)),type='l')
lines(constrend.average+constrend.sd,lwd=2,lty=2,col="grey")
lines(constrend.average-constrend.sd,lwd=2,lty=2,col="grey")
axis(4)
mtext("Real Consumption",side=4,line=3)
dev.copy2eps(file=paste(folder,"/InvCons.ps",sep=""))
#dev.off()

#REAL INVESTMENT AND REAL CONSUMPTION (UNITS)
par(mfrow = c(1, 1))
plot(invUnitstrend.average,type="l",main="Consumption and Investment (Units)",ylab="",xlab="",lwd=2,
     ylim=range(c(invUnitstrend.average+invUnitstrend.sd,invUnitstrend.average-invUnitstrend.sd),na.rm=T))
lines(invUnitstrend.average+invUnitstrend.sd,lwd=2,lty=1,col="grey")
lines(invUnitstrend.average-invUnitstrend.sd,lwd=2,lty=1,col="grey")
legend(cex = 1.0,"right",legend=c("Investment Units (left)","Consumption Units (right)"),lty=c(1,2),lwd=2,bty='n')
par(new=TRUE)
plot(constrend.average,lwd=2,lty=2,col=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=range(c(constrend.average+constrend.sd,constrend.average-constrend.sd)),type='l')
lines(constrend.average+constrend.sd,lwd=2,lty=2,col="grey")
lines(constrend.average-constrend.sd,lwd=2,lty=2,col="grey")
axis(4)
mtext("Real Consumption",side=4,line=3)
dev.copy2eps(file=paste(folder,"/InvConsUnits.ps",sep=""))
#dev.off()

#C FIRMS AND K FIRMS MARKET SHARES
#FIRST CLEAN kSales AND cons from crashed Simulations, then compute mkt shares
nbcFirms=length(cons[1, grep(paste("Exp1Run1cFirm"),colnames(cons))])
nbkFirms=length(inv[1, grep(paste("Exp1Run1kFirm"),colnames(kSales))])

for (i in 1:nbRuns0){
  if (paste("Exp1Run",i,sep="")%in% simCrashed){
    crashedK=grep(paste("Exp1Run",i,"kFirm001",sep=""), colnames(kSales))
    crashedC=grep(paste("Exp1Run",i,"cFirm001",sep=""), colnames(cons))
    kSales=kSales[,-(crashedK:(crashedK+nbkFirms-1))]
    cons=cons[,-(crashedC:(crashedC+nbcFirms-1))]
  }
}
cMktShares=cons
kMktShares=kSales
a=-1
for (i in 2:ncol(consAggregate)){
  nbSimulation=i-1
  if ((paste("Exp1Run",nbSimulation,sep="")%in% simCrashed)){
  a=a+1
  nbSimulation=i+a}
  indexSimk=grep(paste("Exp1Run",nbSimulation,"kFirm001",sep=""), colnames(kMktShares))
  indexSimc=grep(paste("Exp1Run",nbSimulation,"cFirm001",sep=""), colnames(cMktShares))
  cMktShares[,indexSimc:(indexSimc+nbcFirms-1)]=cMktShares[,indexSimc:(indexSimc+nbcFirms-1)]/(consAggregate[,i])
  kMktShares[,indexSimk:(indexSimk+nbkFirms-1)]=kMktShares[,indexSimk:(indexSimk+nbkFirms-1)]/(invUnitsAggregate[,i])
}

#PLOT FIRST TEN C FIRMS MKT SHARES FROM THE FIRST SIMULATION IN THE QUASI-SS
timespan=seq(200,400)
par(mfrow = c(1, 1))
matplot(timespan,cMktShares[200:400,2:21],type = "l",lty=1, col=seq(1,21),xlab="Time Span", ylab="Market Shares", main="Persistency of Market Shares (C Firms)")
dev.copy2eps(file=paste(folder,"/cmktSharesPers.ps",sep=""))
#PLOT K FIRMS MKT SHARES FROM THE FIRST SIMULATION IN THE QUASI-SS
par(mfrow = c(1, 1))
matplot(timespan,kMktShares[200:400,2:21],type = "l",lty=1,col=seq(1,21),xlab="Time Span",ylab="Market  Shares", main="Persistency of Market Shares (K Firms)")
dev.copy2eps(file=paste(folder,"/kmktSharesPers.ps",sep=""))

#LUMPY INVESTMENT
timespan=seq(200,300)
par(mfrow = c(1, 1))
matplot(timespan,inv[200:300,2:6],type = "l",lwd=1, col=seq(1,21), xlab="Time Span", ylab="Investment (Units)", main="Lumpiness of Investment",lty=1)
dev.copy2eps(file=paste(folder,"/cLumpyInvestment.ps",sep=""))

#PLOT CREDIT GAP
par(mfrow = c(1, 1))
plot(credGaptrend.average,type="l",main="Credit Gap (Absolute)",ylab="",xlab="",lwd=2,ylim=range(c(credGaptrend.average+credGaptrend.sd,credGaptrend.average-credGaptrend.sd)))
lines(credGaptrend.average+credGaptrend.sd,lwd=2,lty=2)
lines(credGaptrend.average-credGaptrend.sd,lwd=2,lty=2)
dev.copy2eps(file=paste(folder,"/consCredit.ps",sep=""))
#dev.off()

#PLOT CREDIT GAP SHARE (i.e. demand of credit/credit actually granted)
creditTot=read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
#WE WANT creditTot TO HAVE THE SAME STRUCTURE OF GDP, 
#UNEMPLOYMENT, AND OTHERS BEFORE REMOVING CRASHED SIMULATIONS

#z=-7
z=-8
for (j in 2:ncol(creditTot)){
  #z=z+7
  z=z+8
  creditTot[,j]=(-cFirmsBS[,j+z+4])
}
if (!is.null(toBeRemoved)){
  creditTot=creditTot[,-toBeRemoved]
}
credGapShare=credGap[,2:ncol(creditTot)]/(credGap[,2:ncol(creditTot)]+creditTot[,2:ncol(creditTot)])


###APPLY THE HP FILTER TO CREDIT and CREDIT GAP SHARE
#DECLARE THE VARIABLES WHERE STORING TREND AND CYCLE COMPONENTS
cCredit_cycle<-creditTot
cCredit_trend<-creditTot
lcCredit_cycle<-creditTot
lcCredit_trend<-creditTot
credGapSharehp_cycle<-credGapShare
credGapSharehp_trend<-credGapShare
#APPLY THE FILTER
for(i in 2:ncol(credGapShare)){
  cCreditts.hp<-hpfilter(as.ts(creditTot[,i],frequency=4),freq=1600,type="lambda")
  cCredit_cycle[,i]<-cCreditts.hp$cycle
  cCredit_trend[,i]<-cCreditts.hp$trend
  lcCreditts.hp<-hpfilter(as.ts(log(creditTot[,i]),frequency=4),freq=1600,type="lambda")
  lcCredit_cycle[,i]<-lcCreditts.hp$cycle
  lcCredit_trend[,i]<-lcCreditts.hp$trend
  credgapSharets.hp<-hpfilter(as.ts(credGapShare[,i],frequency=4),freq=1600,type="lambda")
  credGapSharehp_cycle[,i]<-credgapSharets.hp$cycle
  credGapSharehp_trend[,i]<-credgapSharets.hp$trend
}
#COMPUTE AVERAGE TREND AND STANDARD DEVIATION
cCredittrend.average=apply(cCredit_trend[,-1],1,mean)
cCredittrend.sd<-apply(cCredit_trend[,-1],1,sd)
credGapSharetrend.average<-apply(credGapSharehp_trend[,-1],1,mean)
credGapSharetrend.sd<-apply(credGapSharehp_trend[,-1],1,sd)

#PLOT CREDIT GAP SHARE
par(mfrow = c(1, 1))
plot(credGapSharetrend.average,type="l",main="C Firms' Credit Gap",ylab="",xlab="",lwd=2,ylim=range(c(credGapSharetrend.average+credGapSharetrend.sd,credGapSharetrend.average-credGapSharetrend.sd)))
lines(credGapSharetrend.average+credGapSharetrend.sd,lwd=2,lty=2)
lines(credGapSharetrend.average-credGapSharetrend.sd,lwd=2,lty=2)
dev.copy2eps(file=paste(folder,"/creditGapShare.ps",sep=""))
#dev.off()


#CONSUMPTION FIRMS DEPOSITS AND CREDIT
#EXTRACT DEPOSITS AND LOANS FROM CONSUMPTION FIRMS' BS
indexDep<-grep("DEPOSIT",colnames(cFirmsBS))
indexLoan<-grep("LOANS",colnames(cFirmsBS))
dephp_cycle<-cFirmsBS[,c(1,indexDep)]
dephp_trend<-cFirmsBS[,c(1,indexDep)]
loanhp_cycle<-cFirmsBS[,c(1,indexLoan)]
loanhp_trend<-cFirmsBS[,c(1,indexLoan)]
#REMOVED CRASHED SIMULATIONS
if (!is.null(toBeRemoved)){
  dephp_cycle=dephp_cycle[,-toBeRemoved]
  dephp_trend=dephp_trend[,-toBeRemoved]
  loanhp_cycle=loanhp_cycle[,-toBeRemoved]
  loanhp_trend=loanhp_trend[,-toBeRemoved]
}
#APPLY THE HP FILTER
for(i in 2:ncol(dephp_trend)){
  dep.hp<-hpfilter(as.ts(dephp_trend[,i],frequency=4),freq=1600,type="lambda")
  loan.hp<-hpfilter(as.ts(loanhp_trend[i],frequency=4),freq=1600,type="lambda")
  dephp_cycle[,i]<-dep.hp$cycle
  dephp_trend[,i]<-dep.hp$trend
  loanhp_cycle[,i]<-loan.hp$cycle
  loanhp_trend[,i]<-loan.hp$trend
}

#COMPUTE AVERAGE TREND AND STANDARD DEVIATION
deptrend.average<-apply(dephp_trend[,-1],1,mean)
deptrend.sd<-apply(dephp_trend[,-1],1,sd)
loantrend.average<-apply(loanhp_trend[,-1],1,mean)
loantrend.sd<-apply(loanhp_trend[,-1],1,sd)
#PLOT CONSUMPTION FIRMS 'DEPOSITS AND CREDIT
par(mfrow = c(1, 1))
plot(deptrend.average,type="l",main="Consumption Firms' Funds",ylab="",xlab="",lwd=2,
     ylim=range(c(deptrend.average+deptrend.sd,deptrend.average-deptrend.sd,
                  -loantrend.average+loantrend.sd,-loantrend.average-loantrend.sd),na.rm=T))
lines(deptrend.average+deptrend.sd,lwd=2,lty=1,col="grey")
lines(deptrend.average-deptrend.sd,lwd=2,lty=1,col="grey")
lines(-loantrend.average,lwd=2,lty=2,col=1)
lines(-loantrend.average+loantrend.sd,lwd=2,lty=2,col="grey")
lines(-loantrend.average-loantrend.sd,lwd=2,lty=2,col="grey")
legend("topleft",lty=1:2,legend=c("Deposits","Loans"),lwd=2,bty='n')
dev.copy2eps(file=paste(folder,"/cFirmsBS.ps",sep=""))
#dev.off()

#PLOT CONSUMPTION FIRMS 'DEPOSITS AND CREDIT (LOG)
par(mfrow = c(1, 1))
plot(log(deptrend.average),type="l",main="Consumption Firms' Funds (Log)",ylab="",xlab="",lwd=2,
     ylim=range(c(log(deptrend.average+deptrend.sd),log(deptrend.average-deptrend.sd),
                  log(-loantrend.average+loantrend.sd),log(-loantrend.average-loantrend.sd)),na.rm=T))
lines(log(deptrend.average+deptrend.sd),lwd=2,lty=1,col="grey")
lines(log(deptrend.average-deptrend.sd),lwd=2,lty=1,col="grey")
lines(log(-loantrend.average),lwd=2,lty=2,col=1)
lines(log(-loantrend.average+loantrend.sd),lwd=2,lty=2,col="grey")
lines(log(-loantrend.average-loantrend.sd),lwd=2,lty=2,col="grey")
legend("topleft",lty=1:2,legend=c("Deposits","Loans"),lwd=2,bty='n')
dev.copy2eps(file=paste(folder,"/LogcFirmsBS.ps",sep=""))
#dev.off()

#MARKUP
cMarkup=cPrices
kMarkup=kPrices
#markup defined as price/unit variable costs with unit variables costs= wage/labor productivity in the sector
cMarkup[,2:ncol(cMarkup)]=(cPrices[,2:ncol(cPrices)]/(hhWages[,2:ncol(hhWages)]/6.4))-1
kMarkup[,2:ncol(kMarkup)]=(kPrices[,2:ncol(kPrices)]/(hhWages[,2:ncol(hhWages)]/2.0))-1

###APPLY THE HP FILTER TO MARKUP
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
cMarkup_cycle<-cMarkup
cMarkup_trend<-cMarkup
kMarkup_cycle<-kMarkup
kMarkup_trend<-kMarkup

#APPLY THE FILTER
for(i in 2:ncol(cMarkup)){
  cMarkup.hp<-hpfilter(as.ts(cMarkup[,i],frequency=4),freq=1600,type="lambda")
  cMarkup_cycle[,i]<-cMarkup.hp$cycle
  cMarkup_trend[,i]<-cMarkup.hp$trend
  kMarkup.hp<-hpfilter(as.ts(kMarkup[,i],frequency=4),freq=1600,type="lambda")
  kMarkup_cycle[,i]<-kMarkup.hp$cycle
  kMarkup_trend[,i]<-kMarkup.hp$trend
}

#COMPUTE AVERAGE TREND AND STANDARD DEVIATION
cMarkuptrend.average<-apply(cMarkup_trend[,-1],1,mean)
cMarkuptrend.sd<-apply(cMarkup_trend[,-1],1,sd)
kMarkuptrend.average<-apply(kMarkup_trend[,-1],1,mean)
kMarkuptrend.sd<-apply(kMarkup_trend[,-1],1,sd)

#CORRELATION prices and markup VS REAL GDP
corrmarkup<-as.data.frame(matrix(NA,ncol=85,nrow=ncol(rgdphp_trend)-1))
colnames(corrmarkup)<-c("RunExp",
                     paste("cmarkup_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep=""),
                     paste("kmarkup_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep=""),
                     paste("cInflation_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep=""),
                     paste("kInflation_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep="")
)


j<-1
for(i in 1:nbRuns0){
  if (!(paste("Exp1Run",i,sep="")%in% simCrashed)){
    #COERCE TO TIME SERIES 
    rgdp_ts<-rgdphp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(rgdphp_cycle))]
    cmarkup_ts<-cMarkup_cycle[,grep(paste("Run",i,"$",sep=""),colnames(cMarkup_cycle))]
    kmarkup_ts<-kMarkup_cycle[,grep(paste("Run",i,"$",sep=""),colnames(kMarkup_cycle))]
    cInflation_ts=cphp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(cphp_cycle))]
    kInflation_ts=kphp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(kphp_cycle))]
    #COMPUTE CROSS CORRELATIONS
    cmarkup.ccf<-ccf(cmarkup_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
    kmarkup.ccf<-ccf(kmarkup_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
    cInflation.ccf=ccf(cInflation_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
    kInflation.ccf=ccf(kInflation_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
    
    #STORE RESULTS
    corrmarkup[j,1]<-paste("Exp1Run",i,sep="")
    corrmarkup[j,2:85]<-c(cmarkup.ccf$acf,kmarkup.ccf$acf,cInflation.ccf$acf,kInflation.ccf$acf)
    j<-j+1
  }
}

#COMPUTE AVERAGE (ACROSS MONTE CARLO SIM) CORRELATIONS AND STANDARD DEVIATIONS
corrmarkup[ncol(rgdp),1]<-paste("Exp1Av",sep="")
corrmarkup[ncol(rgdp),2:85]<-c(apply(corrmarkup[1:(ncol(rgdp)-1),2:85],2,mean))
corrmarkup[ncol(rgdp)+1,1]<-paste("Exp1Var",sep="")
corrmarkup[ncol(rgdp)+1,2:85]<-c(apply(corrmarkup[1:(ncol(rgdp)-1),2:85],2,var))
corResultsmarkup<-corrmarkup
nbRuns<-nrow(corrmarkup)-2

#PLOT CROSS CORRELATION markup VS RGDP
lagseq<-seq(-10,10)
nozeroLag<-c(seq(1,10),seq(12,21))
colnames(corResultsmarkup)<-gsub("-",".",colnames(corrmarkup))

layout(matrix(c(1,3,2,4), 2, 2, byrow = TRUE))
namelag<-"cmarkup_realgdpcyclelag"
avg<-as.numeric(corResultsmarkup[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultsmarkup[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="C Firms' Mark Up - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultsmarkup[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)

namelag<-"kmarkup_realgdpcyclelag"
avg<-as.numeric(corResultsmarkup[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultsmarkup[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="K Firms' Mark Up - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultsmarkup[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)


namelag<-"cInflation_realgdpcyclelag"
avg<-as.numeric(corResultsmarkup[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultsmarkup[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="C Prices Inflation - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultsmarkup[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)


namelag<-"kInflation_realgdpcyclelag"
avg<-as.numeric(corResultsmarkup[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultsmarkup[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="K Prices Inflation - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultsmarkup[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)
 dev.copy2eps(file=paste(folder,"/markupRGDPCrossCorrelation_stdev.ps",sep=""),width = 10, height = 7)
# dev.off()


#INVENTORIES 
#WE WANT the datasets TO HAVE THE SAME STRUCTURE OF GDP, 
#UNEMPLOYMENT, AND OTHERS BEFORE REMOVING CRASHED SIMULATIONS
inventoriesC=read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
nominalCSales=read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
inventoriesK=read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
nominalKSales=read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
#
z=-8
for (j in 2:ncol(inventoriesC)){
  z=z+8
  inventoriesC[,j]=(cFirmsBS[,j+z+2])
  inventoriesK[,j]=(kFirmsBS[,j+z+2])
  nominalCSales[,j]=(hhBS[,j+z+2])
}
if (!is.null(toBeRemoved)){
  inventoriesC=inventoriesC[,-toBeRemoved]
  inventoriesK=inventoriesK[,-toBeRemoved]
  nominalCSales=nominalCSales[,-toBeRemoved]
}
#compute inventories/sales ratio
invSalesRatioC=inventoriesC[2:ncol(inventoriesC)]/nominalCSales[2:ncol(nominalCSales)]
#compute change in inventories
inventoriesChangeC=inventoriesC
inventoriesChangeC=inventoriesChangeC[-1,]
for (i in 2:ncol(inventoriesC)){
  inventoriesC.ts<-as.ts(inventoriesC[,i],frequency=4)
  inventoriesChangeC[,i]=(inventoriesC.ts[-1]-inventoriesC.ts[-length(inventoriesC.ts)])
}

###APPLY THE HP FILTER TO INVENTORIES MEASURES
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
cInvSalesRatio_cycle<-invSalesRatioC
cInvSalesRatio_trend<-invSalesRatioC
inventoriesChangeC_cycle<-inventoriesChangeC
inventoriesChangeC_trend<-inventoriesChangeC

#APPLY THE FILTER
for(i in 2:ncol(invSalesRatioC)){
  cInvSalesRatio.hp<-hpfilter(as.ts(invSalesRatioC[,i],frequency=4),freq=1600,type="lambda")
  cInvSalesRatio_cycle[,i]<-cInvSalesRatio.hp$cycle
  cInvSalesRatio_trend[,i]<-cInvSalesRatio.hp$trend
  inventoriesChangeC.hp<-hpfilter(as.ts(inventoriesChangeC[,i],frequency=4),freq=1600,type="lambda")
  inventoriesChangeC_cycle[,i]<-inventoriesChangeC.hp$cycle
  inventoriesChangeC_trend[,i]<-inventoriesChangeC.hp$trend
}

#COMPUTE AVERAGE TREND AND STANDARD DEVIATION
cInvSalesRatio.average<-apply(cInvSalesRatio_trend[,-1],1,mean)
cInvSalesRatio.sd<-apply(cInvSalesRatio_trend[,-1],1,sd)
inventoriesChangeC.average<-apply(inventoriesChangeC_trend[,-1],1,mean)
inventoriesChangeC.sd<-apply(inventoriesChangeC_trend[,-1],1,sd)

#CORRELATION prices and markup VS REAL GDP
corrinventories<-as.data.frame(matrix(NA,ncol=43,nrow=ncol(rgdphp_trend)-1))
colnames(corrinventories)<-c("RunExp",
                        paste("inventoriesChangeC_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep=""),
                        paste("cInvSalesRatio_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep="")
)

j<-1
for(i in 1:nbRuns0){
  if (!(paste("Exp1Run",i,sep="")%in% simCrashed)){
    #COERCE TO TIME SERIES 
    rgdp_ts<-rgdphp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(rgdphp_cycle))]
    inventoriesChangeC_ts<-inventoriesChangeC_cycle[,grep(paste("Run",i,"$",sep=""),colnames(inventoriesChangeC))]
    cInvSalesRatio_ts<-cInvSalesRatio_cycle[,grep(paste("Run",i,"$",sep=""),colnames(cInvSalesRatio_cycle))]
    #COMPUTE CROSS CORRELATIONS
    inventoriesChangeC.ccf<-ccf(inventoriesChangeC_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
    cInvSalesRatio.ccf<-ccf( cInvSalesRatio_ts,rgdp_ts,lag.max=10,plot=F,type="correlation")
    
    #STORE RESULTS
    corrinventories[j,1]<-paste("Exp1Run",i,sep="")
    corrinventories[j,2:43]<-c(inventoriesChangeC.ccf$acf,cInvSalesRatio.ccf$acf)
    j<-j+1
  }
}

#COMPUTE AVERAGE (ACROSS MONTE CARLO SIM) CORRELATIONS AND STANDARD DEVIATIONS
corrinventories[ncol(rgdp),1]<-paste("Exp1Av",sep="")
corrinventories[ncol(rgdp),2:43]<-c(apply(corrinventories[1:(ncol(rgdp)-1),2:43],2,mean))
corrinventories[ncol(rgdp)+1,1]<-paste("Exp1Var",sep="")
corrinventories[ncol(rgdp)+1,2:43]<-c(apply(corrinventories[1:(ncol(rgdp)-1),2:43],2,var))
corResultsinventories<-corrinventories
nbRuns<-nrow(corrinventories)-2


#PLOT CROSS CORRELATION markup VS RGDP
lagseq<-seq(-10,10)
nozeroLag<-c(seq(1,10),seq(12,21))
colnames(corResultsinventories)<-gsub("-",".",colnames(corrinventories))

par(mfrow = c(1, 1))
namelag<-"inventoriesChangeC_realgdpcyclelag"
avg<-as.numeric(corResultsinventories[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultsinventories[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="Inventories Change - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultsinventories[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)
dev.copy2eps(file=paste(folder,"/inventoriesCrossCorr.ps",sep=""))

namelag<-"cInvSalesRatio_realgdpcyclelag"
avg<-as.numeric(corResultsinventories[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultsinventories[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="Inventories/Sales Ratio - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultsinventories[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)
dev.copy2eps(file=paste(folder,"/invSalesCrossCorr.ps",sep=""))


#PLOT C FIRMS LOANS/DEPOSIT RATIO (SIMILAR TO LEVERAGE)
levtrend.average<-apply(-loanhp_trend[,-1]/dephp_trend[,-1],1,mean)
levtrend.sd<-apply(-loanhp_trend[,-1]/dephp_trend[,-1],1,sd)

par(mfrow = c(1, 1))
plot(levtrend.average,type="l",main="Consumption Firms' Loans/Deposit",ylab="",xlab="",lwd=2,
     ylim=range(c(levtrend.average+levtrend.sd,levtrend.average-levtrend.sd),na.rm=T))
lines(levtrend.average+levtrend.sd,lwd=2,lty=2)
lines(levtrend.average-levtrend.sd,lwd=2,lty=2)
dev.copy2eps(file=paste(folder,"/cFirmsLoanToDepositRatio.ps",sep=""))
#dev.off()


#PLOT C FIRMS LEVERAGE DEFINED AS DEBT/NW
#COMPUTE C FIRMS LEVERAGE
cFirmsLev=read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
#WE WANT cFirmsLev TO HAVE THE SAME STRUCTURE OF GDP, 
#UNEMPLOYMENT, AND OTHERS BEFORE REMOVING CRASHED SIMULATIONS
z=-8
for (j in 2:ncol(cFirmsLev)){
  z=z+8
  cFirmsLev[,j]=(-cFirmsBS[,j+z+4])/(cFirmsBS[,j+z+1]+cFirmsBS[,j+z+2]+cFirmsBS[,j+z+3]+cFirmsBS[,j+z+4])
}

#NOW REMOVE THE CRASHED SIMULATIONS
if (!is.null(toBeRemoved)){
  cFirmsLev=cFirmsLev[,-toBeRemoved]
}
###APPLY THE HP FILTER TO CFIRMS LEV
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
cFirmsLevhp_cycle<-cFirmsLev
cFirmsLevhp_trend<-cFirmsLev
lcFirmsLevhp_cycle<-cFirmsLev
lcFirmsLevhp_trend<-cFirmsLev

#APPLY THE FILTER
for(i in 2:ncol(cFirmsLev)){
  cFirmsLev.hp<-hpfilter(as.ts(cFirmsLev[,i],frequency=4),freq=1600,type="lambda")
  lcFirmsLev.hp<-hpfilter(as.ts(log(cFirmsLev[,i]),frequency=4),freq=1600,type="lambda")
  cFirmsLevhp_cycle[,i]<-cFirmsLev.hp$cycle
  cFirmsLevhp_trend[,i]<-cFirmsLev.hp$trend
  lcFirmsLevhp_cycle[,i]<-lcFirmsLev.hp$cycle
  lcFirmsLevhp_trend[,i]<-lcFirmsLev.hp$trend
}

#COMPUTE AVERAGE TREND AND STANDARD DEVIATION FOR C FIRMS LEV
cFirmsLevtrend.average<-apply(cFirmsLevhp_trend[,-1],1,mean)
cFirmsLevtrend.sd<-apply(cFirmsLevhp_trend[,-1],1,sd)



##PLOT C FIRMS LEVERAGE DEFINED AS DEBT RATIO=DEBT/ASSETS
#COMPUTE C FIRMS LEVERAGE
cFirmsLev1=read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
#WE WANT cFirmsLev1 TO HAVE THE SAME STRUCTURE OF GDP, 
#UNEMPLOYMENT, AND OTHERS BEFORE REMOVING CRASHED SIMULATIONS
z=-8
for (j in 2:ncol(cFirmsLev1)){
  z=z+8
  cFirmsLev1[,j]=(-cFirmsBS[,j+z+4])/(cFirmsBS[,j+z+1]+cFirmsBS[,j+z+2]+cFirmsBS[,j+z+3])
}

#NOW REMOVE THE CRASHED SIMULATIONS
if (!is.null(toBeRemoved)){
  cFirmsLev1=cFirmsLev1[,-toBeRemoved]
}
###APPLY THE HP FILTER TO CFIRMS LEV
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
cFirmsLev1hp_cycle<-cFirmsLev1
cFirmsLev1hp_trend<-cFirmsLev1
lcFirmsLev1hp_cycle<-cFirmsLev1
lcFirmsLev1hp_trend<-cFirmsLev1
#APPLY THE FILTER
for(i in 2:ncol(cFirmsLev1)){
  cFirmsLev1.hp<-hpfilter(as.ts(cFirmsLev1[,i],frequency=4),freq=1600,type="lambda")
  lcFirmsLev1.hp<-hpfilter(as.ts(log(cFirmsLev1[,i]),frequency=4),freq=1600,type="lambda")
  cFirmsLev1hp_cycle[,i]<-cFirmsLev1.hp$cycle
  cFirmsLev1hp_trend[,i]<-cFirmsLev1.hp$trend
  lcFirmsLev1hp_cycle[,i]<-lcFirmsLev1.hp$cycle
  lcFirmsLev1hp_trend[,i]<-lcFirmsLev1.hp$trend
}

#COMPUTE AVERAGE TREND AND STANDARD DEVIATION FOR C FIRMS LEV
cFirmsLev1trend.average<-apply(cFirmsLev1hp_trend[,-1],1,mean)
cFirmsLev1trend.sd<-apply(cFirmsLev1hp_trend[,-1],1,sd)


#BANKS LEVERAGE DEFINED AS LOANS/NW
banksLev=read.csv(paste(folder,"/Merged_nominalGDP.csv",sep=""))
#WE WANT banksLev TO HAVE THE SAME STRUCTURE OF GDP, 
#UNEMPLOYMENT, AND OTHERS BEFORE REMOVING CRASHED SIMULATIONS
z=-8
for (j in 2:ncol(banksLev)){
  z=z+8
  banksLev[,j]=(banksBS[,j+z+4])/(banksBS[,j+z+1]+banksBS[,j+z+4]+banksBS[,j+z+5]+banksBS[,j+z+6]+banksBS[,j+z+7])
}

#NOW REMOVE THE CRASHED SIMULATIONS
if (!is.null(toBeRemoved)){
  banksLev=banksLev[,-toBeRemoved]
}
###APPLY THE HP FILTER TO CFIRMS LEV
#DECLARE THE VARIABLES WHERE STORING THE TREND AND CYCLE COMPONENTS
banksLevhp_cycle<-banksLev
banksLevhp_trend<-banksLev
lbanksLevhp_cycle<-banksLev
lbanksLevhp_trend<-banksLev

#APPLY THE FILTER
for(i in 2:ncol(banksLev)){
  banksLev.hp<-hpfilter(as.ts(banksLev[,i],frequency=4),freq=1600,type="lambda")
  lbanksLev.hp<-hpfilter(as.ts(log(banksLev[,i]),frequency=4),freq=1600,type="lambda")
  lbanksLevhp_cycle[,i]<-lbanksLev.hp$cycle
  lbanksLevhp_trend[,i]<-lbanksLev.hp$trend
  banksLevhp_cycle[,i]<-banksLev.hp$cycle
  banksLevhp_trend[,i]<-banksLev.hp$trend
}

#COMPUTE AVERAGE TREND AND STANDARD DEVIATION FOR BANKS LEV
banksLevtrend.average<-apply(banksLevhp_trend[,-1],1,mean)
banksLevtrend.sd<-apply(banksLevhp_trend[,-1],1,sd)


#CORRELATION LEVERAGE VS REAL GDP
corrlev<-as.data.frame(matrix(NA,ncol=85,nrow=ncol(rgdphp_trend)-1))
colnames(corrlev)<-c("RunExp",
                  paste("clev_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep=""),
                  paste("clev1_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep=""),
                  paste("bankslev_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep=""),
                  paste("cCredit_realgdpcycle",paste("lag",seq(-10,10),sep=""),sep="")
                  #,paste("lev_realgdtrend",paste("lag",seq(-10,10),sep=""),sep=""),
                  #paste("lev1_realgdptrend",paste("lag",seq(-10,10),sep=""),sep="")
)

j<-1
for(i in 1:nbRuns0){
  if (!(paste("Exp1Run",i,sep="")%in% simCrashed)){
    #COERCE TO TIME SERIES 
    rgdp_ts<-lrgdphp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(rgdphp_cycle))]
    lev_ts<-lcFirmsLevhp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(cFirmsLevhp_cycle))]
    lev1_ts<-lcFirmsLev1hp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(cFirmsLev1hp_cycle))]
    banksLev_ts=lbanksLevhp_cycle[,grep(paste("Run",i,"$",sep=""),colnames(lbanksLevhp_cycle))]
    cCred_ts=lcCredit_cycle[,grep(paste("Run",i,"$",sep=""),colnames(lcCredit_cycle))]
    #COMPUTE CROSS CORRELATIONS
    lev.ccf<-ccf(lev_ts[200:400],rgdp_ts[200:400],lag.max=10,plot=F,type="correlation")
    lev1.ccf<-ccf(lev1_ts[200:400],rgdp_ts[200:400],lag.max=10,plot=F,type="correlation")
    blev.ccf=ccf(banksLev_ts[200:400], rgdp_ts[200:400],lag.max=10,plot=F,type="correlation")
    cCred.ccf=ccf(cCred_ts[200:400], rgdp_ts[200:400],lag.max=10,plot=F,type="correlation")
    #STORE RESULTS
    corrlev[j,1]<-paste("Exp1Run",i,sep="")
    corrlev[j,2:85]<-c(lev.ccf$acf,lev1.ccf$acf,blev.ccf$acf,cCred.ccf$acf)
    j<-j+1
  }
}


#COMPUTE AVERAGE (ACROSS MONTE CARLO SIM) CORRELATIONS AND STANDARD DEVIATIONS
corrlev[ncol(rgdp),1]<-paste("Exp1Av",sep="")
corrlev[ncol(rgdp),2:85]<-c(apply(corrlev[1:(ncol(rgdp)-1),2:85],2,mean))
corrlev[ncol(rgdp)+1,1]<-paste("Exp1Var",sep="")
corrlev[ncol(rgdp)+1,2:85]<-c(apply(corrlev[1:(ncol(rgdp)-1),2:85],2,var))
corResultslev<-corrlev
nbRuns<-nrow(corrlev)-2

#PLOT CROSS CORRELATION LEVERAGE VS RGDP
lagseq<-seq(-10,10)
nozeroLag<-c(seq(1,10),seq(12,21))
colnames(corResultslev)<-gsub("-",".",colnames(corrlev))

layout(matrix(c(1,3,2,4), 2, 2, byrow = TRUE))
namelag<-"clev_realgdpcyclelag"
avg<-as.numeric(corResultslev[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultslev[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="C Firms Leverage - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultslev[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)

namelag<-"clev1_realgdpcyclelag"
avg<-as.numeric(corResultslev[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultslev[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="C Firms Leverage 1 - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultslev[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)


namelag<-"bankslev_realgdpcyclelag"
avg<-as.numeric(corResultslev[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultslev[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="Banks' Leverage - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultslev[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)

namelag<-"cCredit_realgdpcyclelag"
avg<-as.numeric(corResultslev[nbRuns+1,gsub("-",".",paste(namelag,lagseq,sep=""))])
sdev<-as.numeric(sqrt(corResultslev[nbRuns+2,gsub("-",".",paste(namelag,lagseq,sep=""))]))
plot(lagseq,avg,main="C Firms' Loans - RGDP",ylab="",xlab="Lags",
     ylim=range(corResultslev[,gsub("-",".",paste(namelag,lagseq,sep=""))],na.rm=T),type="l")
arrows(lagseq, (avg-sdev), lagseq, (avg+sdev), length=0.05, angle=90, code=3)

dev.copy2eps(file=paste(folder,"/LevRGDPCrossCorrelation_stdev.ps",sep=""),width = 10, height = 7)
# dev.off()

#WE CALCULATE THE SD OF THE NORMALIZED SERIES OF BANK LEVERAGE
sd_banksLev<-mean(c(apply(banksLevhp_cycle[,2:ncol(rgdphp_cycle)]/banksLevhp_trend[,2:ncol(banksLevhp_cycle)],2,sd)))*100

#AND THE RELATIVE (TO RGDP) SD
relativeSD_banksLeverage=sd_banksLev/sd_rgdp


#TOT BANKRUPTICIES
par(mfrow = c(1, 1))
totBankruptcies=cFirmsBankruptcy
totBankruptcies[2:ncol(totBankruptcies)]=cFirmsBankruptcy[2:ncol(totBankruptcies)]+kFirmsBankruptcy[2:ncol(totBankruptcies)]
avbankruptcies=apply(totBankruptcies[,-1],1,mean)
plot(avbankruptcies, type="l", main="Average Firms' Bankruptcies", xlab="", ylab="")
dev.copy2eps(file=paste(folder,"/avBankruptcies.ps",sep=""))


############################################################
#DISTRIBUTIONS ##
###########################################################

#C FIRMS DISTRIBUTION

#analyze c firms size distribution using capacity using the poweRlaw package
cFirmsCapacity=read.csv(paste(folder,"/Merged_cFirmsCapacity.csv",sep=""))
cFirmsCapacity<-cFirmsCapacity[,-1]
cFirmsCapacity<-cFirmsCapacity[,-grep("SUM",colnames(cFirmsCapacity))]
cFirmsCapacity1=cFirmsCapacity[400,]
#cFirmsCapacity1=cFirmsCapacity[50:dim(cFirmsCapacity)[1],]
cFirmsCapacity1=unlist(cFirmsCapacity1)
cFirmsCapacity1<-round(cFirmsCapacity1,digit=6)
cFirmsCapacity1=cFirmsCapacity1[cFirmsCapacity1>1]
cCapacitySkewness=skewness(cFirmsCapacity1)
cCapacityKurtosis=kurtosis(cFirmsCapacity1)

jarque.bera.test(cFirmsCapacity1)


#FIT POWER LAW
cDim_pl=conpl$new(cFirmsCapacity1)
est1 = estimate_xmin(cDim_pl)
cDim_pl$setXmin(est1)
est2 = estimate_pars(cDim_pl)
cDim_pl$setPars(est2)
#BOOTSTRAP TO CHECK PLAUSIBILITY OF PL (P VALUE>0.1)
bs_cDim_pl = bootstrap_p(cDim_pl, no_of_sims=100, threads=2)
bs_cDim_pl$p

#FIT LOG-NORMAL
cDim_ln=conlnorm$new(cFirmsCapacity1)
est3 = estimate_xmin(cDim_ln)
cDim_ln$setXmin(est3)
est4 = estimate_pars(cDim_ln)
cDim_ln$setPars(est4)

#COMPARE THE PL AND LN FITS 
#first set the xmin of the log normal= to xmin of the power low
cDim_ln$setXmin(cDim_pl$getXmin())
#then re-estimate and set parameters
est5 = estimate_pars(cDim_ln)
cDim_ln$setPars(est5)
#then compare
compCDimensions = compare_distributions(cDim_pl, cDim_ln)

#IF THE LIKELIHOOD RATIO NEGATIVE AND SIGNIFICANTLY DIFFERENT FROM 0
#LOG-NORMAL WINS AND VICEVERSA.
compCDimensions$test_statistic
#P-VALUE OF TEST
compCDimensions$p_two_sided


#LOG LOG PLOT OF DATA WITH PL AND LN FITS
plot(cDim_pl, main="Consumption Firm Size Distribution (Capacity)", xlab="Log(Size)", ylab="Log(Rank)", cut=TRUE)
lines(cDim_pl, col=2, cut=TRUE)
lines(cDim_ln, col=3, cut=TRUE)
dev.copy2eps(file=paste(folder,"/FirmsCapProdTail.ps",sep=""))



#analyze c firms size distrbution using real sales using the poweRlaw package
cFirmsSales=cons
cFirmsSales<-cFirmsSales[,-1]
cFirmsSales<-cFirmsSales[,-grep("SUM",colnames(cFirmsSales))]
cFirmsSales1=cFirmsSales[400,]
cFirmsSales1=unlist(cFirmsSales1)
cFirmsSales1<-round(cFirmsSales1,digit=6)
cFirmsSales1=cFirmsSales1[cFirmsSales1>1]
cSalesSkewness=skewness(cFirmsSales1)
cSalesKurtosis=kurtosis(cFirmsSales1)

jarque.bera.test(cFirmsSales1)


#FIT POWER LAW
cDim1_pl=conpl$new(cFirmsSales1)
est1.1 = estimate_xmin(cDim1_pl)
cDim1_pl$setXmin(est1.1)
est2.1 = estimate_pars(cDim1_pl)
cDim1_pl$setPars(est2.1)
#BOOTSTRAP TO CHECK PLAUSIBILITY OF PL (P VALUE>0.1)
bs_cDim1_pl = bootstrap_p(cDim1_pl, no_of_sims=100, threads=2)
bs_cDim1_pl$p

#FIT LOG-NORMAL
cDim1_ln=conlnorm$new(cFirmsSales1)
est3.1 = estimate_xmin(cDim1_ln)
cDim1_ln$setXmin(est3.1)
est4.1 = estimate_pars(cDim1_ln)
cDim1_ln$setPars(est4.1)


#COMPARE THE PL AND LN FITS 
#first set the xmin of the log normal= to xmin of the power low
cDim1_ln$setXmin(cDim1_pl$getXmin())
#then re-estimate and set parameters
est5.1 = estimate_pars(cDim1_ln)
cDim1_ln$setPars(est5.1)
#then compare
compCDimensions1 = compare_distributions(cDim1_pl, cDim1_ln)

#IF THE LIKELIHOOD RATIO NEGATIVE AND SIGNIFICANTLY DIFFERENT FROM 0
#LOG-NORMAL WINS AND VICEVERSA.
compCDimensions1$test_statistic
#P-VALUE OF TEST
compCDimensions1$p_two_sided

#LOG LOG PLOT OF DATA WITH PL AND LN FITS
plot(cDim1_pl, main="Consumption Firm Size Distribution (Sales)", xlab="Log(Size)", ylab="Log(Rank)", cut=TRUE)
lines(cDim1_pl, col=2, cut=TRUE)
lines(cDim1_ln, col=3, cut=TRUE)
dev.copy2eps(file=paste(folder,"/cFirmsSalesDistrTail.ps",sep=""))

#ANALYZE K FIRMS DISTRIBUTION
#analyze c firms size distrbution using real sales using the poweRlaw package
kFirmsSales=kSales
kFirmsSales<-kFirmsSales[,-1]
kFirmsSales<-kFirmsSales[,-grep("SUM",colnames(kFirmsSales))]
kFirmsSales1=kFirmsSales[400,]
kFirmsSales1=unlist(kFirmsSales1)
kFirmsSales1<-round(kFirmsSales1,digit=6)
kFirmsSales1=kFirmsSales1[kFirmsSales1>0]
kSalesSkewness=skewness(kFirmsSales1)
kSalesKurtosis=kurtosis(kFirmsSales1)

jarque.bera.test(kFirmsSales1)

#FIT POWER LAW
kDim_pl=conpl$new(kFirmsSales1)
est1.2 = estimate_xmin(kDim_pl)
kDim_pl$setXmin(est1.2)
est2.2 = estimate_pars(kDim_pl)
kDim_pl$setPars(est2.2)
#BOOTSTRAP TO CHECK PLAUSIBILITY OF PL (P VALUE>0.1)
bs_kDim_pl = bootstrap_p(kDim_pl, no_of_sims=100, threads=2)
bs_kDim_pl$p

#FIT LOG-NORMAL
kDim_ln=conlnorm$new(kFirmsSales1)
est3.2 = estimate_xmin(kDim_ln)
kDim_ln$setXmin(est3.2)
est4.2 = estimate_pars(kDim_ln)
kDim_ln$setPars(est4.2)

#COMPARE THE PL AND LN FITS 
#first set the xmin of the log normal= to xmin of the power low
kDim_ln$setXmin(kDim_pl$getXmin())
#then re-estimate and set parameters
est5.2 = estimate_pars(kDim_ln)
kDim_ln$setPars(est5.2)
#then compare
compKDimensions = compare_distributions(kDim_pl, kDim_ln)

#IF THE LIKELIHOOD RATIO NEGATIVE AND SIGNIFICANTLY DIFFERENT FROM 0
#LOG-NORMAL WINS AND VICEVERSA.
compKDimensions$test_statistic
#P-VALUE OF TEST
compKDimensions$p_two_sided

#LOG LOG PLOT OF DATA WITH PL AND LN FITS
plot(kDim_pl, main="Capital Firm Size Distribution (Sales)", xlab="Log(Size)", ylab="Log(Rank)", cut=TRUE)
lines(kDim_pl, col=2, cut=TRUE)
lines(kDim_ln, col=3, cut=TRUE)
dev.copy2eps(file=paste(folder,"/kFirmsSalesDistrTail.ps",sep=""))


#ANALYZE BANK CREDIT DISTRIBUTION
banksCreditDistr=banksCredit
banksCreditDistr<-banksCreditDistr[,-1]
banksCreditDistr<-banksCreditDistr[,-grep("SUM",colnames(banksCreditDistr))]
banksCreditDistr1=banksCreditDistr[350,]
banksCreditDistr1=unlist(banksCreditDistr1)
banksCreditDistr1<-round(banksCreditDistr1,digit=6)
banksCreditDistr1=banksCreditDistr1[banksCreditDistr1>0]
banksCreditSkewness=skewness(banksCreditDistr1)
banksCreditKurtosis=kurtosis(banksCreditDistr1)

jarque.bera.test(banksCreditDistr1)
#FIT POWER LAW
credDistr_pl=conpl$new(banksCreditDistr1)
est1.3 = estimate_xmin(credDistr_pl)
credDistr_pl$setXmin(est1.3)
est2.3 = estimate_pars(credDistr_pl)
credDistr_pl$setPars(est2.3)
#BOOTSTRAP TO CHECK PLAUSIBILITY OF PL (P VALUE>0.1)
bs_credDistr_pl = bootstrap_p(credDistr_pl, no_of_sims=100, threads=2)
bs_credDistr_pl$p

#FIT LOG-NORMAL
credDistr_ln=conlnorm$new(banksCreditDistr1)
est3.3 = estimate_xmin(credDistr_ln)
credDistr_ln$setXmin(est3.3)
est4.3 = estimate_pars(credDistr_ln)
credDistr_ln$setPars(est4.3)


#COMPARE THE PL AND LN FITS 
#first set the xmin of the log normal= to xmin of the power low
credDistr_ln$setXmin(credDistr_pl$getXmin())
#then re-estimate and set parameters
est5.3 = estimate_pars(credDistr_ln)
credDistr_ln$setPars(est5.3)
#then compare
compcredDistr = compare_distributions(credDistr_pl, credDistr_ln)

#IF THE LIKELIHOOD RATIO NEGATIVE AND SIGNIFICANTLY DIFFERENT FROM 0
#LOG-NORMAL WINS AND VICEVERSA.
compcredDistr$test_statistic
#P-VALUE OF TEST
compcredDistr$p_two_sided

#LOG LOG PLOT OF DATA WITH PL AND LN FITS
plot(credDistr_pl, main="Bank Credit Distribution", xlab="Log(Credit)", ylab="Log(Rank)", cut=TRUE)
lines(credDistr_pl, col=2, cut=TRUE)
lines(credDistr_ln, col=3, cut=TRUE)
dev.copy2eps(file=paste(folder,"/BanksCreditDistrTail.ps",sep=""))


#ANALYZE BANK CREDIT DEGREE DISTRIBUTION
banksCreditDegreeDistr=banksCreditDegree
banksCreditDegreeDistr<-banksCreditDegreeDistr[,-1]
banksCreditDegreeDistr<-banksCreditDegreeDistr[,-grep("SUM",colnames(banksCreditDegreeDistr))]
banksCreditDegreeDistr1=banksCreditDegreeDistr[350,]
averagebanksCreditDegreeDistr1=mean (banksCreditDegreeDistr1)
banksCreditDegreeDistr1=unlist(banksCreditDegreeDistr1)
banksCreditDegreeDistr1<-round(banksCreditDegreeDistr1,digit=6)
banksCreditDegreeDistr1=banksCreditDegreeDistr1[banksCreditDegreeDistr1>0]
banksCreditDegreeSkewness=skewness(banksCreditDegreeDistr1)
banksCreditDegreeKurtosis=kurtosis(banksCreditDegreeDistr1)

jarque.bera.test(banksCreditDegreeDistr1)

#FIT POWER LAW
credDegreeDistr_pl=displ$new(banksCreditDegreeDistr1)
est1.4 = estimate_xmin(credDegreeDistr_pl)
credDegreeDistr_pl$setXmin(est1.4)
est2.4 = estimate_pars(credDegreeDistr_pl)
credDegreeDistr_pl$setPars(est2.4)
#BOOTSTRAP TO CHECK PLAUSIBILITY OF PL (P VALUE>0.1)
bs_credDegreeDistr_pl = bootstrap_p(credDegreeDistr_pl, no_of_sims=100, threads=2)
bs_credDegreeDistr_pl$p

#FIT LOG-NORMAL
credDegreeDistr_ln=dislnorm$new(banksCreditDegreeDistr1)
est3.4 = estimate_xmin(credDegreeDistr_ln)
credDegreeDistr_ln$setXmin(est3.4)
est4.4 = estimate_pars(credDegreeDistr_ln)
credDegreeDistr_ln$setPars(est4.4)

#COMPARE THE PL AND LN FITS 
#first set the xmin of the log normal= to xmin of the power low
credDegreeDistr_ln$setXmin(credDegreeDistr_pl$getXmin())
#then re-estimate and set parameters
est5.4 = estimate_pars(credDegreeDistr_ln)
credDegreeDistr_ln$setPars(est5.4)
#then compare
compcredDegreeDistr = compare_distributions(credDegreeDistr_pl, credDegreeDistr_ln)

#IF THE LIKELIHOOD RATIO NEGATIVE AND SIGNIFICANTLY DIFFERENT FROM 0
#LOG-NORMAL WINS AND VICEVERSA.
compcredDegreeDistr$test_statistic
#P-VALUE OF TEST
compcredDegreeDistr$p_two_sided

#LOG LOG PLOT OF DATA WITH PL AND LN FITS
plot(credDegreeDistr_pl, main="Bank Credit Degree Distribution", xlab="Log(Degree)", ylab="Log(Rank)", ylim=c(0.01,1), cut=TRUE)
lines(credDegreeDistr_pl, col=2, cut=TRUE)
lines(credDegreeDistr_ln, col=3, cut=TRUE)
dev.copy2eps(file=paste(folder,"/BanksCreditDegreeTail.ps",sep=""))


#ANALYZE OUTPUT GROWTH RATES DISTRIBUTION
#COMPUTE GROWTH RATES
rgdpGRDistr=c()
for(i in 2:(ncol(rgdp))){
  rgdp.ts<-as.ts(rgdp[,i],frequency=4)
  rgdpGr<-(rgdp.ts[-1]-rgdp.ts[-length(rgdp.ts)])/rgdp.ts[-length(rgdp.ts)]
  rgdpGRDistr=append(rgdpGRDistr,as.numeric(rgdpGr))
}

hist(rgdpGRDistr, breaks=80, prob=TRUE, ylim=c(0,45), xlim=c(-0.2, 0.2), main="Real Output Growth Rate Distribution")
lines(density(rgdpGRDistr))
dev.copy2eps(file=paste(folder,"/rGDPGrowthRates.ps",sep=""))
rgdpGRSkewness=skewness(rgdpGRDistr)
rgdpGRKurtosis=kurtosis(rgdpGRDistr)


#ANALYZE BAD DEBT DISTRIBUTION
badDebtDistr=read.csv(paste(folder,"/Merged_cFirmsBailoutCost.csv",sep=""))
badDebtBankDistr=read.csv(paste(folder,"/Merged_banksLossBadDebt.csv",sep=""))
badDebtDistr<-badDebtDistr[,-1]
badDebtBankDistr=badDebtBankDistr[,-1]
badDebtDistr<-badDebtDistr[,-grep("SUM",colnames(badDebtDistr))]
badDebtBankDistr<-badDebtBankDistr[,-grep("SUM",colnames(badDebtBankDistr))]
badDebtDistr1=badDebtDistr[350:400,]
badDebtDistr1=unlist(badDebtDistr1)
badDebtDistr1<-round(badDebtDistr1,digit=6)
badDebtDistr1=badDebtDistr1[badDebtDistr1>0]
badDebtSkewness=skewness(badDebtDistr1)
badDebtKurtosis=kurtosis(badDebtDistr1)
jarque.bera.test(badDebtDistr1)


#FIT POWER LAW
baddebt_pl=conpl$new(badDebtDistr1)
est1.5 = estimate_xmin(baddebt_pl)
baddebt_pl$setXmin(est1.5)
est2.5 = estimate_pars(baddebt_pl)
baddebt_pl$setPars(est2.5)
#BOOTSTRAP TO CHECK PLAUSIBILITY OF PL (P VALUE>0.1)
bs_baddebt_pl = bootstrap_p(baddebt_pl, no_of_sims=100, threads=2)
bs_baddebt_pl$p

#FIT LOG-NORMAL
baddebt_ln=conlnorm$new(badDebtDistr1)
est3.5 = estimate_xmin(baddebt_ln)
baddebt_ln$setXmin(est3.5)
est4.5 = estimate_pars(baddebt_ln)
baddebt_ln$setPars(est4.5)


#COMPARE THE PL AND LN FITS 
#first set the xmin of the log normal= to xmin of the power low
baddebt_ln$setXmin(baddebt_pl$getXmin())
#then re-estimate and set parameters
est5.5 = estimate_pars(baddebt_ln)
baddebt_ln$setPars(est5.5)
#then compare
compbaddebt = compare_distributions(baddebt_pl, baddebt_ln)

#IF THE LIKELIHOOD RATIO NEGATIVE AND SIGNIFICANTLY DIFFERENT FROM 0
#LOG-NORMAL WINS AND VICEVERSA.
compbaddebt$test_statistic
#P-VALUE OF TEST
compbaddebt$p_two_sided

#LOG LOG PLOT OF DATA WITH PL AND LN FITS
plot(baddebt_pl, main="Bad Debt Distribution", xlab="Log(Size)", ylab="Log(Rank)",  cut=TRUE)
lines(baddebt_pl, col=2, cut=TRUE)
lines(baddebt_ln, col=3, cut=TRUE)
dev.copy2eps(file=paste(folder,"/BadDebtDistrTail.ps",sep=""))

#FIRMS BANKRUPTCIES DISTRIBUTION
firmsBankruptcy=cFirmsBankruptcy
firmsBankruptcy[2:ncol(firmsBankruptcy)]=cFirmsBankruptcy[2:ncol(cFirmsBankruptcy)]+kFirmsBankruptcy[2:ncol(kFirmsBankruptcy)]

firmsBankruptcy1=firmsBankruptcy[,-1]
firmsBankruptcy1=firmsBankruptcy1
firmsBankruptcy1=unlist(firmsBankruptcy1)
firmsBankruptcy1<-round(firmsBankruptcy1,digit=6)
firmsBankruptcy1=firmsBankruptcy1[firmsBankruptcy1>0]
jarque.bera.test(firmsBankruptcy1)

bankruptciesSkewness=skewness(firmsBankruptcy1)
bankruptciesKurtosis=kurtosis(firmsBankruptcy1)
#FIT POWER LAW
exitrate_pl=displ$new(firmsBankruptcy1)
est1.6 = estimate_xmin(exitrate_pl)
exitrate_pl$setXmin(est1.6)
est2.6 = estimate_pars(exitrate_pl)
exitrate_pl$setPars(est2.6)
#BOOTSTRAP TO CHECK PLAUSIBILITY OF PL (P VALUE>0.1)
bs_exitrate_pl = bootstrap_p(exitrate_pl, no_of_sims=100, threads=2)
bs_exitrate_pl$p

#FIT LOG-NORMAL
exitrate_ln=dislnorm$new(firmsBankruptcy1)
est3.6 = estimate_xmin(exitrate_ln)
exitrate_ln$setXmin(est3.6)
est4.6 = estimate_pars(exitrate_ln)
exitrate_ln$setPars(est4.6)

#COMPARE THE PL AND LN FITS 
#first set the xmin of the log normal= to xmin of the power low
exitrate_ln$setXmin(exitrate_pl$getXmin())
#then re-estimate and set parameters
est5.6 = estimate_pars(exitrate_ln)
exitrate_ln$setPars(est5.6)
#then compare
compexitrate = compare_distributions(exitrate_pl, exitrate_ln)

#IF THE LIKELIHOOD RATIO NEGATIVE AND SIGNIFICANTLY DIFFERENT FROM 0
#LOG-NORMAL WINS AND VICEVERSA.
compexitrate$test_statistic
#P-VALUE OF TEST
compexitrate$p_two_sided

#LOG LOG PLOT OF DATA WITH PL AND LN FITS
plot(exitrate_pl, main="Bankruptcies", xlab="Log(Number)", ylab="Log(Rank)", ylim=c(0.01,5),cut=TRUE)
lines(exitrate_pl, col=2, cut=TRUE)
lines(exitrate_ln, col=3, cut=TRUE)
dev.copy2eps(file=paste(folder,"/exitrateDistrTail.ps",sep=""))

