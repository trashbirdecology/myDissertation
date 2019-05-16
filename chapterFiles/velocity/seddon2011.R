# Annotated R scripts and sediment core data used to investigate regime shift dynamics in a Galapagos coastal lagoon. 
# Ecological Archives E095-262-S1.



# SCript provided by authros ----------------------------------------------

# A quantitative framework for analysis of regime shifts in a Galapagos coastal lagoon

# Alistair W.R. Seddon, Cynthia A. Froyd, Andrzej Witkowski, and Katherine J. Willis
# Final version submitted to Ecology. April 2014

# R Script for investigating extrinsic vs. intrinsic dynamics in diatom assemblage changes.

# Place the data file (data.csv) and the R version of the STARS programme (STARS.R) into a new directory. Then set the working directory to that folder and load the data and STARS files (Example code below). It will also be necessary to install the "rioja", "analogue", "nlme", "ncf", and "vegan" packages [e.g. install.packages("rioja")]. See R documentation for more information.

# Set working directory and load necessary packages
# setwd("~/Appendix/EcologicalArchives/Supplement")
library("rioja")
library("vegan")
library("nlme")
library("ncf")
library("analogue")
source("STARS.R") # R version of the STARS VBA algorithm (Rodionov, 2006)

# Read in data. 
core <- read.csv("/Users/jessicaburnett/Desktop/seddon2011data.csv")
# core = read.csv("data.csv", header=T) 
# Depth         - depth in cm 
# Age           - calibrated ages
# AchSp2-UnkSp1 - Diatom species proportions. 
# Ti            - Ti influx (Âµg/cm^2/yr). Sampled 1-cm down from diatom data and linearly interpolated to same sampling intervals prior to analyses. 
# d13C 	      -	Stable carbon isotope data (per mille, â€°) from Seddon et al. 2011. Note that data handling errors identified from Seddon et al. 2011 corrected.


#-----------------------------------------------------------------------------
# Function for testing for autocorrelation of residuals in a model using Moran's I, then fitting an exponential model to the Moran's I estimates. 

# Returns the range (i.e distance (in years) at which autocorrelation has an effect, defined as distance at which exponential curve predicts Moran's I to go below 0.1
# -----------------------------------------------------------------------------


acfTest = function(x, y.model){
  
  a <- NA
  # x = age vector
  # y.model = model
  
  
  par(mfrow=c(2,1))
  plot(x, residuals(y.model), xlab = "Age (cal yr BP)", ylab ="PC1 residuals")
  abline(h=0, lty=2)
  
  y= residuals(y.model)
  
  ncf.cor = correlog(x, y, y, increment=20, quiet=T, resamp=1000, na.rm=T)
  mIcoef = ncf.cor$correlation
  
  # Truncate values greater than 1/ less than -1 (see Duitilleul et al. 2012, pp. 533)	
  mIcoef[mIcoef>1]<-1
  mIcoef[mIcoef<(-1)]<--1
  
  acfResult = mIcoef
  ageDif= ncf.cor$mean.of.class
  
  acfcheckResult= ageDif
  acfP = ncf.cor$p
  
  # Put the acf scores into a matrix- each column is a time window containing a set of Moran's I scores where appropriate.
  
  acfResult1 = acfResult	
  acfResult1[acfP>0.05]<-0
  
  binACF <-c(1, acfResult[1:30])
  binTime<-c(0, ageDif[1:30])
  
  plot(binTime, binACF, pch=20, col="red", ylab="Correlation", xlab= "Distance (years)", xlim=c(0,300))
  points(binTime, binACF, pch=1, col="blue")
  abline(h=0, lty=2)
  
  
  # Fit exponential decay curve where 0 years =1, asymptote = 0. 
  b0=0 
  b1=1
  
  fit.nls<- tryCatch(fit.nls<-nls(binACF~b0+b1*exp(b2*binTime),start=c(b2=-0.02)), error=function(e) a)
  
  b<-class(fit.nls)
  if(b== "nls"){
    
    # summary(fit.nls)$coefficients[1]
    predict.x <- seq(0, 300)
    predict.y<- predict(fit.nls, newdata= list(binTime=predict.x))
    lines(predict.x, predict.y, col="red")
    
    which(predict.y<0.1)[1]
    
  } else {
    
    fit.nls	
  } 
}



# -----------------------------------------------------------------------------
# Function for binning samples in core.
# -----------------------------------------------------------------------------

binFunc = function(xDF, Ages, binWidth, minBin, maxBin){
  
  coreMat = xDF
  
  coreBin = seq(minBin, maxBin, binWidth)
  sampleBin = vector("list", length(coreBin)-1)
  
  for(i in 1:length(sampleBin)){
    toBin = which(Ages>=coreBin[i] & Ages<coreBin[i+1])
    sampleBin[[i]] = coreMat[toBin,]
  }
  
  coreRes = matrix(NA, ncol=ncol(xDF), nrow=length(sampleBin))
  if(ncol(xDF) > 1) {
    for(i in 1:length(sampleBin)) coreRes[i,] = apply(sampleBin[[i]], 2, function(x)mean(x, na.rm=T))
  } else { 
    for(i in 1:length(sampleBin)) coreRes[i,] = mean(sampleBin[[i]], na.rm=T)
  }
  colnames(coreRes) = colnames(xDF)
  rownames(coreRes) = coreBin[-1]
  
  coreRes <- as.data.frame(coreRes)
  coreRes
  
}




# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

# S T E P  1  :  R E G I M E   S H I F T   D E T E C T I O N

# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

# -------------------------------
# DIATOM REGIME SHIFT DETECTION
# -------------------------------

# Extract Diatom Data
diatoms = core[,3:62]
rownames(diatoms) = core[,2]

ages= core[,2]

# Find the zeros in the diatom data (i.e. no diatoms found in the samples)
diaZEROS <- c(match(names(which(apply(diatoms, 1, sum)==0)), rownames(diatoms)))

# Hellinger transformation + PCA
diaHel = decostand(na.omit(diatoms[-diaZEROS,]),method="hellinger", na.rm=TRUE)
pcaDia = rda(diaHel, scale=TRUE, na.action= "na.exclude")

# Extract importance of different axes
pcImp = summary(pcaDia)$cont$importance


# Set up the plotting area
par(mfrow=c(1,1), mar= c(5,4,4,2))

# select the 10 highest species on each PCA according to the absolute loadings score
loadPC1 = order(abs(summary(pcaDia)$species[,1]), decreasing=T)[1:8]
loadPC2 = order(abs(summary(pcaDia)$species[,2]), decreasing=T)[1:8]

# plot the PCA
plot(summary(pcaDia)$sites[,1], summary(pcaDia)$sites[,2], cex=0.6, xlab="PC1", ylab="PC2", type="n", ylim=c(-1.5,1.4), main= "PCA (Diatoms)" )
abline(h=0, v=0, lty=2)
points(summary(pcaDia)$sites[,1], summary(pcaDia)$sites[,2], cex=0.6, xlab="PC1", ylab="PC2", col="dark grey", pch=20 )
specPC1 = summary(pcaDia)$species[,1]
specPC2 = summary(pcaDia)$species[,2]
spec = c(loadPC1, loadPC2)
arrows(x0=rep(0,24), y0=rep(0,24), x1=c(specPC1[spec]), y1=c(specPC2[spec]), col="dark red", length=0.07 )
text(x=c(specPC1[spec]), y=c(specPC2[spec]), labels=names(specPC1[spec]), cex=0.6)


# Compare variance explained against broken stick model (see Bennett 1997)
n = ncol(diaHel)
k = seq(1, n)
sumFrac = 1/k
bstick = rep(NA,n)
for(j in 1:n) bstick[j] = 1/n*sum(sumFrac[j:n])
plot(k[1:10], pcImp[2,1:10], type="o", col="black", xlab="Component", ylab="% Var Expl", main="Broken Stick Model (PCA)")
points(k[1:10], bstick[1:10], type="o", col="red", xlab="Component", ylab="% Var Expl")

# identify significant PCs
sigTest = rep(NA, n)
for(j in 1:n) if(pcImp[2,j]>bstick[j]) sigTest[j] = 1
sigPCList = which(sigTest==1)
sigPC = pcImp[,sigPCList]
barplot(sigPC[2,], main= " PCA Variance Explained")  


# -----------------------------------------------------------------------------------
# Cluster analyses
# -----------------------------------------------------------------------------------


par(mar=c(5,5,5,5))
diss = vegdist(diaHel, method="bray")
clust  =  chclust(diss)
plot(clust,hang=-1, main="Constrained Cluster Analysis", cex=0.4)

# Estimate broken stick model
n = ncol(diaHel)
k = seq(1,n)
sumFrac = 1/k
bstick = rep(NA,n)
for(j in 1:n) bstick[j] = 1/n*sum(sumFrac[j:n])

# Compare variance explained by each split
clustVarEx = rev(diff(clust$height)/max(clust$height))
plot(k[1:10], clustVarEx[1:10], type="o", col="black", xlab="Number of Splits", ylab="% Var Expl", main="Broken Stick Model (cClust)")
points(k[1:10], bstick[1:10], type="o", col="red")

# 3 splits = 4 zones, so split the groups of the ccluster analysis accordingly
sigClust = cutree(clust, k=4)

# Do an unconstrained cluster analysis
nclust = hclust(diss, method="ward")
plot(nclust, main = "Cluster Analysis", cex=0.4)

# Compare against same broken split model
plot(seq(1,10, 1), (rev(diff(nclust$height))[1:10])/max(nclust$height), type="o", main= "Broken Stick (Cluster Analysis)", xlab="Number of Splits", ylab="% Var Expl")
points(k[1:10], bstick[1:10], type="o", col="red", main = "Broken Stick (Cluster Analysis)") 

# 3 splits = 4 zones, so split the groups of the ccluster analysis accordingly
sigNClust = cutree(nclust, k=4)

agesPCA = as.numeric(names(sigClust))
zonesClust = rep(NA, 3)
for(i in 1:3) zonesClust[i] = mean(c(max(agesPCA[sigClust==i]), min(agesPCA[sigNClust==(i+1)])))


# Make an exploratory plot looking at changes of PC1 and 2
# Extract PC1- 3 + importance of each PC
PC1 = summary(pcaDia)$sites[,1]; PC2 = summary(pcaDia)$sites[,2]

# Find the intervals of the zones identified by the cluster analysis

agesClust = as.numeric(names(sigClust))
zones = rep(NA, 3)
for(i in 1:3) zones[i] = mean(c(max(agesClust[sigClust==i]), min(agesClust[sigClust==(i+1)])))

agesPCA= as.numeric(names(PC1))

par(mfrow=c(2,1), mar=c(4,4,2,1))
plot(agesPCA, PC1 , xlab="Age cal yr BP", type="o")
for(i in 1:3) abline(v=zones[i], col=i+1, lty=2, lwd=2)
plot(agesPCA, PC2, xlab="Age cal yr BP", type="o")
for(i in 1:3) abline(v=zones[i], col=i+1, lty=2, lwd=2)


#  -------------------------------------------------------------------------------
# STARS algorithm. 
#  -------------------------------------------------------------------------------

# Make bins to enable use of autocorrelation correction in Rodionov 2006
diatomBin = binFunc(diatoms, as.numeric(rownames(diatoms)), 40, 20, 2660)

names(PC1) = agesPCA
names(PC2) = agesPCA

PC1 <-as.data.frame(PC1)
PC1Bin = binFunc(PC1, as.numeric(rownames(PC1)), 40, 20, 2660)
PC1Bin1 = c(PC1Bin$PC1)
names(PC1Bin1) <- rownames(PC1Bin)

# Standardise to zscores
PC1Bin1 <- PC1Bin1-mean(PC1Bin1, na.rm=T)
PC1Bin1 <- PC1Bin1/ sd(PC1Bin1, na.rm=T)

PC1stars = stars(rev(PC1Bin1), L=15, p=0.05,  h=1, AR1red="est", prewhitening = T)
plot.stars(PC1stars, rev(as.numeric(rownames(PC1Bin))))

PC2 <-as.data.frame(PC2)
PC2Bin = binFunc(PC2, as.numeric(rownames(PC2)), 40, 20, 2660)
PC2Bin1 = c(PC2Bin$PC2)
names(PC2Bin1) <- rownames(PC2Bin)

# Standardise to zscores

PC2Bin1 <- PC2Bin1-mean(PC2Bin1, na.rm=T)
PC2Bin1 <- PC2Bin1/ sd(PC2Bin1, na.rm=T)

PC2stars = stars(rev(PC2Bin1), L=15, p=0.05,  h=1, AR1red="est", prewhitening = T)
plot.stars(PC2stars, rev(as.numeric(rownames(PC2Bin))))

# ---------
# d13C
# ---------

d13C <-  core$d13C
names(d13C) <- ages
d13C.1 <-as.data.frame(d13C)
d13CBin = binFunc(d13C.1, as.numeric(rownames(d13C.1)), 40, 20, 2660)
d13CBin1 = c(d13CBin$d13C)
names(d13CBin1) <- rownames(d13CBin)

# Standardise to zscores
d13CBin1 <- d13CBin1-mean(d13CBin1, na.rm=T)
d13CBin1 <- d13CBin1/ sd(d13CBin1, na.rm=T)

d13Cstars<- stars(y=rev(d13CBin1), L=15, p=0.05, h=1, AR1red="est", prewhitening= T)

plot.stars(d13Cstars, rev(as.numeric(names(d13CBin1))))

# Set PC1 back
PC1 = summary(pcaDia)$sites[,1]; PC2 = summary(pcaDia)$sites[,2]
names(PC1) = agesPCA
names(PC2) = agesPCA

# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

# S T E P S   2 + 3  : R E S P O N S E   F U N C T I O N S //  I N T R I N S I C  V S.  E X T R I N S I C  D Y N A M I C S  

# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

# Example here is testing for regime shifts in diatom temporal data of RS1 (i.e. x = time, y = response).

# First estimate best fitting model using AIC and gnls() function. When best model is estimated check whether variance models are better fits. Also test for autocorrelation and see whether model fits improve when these are taken into account. See R file "Modelfitting.R" for theoretical examples of different models used.

diatoms = diatoms[-diaZEROS,]

# Set age vector.
ages = as.numeric(rownames(diatoms))

# Identify the zones. (Identified from step 1, using constrained cluster analysis results as separators)
RS1 = seq(103, 62, -1)
RS2 = seq(79, 33, -1)
RS3 = seq(61, 1, -1)

# Set which transition you are interested in and extract the relevant local environmental data
RS = RS1

# Extract the diatom data of interest, Hellinger transform, run the local PCA and extract the axis
diatomsRS = diatoms[RS,]
diaHelRS = decostand(diatomsRS,method="hellinger", na.rm=T)

pcaDia = rda(diaHelRS, scale=T, na.action= "na.exclude")

# Check variance explained by PCA and do plot to check species/ gradient associations makes sense.
pcImp = summary(pcaDia)$cont$importance
barplot(pcImp[2, 1:4])
plot(pcaDia)

PC1 = summary(pcaDia)$sites[,1]

# Set variable of interest 
x = ages[RS]
y = PC1

ylabel = "PC1"

par(mfrow=c(3,2), mar=c(5,4,4,2))

# -------------------------------------------------------------
# Null Model
# -------------------------------------------------------------

gnls0 <-gnls(y ~ b0 + (0*x), start=c(b0 = 0), na.action=na.omit)
gnls0Coef = as.vector(summary(gnls0)$coefficients)

plot(x, y, pch=20, main = "Null Model", xlab= "Years", ylab=ylabel, xlim =  c(max(x), min(x)))
sig = summary(gnls0)$sigma # standard error
full= rep(NA, length(x))
I1 = !is.na(y)
full[I1]= predict(gnls0)
lines(x[!is.na(full)], full[!is.na(full)], col="red") ; lines(x[!is.na(full)], full[!is.na(full)]+sig, lty=2, col="red") ; lines(x[!is.na(full)], full[!is.na(full)]-sig, lty=2, col="red") 
text(1800, 0.2, label= paste("AIC = ", round(AIC(gnls0), 2), sep=""))



# -------------------------------------------------------------
# Linear Model
# -------------------------------------------------------------

gnls1 <-gnls(y ~ b0 + b1*(x), start=c(b0 = 0 , b1 = 0), na.action=na.omit)
gnls1Coef = as.vector(summary(gnls1)$coefficients)
plot(x, y, pch=20, main = "Null Model", xlab= "Years", ylab=ylabel, xlim =  c(max(x), min(x)))
sig = summary(gnls1)$sigma # standard deviation of the residuals (called standard error in R)
full= rep(NA, length(x))
I1 = !is.na(y)
full[I1]= predict(gnls1)
lines(x[!is.na(full)], full[!is.na(full)], col="red") ; lines(x[!is.na(full)], full[!is.na(full)]+sig, lty=2, col="red") ; lines(x[!is.na(full)], full[!is.na(full)]-sig, lty=2, col="red") 
text(1800, 0.2, label= paste("AIC = ", round(AIC(gnls1), 2), sep=""))


# -------------------------------------------------------------
# Segmented Model
# -------------------------------------------------------------

# Set this according to regime shift period.
k1 = seq(2400, 1700, -20)

# Iterate through to find optimal value of k/ potential starting values
logLikResult = rep(NA, length(k1) )
for(i in 1:length(k1)){
  k = rep(k1[i], length(x))
  
  gnls2<-gnls(y ~ b0+ ifelse(x- k <=0, b1*x, b1*x + b2*(x-k)),  start=c(b0= 0, b1=0.001, b2 =0.005), na.action=na.omit) 	
  logLikResult[i] = logLik(gnls2)
}
k = rep(k1[which(logLikResult == max(logLikResult, na.rm=T))[1]], length(y)) # is best logLik score


gnls2<-gnls(y ~ b0+ ifelse(x- k <=0, b1*x, b1*x + b2*(x-k)),  start=c(b0= -27.5, b1=0.001, b2 =0.005), na.action= na.omit)

# retrieve the starting values then run the full regression
k2 = k[1]
rm(k)
gnls2Coef = as.vector(summary(gnls2)$coefficients)
gnls2<-gnls(y ~ b0+ ifelse(x- k <=0, b1*x, b1*x + b2*(x-k)),  start=c(k = k2, b0= gnls2Coef[1], b1= gnls2Coef[2], b2 = gnls2Coef[3]), na.action= na.omit)
gnls2Coef = as.vector(summary(gnls2)$coefficients)

plot(x, y, pch=20, main = "Segmented Linear Model", xlab= "Years", ylab=ylabel, xlim =  c(max(x), min(x)))
sig = summary(gnls2)$sigma # standard deviation of the residuals (called standard error in R)
full= rep(NA, length(x))
I1 = !is.na(y)
full[I1]= predict(gnls2)
lines(x[!is.na(full)], full[!is.na(full)], col="red") ; lines(x[!is.na(full)], full[!is.na(full)]+sig, lty=2, col="red") ; lines(x[!is.na(full)], full[!is.na(full)]-sig, lty=2, col="red") 
text(1800, 0.2, label= paste("AIC = ", round(AIC(gnls2), 2), sep=""))



# -------------------------------------------------------------
# Step increase in mean model
# -------------------------------------------------------------

logLikResult = rep(NA, length(k1))
for(i in 1:length(k1)){
  k = rep(k1[i], length(x))
  
  gnls3<-gnls(y~ ifelse(x- k <= 0, b0, b0 + b1),  start=c(b0=0 ,b1=0.3), na.action= na.omit)
  logLikResult[i] = logLik(gnls3)
}

k = rep(k1[which(logLikResult == max(logLikResult, na.rm=T))[1]], length(x)) # is best logLik score
gnls3<-gnls(y~ ifelse(x- k <= 0, b0, b0 + b1),  start=c(b0=0 ,b1=0.3), , na.action=na.omit)
gnls3Coef = as.vector(summary(gnls3)$coefficients)
plot(x, y, pch=20, main = "Step Model", xlab= "Years", ylab=ylabel, xlim =  c(max(x), min(x)))
sig = summary(gnls3)$sigma # standard deviation of the residuals (called standard error in R)
full= rep(NA, length(x))
I1 = !is.na(y)
full[I1]= predict(gnls3)
lines(x[!is.na(full)], full[!is.na(full)], col="red") ; lines(x[!is.na(full)], full[!is.na(full)]+sig, lty=2, col="red") ; lines(x[!is.na(full)], full[!is.na(full)]-sig, lty=2, col="red") 
text(1800, 0.2, label= paste("AIC = ", round(AIC(gnls3), 2), sep=""))



# -------------------------------------------------------------
# Sigmoidal model
# -------------------------------------------------------------

gnls4<-gnls(y~A+((K-A)/(1+ exp(-1*B*(x-M)))), start=c(B=-0.01, M = 2200, K= max(y, na.rm =T), A = min(y, na.rm=T)), na.action=na.omit)

plot(x, y, pch=20, main = "Sigmoidal Model", xlab= "Years", ylab=ylabel, xlim =  c(max(x), min(x)))
sig = summary(gnls4)$sigma # standard deviation of the residuals (called standard error in R)
full= rep(NA, length(x))
I1 = !is.na(y)
full[I1]= predict(gnls4)
lines(x[!is.na(full)], full[!is.na(full)], col="red") ; lines(x[!is.na(full)], full[!is.na(full)]+sig, lty=2, col="red") ; lines(x[!is.na(full)], full[!is.na(full)]-sig, lty=2, col="red") 
text(1800, 0.35, label= paste("AIC = ", round(AIC(gnls4), 2), sep=""))
title(main= paste("RS1", "PC1"), outer=T, line=-2)



gnls0AIC <- data.frame(model="null", logLik = logLik(gnls0)[1],No.parameters = 2, AIC= -2*logLik(gnls0)[1]+2*2)
gnls1AIC <- data.frame(model= "linear", logLik = logLik(gnls1)[1], No.parameters = 3, AIC =-2*logLik(gnls1)[1]+2*3)
gnls2AIC <- data.frame(model="segmented-linear", logLik = logLik(gnls2)[1], No.parameters= 5, AIC = -2*logLik(gnls2)[1]+2*5)
gnls3AIC <- data.frame(model="Step.mean", logLik = logLik(gnls3)[1], No.parameters= 4, AIC = -2*logLik(gnls3)[1]+2*4)
gnls4AIC <- data.frame(model="Sigmoidal", logLik = logLik(gnls4)[1], No.parameters= 5, AIC = -2*logLik(gnls4)[1]+2*5)
rbind(gnls0AIC, gnls1AIC, gnls2AIC, gnls3AIC, gnls4AIC)

# -------------------------------------------------------------
# Then test best model for autocorrelation in residuals/ changes in variance # -------------------------------------------------------------


RS1xPC1=x
acfTest(x, gnls4)
d = 18
agesD = data.frame(x1 = RS1xPC1)

gnls4var<-gnls(y~A+((K-A)/(1+ exp(-1*B*(x-M)))), start=c(B=-0.01, M = 2000, K= max(y, na.rm =T), A = min(y, na.rm=T)), weights=varFixed(~x))
AIC(gnls4, gnls4var) # quick check, but AIC may not be correct if not including floating parameters

gnls4AR<-gnls(y~A+((K-A)/(1+ exp(-1*B*(x-M)))), start=c(B=-0.01, M = 2000, K= max(y, na.rm =T), A = min(y, na.rm=T)), correlation=corExp(d, form=~x, fix=F))
AIC(gnls4, gnls4AR)

gnls4varAIC <- data.frame(model="Sigmoidal+var", logLik = logLik(gnls4var)[1], No.parameters= 5, AIC = -2*logLik(gnls4var)[1]+2*5)
gnls4ARAIC <- data.frame(model="Sigmoidal+AR", logLik = logLik(gnls4AR)[1], No.parameters= 6, AIC = -2*logLik(gnls4AR)[1]+2*5)

# RESULT = AR model not needed. Variance increase not needed.
write.csv(rbind(gnls0AIC, gnls1AIC, gnls2AIC, gnls3AIC, gnls4AIC, gnls4varAIC, gnls4ARAIC), file= "RS1PC1_AIC.csv")


# Continue for other variables and other regime shifts.
