# Modified from Sergei Rodionov's VBA and Andrew Gardner's Matlab codes to produce an R version of STARS. Go to http://www.climatelogic.com/overview for more information, or see the Rodionov (2004; 2006) references in the paper.

# Red noise correction included, but some other functions from VBA code (version 3.2) still missing (e.g. tests in shifts in variance).

# Checked RSI results using two independent time-series against Rodionov plug in.

# Alistair Seddon, October 2013. 
# alistair.seddon@gmail.com

stars<- function(y=c(rnorm(50), rnorm(50,2,1)), L=20, p=0.05, h=1,  AR1red="none", prewhitening = F) {
  
  
  if(AR1red=="none" && prewhitening== T) stop("Impossible to perform prewhitening if AR1 coefficient is not estimated")
  
  m= round((L+1)/3)				# formula to estimate subsample size for calculating alpha (Rodionov 2006 + http://www.climatelogic.com/documentation/red-noise-estimation)    	  
  
  # library("MASS") needed if you want to use Huber correction parameter built in to MASS   
  
  
  
  
  
  # -------------------------------------------------------------------------
  #    hWeightedAverage(xwin)
  
  #     Calculates the mean estimate for a given range using Huber's weights.
  # -------------------------------------------------------------------------
  
  
  hWeightedAverage<-function(xwin, h){
    
    # simple estimate of the regime mean for the windowed clip
    dblEstAve <- mean(xwin);
    
    for(jjj in 1:2){
      sumWeights = 0
      sumAve = 0
      
      # Estimate normalised deviation
      xDev = (xwin-dblEstAve)/sqrt(sigL)
      
      # Estimate weights or normalised deviation
      xDev[xDev==0] = 1
      wDev = pmin(rep(1, length(xwin)), h/abs(xDev), na.rm=T)
      
      #sum weights and weighed values
      sumWeights = sum(wDev)
      sumAve = sum(xDev*wDev)
      
      sumAve = sumAve/sumWeights
      sumAve = sumAve*sqrt(sigL) + dblEstAve
      dblEstAve = sumAve
    }
    
    dblWeightedAve = dblEstAve
    # hestimate<- huber(xwin, h)
    # dblWeightedAve = hestimate$mu
  }
  
  
  
  #-------------------------------------------------------------------
  # estimateSigma
  # Estimate the long-term, L-pt variance (assume homoskedastic signals).
  #-------------------------------------------------------------------
  
  estimateSigma<-function(x, L){
    
    # Estimate the long-term length-L variance. If the signal >> length of the analysis window, sample to estimate the variance.
    
    nx<-length(x)
    if(nx/L>300) ix <- as.integer(runif(100)*(nx-2*L)+L) else ix<-seq(L,nx,1)
    s<-0
    for(i in 1:length(ix)){
      xwin <- x[(ix[i]-L+1):ix[i]]
      s <- s + var(xwin, na.rm=T)
    }
    sigL1 = s / length(ix)
    sigL1
  }
  
  
  # ------------------------------------------------------------------
  #   getThreshold()
  #
  #   Calculate the critical threshold of deviation that signals regime changes.  This does not change over the signal.
  # ---------------------------------------------------------------
  getThreshold<-function(L, p, sigL){
    
    if(prewhitening == T){
      dof <- 2*L-2						# number degrees freedom
    } else {
      dof <- EqN((2*L-2), alpha)
    }
    
    t <- abs(qt(p/2, dof));              # crit 2-sided t-value
    thresh1 = t*sqrt(2*sigL/L);          # crit deviation
    thresh1
  }
  
  
  
  # -------------------------------------------------------------------
  
  # OLS estimate of AR1 coefficient from Orcutt and Winokur, 1969, Econometrica, 37:1,1-14
  
  # -------------------------------------------------------------------
  
  OLScalc<-function(x){
    Nobs = length(x)
    
    ave1 = mean(x[2:Nobs])
    ave2 = mean(x[1:(Nobs-1)])
    
    sumNom=0
    sumDenom=0
    for(i in 2:Nobs){
      sumNom = sumNom + (x[i] - ave1) * (x[i - 1] - ave2)
      sumDenom = sumDenom + (x[i - 1] - ave2) * (x[i - 1] - ave2)
    }
    if(sumDenom > 0) OLSAR1 = sumNom / sumDenom else OLSAR1 = 0
    OLSAR1
  }
  
  
  # -------------------------------------------------------------------
  # AR1 correlation estimate (alpha)
  
  # ------------------------------------------------------------------- 
  
  AR1cor<-function (m, y){
    m= m #define this in big function above
    
    ny=length(y)
    iy=seq(1,ny-m+1,1)
    OLS=rep(NA, length(iy))
    
    # Calculate OLS for sequential samples of length m
    for(i in 1:length(iy)){
      
      xwin = y[(iy[i]):(iy[i]+m-1)]
      
      if(length(xwin[is.na(xwin)]) == 0)   OLS[i] <- OLScalc(xwin)
    }
    
    est<-median(OLS, na.rm=T)
    
    # Calculate IP4	
    IP4= est + 1/m
    for(j in 1:3) IP4=IP4 + abs(IP4)/m
    
    # Calculate MPK
    if (m>4) MPK=((m-1)*est+1)/(m-4) else MPK= est
    
    alphaEst<-c(est, MPK, IP4) 	
    
  } 	
  
  
  # -------------------------------------------------------------------
  # Function EqP: calculates t-test using equivalent sample size as in von Storch and Zwiers (1999, p.115)
  # ------------------------------------------------------------------- 
  
  EqP= function(rng1, rng2){   
    
    # Set standard no-result for if command at end
    EqP = 0
    
    # Calculate means and variances
    ave1 = mean(rng1, na.rm =T)
    ave2 = mean(rng2, na.rm =T)   
    
    var1 = sd(rng1, na.rm = T)
    var2 = sd(rng2, na.rm = T)   
    
    # Calculate effective sample sizes   
    Ns1 = length(na.omit(rng1))
    if(Ns1 < 2){
      EqP = -1
    } 
    eN1 = EqN(Ns1, alpha)
    
    Ns2 = length(na.omit(rng2))
    if(Ns2 < 2){
      EqP = -1
    } 
    eN2 = EqN(Ns2, alpha)
    
    if(EqP == -1){
      EqP
    } else{
      # Calculate t-statistics
      T_stat = sqrt(var1/eN1 + var2/ eN2)
      T_stat = abs(ave1 - ave2)/ T_stat
      
      EqP = (1-pt(T_stat, eN1 + eN2 -2))*2
      EqP
    }
    
  }
  
  # -------------------------------------------------------------------
  # EqN: Calculates equivalent sample size as in von Storch and Zwiers (1999, p.115)
  # -------------------------------------------------------------------
  
  EqN = function(Ns, alpha){
    
    sumEqN = rep(NA, Ns-1)
    for(i in 1: (Ns-1)){
      sumEqN[i] = (1-i/Ns)*alpha^i
    }
    
    
    EqN = Ns / (1 + sum(c(sumEqN)))
    
    # just in case
    if( EqN <=2) EqN = 2
    if(EqN > Ns) EqN =Ns
    EqN
  }
  
  
  
  # ------------------------------------------------------------------
  #   cusumUp()
  
  #       Compute the L-pt cusum for positive level changes.  For a positive regime change to be accepted, we require the L-pt lookahead samples	to produce a cusum sequence which does not go negative.
  
  # -------------------------------------------------------------------
  
  cusumUp<-function(k){
    # LL sets the look ahead length: L, or the number of points until the end of the signal. k is the sample point running through the iteration
    LL <- min(L, N-k+1)
    
    # dblXdev is the length-LL vector of normalized deviations of x outside of the range lvl +/- thresh
    dblXdev = ((x[k:(k+LL-1)]) - (lvl+thresh)) / sqrt(sigL)
    
    #  these are Huber weight values, so large deviations are deemphasized
    dblXdev[dblXdev==0] = 1
    dblXweight = pmin(rep(1, length(dblXdev)), h/abs(dblXdev), na.rm=T)
    
    # % the cusum is the integral of the weighted deviations; we normalize
    # % here, too, by dividing by the sum of the weights
    
    cs<- cumsum(dblXweight*dblXdev)/sum(dblXweight)
    
    # cs<-cumsum(dblXdev) #simple non weighted version
    
    # we check for cusum values below zero, which would indicate a failed
    # regime change; otherwise, we have a positive shift
    if (length(which(cs < 0) > 0)) cs = 0 else  cs = cs[LL]
    cs
  }    
  
  # ------------------------------------------------------------------
  #   cusumDown()
  
  #       Compute the L-pt cusum for positive level changes.  For a positive regime change to be accepted, we require the L-pt lookahead samples	to produce a cusum sequence which does not go negative.
  
  # -------------------------------------------------------------------
  
  cusumDown<-function(k){
    # LL sets the look ahead length: L, or the number of points until the end of the signal. k is the sample point running through the iteration
    LL <- min(L, N-k+1)
    
    # dblXdev is the length-LL vector of normalized deviations of x outside of the range lvl +/- thresh
    dblXdev = ((x[k:(k+LL-1)]) - (lvl-thresh)) / sqrt(sigL)
    
    #  these are Huber weight values, so large deviations are deemphasized
    dblXdev[dblXdev==0] = 1
    dblXweight = pmin(rep(1, length(dblXdev)), h/abs(dblXdev), na.rm=T)
    
    # % the cusum is the integral of the weighted deviations; we normalize
    # % here, too, by dividing by the sum of the weights
    
    cs<- cumsum(dblXweight*dblXdev)/sum(dblXweight)
    
    # cs<-cumsum(dblXdev) # simple non-weighted version
    
    # we check for cusum values above zero, which would indicate a failed
    # regime change; otherwise, we have a positive shift
    if (length(which(cs > 0) > 0)) cs = 0 else  cs = cs[LL]
    cs
    
  }    
  
  
  #  -------------------------------------------------------------------------
  #      rsi(k)
  
  #      Compute the rsi for a given sample index, regime mean, and critical
  #      threshold.
  #    -------------------------------------------------------------------------
  rsi<-function(k){
    if(x[k] > (lvl + thresh)){
      r = cusumUp(k)
    } else if(x[k] < (lvl - thresh)){
      r = cusumDown(k)
    } else {
      r = 0
    }
    r
  }  
  
  
  
  #  -------------------------------------------------------------------------
  
  # Red noise filtering of timeseries.
  
  
  #  -------------------------------------------------------------------------
  
  
  
  alpha<-AR1cor(m,y) # calculate alpha estimates
  
  if(AR1red=="est"){
    alpha = alpha[1]
  }else  if(AR1red=="MPK"){
    alpha = alpha[2]	
  }else if(AR1red=="IP4"){
    alpha = alpha[3]
  }else if(AR1red=="none"){
    alpha= 0
  }
  
  if(alpha<0) alpha <- 0 ; if(alpha>1) alpha <- 1
  
  # Filter time series if selected and select as x for main procedure, otherwise use timeseries
  
  if(prewhitening == T){ 	
    Zt=rep(NA, length(y))
    for(j in 2:length(y)) Zt[j]<-y[j]-(y[j-1]*alpha)
    
    
    if(alpha>0) x=Zt[-1] else x=y[-1]
    names(x) <- names(y)[-1]
    
  } else x=y[-1]
  
  x <- na.omit(x)
  
  #  -------------------------------------------------------------------------
  
  # initialisation 
  
  
  #  -------------------------------------------------------------------------
  
  
  sigL = estimateSigma(x, L);           			# sample L-pt variance
  thresh = getThreshold(L, p, sigL);          # critical threshold
  lvl = hWeightedAverage(x[1:L], h);             # initial mean level
  R = rep(0, length(x));                      # rsi values
  RpVal<-rep(0, length(x))
  cp = 1;                                     # current change-point index
  N = length(x)                              # number of samples
  
  if(length(names(y))==0) {
    stop("Stopped: No ages supplied with timeseries")
    
    
  } else ages = names(y) 
  
  
  
  # Main routine.
  for (k in 2:N){
    R[k] = rsi(k)
    
    #   too few samples to confirm last regime change?
    if (abs(R[k]) > 0 && k > (N-L+1)) break           
    
    #   test for regime shifts and update current regime mean (unless we are within L-pts of most recent change-point)
    if(R[k] == 0){
      if(k >= (cp + L)) lvl = hWeightedAverage(x[cp:k], h)    # same regime, far enough 
      
    } else{
      cp = k                              # regime change
      lvl = hWeightedAverage(x[k:(k+L-1)], h); # same regime, far enough from cp
    }
  }
  
  #  Calculation of new regime sample means and associated pvalues of shifts)
  if(R[length(R)] != 0) R[length(R)]<- 0
  cps<-which(abs(R)>0)
  
  rID<-rep(1, length(x))
  rLabel<-seq(2, length(cps)+1,1)
  Rmean<-rep(0, length(cps)+1)
  
  for(j in 1:length(cps)) rID[cps[j]:N]<-rLabel[j]
  for(j in 1:length(Rmean)) Rmean[j]<- hWeightedAverage(x[rID==j], h)
  # for(j in 1:length(Rmean)) Rmean[j]<- mean(x[rID==j])
  xNames= names(x)
  
  rID1 = rID
  for(j in 1:length(Rmean)) rID[rID==j]<-Rmean[j]
  
  xNA=rep(NA, length(y))
  xNA[match(xNames, names(y))] <- x
  
  RNA=rep(NA, length(y))
  RNA[match(xNames, names(y))] <- c(R)
  
  rIDNA=rep(NA, length(y)) 
  rIDNA[match(xNames, names(y))] <- c(rID)
  starsResult<-cbind(y,xNA, RNA , rIDNA) 
  
  colnames(starsResult) = c("ts", "AR1.cor", "rsi", "mean"); rownames(starsResult) = ages
  
  
  
  # Estimate pValues of shifts on either white-noise filtered series, or by using the AR1 correction parameter
  
  pVal = rep(0, length(cps))
  
  for(j in 1:length(cps)) {
    
    rs1 = x[rID1==j]
    rs2 = x[rID1==(j+1)]
    
    if(length(rs2)==1) {
      next
    } else {
      ifelse(prewhitening ==T, pVal[j] <- t.test(rs1, rs2)$p.value, pVal[j] <- EqP(rs1, rs2))
    }
  }
  
  if(length(which(pVal == -1)) >0) warning("pValue calculation of -1 due regime containing only one sample")		
  
  starsOUT=list(starsResult, alpha, pVal)
  names(starsOUT)=c("starsResult", "alpha", "pVal") 
  starsOUT
  
}





#  -------------------------------------------------------------------------

# plot.stars()


#  -------------------------------------------------------------------------

plot.stars<-function(starsOb, age1){
  
  starsPlot = starsOb$starsResult
  alpha = starsOb$alpha
  
  naRaw <- attr(na.omit(starsPlot[,1]), "na.action")
  naWhit <- attr(na.omit(starsPlot[,2]), "na.action")
  naMean <- attr(na.omit(starsPlot[,4]), "na.action")
  
  par(mfrow=c(2,1))
  plot(age1[-naRaw], starsPlot[-naRaw,1], type="o", cex=0.3, lty=3, col="lightblue", lwd=0.8, xlab=" ", ylab=colnames(starsPlot)[1])
  points(age1[-naWhit], starsPlot[-naWhit,2], type= "o", cex=0.3, lty=3, col="darkblue", pch=1)	
  points(age1[-naMean], starsPlot[-naMean,4], type="l", col="red", lty=1, lwd=1.5)
  if(alpha>0) {
    text(as.numeric(max(age1))*0.9, min(starsPlot[,2], na.rm=T), labels=paste("AR1 coeff = ", round(alpha, 2), sep=""), adj=c(NA,1))
  }
  
  starsPlot[which(starsPlot[,3]==0),3] = NA
  plot(age1, abs(starsPlot[,3]), type="h", lty=1, lwd=3, xlab=" Age (cal yr BP)", ylim=c(0, max(starsPlot[,3], na.rm=T)*1.1), col="red", ylab="RSI")
  
  
}















