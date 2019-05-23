## This script was adapted from the supplementary materials for the paper:  A method to detect discontinuities in census data (2018). Chris Barichievy*, David G. Angeler, Tarsha Eason, Ahjond S. Garmestani,  Kirsty L. Nash, Craig A. Stow, Shana Sundstrom, and Craig R. Allen.

#1.Neutral.Null
Neutral.Null <- function(log10.mass, resolution = 4000) {
  log10.mass <- sort(log10.mass)
  # get min and max body masses
  Dmax = max(log10.mass, na.rm = TRUE)
  Dmin = min(log10.mass, na.rm = TRUE)
  # get time between sampling points
  ds = (Dmax - Dmin) / resolution
  MaxK = (Dmax - Dmin) / 2
  MinK = ds * 2
  
  # create a vector of values for analysis (h?)
  ks = seq(MinK, MaxK, by = 1 / resolution)
  
  # generate an empty matrix matrix
  bws = matrix(data = NA,
               nrow = length(ks),
               ncol = 1)
  
  # Run loop over the vector of ks
  for (i in 1:length(ks)) {
    
    # calculate KS density estimate
    KSdens <- density(log10.mass, bw = ks[i], "gaussian", adjust = 1)
    
    # Test if the ksdensity is unimodal
    TF <- which(diff(sign(diff(KSdens$y))) == 2) + 1
    if (length(TF) == 0) bws[i] = 1
    if(length(TF) != 0)  bws[i] = 0
  }
  #Define the neutral Null
  r = min(which(bws == 1))
  hnull = ks[r]
  return(hnull)
}

#2. bootstrap function
DD <- function(log10.mass, hnull, Sample.N = 1000, adjust=1, thresh=0.95) {
  NNull <- density(log10.mass, bw = hnull, "gaussian", adjust = adjust)
  N <- length(log10.mass)
  
  # generate empty matrix
  null.samples <- matrix(data = 0,
                         ncol = Sample.N,
                         nrow = N)
  
  for (i in 1:Sample.N) {
    #sample the null model
    rand.N <- sample(NNull$x, N, replace = TRUE, prob = NNull$y)
    #calculate the gaps and insert into the matrix
    null.samples[, i] <- sort(rand.N, decreasing = FALSE)
  }
  
  #generate gaps
  gaps.log10.mass <- diff(log10.mass)
  gaps.null.samples <- diff(null.samples, decreasing = FALSE)
  gap.percentile <- matrix(data = 0,
                           nrow = length(gaps.log10.mass),
                           ncol = 1)
  for (i in 1:length(gaps.log10.mass)) {
    #generate distribution of gaps per row (per gap rank)
    gap.percentile[i] <-
      ecdf(gaps.null.samples[i, ])(gaps.log10.mass[i])
    
  }
  
  gap.percentile<-rbind(gap.percentile,0)
  gap.percentile<-data.frame(log10.mass,gap.percentile) %>% 
    mutate(
      isGap = as.factor(ifelse(gap.percentile>=thresh, "yes","no")))
           
  
  return(gap.percentile)
  
}


