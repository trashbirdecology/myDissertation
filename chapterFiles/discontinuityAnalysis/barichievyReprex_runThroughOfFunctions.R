### A reprex for trying to interpret the code from Barichievy et al. 2018 Ecology and Evolution paper
# Create data -------------------------------------------------------------

# Create a simple bimodal body mass distirbution
log10.data <- c(log(rnorm(4, .5, n = 50)), log(rnorm(6, .1, n = 50)))
hist(log10.data)


# Eplore the netutral null function ---------------------------------------

# set the resolution over which we will generate random samples?
resolution = 100

## Run through the neutral null function
# Neutral.Null <- function(log10.data, resolution=4000) {
  log10.data <- sort(log10.data)
  
  # get min and max body masses
  Dmax = max(log10.data, na.rm = TRUE)
  Dmin = min(log10.data, na.rm = TRUE)
  
  # get space elapsed between sampling within a simulation (unit takes on unit log10.data)
  ds = (Dmax - Dmin) / resolution
  
  # Get the upper and lower limits the bandwidth values
  MaxK = (Dmax - Dmin) / 2
  MinK = ds * 2
  
  # create a vector of bandwidth (bw) values for use in the density function below
  ks = seq(MinK, MaxK, by = 1 / resolution)
  
  # generate an empty matrix 
  bws = matrix(data = NA,
               nrow = length(ks),
               ncol = 1)
  
  # Run loop over the vector of ks
  for (i in 1:length(ks)) {
    
    # calculate KS density estimate
    KSdens <- density(log10.data, bw = ks[i], "gaussian", adjust = 1)
    
    
    # Test if the ksdensity is unimodal by...
      ## take first difference (convert to -1,0,or 1)
      ### take another first difference to get -2, 0, or 2
    TF <- which(diff(sign(diff(KSdens$y))) == 2) + 1
    
    ### So, is this saying that if ANY differ from 
    if (length(TF) == 0) bws[i] = 1
    if (length(TF) != 0)  bws[i] = 0
  }
  
  #Define the neutral Null
    ## first get the minimum of the bandwidths, which indicates I think the FIRST break in the distirbuiton?
    r = min(which(bws == 1))
  
    # Get the bandwidth for the neutral null distirbution we will compare to ours....
  hnull = ks[r]
  return(hnull)

  # }





# Explore bootstrap function ----------------------------------------------

    
# DD <- function(log10.data, hnull, Sample.N = 1000, adjust=1, thresh=0.90) {

  # build a null distribution based on the bandwidth identified in the Neutral Null function
  NNull <- density(log10.data, bw = hnull, "gaussian", adjust = adjust)
  
  ## compare the original density plot to the new Nnull density
  # par(mfrow=c(1,2))  
  #     plot(NNull)
  #     plot(density(log10.data))
  
                  
  N <- length(log10.data)
  
  # generate empty matrix
  null.samples <- matrix(data = 0,
                         ncol = Sample.N,
                         nrow = N)
  
  ## Build a matrix containing all the random samples from the NNull where rows represent a new simulated data
  for (i in 1:Sample.N) {
    #sample the null model
    rand.N <- sample(NNull$x, N, replace = TRUE, prob = NNull$y)
    #calculate the gaps and insert into the matrix
    null.samples[, i] <- sort(rand.N, decreasing = FALSE)
  }
  
  #generate gaps of observed data
  
  ## First difference of observed data
  gaps.log10.data <- diff(log10.data)
  # First differences
  gaps.null.samples <- diff(null.samples, decreasing = FALSE)
  
  # create empty matrix to save gap percentile results
  gap.percentile <- matrix(data = 0,
                           nrow = length(gaps.log10.data),
                           ncol = 1)
  
  # For each space between observations in the original data (i.e. # =N-1)
  for (i in 1:length(gaps.log10.data)) {
    #generate distribution of gaps per row (per gap rank)
    gap.percentile[i] <-
      ecdf(gaps.null.samples[i, ])(gaps.log10.data[i])
    
  }
  
  Bootstrap.gaps <- rbind(gap.percentile, 0)
  Bootstrap.gaps <- data.frame(log10.data, Bootstrap.gaps) %>%
    mutate(rank = seq(1, length(Bootstrap.gaps))) %>%
    mutate(isGap = as.factor(ifelse(Bootstrap.gaps >= thresh, "yes", "no")))
  
  
  
  # return(Bootstrap.gaps)
  
# }


# Compare distribution to previous year distribution ----------------------



# visualizing the body masses ---------------------------------------------
plotDD <- function(Boostrap.gaps, perc.cutoff = 0.95, type=c("hist","density")){
  
  # if(type=="density") 
  p <- ggplot(Bootstrap.gaps %>% filter(gap.percentile>=perc.cutoff))+
    geom_density(aes(log10.data))
  
  
  p<-p+theme_bw()
  
  p2 <- ggplot(Bootstrap.gaps %>% filter(gap.percentile<=1-perc.cutoff))+
    geom_point(aes(x =rank, y = log10.data))+
    geom_vline(aes(xintercept=))
  
  p2  
  
}
  