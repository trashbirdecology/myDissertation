### A reprex for trying to interpret the code from Barichievy et al. 2018 Ecology and Evolution paper
set.seed=42
## set threshold for gap.percentile value
thresh=0.90

# set the resolution over which we will generate random samples
resolution = 100
########## PART I #########
# Actual Barichievy data ---------------------------------------------------------
barFig3.dat <- data.frame(n= c(7,10,3,5,7), 
  min = c(0.5, 1.8, 2.2, 2.6,3.5),
  max = c(1.2, 2.1, 2.3, 3.4, 5.5)
)

# Simulate data -----------------------------------------------------------
barFig3.dat.sim <- NULL
for(i in 1:nrow(barFig3.dat)){
  barFig3.dat.sim <- c(barFig3.dat.sim,
              sample(seq(barFig3.dat$min[i],barFig3.dat$max[i], by=0.01), size = barFig3.dat$n[i], replace = FALSE)
              )
}
length(barFig3.dat.sim)==sum(barFig3.dat$n)

# Run DD on actual data while varying the % of data sampled-----------------------------------
p <- c(0.50, 0.75, 1.00)*length(barFig3.dat.sim)
p.dd <-NULL
for(i in seq_along(p)){
log10.data <- sample(x=barFig3.dat.sim,
                      size=p[i],
                     replace=FALSE)


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
  for (l in 1:length(ks)) {
    
    # calculate KS density estimate
    KSdens <- density(log10.data, bw = ks[l], "gaussian", adjust = 1)
    
    # Test if the ksdensity is unimodal by...
      ## take first difference (convert to -1,0,or 1)
      ### take another first difference to get -2, 0, or 2
    TF <- which(diff(sign(diff(KSdens$y))) == 2) + 1
    
    ### So, is this saying that if ANY differ from 
    if (length(TF) == 0) bws[l] = 1
    if (length(TF) != 0)  bws[l] = 0
  }
  
  #Define the neutral Null
    ## first get the minimum of the bandwidths, which indicates I think the FIRST break in the distirbuiton?
    r = min(which(bws == 1))
  
    # Get the bandwidth for the neutral null distirbution we will compare to ours....
  hnull = ks[r]
  # return(hnull)

  # }


# Explore bootstrap function ----------------------------------------------

# DD <- function(log10.data, hnull, Sample.N = 1000, adjust=1, thresh=0.90) {
Sample.N = 1000
  # build a null distribution based on the bandwidth identified in the Neutral Null function
  NNull <- density(log10.data, bw = hnull, "gaussian", adjust = 1)
  
  ## compare the original density plot to the new Nnull density
  par(mfrow=c(1,2))
      plot(NNull)
      plot(density(log10.data))

                  
  N <- length(log10.data)
  
  # generate empty matrix
  null.samples <- matrix(data = 0,
                         ncol = Sample.N,
                         nrow = N)
  
  ## Build a matrix containing all the random samples from the NNull where rows represent a new simulated data
  for (k in 1:Sample.N) {
    #sample the null model
    rand.N <- sample(NNull$x, N, replace = TRUE, prob = NNull$y)
    #calculate the gaps and insert into the matrix
    null.samples[, k] <- sort(rand.N, decreasing = FALSE)
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
  for (j in 1:length(gaps.log10.data)) {
    #generate distribution of gaps per row (per gap rank)
    gap.percentile[j] <-
      ecdf(gaps.null.samples[j, ])(gaps.log10.data[j])
    
  }
  
  Bootstrap.gaps <- rbind(gap.percentile, 0)
  Bootstrap.gaps <- data.frame(log10.data, Bootstrap.gaps) %>%
    mutate(rank = seq(1, length(Bootstrap.gaps))) %>%
    mutate(isGap = as.factor(ifelse(Bootstrap.gaps >= thresh, "yes", "no")))
  
  # return(Bootstrap.gaps)
  
# }



# Visualize ---------------------------------------------------------------
p.dd[[i]] <-  ggplot(data=Bootstrap.gaps, aes(x=rank, y = log10.data))+
    geom_point(aes(color=isGap, shape=isGap))+
    scale_color_manual(values=c("yes"="red","no"="black"))+
    scale_shape_manual(values=c("yes"=17,"no"=16))+
    theme_bw()+
    ylab("log mass")+
    xlab("rank order")+
    labs(caption = "")+
    ggtitle(p[i])
  
}


########## PART II #########
### let's see what happens when we add x% more data within each "aggregation" (as id'd in barichievy paper)
# Actual Barichievy data ---------------------------------------------------------
barFig3.dat <- data.frame(n= c(7,10,3,5,7), 
                          min = c(0.5, 1.8, 2.2, 2.6,3.5),
                          max = c(1.2, 2.1, 2.3, 3.4, 5.5)
)

# Simulate data -----------------------------------------------------------


p <- c(2, 3, 5) # % of orginal data to be sampled
for(A in 1:seq_along(p)){
## in this loop we will add p% more data within each aggreagtion 
barFig3.dat.sim <- NULL
for(i in 1:nrow(barFig3.dat)){
  barFig3.dat.sim <- c(barFig3.dat.sim,
                       sample(seq(barFig3.dat$min[i],barFig3.dat$max[i], by=0.0001), 
                              size = barFig3.dat$n[i]*p[A], replace = FALSE)
  )
}


# Run DD on actual data while varying the % of data sampled (-----------------------------------
p <- c(0.01, 0.75, 1.00)*length(barFig3.dat.sim)
p.dd <-NULL
for(i in seq_along(p)){
  log10.data <- sample(x=barFig3.dat.sim,
                       size=p[i],
                       replace=FALSE)
  
  
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
  for (l in 1:length(ks)) {
    
    # calculate KS density estimate
    KSdens <- density(log10.data, bw = ks[l], "gaussian", adjust = 1)
    
    # Test if the ksdensity is unimodal by...
    ## take first difference (convert to -1,0,or 1)
    ### take another first difference to get -2, 0, or 2
    TF <- which(diff(sign(diff(KSdens$y))) == 2) + 1
    
    ### So, is this saying that if ANY differ from 
    if (length(TF) == 0) bws[l] = 1
    if (length(TF) != 0)  bws[l] = 0
  }
  
  #Define the neutral Null
  ## first get the minimum of the bandwidths, which indicates I think the FIRST break in the distirbuiton?
  r = min(which(bws == 1))
  
  # Get the bandwidth for the neutral null distirbution we will compare to ours....
  hnull = ks[r]
  # return(hnull)
  
  # }
  
  
  # Explore bootstrap function ----------------------------------------------
  
  # DD <- function(log10.data, hnull, Sample.N = 1000, adjust=1, thresh=0.90) {
  Sample.N = 1000
  # build a null distribution based on the bandwidth identified in the Neutral Null function
  NNull <- density(log10.data, bw = hnull, "gaussian", adjust = 1)
  
  ## compare the original density plot to the new Nnull density
  par(mfrow=c(1,2))
  plot(NNull)
  plot(density(log10.data))
  
  
  N <- length(log10.data)
  
  # generate empty matrix
  null.samples <- matrix(data = 0,
                         ncol = Sample.N,
                         nrow = N)
  
  ## Build a matrix containing all the random samples from the NNull where rows represent a new simulated data
  for (k in 1:Sample.N) {
    #sample the null model
    rand.N <- sample(NNull$x, N, replace = TRUE, prob = NNull$y)
    #calculate the gaps and insert into the matrix
    null.samples[, k] <- sort(rand.N, decreasing = FALSE)
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
  for (j in 1:length(gaps.log10.data)) {
    #generate distribution of gaps per row (per gap rank)
    gap.percentile[j] <-
      ecdf(gaps.null.samples[j, ])(gaps.log10.data[j])
    
  }
  
  Bootstrap.gaps <- rbind(gap.percentile, 0)
  Bootstrap.gaps <- data.frame(log10.data, Bootstrap.gaps) %>%
    mutate(rank = seq(1, length(Bootstrap.gaps))) %>%
    mutate(isGap = as.factor(ifelse(Bootstrap.gaps >= thresh, "yes", "no")))
  
  # return(Bootstrap.gaps)
  
  # }
  
  
  
  # Visualize ---------------------------------------------------------------
  p.dd[[i]] <-  ggplot(data=Bootstrap.gaps, aes(x=rank, y = log10.data))+
    geom_point(aes(color=isGap, shape=isGap))+
    scale_color_manual(values=c("yes"="red","no"="black"))+
    scale_shape_manual(values=c("yes"=17,"no"=16))+
    theme_bw()+
    ylab("log mass")+
    xlab("rank order")+
    labs(caption = "")+
    ggtitle(p[i])
  
}
p.dd[i]