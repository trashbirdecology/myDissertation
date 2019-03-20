# About -------------------------------------------------------------------
# this is the script for analysing data for the rsampling chapter, which  explores the impact of data quality and information on various RDMs
# Setup -------------------------------------------------------------------
library(caTools); library(kedd); library(PerformanceAnalytics)
library(tidyverse); library(feather)
# Set random seed for repeatability
set.seed(12345)

# Source functions
source("./chapterFiles/resampling/05-chap-resampling-myFunctions.R")

# Load the data -----------------------------------------------------------
myDf.long <- getSpanbauerData()

# Define subsetting parameters --------------------------------------------
## Which methods of data subsetting to observe..
myMethods <- c("species", "observations", "dominance")

# Which proportions of myMethods to explore
prop = c(0.25, 0.5, 0.75, 1.0)  # one or more numbers between 0 and 1

# Define the number of random draws for each method
nDraws <- 1e2

# Proportion to increase the window size by
winMove <- 0.20

resultsDir<- "./chapterFiles/resampling/results/"
distDir<- paste0(resultsDir, "distances/")
ewsDir <- paste0(resultsDir, "ews/")
dir.create(resultsDir)
dir.create(distDir)
dir.create(ewsDir)

# Create random subsets of data -------------------------------------------
# Loop over methods and proportions 
subsetData <- NULL
for(h in seq_along(prop)){
  for (i in 1:length(myMethods)) {
    for (j in 1:nDraws) {
      # Subset the data
  temp <- random_subset(myDf.long, myMethods[i], prop[h]) %>%
      mutate(nDraw = j, 
             winMove = winMove)
    
  # Calculate distance travelled
    if(exists("results")) rm(results)  
    results <- temp %>%
        # Distance between species
        arrange(variable, method, prob, nDraw, time) %>%
        group_by(variable, method, prob, nDraw) %>%
        mutate(dx = value - lag(value)) %>%
        ungroup() %>%
        na.omit(dx) %>%
        # Sum of distances (across species at each time)
        group_by(method, prob, nDraw, time, winMove) %>%
        summarize(ds = sqrt(sum(dx ^ 2))) %>%
        filter(ds != 0) %>%
        ungroup() %>%
        # Calculate cumulative ds and derivatives
        group_by(method, prob, nDraw) %>%
        mutate(s = cumsum(ds),
               dsdt = ((s - lag(s)) / (time - lag(time))),
               d2sdt2 = ((dsdt - lag(dsdt)) / (time - lag(time)))) %>%
        ungroup() %>%
        # Drop NA's
        na.omit(p)
        
    # Save the distance results to file
    if(exists("fn")) rm(fn)
    fn <- paste0(distDir, 'distResults_', prop[h],"_", myMethods[i], "_draw", j)
    write_feather(x=results,path=fn)
    
    # Calculate EWSs
    # if(exists("results")) rm(results)  
    temp <- full_join(results, temp) %>% 
      arrange(time)
    
    if (nrow(temp) <= 5) {
      warning("Five or less observations in data subset--not calculating EWS")
      next
    }
    
    
    # Calculate EWS 
        # Window size
        time <- temp$time
        timeSpan <- range(time)
        TT <- timeSpan[2] - timeSpan[1]
        winSize <- winMove * TT
        # Window spacing
        winSpace <- max(lead(time) - time, na.rm = T)
    
        # calculate Fisher Information and Variance Index within the window
        require(caTools)
        winResults_fivi <- window_analysis(temp, winSize, winSpace)  
        
        winResults_ews <- window_analysis_EWS(temp, winSize, winSpace)
        
    results <- full_join(winResults_fivi, winResults_ews)

    # Save the EWS results to file
    if(exists("fn")) rm(fn)
    fn <- paste0(ewsDir, 'ewsResults_', prop[h],"_", myMethods[i], "_draw" , j)
    write_feather(x=results,path=fn)
    
    
    } # end nDraws loop (j)
  } # end myMethods loop
} #end prop loop

