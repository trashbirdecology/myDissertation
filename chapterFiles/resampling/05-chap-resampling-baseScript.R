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


# Conduct reasmpling analysis --------------------------------------------------------
# This will run and save results to feathers. if you want specific results only, specify ews or fivi = t/f
resamplingAnalysis(prop, myMethods, nDraws, winMove, 
                               ews = FALSE, 
                               fivi = TRUE, 
                               fi.method ="7.12") #7.12 is the derivatives method



# Plot resampling results -------------------------------------------------

