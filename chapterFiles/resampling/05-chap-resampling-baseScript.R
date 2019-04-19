# About -------------------------------------------------------------------
# this is the script for analysing data for the rsampling chapter, which  explores the impact of data quality and information on various RDMs
# Setup -------------------------------------------------------------------
rm(list=ls())

library(caTools)
library(kedd)
library(PerformanceAnalytics)
library(tidyverse)
library(feather)

# Set random seed for repeatability
set.seed(12345)

# Source functions
source("./chapterFiles/resampling/05-chap-resampling-myFunctions.R")


# Anlaysis paramters ------------------------------------------------------

# Which dataset to use
diatoms <- TRUE # do you want to analyze the Spanbuaer data? if not, program will create dummy data 
dummyData <- FALSE # do you want to create a dummy data rather than using the Spanbauer data?

# Spit an error to ensure no issues
if(diatoms & dummyData) stop("The logicals diatoms and dummyData are both 'TRUE'. I cannot handle this, please make one FALSE.")

# Define how to subset the data
## Which methods of data subsetting to observe?
myMethods <- c("species", "observations", "dominance")

## Which proportions of myMethods to explore?
prop = c(0.25, 0.5, 0.75, 1.0)  # one or more numbers between 0 and 1

## Define the number of random draws for each method
nDraws <- 1e4

## Proportion of observations over which the moving window will advance
winMove <- 0.20 # between 0 and 1


# Create or load the observations -----------------------------------------------------------
# Use Spanbauer 2014 diatom time series
if (diatoms) {
  dirNameInd <- "diatoms"
  myDf.long <- getSpanbauerData() # a data.frame with
}

# Create some data
if (dummyData) {
  dirNameInd <- "dummy"
  myDf.long <- NULL
  nobs = 1e2 # number of observations
  nspp = 1e1 # number of variables
  x    = 0:100   # the range of values to choose from for potential value
  
  for (i in 1:nspp) {
    temp <- NULL
    
    mean = base::sample(abs(100 - sample(50, 1)), size  = 1)
    mean2  = base::sample(100, size = 1)
    
    # get a series of values for each species
    value <- c(
      rnorm(
        n = nobs / 2,
        mean = mean ,
        sd = mean - mean / 2
      ),
      rnorm(
        n = nobs / 2,
        mean = mean2,
        sd = abs(mean2 - mean) / sample(1:4, 1)
      )
    )
    
    temp <- tibble(
      time = rep(1:nobs),
      site = "A",
      variable = letters[i],
      value =   value
    )
    
    # create the final data frame in long format
    myDf.long <- bind_rows(myDf.long, temp)
  } # end i for
} # end if dummyData


# Visualize the Raw Data --------------------------------------------------
(p.orig <- ggplot(myDf.long) + geom_line(aes(
  x = time,
  y = value,
  color = (variable)
)) + 
  theme_classic() +
  theme(legend.position = 'none'))


# Create directories in which to store files.  ----------------------------
## If dirs exist will not create new.
  {
  dir.create("./chapterFiles/resampling/results/")
  
  resultsDir <-
    paste0(
           "./chapterFiles/resampling/results/", dirNameInd, "/")
  dir.create(resultsDir)
  
  distDir <- paste0(resultsDir, "", "distances/")
  ewsDir <- paste0(resultsDir, "ews/")
  fiviDir <- paste0(resultsDir, "fiVi/")
  origDataDir <- paste0(resultsDir, "originalData/")
  dir.create(distDir)
  dir.create(ewsDir)
  dir.create(fiviDir)
  dir.create(origDataDir)
}


# Conduct reasmpling analysis --------------------------------------------------------
# This will run and save results to feathers. if you want specific results only, specify ews or fivi =TRUE/FALSE. This function will always calc and save distances to file. Can also specify to save the original data (origDat= TRUE)
resamplingAnalysis(
  myDf.long,
  prop,
  myMethods,
  nDraws,
  winMove=.20,
  origData = TRUE, 
  ews = TRUE,
  fivi = TRUE,
  fi.method = "7.12" #7.12 is the derivatives method
)


# Plot resampling results -------------------------------------------------
origData <- purrr::map_df(list.files(origDataDir, full.names = TRUE), read_feather)
distResults <- purrr::map_df(list.files(distDir, full.names = TRUE), read_feather)
ewsResults <- purrr::map_df(list.files(ewsDir, full.names = TRUE), read_feather)
fiviResults <- purrr::map_df(list.files(fiviDir, full.names = TRUE), read_feather)
View(ewsResults)

