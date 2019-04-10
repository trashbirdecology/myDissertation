# About -------------------------------------------------------------------
# this is the script for analysing data for the rsampling chapter, which  explores the impact of data quality and information on various RDMs
# Setup -------------------------------------------------------------------
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
diatoms <- FALSE
dummyData <-
  TRUE # do you want to create a dummy data rather than using the Spanbauer data?

# Load the data -----------------------------------------------------------

if (diatoms)
  myDf.long <- getSpanbauerData() # a data.frame with
if (dummyData) {
  myDf.long <- NULL
  
  nobs = 100 # number of observations
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

ggplot(myDf.long) + geom_line(aes(
  x = time,
  y = value,
  color = (variable)
)) + theme(legend.position = 'none')

# Define subsetting parameters --------------------------------------------
## Which methods of data subsetting to observe..
myMethods <- c("species", "observations", "dominance")

# Which proportions of myMethods to explore
prop = c(0.25, 0.5, 0.75, 1.0)  # one or more numbers between 0 and 1

# Define the number of random draws for each method
nDraws <- 1e2

# Proportion o the observations over which the moving window will advance
winMove <- 0.20

resultsDir <- "./chapterFiles/resampling/results/"
distDir <- paste0(resultsDir, "distances/")
ewsDir <- paste0(resultsDir, "ews/")
dir.create(resultsDir)
dir.create(distDir)
dir.create(ewsDir)


# Conduct reasmpling analysis --------------------------------------------------------
# This will run and save results to feathers. if you want specific results only, specify ews or fivi = t/f
resamplingAnalysis(
  prop,
  myMethods,
  nDraws,
  winMove,
  ews = FALSE,
  fivi = TRUE,
  fi.method = "7.12" #7.12 is the derivatives method
)


# Plot resampling results -------------------------------------------------
