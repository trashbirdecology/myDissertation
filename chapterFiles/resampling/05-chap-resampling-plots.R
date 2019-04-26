# About -------------------------------------------------------------------
# this is the script for analysing data for the rsampling chapter, which  explores the impact of data quality and information on various RDMs
# Setup -------------------------------------------------------------------

rm(list=ls())

## Libs
library(tidyverse)
library(feather)
library(here)

# Source functions
source(paste0(here::here(),"/chapterFiles/resampling/05-chap-resampling-myFunctions.R"))
source(paste0(here::here(),"/chapterFiles/resampling/05-chap-resampling-myPlottingFunctions.R"))

# What data are we plotting? ----------------------------------------------

# Which dataset to use
diatoms <- TRUE # do you want to analyze the Spanbuaer data? if not, program will create dummy data 

# Create directories to store results, figures, load data etc.
dirs <- createDirs(dirNameInd=ifelse(diatoms==TRUE, "diatoms","dummy")) # creates dirs if null and IDs
dataDir <- dirs$summaryResultsDir
figDir <- dirs$figDir


# Load summarizes results ------------------------------------------------------------
if(exists("results")) stop("If you need to re-load feathers, please run entire script again (including rm(list=ls()) and restart session.") else(results <- loadSummaryResults(dataDir))


# Create Plots ------------------------------------------------------------

# Which figures to create
metrics.to.plot <- "distance"  # names(results)
myLabels <- setLabels(metrics.to.plot, results) 

for(i in seq_along(metrics.to.plot)){
## Boostrapped plots of single metric, facet by prob, single method
  
  myDf <- results[[i]]
    
  for(j in seq_along){
    myMetric <- myLabels[[i]][j]
    
    
    x <- if(metrics.to.plot[i])
plot.bootstrappedFacetGroup(
  df = myDf,
  metric.ind = myMetric,
  method.filter = "observations",
  facet.var = "prob",
  x = "time",
  preview = TRUE
)



  }
}
  


# my.ind <- "*ews"
# my.ind <- "*fivi

