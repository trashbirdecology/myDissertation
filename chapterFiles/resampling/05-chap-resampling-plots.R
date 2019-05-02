# About -------------------------------------------------------------------
# this is the script for PLOTTING the summarised results for the resampling chapter, 
# which explores the impact of data quality and information on various RDMs
# Setup -------------------------------------------------------------------

# rm(list=ls())

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
# figDir <- dirs$figDir
figDir <- dirs$tempFigDir


# Load summarizes results ------------------------------------------------------------
if(exists("results")) stop("If you need to re-load feathers, please run entire script again (including rm(list=ls()) and restart session.") else(results <- loadSummaryResults(dataDir))


# Create Plots ------------------------------------------------------------

# Skip EWS (species-level) results for now...
results <- list(distances = results$distances, fivi = results$fivi)
# Identify the unique  the labels/metrics to plot 
myLabels <- setLabels(names(results), results)
if(any(names(results) != names(myLabels))) stop("names of results must match myLabels names")

## Boostrapped plots of single metric, facet by prob, single method
for(i in seq_along(results)){
    # subset data by analysis type and metric
    myDf.all  <- results[[i]]  # e.g., distance, FIVI, ews
    
      myMetrics <- myLabels[[i]] #e.g, dsdt, FI, VI
      myMethods <- unique(myDf.all$method) # e.g, dominance, species
      
      for (h in seq_along(myMethods)) {
          for(j in seq_along(myMetrics) ){
        
      # subset data by method
        myDf <- myDf.all %>%
          filter(method ==  myMethods[h])


        if(nrow(myDf)==0)stop("data frame is empty -- this should not be")
        
        
##############################    
plot.bootstrappedFacetGroup(
  df = myDf.all,
  metric.ind = myMetrics[j],
  method.filter =  myMethods[h],
  preview = TRUE, 
  add.baseline=TRUE, 
  savePlot = TRUE, 
  regime.breaks = list(
    c(-6894,-4800),
    c(-2500,-2000), 
    c(-2000, -1600),
    c(-1300, -1000)
    )
)
##############################    
        

# Plot density of error to mean ratio
##############################  

if( h > 1 )next() # only need to plot this one time per metric
plot.densityMeanSdRatio(data = myDf.all,
                        mymetric = myMetrics[i], 
                        figDir = figDir)
      
##############################      


      } # end j-loop (myMetrics)
      } # end h-loop (myMethods)
    }  # end i-loop (individual metrics)



