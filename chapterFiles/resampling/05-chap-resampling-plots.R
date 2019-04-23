# About -------------------------------------------------------------------
# this is the script for analysing data for the rsampling chapter, which  explores the impact of data quality and information on various RDMs
# Setup -------------------------------------------------------------------

rm(list=ls())

## Libs
library(tidyverse)
library(feather)

# Source functions
source(paste0(here::here(),"/chapterFiles/resampling/05-chap-resampling-myFunctions.R"))

# What data are we plotting? ----------------------------------------------

# Which dataset to use
diatoms <- TRUE # do you want to analyze the Spanbuaer data? if not, program will create dummy data 

# Create directories to store results, figures, load data etc.
dirs <- createDirs(dirNameInd=ifelse(diatoms==TRUE, "diatoms","dummy")) # creates dirs if null and IDs
dataDir <- dirs$summaryResultsDir

# Create Plots ------------------------------------------------------------

my.ind <- "*distances"

list.files(dataDir)

results.temp = purrr::map_df(list.files(dataDir, full.names=TRUE, pattern = my.ind), read_feather)


