# About -------------------------------------------------------------------
# this is the script for analysing data for the rsampling chapter, which  explores the impact of data quality and information on various RDMs
# Setup -------------------------------------------------------------------

rm(list=ls())

## Libs
library(tidyverse)
library(feather)

# Source functions
source("./chapterFiles/resampling/05-chap-resampling-myFunctions.R")


# What data are we plotting? ----------------------------------------------

# Which dataset to use
diatoms <- TRUE # do you want to analyze the Spanbuaer data? if not, program will create dummy data 

# Create directories to store results, figures, load data etc.
dirs <- createDirs(dirNameInd=ifelse(diatoms==TRUE, "diatoms","dummy")) # creates dirs if null and IDs
# Return the directotries as objects
for(i in seq_along(dirs)){assign(names(dirs)[i], dirs[[i]])}


# Load resampling results -------------------------------------------------

methods <- c("observations", "species")
prop <- c(25, 50, 75,100)


# Create Plots ------------------------------------------------------------



