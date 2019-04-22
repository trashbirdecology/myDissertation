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
prop <- c(25, 50, 75)

results <- NULL
getResultsSummary <- function(dir.ind, methods, prop){
  
  for(i in seq_along(prop)){
    for(j in seq_along(methods)){
      results.temp <- NULL
      my.ind <- paste0("prop", prop[i], "_", methods[j])
      
      x = purrr::map_df(list.files(dir.ind, full.names=TRUE, pattern = my.ind), read_feather)
      
      if(prop[i]!=100){
      results <-  x %>%
        group_by(method, prob, time, winMove) %>%
        summarise(
          ds.mean      =  mean(ds, na.rm = TRUE),
          s.mean       =  mean(s, na.rm = TRUE),
          dsdt.mean    =  mean(dsdt, na.rm = TRUE),
          d2sdt2.mean  =  mean(d2sdt2, na.rm = TRUE),
          ds.sd      =  sd(ds, na.rm = TRUE),
          s.sd       =  sd(s, na.rm = TRUE),
          dsdt.sd    =  sd(dsdt, na.rm = TRUE),
          d2sdt2.sd  =  sd(d2sdt2, na.rm = TRUE)
        ) 
      } # end ifelse prop!=100
      
      # If prop == 100% then we don't need means and SD, we just need orginal data. 
      if(prop[i]==100){ 
        results.temp<- x %>%
          group_by(method, prob, time, winMove) %>%
          rename(
            ds.mean      =  ds,
            s.mean       =  s,
            dsdt.mean    =  dsdt,
            d2sdt2.mean  =  dsdt) 
      }  # end ifelse prop==100
      
      
      # Bind the current results to the previous results
      
      results <- full_join(results, results.temp)
      
    } # end methods j loop
  } # end prop i loop
  
  return(results)
} # end function getResultsSummary()





# origData <- purrr::map_df(list.files(origDataDir, full.names = TRUE), read_feather)
distResults <- purrr::map_df(list.files(distDir[1:5], full.names = TRUE), read_feather)
ewsResults <- purrr::map_df(list.files(ewsDir, full.names = TRUE), read_feather)
fiviResults <- purrr::map_df(list.files(fiviDir, full.names = TRUE), read_feather)
View(ewsResults)

