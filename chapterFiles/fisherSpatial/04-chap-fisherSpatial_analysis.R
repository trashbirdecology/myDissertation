## Analysis for chapter on using Fisher Information to identify spatial regimes

# Libraries, steup --------------------------------------------------------
## Re-install often as this package is under major development.
devtools::install_github("trashbirdecology/regimedetectionmeasures", force = FALSE)

library(regimeDetectionMeasures)
library(sp)
library(raster)
library(feather)
library(bbsRDM)
library(here)
library(tidyverse)

# Define figure out directory ---------------------------------------------
figDir <- "./chapterFiles/fisherSpatial/figures"
animDir <- "./chapterFiles/fisherSpatial/figures/animations"

## Analysis for chapter on binning
# This should be run only if you need to re-calculate metrics, or get new metrics. 

# Define figure out directory ---------------------------------------------

# Create directories ------------------------------------------------------
# a. Create a directory to store and/or load the BBS data as feathers
bbsDir <- "./chapterFiles/fisherSpatial/bbs_raw_data/"
dir.create(bbsDir) # if already


# If the bbs data already exists inside bbsDir, then we will create a logical to NOT download it (see below)
if (length(list.files(bbsDir, pattern = "*.feather")) > 0) {
  downloadBBSData = FALSE
}else(downloadBBSData = TRUE)

# If this returns a warning, proceed with caution as directory already exists, and results WILL be OVERRIDDEN.

# b. Create a directory to store and/or load the BBS data as feathers
resultsDir <- "./chapterFiles/fisherSpatial/myResults"
dir.create(paste0(resultsDir))

# c. Create directory for storing early warning signal results
dir.create(paste0(resultsDir, "/ews"))

# d. Create directory for storing distance travelled results
dir.create(paste0(resultsDir, "/distances"))


# e. Create dir to store rObjects
rObjs <- "./chapterFiles/fisherSpatial/rObjs"
dir.create(rObjs)

# Get the BBS data --------------------------------------------------------
# a. Load the regional .txt file from Patuxent's FTP server (you must be connected to the internet to perform this step)
regions <- bbsRDM::GetRegions()

# b. Create a series or one filenames for states, regions
regionFileName <- regions$zipFileName %>% na.omit()

# c.  Download and unzip the BBS data.
if (downloadBBSData == TRUE) {
  for (i in 1:length(regionFileName)) {
    bbsData <-  importDataBBS(
      # arguments for getDataBBS()
      file = regionFileName[i],
      dir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/",
      year = NULL,
      aou = NULL,
      countrynum = NULL,
      states = NULL,
      #  arguments for getRouteInfo():
      routesFile = "routes.zip",
      routesDir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/",
      RouteTypeID = 1,
      # one or more of c(1,2,3)
      Stratum = NULL,
      BCR = NULL
    )
    
    
    # d. Save the unzipped files to disk.
    birdsToFeathers(dataIn  = bbsData,
                    newDir  = bbsDir,
                    filename = regionFileName[i])
    # e. Clear object from memory
    rm(bbsData)
  } # end section I. loop
}else(
    paste0(
      "NOT DOWNLOADING BBS DATA. If you wish to download the BBS data, please remove files from directory: ", bbsDir
    )
  ) # end if-else to download the data


# Sampling grid, route points, mil bases, plots ---------------------------
# Build sampling grid
# Define the grid's cell size (lat, long; unit:degrees)
## 1 deg latitude ~= 69 miles
## 1 deg longitude ~= 55 miles
cs <-
  c(0.5, 0.5)  # default is cell size 0.5 deg lat x 0.5 deg long

# Create and save the sampling grid. 
routes_gridList <- bbsRDM::createSamplingGrid(cs = cs)
saveRDS(routes_gridList, file = paste0(rObjs, "/samplingGrid.RDS"))

# Define the components of the sampling grid as individual objects
routes_grid <- routes_gridList$routes_grid 
sp_grd <- routes_gridList$sp_grd

# Load the bbs data and overlay onto the grid
feathers <- NULL
featherNames <- list.files(bbsDir, pattern = ".feather")
featherNames <- str_c("/", featherNames) #add separator
for (i in 1:length(featherNames)) {
  feather <- NULL
  feather <- bbsRDM::loadBirdFeathers(newDir  = bbsDir,
                              filename = featherNames[i])
  
  feather <- feather %>%
    dplyr::rename(lat = latitude,
                  long = longitude) %>%
    left_join(routes_grid ,
              by = c("countrynum", "statenum", "route", "lat", "long"))
  
  feathers <- rbind(feathers, feather)
  rm(feather)
  
}

# Get USA states layer
us_states <- map_data("state")

# get military bases
milBases <- bbsRDM::getMilBases(shploc = "http://www.acq.osd.mil/eie/Downloads/DISDI/installations_ranges.zip", 
                                shpfile = "MIRTA_Points")

# Get a df of just   the BBS route locations
routePts <- routes_grid %>%
  distinct(lat, long)


# Subset the data by AOU codes --------------------------------------------
feathers <-
  bbsRDM::subsetByAOU(myData = feathers, subset.by = 'remove.shoreWaderFowl')

########################### BEGIN USER-DEFINED PARAMTERS###########################
# Define metric calculation parameters ------------------------------------

# First, define the parameters required to calcualte the metrics.
# Which metrics do you want to calculate?
metrics.to.calc <- c("distances", "ews") # these are currently the only options, so it will calculate both sets

# If calculating "EWSs, you can calculate select metrics.
## Default = all early-warning signals, FI, and VI
to.calc = c("EWS", "FI", "VI")

# Choose spatial or temporal analysis
direction <-
  "East-West" # choose one of : 'South-North', 'East-West', or 'temporal'

# Choose the fill value for species present in the entire time series (or sampling transect) but not present on that year. Using "NA" assumes the species with missing data *was not and could not have been present*. Using zero assumes the species could have been present but was not.
fill = 0

# Minimum number of sites (if spatial) or years (if temporal)  required to be in the entire sample (trnasect or time series)
min.samp.sites = 8

# Minimum number of sites (if spatial) or years (if temporal) required to be within a single window
min.window.dat = 5

# Which Equation of Fisher Information to use (default = 7.12)
fi.equation = "7.12"

# By what % of the entire data should the window move?
winMove = 0.25

# Define some filtering and labeling parameters based on direction of spatial analysis (if applicable)
if (direction == "South-North") {
  dir.use =  unique(feathers$colID) %>% na.omit(colID) %>% sort()
}
if (direction == "East-West") {
  dir.use = unique(feathers$rowID) %>% na.omit(rowID) %>% sort()
}

# Get all possible years
years.use = unique(feathers$year)

# Keep only the years which are divisible by T
t.years = 10
years.use  <-
  years.use[which(years.use %% t.years == 0 & years.use > 1975)] %>% sort()

################## END USER-DEFINED PARAMETERS ###########################

########################### CONDUCT ANALYSIS OF RDMs ########################### 
# CALCULATE THE METRICS  -------------------------------------------------------
for (j in 1:length(dir.use)) {
  # For east-west analysis
  if (direction == "East-West"){
    birdsData <- feathers %>%
      filter(rowID == dir.use[j]) %>%
      mutate(direction = direction,
             dirID = dir.use[j])
  }
  # For south-north analysis
  if (direction == "South-North"){
    birdsData <- feathers %>%
      filter(colID == dir.use[j]) %>%
      mutate(direction = direction,
             dirID = dir.use[j])
  }

  if (nrow(birdsData) < min.samp.sites) {
    next(print(paste0("Not enough data to analyze. Skipping j-loop ", dir.use[j])))
  }

  for (i in 1:length(years.use)){
    # a. Subset the data according to year, colID, rowID, state, country, etc.x
    birdData <- birdsData %>%
      filter(year == years.use[i]) %>%
      dplyr::rename(variable = aou,
                    value = stoptotal)

    if (nrow(birdData) == 0){
      next
    }

    # b. Munge the data further
    birdData <- mungeSubsetData(birdData)


    ## This function analyzes the data and writes results to file (in subdirectory 'myResults') as .feather files.
    # browser()
    calculateMetrics(dataIn = birdData, metrics.to.calc, direction = direction,  yearInd = years.use[i])

    print(paste0("End i-loop (years) ", i, " of ",  length(years.use)))

  } # end i-loop

  print(paste0("End j-loop (transects) ", j, " of ",  length(dir.use)))
} # end j-loop


########################### END ANALYSIS OF RDMs ###########################