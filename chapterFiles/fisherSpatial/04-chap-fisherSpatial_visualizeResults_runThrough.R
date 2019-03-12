########################## I. USER DEFINITIONS ########################## 
## Please pay special attention to this section. 
## Within this section (I.) you will define various parameters for calculating metrics and visualizing results.

# In which direction to visualize metrics?
# direction = "South-North"
direction = "East-West"

# Which indices do we want to plot righ tnow?
# choose one of "distances", "ews"
to.plot <- 
  "distances"

# Which metrics to calculate
if(to.plot == "distances") metric.ind <- c("dsdt", "s") # the metrics to print
if(to.plot == "ews") metric.ind <- c("FI", "VI") # the metrics to print

# This is the row and column # that will be printed as "examples" of my transect grid sampling design. 
rowEx.ind <- 21 # Feel free to change this.

########################## END USER DEFS - DO NOT EDIT BELOW HERE ############
########################## II. SOURCE FILES ##########################

# a. Source helper functions
source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_helperFunctions.R")
# b. Create and write base maps to file
 ## this file will create quite a few base maps which are used in later plotting AND are called in the dissertation as examples of the sampling design.
if(!exists("usBaseMap")) source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_baseMaps.R")
  ## using the if statement avoids us sourcing this if it has already been done

# c. Import the results
source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_importMetricResults.R")

# d. Plot the distances
# source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_plotting_distances.R")
