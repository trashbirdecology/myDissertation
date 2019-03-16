################## IMPORT & MUNGE RDM RESULTS ##################
# Import the calculated metrics (results) & merge with sampling grid --------------------------------------------
# a. Import EWSs
if (to.plot == "ews") {
  print("importing EWSs (e.g., FI, VI, CV)")
  ## FYI: varibles will likely be missing (NA) for metricTypes FI and VI, because these are calculated across ALL variables at a time...
  results <-
    importResults(resultsDir = resultsDir,
                          myPattern = "ews",
                          subset.by = as.character(direction), 
                          metrics.keep = metrics.keep.ind) %>%
    # assign the end of the window as the cellID
    mutate(cellID = cellID_max)
  
  # join with spatial grid
  results <- full_join(sampGridCoords,
                       results) %>%
    na.omit(metricType) %>%
    dplyr::select(-cellID_min, -cellID_max, -winStart  , -winStop)
      ## PLEASE NOTE:
      ### a full join of the sampling grid and the results for EWS will likely produce many cells with NO results data..
      ### however, NO lat or long should == NA!
}

# b. Import distance results
if (to.plot == "distances") {
  print("importing distances")
  results <-
    importResults(resultsDir = resultsDir,
                          myPattern = 'distances',
                          subset.by = as.character(direction))
  
  # join with spatial grid
  results <- full_join(sampGridCoords,
                       results) %>%
    na.omit(metricType)
}

## Set coordinate system and projection for both data sets! (the same)
coordinates(results) <- c("long", "lat")
sp::proj4string(results) <- sp::CRS("+proj=longlat +datum=WGS84")
################## END RUN       ##################




