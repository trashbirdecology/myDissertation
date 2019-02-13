## Analysis for chapter on binning 
# Libraries, steup --------------------------------------------------------
## Re-install often as this package is under major development.
devtools::install_github("trashbirdecology/regimedetectionmeasures", force = F)

library(regimeDetectionMeasures)
library(sp)
library(raster)
library(feather)
library(bbsRDM)
library(here)

# Define figure out directory ---------------------------------------------
figDir <- paste0(here::here(), "/chapterFiles/binningChap/figures")


# Create directories ------------------------------------------------------

# a. Create a directory to store and/or load the BBS data as feathers
bbsDir <- paste0(here::here(),
                 "/chapterFiles/binningChap/bbs_raw_data")
dir.create(bbsDir) # if already


# If the bbs data already exists inside bbsDir, then we will create a logical to NOT download it (see below)
if(length(list.files(bbsDir, pattern = "*.feather")) > 0 ){
  downloadBBSData = FALSE
}else(
  downloadBBSData = TRUE
)

# If this returns a warning, proceed with caution as directory already exists, and results WILL be OVERRIDDEN.

# b. Create a directory to store and/or load the BBS data as feathers
resultsDir<-paste0(here::here(),
                   "/chapterFiles/binningChap/myResults")
dir.create(paste0(resultsDir))

# c. Create directory for storing early warning signal results
dir.create(paste0(resultsDir, "/ews"))

# d. Create directory for storing distance travelled results
dir.create(paste0(resultsDir, "/distances"))


# Get the BBS data --------------------------------------------------------



# a. Load the regional .txt file from Patuxent's FTP server (you must be connected to the internet to perform this step)
regions <- GetRegions()

# b. Create a series or one filenames for states, regions
regionFileName <- regions$zipFileName %>% na.omit()

# c.  Download and unzip the BBS data.
if(downloadBBSData==TRUE){
  for(i in 1:length(regionFileName)){
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
}else(message(paste0("NOT DOWNLOADING BBS DATA. If you wish to download the BBS data, please remove files from directory: ",bbsDir))) # end if-else to download the data


# Sampling grid, route points, mil bases, plots ---------------------------

# Build sampling grid 
# Define the grid's cell size (lat, long; unit:degrees)
## 1 deg latitude ~= 69 miles
## 1 deg longitude ~= 55 miles
cs <-
  c(0.5, 0.5)  # default is cell size 0.5 deg lat x 0.5 deg long

# Create the grid
routes_gridList <- createSamplingGrid(cs = cs)

# Define the components of the sampling grid as individual objects
routes_grid <- routes_gridList$routes_grid
sp_grd <- routes_gridList$sp_grd
rm(cs)


# Load the bbs data and overlay onto the grid
feathers <- NULL
featherNames <- list.files(bbsDir, pattern = ".feather")
featherNames <- str_c("/", featherNames) #add separator
for (i in 1:length(featherNames)) {
  feather <- NULL
  feather <- loadBirdFeathers(newDir  = bbsDir,
                              filename = featherNames[i]) 
  
  feather <- feather %>%
    dplyr::rename(lat = latitude,
                  long = longitude) %>%
    left_join(routes_grid, by = c("countrynum", "statenum", "route", "lat", "long"))
  
  feathers <- rbind(feathers, feather)
  rm(feather)
  
}

#get state layer
library(maps)
us_states <- map_data("state")
# get military bases
milBases <- getMilBases()

## Plot the routes and military bases
routePts <- routes_grid %>% 
  distinct(lat, long)

plot_routes <- ggplot()  + 
  geom_polygon(data = us_states, 
               mapping = aes(x = long, y = lat,
                             group = group), 
               fill = "white", color = "black")+
  xlim(min(routePts$long), max(routePts$long))+
  ylim(min(routePts$lat), max(routePts$lat))+
  geom_point(data = routePts, aes(x=long,y=lat, color = "bbsRoutes"), size = 0.3, 
             show.legend=TRUE)+
  scale_colour_manual(name="Point Color",
                      values=c(bbsRoutes="black"))#, myline2="red"))
plot_routes # plot of all bbs routes, simple map



plot_routes_milbases <- plot_routes + 
geom_point(data = coordinates(milBases) %>% as.data.frame(), aes(x=milBases@coords[,1],y=milBases@coords[,2], color = "milBase"), size = 0.5, 
show.legend=TRUE)+
xlab ("longitude")+ylab("latitude")+
scale_colour_manual(name="Point Color",
values=c(bbsRoutes="black", milBase  = "red"))+
xlim(-125, -69)

## Save the figures to file
ggsave(filename = paste0(figDir, "plot_routes.png"), plot_routes)
ggsave(filename = paste0(figDir, "plot_routes_milbases.png"), plot_routes_milbases)


# Subset the data by AOU codes --------------------------------------------

feathers <- subsetByAOU(myData = feathers, subset.by= 'remove.shoreWaderFowl')




# CALCULATE METRICS! -------------------------------------------------------
## Define analysis paramters 
# First, define the parameters required to calcualte the metrics.
# Which metrics do you want to calculate?
metrics.to.calc <- c("distances", "ews")

# If calculating "EWSs, you can calculate select metrics. 
## Default = all early-warning signals, FI, and VI
to.calc = c("EWS", "FI", "VI")

# Choose spatial or temporal analysis
direction <-
  "South-North" # choose one of : 'South-North', 'East-West', or 'temporal'

# Choose the fill value for species present in the entire time series (or sampling transect) but not present on that year. Using "NA" assumes the species with missing data *was not and could not have been present*. Using zero assumes the species could have been present but was not.
fill = 0

# Minimum number of sites (if spatial) or years (if temporal)  required to be in the entire sample (trnasect or time series)
min.samp.sites = 8

# Minimum number of sites (if spatial) or years (if temporal) required to be within a single window 
min.window.dat = 3

# Which Equation of Fisher Information to use (default = 7.12)
fi.equation = "7.12"

# By what % of the entire data should the window move? 
winMove = 0.25

direction = "East-West"

# Define some filtering and labeling parameters based on direction of spatial analysis (if applicable)
if (direction == "South-North") {
  dir.use =  unique(feathers$colID) %>% na.omit(colID) %>% sort()}
if (direction == "East-West") {
  dir.use = unique(feathers$rowID) %>% na.omit(rowID) %>% sort()}




# Get all possible years
years.use = unique(feathers$year)

# Keep only the years which are divisible by T
T = 10
years.use  <- years.use[which(years.use %% T == 0 & years.use > 1975)] %>% sort()




## Calculate Metrics
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
  
  
  # VX.  Analyze the data ---------------------------------------------------
  
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
    
    
    # X.   Calculate the metrics ---------------------------------------------------
    ## This function analyzes the data and writes results to file (in subdirectory 'myResults') as .feather files.
    # browser()
    calculateMetrics(dataIn = birdData, metrics.to.calc, direction = direction,  yearInd = years.use[i])
    
    print(paste0("End i-loop (years) ", i, " of ",  length(years.use)))
    
  } # end i-loop
  
  print(paste0("End j-loop (transects) ", j, " of ",  length(dir.use)))
} # end j-loop









# Load results ------------------------------------------------------------


