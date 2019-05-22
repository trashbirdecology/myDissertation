rm(list=ls())
# Re-install from soruce periodically as this is currently under development. 
# devtools::install_github("trashbirdecology/bbsRDM")

# Setup -------------------------------------------------------------------
## Where do we want or have stored the bbs_raw_data (raw bbs state-levle data OR feathers...)
bbsDir <- here::here("chapterFiles/discontinuityAnalysis/bbs_raw_data/")


# If the bbs data already exists inside bbsDir, then we will create a logical to NOT download it (see below)
if(length(list.files(bbsDir, pattern = "*.feather")) > 0 ){
  downloadBBSData = FALSE
}else(
  {dir.create(bbsDir)
    downloadBBSData = TRUE}
)

# Load libraries --------------------------------------------------------------------
library(tidyverse)
library(bbsRDM)
library(regimeDetectionMeasures)

# Create a sampling grid --------------------------------------------------
# Next we build the sampling grid to force route information onto a regular grid.
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

# Import the BBS species list for filtering BY AOU codes --------------------------------------------
sppListBBS <- bbsRDM::GetSpNames()
head(sppListBBS)

# Identify species of interest ------------------------------------------
## This will be used to ensure later track the species of interest after the discontinuity analyses. 
grassSpecies <- data.frame(commonName = c(
  "Bobolink",
  "Chestnut-collared Longspur",
  "Chipping Sparrow",
  "Dickcissel",
  "Eastern Meadowlark",
  "Field Sparrow",
  "Grasshopper Sparrow",
  "Henslow's Sparrow",
  "Lark Bunting",
  "Lark Sparrow",
  "Le Conte's Sparrow", # not any data for NE KS
  "McCown's Longspur",  # not any data for NE KS; not much data at all, actually..
  "Savannah Sparrow",   # bad data for NE KS
  "Sprague's Pipit",    # not any data for NE KS
  "Western Meadowlark",
  "Upland Sandpiper", # removed because it is a shorebird
  "Vesper Sparrow",
  "Yellow-headed Blackbird"
)
)

if(nrow(left_join(grassSpecies ,sppListBBS %>%  dplyr::select(aou, commonName))==nrow(grassSpecies))){
  grassSpecies <- left_join(grassSpecies ,sppListBBS %>%  dplyr::select(aou, commonName))
}else(stop("All species in grassSpecies are not identifed in sppListBBS. Check common names align in grassSpecies list and sppListBBS"))



# DOWNLOAD the BBS data for selected areas and SAVE TO FILE  ------------------------------------
### NOTE: this section DOES NOT pull the BBS data into memory. See next section ###
## Select the regions (states) we want to download
bbsRegions <- GetRegions() %>% 
  filter(stateName %in% c("KANSAS"))

regionFileName <- bbsRegions$zipFileName %>% na.omit()

## Get the bbs routes
bbsRoutes <- getRouteInfo()

## Two state files should onlu take a couple of minutes.
if(downloadBBSData==TRUE){
  for(i in 1:length(regionFileName)){
    message("Downloading BBS data. If more than one or two states worth of data is required, you may want to talk a walk...")
    bbsData <-  importDataBBS(
      # arguments for getDataBBS()
      file = regionFileName[i],
      dir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/",
      year = NULL,
      aou = c(),
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
    # Make sure the directory is correcetly defined... 
    
    if(!( endsWith(bbsDir, "//") |  endsWith(bbsDir, "\\"))){bbsDir <- paste0(bbsDir,"/")}


    # Save the unzipped files to disk.
    birdsToFeathers(dataIn  = bbsData,
                    newDir  = bbsDir,
                    filename = regionFileName[i])
    # Clear object from memory
    rm(bbsData)
  } # end section I. loop
}else(message(paste0("NOT DOWNLOADING BBS DATA. If you wish to download the BBS data, please remove files from directory: ",bbsDir))) # end if-else to download the data

# Next, LOAD  the BBS data and align with the sampling grid -------------------------------------------------------
# Now we load in the BBS data from the feathers we created and align with the sampling grid. This requires a bit of memory, proceed with caution.

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


# Filter the BBS data -----------------------------------------------------
## Subset the species by  REMOVING H20FOW and SHOREBIRDS
order.keep = c(
  "Passeri", 
  "Grui", 
  'Apodi', 
  'Pici',
  'Cuculi', 
  'Coracii', 
  'Accipitri',
  'Columbi', 
  'Galli',
  'Catharti', 
  'Charadrii') %>% paste0("formes")

# get aou codes for those species in the orders
aou.keep <- sppListBBS %>%
  filter(order %in% order.keep) %>% 
  distinct(aou)

# SUBSET THE DATA BY AOU 
bbsData <- feathers %>%
  filter(aou %in% aou.keep$aou)



## Remove species which do not occur at least 3 times in the entire time series for each route
bbsData <- feathers %>% group_by(aou,countrynum,statenum,route) %>% 
  mutate(nYrsPres = n_distinct(year)) %>% 
  filter(nYrsPres >= 10) %>% 
  ungroup() 
rm(feathers)



# Identify species OF INTEREST --------------------------------------------



# grassSpecies.aou <- AOU_species_codes %>% 
#   filter(name %in% grassSpecies) %>% 
#   dplyr::select(spp.num, name)
# 
# 
# if(nrow(grassSpecies.aou) != length(grassSpecies)) {
#   warning(paste0(
#     "You're missing ",
#     abs(nrow(grassSpecies.aou) - length(grassSpecies)),
#     "  of your species:  ",
#     setdiff(grassSpecies, grassSpecies.aou$name)
#   ))
# } else{
#   (message("Good work: all species accounted for!"))
# }
# 
# ## make sure all the aous align 
# setdiff(grassSpecies.aou$spp.num, bbsData$aou)
# 
# ### If any come back, mkae sure this makes sense to you. E.g., visit <https://www.mbr-pwrc.usgs.gov/bbs/specl15.shtml> and see if there are any state-listed results , or regional results depending on scale of exercise, for this species. 
# 
# ## I am not sure why Upland Sandpiper isn't showing up, however.......
#     ### ignoring this for now....


# Incorporate IUCN listings into the datasets  --------------------------------------------------------
## retrieve the IUCN listings that were downloaded in May 2019
iucn <- read_csv(here::here("chapterFiles/discontinuityAnalysis/redlistData/assessments.csv")) %>% 
  rename(spp = scientificName) 

## merge iucn status with scientific name of the AOU species codes
iucn.aou <- left_join(AOU_species_codes, iucn %>% dplyr::select(spp,redlistCategory))  %>% 
  mutate(aou =spp.num)

temp <- left_join(bbsData,iucn.aou)


