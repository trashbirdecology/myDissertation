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

# Import the BBS species list for filtering BY AOU codes --------------------------------------------
sppListBBS <- bbsRDM::GetSpNames()

# Identify species of interest ------------------------------------------
## This will be used to ensure later track the species of interest after the discontinuity analyses. 

## These are grassland obligates
grassSpecies <- data.frame(commonName = c(
  "Baird's Sparrow",
  "Bobolink",
  "Cassin's Sparrow",
  "Chestnut-collared Longspur",
  "Chipping Sparrow",
  "Dickcissel",
  "Eastern Meadowlark",
  "Ferruginous Hawk",
  "Field Sparrow",
  "Grasshopper Sparrow",
  "Henslow's Sparrow",
  "Horned Lark",
  "Lark Bunting",
  "Lark Sparrow",
  "LeConte's Sparrow", # 
  "McCown's Longspur",  # not much data at all, actually..
  "Mountain Plover", 
  "Ring-necked Pheasant",
  "Savannah Sparrow",   # 
  "Sedge Wren",
  "Sprague's Pipit",    # 
  "Western Meadowlark",
  "Upland Sandpiper", # 
  "Vesper Sparrow",
  "Yellow-headed Blakcbird"
)
)

if(nrow(left_join(grassSpecies ,sppListBBS %>%  dplyr::select(aou, commonName))==nrow(grassSpecies))){
  grassSpecies <- left_join(grassSpecies , sppListBBS %>%  dplyr::select(aou, commonName))
}else(stop("All species in grassSpecies are not identifed in sppListBBS. Check common names align in grassSpecies list and sppListBBS"))


## these speices are those with declining trends in BBS years 1966-2015 AND have high (blue) Bbs regional credibility for CENTRALBBS REGION!!! 
decliningSpecies <-data.frame(commonName = c(
  ############ SPECIES DECLINLING AND BLUE IN KANSAS below
  # "American Kestrel",
  # "American Crow",
  # "Brown-headed Cowbird",
  # "Barn Swallow",
  # "Brown Thrasher",
  # "Black-capped Chickadee",
  # "Common Grackle",
  # "Chimney Swift",
  # "Common Yellowthroat",
  # "Eastern Meadowlark",
  # "Eastern Kingbird",
  # "Grasshopper Sparrow",
  # "Loggerhead Shrike",
  # "Northern Bobwhite",
  # "Northern Rough-winged Swallow",
  # "House Sparrow",
  # "Nothern Flicker",
  # "Red-headed Woodpecker",
  # "Red-winged Blackbird",
  # "Swainson's Hawk",
  # "Western Kingbird",
  # "Western Meadowlark",
  # "Upland Sandpiper",
  ########### SPECIES DECLINING and (credibility==BLUE AND YELLOW) IN CENTRAL BBS REGION + BCRs 11 , 22
  "Northern Pintail",
  "Little Blue Heron",
  "King Rail",
  "Marbled Godwit",
  "Killdeer",
  "Mountain Plover",
  "Bobwhite Quail",
  "Rock Pigeon",
  "Mourning Dove",
  "Yellow-billed Cuckoo",
  "Downy Woodpecker",
  "Red-headed Woodpecker",
  "Golden-fronted Woodpecker",
  "Red-bellied Woodpecker",
  "Chuck-wills-widow",
  "Common Nighthawk",
  "Chimney Swift", 
  "Eastern Kingbird",
  "Scissor-tailed Flycatcher",
  "Great-crested Flycatcher",
  "Horned Lark",
  "Black-billed Magpie",
  "American Crow",
  "Blue Jay",
  "European Starling",
  "Red-winged Blackbird",
  "Western Meadowlark",
  "Orchard Oriole",
  "Bullock's Oriole",
  "Baltimore Oriole",
  "Common Grackle",
  "Brewer's Blackbird",
  "Chestnut-collared Longspur",
  "Baird's Sparrow",
  "Grasshopper Sparrow",
  "Clay-colored Sparrow",
  "Brewer's Sparrow",
  "Field Sparrow",
  "Black-throated Sparrow",
  "Bachmans's Sparrow",
  "Cassin's Sparrow",
  "Rufous-crowned Sparrow",
  "Green-tailed Towhee",
  "Canyon Towhee",
  "Pyrrhuloxia",
  "Barn Swallow",
  "Lark Bunting",
  "Purple Martin",
  "Loggerhead",
  "Prothonotary Warbler",
  "Prairie Warbler",
  "Ovenbird",
  "Kentucky Warbler",
  "Prothonotary Warbler",
  "House Sparrow",
  "Northern Mockingbird",
  "Sprague's Pipit",
  "Brown Thrasher",
  "Bewick's Wren",
  "Curve-billed Thrasher",
  "Cactus Wren",
  "Rock Wren",
  "Brown-headed Nuthatch",
  "Black-capped Chickadee",
  "Carolina Chickadee",
  "Verdin",
  "Wood Thrush",
  "Veery",
  "Northern Flicker",
  ## Eastern tallgrass delcinling yellow and blue (all not in avoce)
  "Upland Sandpiper",
  "Ring-necked Pheasant",
  "Northern Bobwhite",
  "Greater Prairie-chicken",
  "Great-horned Owl",
  "Yellow-billed Cuckoo",
  "Black-billed Cuckoo",
  "Belted Kingfisher",
  "Common Nighthawk",
  "Eastern Whip-poor-will",
  "Chimney Swift",
  "Eastern Kingbird",
  "Eastern Meadowlark",
  "Western Meadowlark",
  "Savannah Sparrow",
  "Vesper Sparrow",
  "Field Sparrow",
  "Song Sparrow",
  "Dickcissel",
  "Common Yellowthroat",
  "Yellow-bellied Sapsucker",
  ## Prairie potholes declinig yellow and blue (all not in aboce)
  "Common Tern",
  "American Wigeon",
  "Northern Pintail",
  "Gray Partridge",
  "Marbled Godwit",
  "Willet",
  "Northern Harrier",
  "American Kestrel",
  "Short-eared Owl",
  "Western Kingbird",
  "European Starling"
)) %>% 
  # remove the duplicates!
  distinct(commonName)


if(nrow(left_join(decliningSpecies ,sppListBBS %>%  dplyr::select(aou, commonName))==nrow(decliningSpecies))){
  decliningSpecies <- left_join(decliningSpecies ,sppListBBS %>%  dplyr::select(aou, commonName))
}else(stop("All species in grassSpecies are not identifed in sppListBBS. Check common names align in grassSpecies list and sppListBBS"))


# DOWNLOAD the BBS data for selected areas and SAVE TO FILE  ------------------------------------
### NOTE: this section DOES NOT pull the BBS data into memory. See next section ###
## Select the regions (states) we want to download
states.of.interest <- c(
  "ARKANSAS",
  "IOWA",
  "KANSAS", 
  "LOUISIANA",
  "MINNESOTA",
  "MISSISSIPPI",
  "MISSOURI",
  "NEBRASKA", 
  "NORTH DAKOTA",
  "OKLAHOMA",
  "SOUTH DAKOTA",
  "TEXAS"
)

bbsRegions <- GetRegions() %>% 
  filter(stateName %in% states.of.interest)

regionFileName <- bbsRegions$zipFileName %>% na.omit()

## Get the bbs routes
bbsRoutes <- getRouteInfo()

## make sure we have enough data for the bounding box (i.e. all lat and long covered..)
temp <- bbsRoutes %>% 
  filter(statenum %in% as.numeric(bbsRegions$regionCode %>% unique())) 
# summary(temp$latitude)
# summary(temp$longitude)

## TWO state files should only take a couple of minutes...
if(downloadBBSData){
  for(i in 1:length(regionFileName)){
    # browser()
    warning("Downloading BBS data. If more than one or two states worth of data is required, you may want to talk a walk...")
    bbsData <-  importDataBBS(
      # arguments for getDataBBS()
      file = regionFileName[i],
      dir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/",
      year = NULL,
      # aou = c(),
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
}else(warning(paste0("!!! NOT DOWNLOADING BBS DATA !!!\n**If you wish to download the BBS data, please remove files from directory: ",bbsDir))) # end if-else to download the data

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
# BY TAXONONMIC DESIGNATION
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
  distinct(aou, .keep_all=TRUE)
## Fix the AOU.keep to match bbs
aou.keep$scientificName[aou.keep$scientificName=="Circus hudsonius"] <- "Circus cyaneus"
aou.keep$scientificName[aou.keep$scientificName=="Haemorhous mexicanus"] <- "Carpodacus mexicanus"

# BY TIMES ROUTE WAS SAMPLED
## Remove species which do not occur at least 3 times in the entire time series for each route
bbsData <- feathers %>% group_by(aou,countrynum,statenum,route) %>% 
  mutate(nYrsPres = n_distinct(year)) %>% 
  filter(nYrsPres >= 10) %>% 
  ungroup() %>% 
  filter(aou %in% aou.keep$aou)%>% 
  ## Translate counts to presence absence for each year-route combo 
  group_by(aou, year, countrynum, statenum, route) %>% 
  filter(stoptotal>0)  %>% 
  ungroup() %>% 
  ## Merge with AOU
  left_join(aou.keep)

# BY LAT AND LONG
bbsData <- 
  bbsData %>% 
  filter(lat >=28 & lat <=49 & long >-97 & long < -93 )

# Merge body masses with the BBS -------------------------------------------------------
mass <- suppressWarnings(read_csv(here::here("chapterFiles/discontinuityAnalysis/bird.mass.dunning4.csv"))) %>% 
  dplyr::select(-X13, -Season, -Location, -`Source #`, -sortID, -common) %>% 
  mutate(spp = stringr::word(spp, 1,2, sep=" ")) %>%
  filter(!is.na(Mean)) %>% 
  rename(scientificName = spp) %>% 
  group_by(scientificName) %>% 
  mutate(log10.mass = log(mean(Mean, na.rm=TRUE))) %>% 
  ungroup() %>%  
  distinct(log10.mass, scientificName)

## Check for missing spp
### Fix some species names in BBS DATA
bbsData$scientificName[bbsData$scientificName=="Circus cyaneus hudsonius"] <- "Circus cyaneus"
bbsData$scientificName[bbsData$scientificName=="Pipilo maculatus / erythrophthalmus"] <- "Pipilo maculatus"
bbsData$scientificName[bbsData$scientificName=="Circus cyaneus hudsonius"] <- "Circus cyaneus"
bbsData$scientificName[bbsData$scientificName=="Circus hudsonius"] <- "Circus cyaneus"
bbsData$scientificName[bbsData$scientificName=="Haemorhous mexicanus"] <- "Carpodacus mexicanus"
bbsData$scientificName[bbsData$scientificName=="Colaptes auratus auratus"] <- "Colaptes auratus"
bbsData$scientificName[bbsData$scientificName=="Colaptes auratus cafer"] <- "Colaptes auratus"
## I am going to go ahead and assign Sturnella magna to Eastern meadowlark since it is more common in this region than W.
bbsData$scientificName[bbsData$scientificName=="Sturnella magna / neglecta"] <- "Colaptes auratus"
bbsData$scientificName[bbsData$scientificName=="Setophaga coronata audoboni"] <- "Setophaga coronata"
### Force all flickers to aou 4123 
bbsData$aou[bbsData$aou %in% c(4130, 4120)] <- 4123
## Force unid towhee to eastern towhee
bbsData$aou[bbsData$aou %in% c(5871)] <- 5870
setdiff(bbsData$scientificName, mass$scientificName)
# Further munge...
bbsData$scientificName[bbsData$scientificName=="Junco hyemalis hyemalis"] <- "Junco hyemalis"

bbsData$scientificName[bbsData$scientificName=="Haemorhous purpureus"] <- "Carpodacus purpureus"
bbsData$scientificName[bbsData$scientificName=="Baeolophus bicolor / atricristatus"] <- "Baeolophus bicolor"
bbsData$scientificName[bbsData$scientificName=="Setophaga coronata coronata"] <- "Setophaga coronata"

bbsData$scientificName[bbsData$scientificName=="Geranoaetus albicaudatus"] <- "Buteo albicaudatus"
bbsData$scientificName[bbsData$scientificName=="Corvus brachyrhynchos / ossifragus"] <- "Corvus brachyrhynchos"
bbsData$scientificName[bbsData$scientificName=="Petrochelidon pyrrhonota / fulva"] <- "Petrochelidon pyrrhonota"
bbsData$scientificName[bbsData$scientificName=="Porphyrio martinicus"] <- "Porphyrio martinica"
bbsData$scientificName[bbsData$scientificName=="Quiscalus major / mexicanus"] <- "Quiscalus major"
bbsData$scientificName[bbsData$scientificName=="Antigone canadensis"] <- "Grus canadensis"
bbsData$scientificName[bbsData$scientificName=="Rallus crepitans"] <- "Rallus longirostris"


setdiff(bbsData$scientificName, mass$scientificName)
# Remove Woodpecker sp.
bbsData <- bbsData %>% 
  filter(scientificName != "Woodpecker sp.")

# Join mass with bbs data
bbsData.forAnalysis<- left_join(bbsData, mass)

# ## Set all the hybrids/UNID to the first classifiations..
# aou.keep$scientificName=gsub(" /.*","", aou.keep$scientificName)
# aou.keep$scientificName=gsub(" x .*","",aou.keep$scientificName)
# aou.keep$scientificName <- 
# ## Fix the names in aou.keep that appear in KS surveys... others, ignore (e.g., S. calliope)
# aou.keep$scientificName[aou.keep$scientificName=="Buteo jamaicensis harlani"] <- "Buteo jamaicensis"
# aou.keep$scientificName[aou.keep$scientificName=="Colaptes auratus auratus"] <- "Colaptes auratus"
# aou.keep$scientificName[aou.keep$scientificName=="Colaptes auratus cafer"] <- "Colaptes auratus"
# bbsData$scientificName[bbsData$scientificName=="Colaptes auratus cafer"] <- "Colaptes auratus"
# bbsData$scientificName[bbsData$scientificName=="Setophaga coronata audoboni"] <- "Setophaga coronata"
# bbsData$scientificName[bbsData$scientificName=="Setophaga coronata coronota"] <- "Setophaga coronata"


## fix because im being lazy 
# fixmass <- function(bbsData.forAnalysis,latin,mass.aou ){
#   df.out<-bbsData.forAnalysis %>%
#     mutate(log10.mass = ifelse(scientificName==latin, mass.aou$log10.mass[mass.aou$scientificName==latin] , log10.mass))
#   return(df.out)
# }
# 
# ## Check for missing body masses and fill in as necessary
# (missingMasses<- bbsData.forAnalysis %>% 
#     filter(is.na(log10.mass)) %>% 
#     dplyr::select(scientificName, commonName, log10.mass) %>% 
#     distinct( scientificName) %>% arrange(scientificName))
# if(nrow(bbsData.forAnalysis %>% 
#         filter(is.na(log10.mass)))!=0) test <- for(i in seq_along(missingMasses$scientificName)){
#           bbsData.forAnalysis<- fixmass(bbsData.forAnalysis, latin=missingMasses$scientificName[i], mass.aou)
#         }else("all masses etc. looks great")


bbsData.forAnalysis <-
  bbsData.forAnalysis %>%
  group_by(countrynum, statenum, route, scientificName) %>%
  # need to add over combined/reclassified sepcies.
  mutate(stoptotal = sum(stoptotal)) %>%
  mutate(
    stoptotal.3year = ifelse(
      year == min(year),
      stoptotal + lead(stoptotal),
      ifelse(
        year == max(year),
        stoptotal + lag(stoptotal),
        stoptotal + lag(stoptotal) + lead(stoptotal)
      )
    ),
    # create a variable for 3.year presence absence
    pa.3year = ifelse(stoptotal.3year > 0, 1, 0)
  ) %>%
  ## replace NA with actual value from that year
  mutate(stoptotal.3year = ifelse(is.na(stoptotal.3year), 0,  stoptotal.3year)) %>%
  mutate(pa.3year = ifelse(is.na(pa.3year), 0,  pa.3year)) %>%
  ungroup()

glimpse(bbsData.forAnalysis)


# Clear all but final data and species of interest from memory ------------
rm(list= ls()[!(ls() %in% c("states.of.interest","grassSpecies", "decliningSpecies", "bbsData.forAnalysis", "routes_gridList", "mass.aou"))])


# END RUN -----------------------------------------------------------------
if(nrow(bbsData.forAnalysis %>% filter(is.na(log10.mass)))!=0)stop("Some species in `bbsData.forAnalysis` are missing log body masses!!")else("data munging complete.")

# ## RUN WITH CAUTION
# bbsData.forAnalysis <- bbsData.forAnalysis %>% filter(!is.na(log10.mass))
