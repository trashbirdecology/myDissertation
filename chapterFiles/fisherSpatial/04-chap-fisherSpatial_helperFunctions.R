# Custom GGPLOT theme  ------------------------------------------------------
theme.margin <- theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


# Helper Functions -random --------------------------------------------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- stats::quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y <- na.omit(y)
  return(y)
}

# Create function to identify closest military base to those of interst to me.
closestSite <- function(mbs, site, ndeg = 5, by = 0.1) {
  y = 1:999
  ndeg = ndeg + by
  
  for (i in 1:1000) {
    ndeg = ndeg - by
    if (ndeg < 0) {
      break("could not find a single closest site. try new deg and/or by values.")
    }
    
    y = mbs %>% filter(
      mbs$lat <  site$lat  + ndeg &
        mbs$lat > site$lat - ndeg   &
        mbs$long  > site$long - ndeg &
        mbs$long < site$long + ndeg
    )
    
    if (length(y) == 1) {
      break
    }
  }
  
  print(paste0(
    "Closest base is within ",
    paste(ndeg),
    " degrees lat and long of your site."
  ))
  
  return(y)
  
}




# Get ECOREGION maps ------------------------------------------------------
getEcoregions <- function(wgs84=TRUE, level = 2){
  # if(exists('ecoregions')) return(ecoregions)
  if(level == 1) {
    shp.url = "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip"
    shpfile = "NA_CEC_Eco_Level1"
  }
  if(level == 2) {
    shp.url = "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l2.zip"
    shpfile = "NA_CEC_Eco_Level2"
  }
  if(level == 3) {
    shp.url = "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/NA_CEC_Eco_Level3.zip"
      shpfile = "NA_CEC_Eco_Level3"
  }

temp = tempfile()
download.file(shp.url, temp)
exdir = tempdir()
unzip(temp, exdir = exdir)
ecoregions = rgdal::readOGR(exdir, shpfile)[1]
if(wgs84 == TRUE) P4S.latlon <- CRS("+proj=longlat +datum=WGS84")

ecoregions <- spTransform(ecoregions, P4S.latlon)

                          return(ecoregions)}


######################## ANIMATION PLOTS ########################
# Individual transects over space (x) by time (anim) -----------------------------------------------
anim.SingleTsectOverTime <- function(data, metricType.ind, direction.ind, dirID.ind, site.temp = NULL, fn.ind = NULL, get.anim = TRUE, get.static = TRUE ){
    
    # Create temp df for anim plot
    temp <- data %>% as_tibble() %>%
        filter(metricType %in% metricType.ind,
               direction ==direction.ind,
               dirID == dirID.ind) %>%
        mutate(year = as.integer(as.character(year))) %>%
        na.omit(metricValue)
    
    # Number of frames
    # n.frames = length(unique(temp$year)) * 2 - 1
    n.frames = 100    
    
    
## make a filename index to use in the static and animation plots
    fn <- paste0(
        direction.ind, 
        "_transect",
        dirID.ind, 
        "_", fn.ind)

# Remove objs from file to be safe
   suppressWarnings(if(exists("p.static"))rm(p.static))
suppressWarnings(if(exists("p.anim"))rm(p.anim))  
{mbLoc <- NULL
        if(tolower(site.temp) == "riley" &
           direction == "South-North"){  mbLoc <-
               basesOfInterest %>% filter(name == "Fort Riley") %>% dplyr::select(lat) %>% as.numeric()}
        if(tolower(site.temp) == "eglin" &
           direction == "South-North"){ mbLoc <-
               basesOfInterest %>% filter(name == "Eglin AFB") %>% dplyr::select(lat) %>% as.numeric()}
        if(tolower(site.temp) == "riley" &
           direction == "East-West"){
            mbLoc <-
                basesOfInterest %>% filter(name == "Fort Riley") %>% dplyr::select(long) %>% as.numeric()}
        if(tolower(site.temp) == "eglin" &
           direction == "East-West"){ mbLoc <-
               basesOfInterest %>% filter(name == "Eglin AFB") %>% dplyr::select(long) %>% as.numeric()}
    }
    
    # Make the base plot (which will consist of multiple years of data, so hsould look funky)
    {
        if (direction == "East-West")
            p <-
            ggplot(temp, aes(long, metricValue)) +
            geom_line(alpha = 0.7,
                      show.legend = FALSE,
                      size = 1) +
            theme(legend.position = "bottom") + facet_wrap(~ metricType, scales = "free_y", ncol = 1) +
            ggthemes::theme_tufte()
        
        if (direction == "South-North")
            p <-
            ggplot(temp, aes(lat, metricValue)) +
            geom_line(alpha = 0.7,
                      show.legend = FALSE,
                      size = 1) +
            theme(legend.position = "bottom") + facet_wrap(~ metricType, scales = "free_y", ncol = 1) +
            ggthemes::theme_tufte()
    }
    
    
    # Add a Vertical Line for military base of interest  location if applicable
    if (!is.null(mbLoc) &
        direction == "East-West")
        p <-
        p + geom_vline(
            aes(xintercept = mbLoc),
            color = "red",
            alpha = 0.56,
            linetype = 2
        )
    if (!is.null(mbLoc) & direction == "South-North")
        p <- p +
        geom_vline(
            aes(xintercept = mbLoc),
            color = "red",
            alpha = 0.56,
            linetype = 2
        )
    
if(get.anim){    
    # Create the animation of metricValue over time
    p.anim <- p +
        transition_states(year) + #, transition_length=1, state_length= 1) +
        labs(title = 'Year: {closest_state}', y = 'metric value') +
        shadow_trail(color = "grey")
        
    
    anim_save(filename = paste0(fn, ".gif"),
              path = animDir,
              animation = p.anim)

    }
    
    
# Get the static (last plot of the animation)    
    #Colour Palette
    maxYear = max(temp$year)
    minYear = min(temp$year)
    otherYears = setdiff(unique(temp$year), c(maxYear, minYear))
    
    
    temp2 <- temp %>% 
        mutate(year.col = as.factor(ifelse(year == max(year), 3, ifelse(year == min(year), 1, 2))))
    
    cols <- c("1" = "black", "2"= "grey10", "3"="red")
    
 if(get.static==TRUE){  
    p.static <- ggplot(temp2, aes(long, metricValue)) +
        geom_line(alpha = 0.7,
                  show.legend = FALSE,
                  size = 1,
                  aes(group=year, color = year.col))+
        facet_wrap(~ metricType, scales = "free_y", ncol = 1) +
      theme(strip.text.x = element_text(size = 12)) +
      theme(title = element_text(size = 12)) +
      ggthemes::theme_tufte()+
        xlab("Longitude")+ylab("Metric value")+
        ggtitle(paste0(unique(temp2$direction), " transect ", unique(temp2$dirID))) +
      scale_color_grey()
    
    # Add vline
    # Add a Vertical Line for military base of interest  location if applicable
    if (!is.null(mbLoc) &
        direction == "East-West" & mbLoc < max(temp2$long)
        )
        p.static <-
        p.static + geom_vline(
            aes(xintercept = mbLoc),
            color = "red",
            alpha = 0.56,
            linetype = 2
        )
    if (!is.null(mbLoc) & direction == "South-North" & mbLoc >min(temp2$lat))
        p.static <- p.static +
        geom_vline(
            aes(xintercept = mbLoc),
            color = "red",
            alpha = 0.56,
            linetype = 2
        )
    
    if(exists("p.static")){ggsave(p.static, filename = paste0(figDissDir,"/lastFrameAnim_", fn, ".png"))}else(return())
    }

if(get.anim)return(p.anim)    

    }


# Multiple transects over space (x) by time (anim) -----------------------------------------------

