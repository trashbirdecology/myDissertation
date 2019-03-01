# Individual transects over space (x) by time (anim) -----------------------------------------------
anim.SingleTsectOverTime <- function(data, metricType.ind, direction.ind, dirID.ind, site.temp = NULL, fn.ind = NULL ){
    
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
    
    # Make the base plot
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
    
    
    # Create the animation of metricValue over time
    p.anim <- p +
        transition_states(year) + #, transition_length=1, state_length= 1) +
        labs(title = 'Year: {closest_state}', y = 'metric value') +
        shadow_trail(color = "grey")
        
    
    fn <- paste0(
        direction.ind, 
        "_transect",
        dirID.ind, 
        "_", fn.ind)
    
    anim_save(filename = paste0(fn, ".gif"),
              path = animDir,
              animation = p.anim)

    
    # Saves last frame to file as .png
        # cant get it to work with `plot` for some reason it reners the entire animation when saving, but not when previewing in console....
        # plot(p.anim, frame = 95, nframes = 100)
    
    
    #Colour Palette
    maxYear = max(temp$year)
    minYear = min(temp$year)
    otherYears = setdiff(unique(temp$year), c(maxYear, minYear))
    
    temp2 <- temp %>% 
        mutate(year.col = ifelse(year == max(year), 3, ifelse(year == min(year), 2, 1)))
    
    p.static <- ggplot(temp2, aes(long, metricValue)) +
        geom_line(alpha = 0.7,
                  show.legend = FALSE,
                  size = 1.25,
                  aes(group=year, color = year.col))+
        theme(legend.position = "bottom") + facet_wrap(~ metricType, scales = "free_y", ncol = 1) +
        ggthemes::theme_tufte()+
    scale_colour_gradientn(colours=c("grey", "black"))
    
    ggsave( p.static, filename = paste0(figDissDir,"/lastFrameAnim_", fn, ".png"))
    
    
    return(p.anim)    

    }



# Multiple transects over space (x) by time (anim) -----------------------------------------------
