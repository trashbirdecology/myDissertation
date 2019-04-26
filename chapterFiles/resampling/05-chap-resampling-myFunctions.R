# Wrapper Function --------------------------------------------------------
resamplingAnalysis <- function(myDf.long,
                               prop,
                               myMethods,
                               nDraws,
                               winMove,
                               origData = TRUE,
                               ews = TRUE,
                               fivi = TRUE,
                               fi.method = "7.12") {
  runTime <- Sys.time()
  
  # Print estimated runtime for diatoms
  if (dirNameInd == 'diatoms') {
    if (ews & fivi)
      r <- 4.2
    if (ews & !fivi)
      r <- 2.5
    if (fivi & !ews)
      r <- 1.8
    est <-
      r * length(prop) * length(myMethods) * nDraws / 60 # # minutes estimated for runtime
    if (est < 15) {
      warning(
        paste0(
          nDraws,
          " draws for the diatom data should complete in approx. ",
          est,
          " minutes"
        ),
        immediate. = TRUE
      )
    } else
      (warning(paste0("take a chill pill, this should take ~ ", est / 60, " hours!"),
               immediate. = TRUE))
    rm(est)
  }
  
  # Create an empty dataset
  subsetData <- NULL
  
  # Loop over proportion(s), method(s), and draws
  for (h in seq_along(prop)) {
    for (i in 1:length(myMethods)) {
      for (j in 1:nDraws) {
        print(paste0("begin loops: h = ", h, " | i = ", i, " | j = ", j))

                # Line below is used to restart nalysis at last location.
        if(j < 1663 & prop[h]== .25){next}

              # Subset the data
        temp <- random_subset(myDf.long, myMethods[i], prob = prop[h]) %>%

        # Subset the data
        temp <-
          random_subset(myDf.long, myMethods[i], prob = prop[h]) %>%
                    mutate(nDraw = j,
                 winMove = winMove)
        
        if (origData)
          writeResults(resultsDf = temp, myDir = origDataDir, h, i, j)
        
        # Calculate distance travelled
        if (exists("results"))
          results <- NULL
        
        results <- temp %>%
          # Distance between species
          arrange(variable, method, prob, nDraw, time) %>%
          group_by(variable, method, prob, nDraw) %>%
          mutate(dx = value - lag(value)) %>%
          ungroup() %>%
          na.omit(dx) %>%
          # Sum of distances (across species at each time)
          group_by(method, prob, nDraw, time, winMove) %>%
          summarize(ds = sqrt(sum(dx ^ 2))) %>%
          filter(ds != 0) %>%
          ungroup() %>%
          # Calculate cumulative ds and derivatives
          group_by(method, prob, nDraw) %>%
          mutate(s = cumsum(ds),
                 dsdt = ((s - lag(s)) / (time - lag(time))),
                 d2sdt2 = ((dsdt - lag(dsdt)) / (time - lag(time)))) %>%
          ungroup() %>%
          # Drop NA's
          na.omit(p)
        
        
        writeResults(resultsDf = results, myDir = distDir, h, i, j)
        
        # Calculate EWSs
        temp <- suppressMessages(full_join(results, temp)) %>%
          arrange(time)
        
        if (nrow(temp) <= 5) {
          warning("Five or less observations in data subset--not calculating EWS")
          next
        }
        
        
        # Calculate FI and VI
        # Window size
        time <- temp$time
        timeSpan <- range(time)
        TT <- timeSpan[2] - timeSpan[1]
        winSize <- winMove * TT
        # Window spacing
        winSpace <- max(lead(time) - time, na.rm = T)
        
        # calculate Fisher Information and Variance Index within the window
        require(caTools)
        if (fivi) {
          results <- NULL
          results <-
            window_analysis(data = temp,
                            winSize = winSize,
                            winSpace = winSpace) %>%
            mutate(method = myMethods[i],
                   prob = prop[h],
                   nDraw = j)
          
          writeResults(resultsDf = results, myDir = fiviDir, h, i, j)
        }
        
        
        
        if (ews) {
          results <- NULL
          results <-
            window_analysis_EWS(temp, winSize, winSpace) %>%
            mutate(method = myMethods[i],
                   prob = prop[h],
                   nDraw = j)
          writeResults(resultsDf = results, myDir = ewsDir, h, i, j)
        }
        
        
        
        
        
        
      } # end nDraws loop (j)
    } # end myMethods loop
  } #end prop loop
  
  # Print run time
  print(paste0("this run took ~  ", (Sys.time() - runTime), " minutes"))
  
  
  # Audio completion alerts
  {
    system("rundll32 user32.dll,MessageBeep -5") # will alert on Windows
    system("Hay girl! I D K if it's right but it's done!")
  }
  
} # End function resamplingAnalysis

# Window Analyses and Binning Prior to Calc -------------------------------
window_analysis <-
  function(data, winSize, winSpace, fi.method = "7.12") {
    # Start and stop points for windows
    winStart <-
      round(seq(min(data$time),
                max(data$time) - winSize,
                by = winSpace))
    winStop <- winStart + winSize
    
    # Number of windows
    nWin <- length(winStart)
    
    VI <- FI <- rep(NA, length = nWin)
    EWS <- list()
    
    for (l in 1:nWin) {
      # Data from the time period within winStart:winStop
      winData <- data %>%
        filter(time >= winStart[l],
               time < winStop[l])
      
      # if (nrow(winData) <= 2) {
      # warning("Two or less observations in window" )
      # }
      
      # Calculate FI
      FI[l] <- calculate_FI(winData, method = fi.method)
      
      # Calculate variance index
      
      if (!is.na(FI[l])) {
        VI[l] <- calculate_VI(winData)
      } else
        (VI[l] <- NA)
      
      
    }
    
    result <- data_frame(winStart, winStop, FI, VI)
    
    return(result)
    
  }
window_analysis_EWS <- function(subData, winSize, winSpace) {
  # Start and stop points for windows
  winStart <-
    round(seq(min(subData$time),
              max(subData$time) - winSize,
              by = winSpace))
  winStop <- winStart + winSize
  
  # Number of windows
  nWin <- length(winStart)
  
  EWS <- list()
  for (k in 1:nWin) {
    # Data from the time period within winStart:winStop
    winData <- subData %>%
      filter(time >= winStart[k],
             time < winStop[k])
    
    if (nrow(winData) <= 2) {
      warning("Two or less observations in window")
      next
    }
    
    
    # Calculate species-specific indicators
    winStartInd <- winStart[k]
    winStopInd <- winStop[k]
    EWS[[k]] <-  calculate_EWS(winData, winStartInd, winStopInd)
    
  }
  result <-  plyr::ldply(EWS, data.frame) %>% as_tibble()
  return(result)
  
  
}


# Calculations ------------------------------------------------------------
calculate_VI <- function(winData) {
  spp.keep <-
    winData %>%
    dplyr::select(variable, value) %>%
    group_by(variable) %>%
    summarise(value = sum(value)) %>%
    filter(value > 0) %>%
    dplyr::select(variable)
  
  ts <-
    winData %>%
    dplyr::select(variable, value, time) %>%
    filter(variable %in% spp.keep$variable) %>%
    spread(variable, value) %>%
    select(-time) %>%
    as.matrix()
  
  eigCov <- eigen(cov(ts))
  VI <- max(eigCov$values)
  
  return(VI)
  
  
}
calculate_FI <- function(myDat, method = "7.12") {
  # print('calculating FI')
  FI <- NA
  # Calculate distribution of distance travelled
  
  data <-
    myDat %>%
    na.omit(dsdt)
  
  if (nrow(data) == 0)
    return(FI)
  data <- data %>%
    distinct(time, s, dsdt, d2sdt2) %>%
    mutate(TT = max(time) - min(time),
           p = (1 / TT) * (1 / dsdt))
  
  # if (nrow(data) <= 2) {
  #   warning("Two or less observations in window")
  # }
  
  if (method == "7.3b") {
    # print("FI method 7.3b")
    # Equation 7.3b
    p <- data$p
    s <- data$s
    dp <- lead(p) - p
    ds <- lead(s) - s
    dpds <- dp / ds
    ind <- 1:(length(s) - 1)
    FI <- trapz(s[ind], (1 / p[ind]) * dpds[ind] ^ 2)
  } else if (method == "7.3c") {
    # print("FI method 7.3c")
    # Equation 7.3c
    q <- sqrt(data$p)
    s <- data$s
    dq <- lead(q) - q
    ds <- lead(s) - s
    dqds <- dq / ds
    ind <- 1:(length(s) - 1)
    FI <- 4 * trapz(s[ind], dqds[ind] ^ 2)
    
  } else if (method == "7.12") {
    # Equation 7.12
    # print("FI method 7.12")
    t <- data$time
    s <- data$s
    TT <- max(t) - min(t)
    dsdt <- data$dsdt
    d2sdt2 <- data$d2sdt2
    ind <- 1:(length(s) - 1)
    FI <- (1 / TT) * trapz(t[ind], d2sdt2 ^ 2 / dsdt ^ 4)
  } else {
    warning("Unrecognized method")
    FI <- NULL
    
  }
  
  return(FI)
  
  
}
calculate_EWS <- function(winData, winStartInd, winStopInd) {
  spp.keep <-
    winData %>%
    dplyr::select(variable, value) %>%
    group_by(variable) %>%
    summarise(value = sum(value)) %>%
    filter(value > 0) %>%
    dplyr::select(variable)
  
  ts <-
    winData %>%
    dplyr::select(variable, value, time) %>%
    filter(variable %in% spp.keep$variable)
  
  
  ews <- ts %>%
    filter(variable %in% spp.keep$variable) %>%
    group_by(variable) %>%
    mutate(
      # mean = mean(value),
      # median = median(value),
      # mode = getmode(value),
      sd = sd(value),
      CV = sd(value) / mean(value),
      # kurtosis using the "PerformanceAnalytics" function
      kurtosis =  kurtosis(value, method = "fisher"),
      # skewness using two methods
      skewMode = (mean(value) - getmode(value)) / sd(value),
      skewMean = (median(value) * 3) / sd(value)
      # ,
      # autocorr1 = acf(value, lag = 1)[1]$acf %>% as.numeric()
    ) %>%
    ungroup() %>%
    select(-time, -value) %>%
    distinct(variable, .keep_all = T) %>%
    na.omit(mean) %>%
    mutate(winStart = winStartInd,
           winStop = winStopInd)
  
  return(ews)
  
}

# Create directories ----------------------------
## If dirs exist will not create new.
createDirs <- function(dirNameInd) {
  figDir <-
    paste0(here::here(),
           '/chapterFiles/resampling/figsCalledInDiss/')
  dir.create(paste0(
    here::here(),
    "/chapterFiles/resampling/figsCalledInDiss/"
  ))
  
  dir.create(paste0(here::here(), "/chapterFiles/resampling/results/"))
  
  resultsDir <-
    paste0(here::here(),
           "/chapterFiles/resampling/results/",
           dirNameInd,
           "/")
  dir.create(resultsDir)
  
  distDir <- paste0(resultsDir, "distances/")
  ewsDir <- paste0(resultsDir, "ews/")
  fiviDir <- paste0(resultsDir, "fiVi/")
  origDataDir <- paste0(resultsDir, "originalData/")
  dir.create(distDir)
  dir.create(ewsDir)
  dir.create(fiviDir)
  dir.create(origDataDir)
  
  
  # Saves the summarised results (called in resampling-plots.R)
  summaryResultsDir <- paste0(resultsDir, "summaryResults/")
  dir.create(summaryResultsDir)
  
  
  return(
    list(
      resultsDir = resultsDir,
      figDir = figDir,
      distDir = distDir,
      ewsDir = ewsDir,
      fiviDir = fiviDir,
      origDataDir = origDataDir,
      summaryResultsDir = summaryResultsDir
    )
  )
}


# Helper Functions --------------------------------------------------------
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getSpanbauerData <- function(scale.spp = TRUE) {
  if (scale.spp)
    print("scaling the spanbauer data")
  else
    ("species observations are not scaled")
  
  # Load data
  data = suppressMessages(read_csv(
    url(
      "http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0108936.s001"
    )
  ))
  
  
  # Convert data to long format
  myDf.long <-
    data %>%
    select(-Sample) %>%
    gather(variable, value, -YB1950) %>%
    rename(time = YB1950) %>%
    mutate(site = "Foy")
  
  # Calculate some statistics for species at every observation
  sumData <-
    myDf.long %>%
    group_by(time) %>%
    summarize(
      med = median(value),
      minRange = min(value),
      maxRange = max(value),
      q1 = quantile(value, 0.25),
      q3 = quantile(value, 0.75),
      v = var(value),
      xbar = mean(value),
      total = sum(value)
    )
  
  if(scale.spp){
    stand01 <- function(x) { # standardize the data
      (x - min(x)) / diff(range(x))
    }}
  
  myDf.long <- myDf.long %>%
    group_by(time, site) %>%
    ungroup()
  
  
  # myMat <- myDf.long %>%
  #   dplyr::select(site, time, variable, value) %>%
  #   spread(-time, -site)
  #
  #
  # myMat.long <- myMat %>%
  #   gather(variable, value, -time, -site)
  
  
  return(myDf.long)
}


random_subset <- function(data, method, prob = runif(1)) {
  # Switch method
  if (method == "species") {
    randVars <-
      data %>%
      distinct(variable) %>%
      sample_frac(size = prob, replace = F)
    
    dataOut <- data %>%
      filter(variable %in%  randVars$variable)
    
  } else if (method == "dominance") {
    predominantSpp <-
      data %>%
      group_by(time, site) %>%
      # Note: sum(value) = 1
      mutate(scaledValue = value / sum(value)) %>%
      filter(scaledValue >= prob) %>%
      ungroup() %>%
      distinct(variable)
    
    dataOut <-
      data %>%
      filter(variable %in% predominantSpp$variable)
    
  } else if (method == "observations") {
    timeTemp <- unique(data$time)
    
    randObs <- sample(timeTemp, round(length(timeTemp) * prob))
    
    dataOut <-
      data %>%
      filter(time %in% randObs)
    
  } else if (method == "none") {
    dataOut <- data
    
  } else {
    warning("Unrecognized method")
    dataOut <- NULL
    
  }
  
  if (nrow(dataOut) == 0) {
    warning("Returning empty data frame")
  }
  
  result <-
    dataOut %>%
    mutate(method = method,
           prob = prob)
  
  return(result)
}



# Manipulate and Load Results ---------------------------------------------
# Save the results to file after analysis
writeResults <- function(resultsDf, myDir, h, i, j) {
  fn <-
    paste0(myDir,
           "prop",
           prop[h] * 100,
           "_",
           myMethods[i],
           "_draw" ,
           j,
           ".feather")
  write_feather(resultsDf, path = fn)
}



# Summarise the Results -------------------------------------------------------
# Returns a list of results from feather for each method/proportion/RDM combination, summarises, and saves to a list object
summariseResults <-
  function(dataDir,
           myMethods,
           prop,
           summaryResultsDir) {
    results <- list() # initialize an empty df to store results
    for (i in seq_along(prop)) {
      for (j in seq_along(myMethods)) {
        results.temp <- NULL
        if (!all(prop > 1))
          prop <- prop * 100 # if proportions are <1, then need to adjust
        my.ind <- paste0("prop", prop[i], "_", myMethods[j])
        
        results.temp = purrr::map_df(list.files(dataDir, full.names = TRUE, pattern = my.ind),
                                     read_feather)
        
        
        ## Summarise the DISTANCES
        if (str_detect(string = dataDir, pattern = "distances")) {
          my.ind <- paste0("distances_", my.ind)
          
          if (prop[i] != 100) {
            results.temp <-  results.temp %>%
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
          if (prop[i] == 100) {
            if(nDraw > 1){next}
            results.temp <- results.temp %>%
              group_by(method, prob, time, winMove) %>%
              rename(
                ds.mean      =  ds,
                s.mean       =  s,
                dsdt.mean    =  dsdt,
                d2sdt2.mean  =  dsdt
              ) %>%
              dplyr::select(-nDraw)
          }  # end ifelse prop==100
        }
        
        ## Summarise the FIVI
        if (str_detect(string = dataDir, pattern = "fiVi")) {
          my.ind <- paste0("fivi_", my.ind)
          
          if (prop[i] != 100) {
            results.temp <-  results.temp %>%
              group_by(method, prob, winStart, winStop) %>%
              summarise(
                FI.mean      =  mean(FI, na.rm = TRUE),
                VI.mean       =  mean(VI, na.rm = TRUE),
                FI.sd    =  sd(FI, na.rm = TRUE),
                VI.sd  =  sd(VI, na.rm = TRUE)
              )
          } # end ifelse prop!=100
          
          # If prop == 100% then we don't need means and SD, we just need orginal data.
          if (prop[i] == 100) {
            if(nDraw > 1){next}
            results.temp <- results.temp %>%
              group_by(method, prob, winStart, winStop) %>%
              rename(FI.mean      =  FI,
                     VI.mean      =  VI) %>%
              dplyr::select(-nDraw)
          }  # end ifelse prop==100
        }
        
        ## Summarise the EWSs
        if(str_detect(string = dataDir, pattern = "ews")
        ){
          my.ind <- paste0("ews_",my.ind)  
          
          if(prop[i]!=100){
            results.temp <-  results.temp %>%
              group_by(variable, method, prob, winStart, winStop) %>%
              summarise(
                sd.mean      =  mean(sd, na.rm = TRUE),
                sd.sd    =  sd(sd, na.rm = TRUE),
                CV.mean      =  mean(CV, na.rm = TRUE),
                CV.sd    =  sd(CV, na.rm = TRUE),
                kurtosis.mean      =  mean(kurtosis, na.rm = TRUE),
                kurtosis.sd    =  sd(kurtosis, na.rm = TRUE),
                skewMode.mean      =  mean(skewMode, na.rm = TRUE),
                skewMode.sd    =  sd(skewMode, na.rm = TRUE),
                skewMean.mean      =  mean(skewMean, na.rm = TRUE),
                skewMean.sd    =  sd(skewMean, na.rm = TRUE)
              ) 
          } # end ifelse prop!=100
          
          # If prop == 100% then we don't need means and SD, we just need orginal data. 
          if(prop[i]==100){ 
            results.temp <-  results.temp %>%
              filter(nDraw == 1) %>% 
              group_by(variable, method, prob, winStart, winStop) %>%
              summarise(
                sd.mean = sd, 
                CV.mean = CV, 
                kurotsis.mean = kurtosis,
                skewMode.mean = skewMode, 
                skewMean.mean = skewMean
              )
          }  # end ifelse prop==100
        }
        
        
        # Save summary results to file
        fn <-
          paste0(summaryResultsDir,
                 "summaryResults_",
                 "_" ,
                 my.ind,
                 ".feather")
        write_feather(results.temp, path = fn)
        
      } # end methods j loop
    } # end prop i loop
    
    print(paste0("results saved in ", summaryResultsDir))
    
  } # end function summariseResults



# Plotting Functions ------------------------------------------------------

## My plotting theme
theme_mine <- function(base_size = 18,
                       base_family = "Helvetica") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background = element_blank(),
      # strip.text.x = element_text(size = 15),
      # strip.text.y = element_text(size = 15),
      # axis.text.x = element_text(size=14),
      # axis.text.y = element_text(size=14,hjust=1.5),
      # axis.ticks =  element_line(colour = "black"),
      # axis.title.x= element_text(size=16),
      # axis.title.y= element_text(size=16,angle=90),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1.0, "lines"),
      plot.background = element_blank(),
      # axis.line.x = element_line(color="black", size = 1),
      # axis.line.y = element_line(color="black", size = 1)
      plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines")
    )
}


