# About -------------------------------------------------------------------
# this is the script for analysing data for the rsampling chapter, which  explores the impact of data quality and information on various RDMs
# Setup -------------------------------------------------------------------
rm(list = ls())

library(caTools)
library(kedd)
library(PerformanceAnalytics)
library(tidyverse)
library(feather)

# Set random seed for repeatability
set.seed(12345)

# Source functions
source(
  paste0(
    here::here(),
    "/chapterFiles/resampling/05-chap-resampling-myFunctions.R"
  )
)


# Analysis paramters ------------------------------------------------------

# Which dataset to use
diatoms <-
  TRUE # do you want to analyze the Spanbuaer data? if not, program will create dummy data
dummyData <-
  FALSE # do you want to create a dummy data rather than using the Spanbauer data?

# Create directories to store results, figures, load data etc.
dirs <-
  createDirs(dirNameInd = ifelse(diatoms == TRUE, "diatoms", "dummy")) # creates dirs if null and IDs
# Return the directotries as objects
for (i in seq_along(dirs)) {
  assign(names(dirs)[i], dirs[[i]])
}


# Spit an error to ensure no issues
if (diatoms &
    dummyData)
  stop(
    "The logicals diatoms and dummyData are both 'TRUE'. I cannot handle this, please make one FALSE."
  )

# Define how to subset the data
## Which methods of data subsetting to observe?
myMethods <- c("dominance", "species", "observations")#

## Which proportions of myMethods to explore?
prop = c(0.25,
         0.5,
         0.75,
         1.0)  # one or more numbers between 0 and 1

## Define the number of random draws for each method
nDraws <- 1e4

## Proportion of observations over which the moving window will advance
winMove <- 0.20 # between 0 and 1


# Create or load the observations -----------------------------------------------------------
# Use Spanbauer 2014 diatom time series
if (diatoms) {
  dirNameInd <- "diatoms"
  myDf.long <- getSpanbauerData() # a data.frame with
}

# Create some data
if (dummyData) {
  dirNameInd <- "dummy"
  myDf.long <- NULL
  nobs = 1e2 # number of observations
  nspp = 1e1 # number of variables
  x    = 0:100   # the range of values to choose from for potential value
  
  for (i in 1:nspp) {
    temp <- NULL
    
    mean = base::sample(abs(100 - sample(50, 1)), size  = 1)
    mean2  = base::sample(100, size = 1)
    
    # get a series of values for each species
    value <- c(
      rnorm(
        n = nobs / 2,
        mean = mean ,
        sd = mean - mean / 2
      ),
      rnorm(
        n = nobs / 2,
        mean = mean2,
        sd = abs(mean2 - mean) / sample(1:4, 1)
      )
    )
    
    temp <- tibble(
      time = rep(1:nobs),
      site = "A",
      variable = letters[i],
      value =   value
    )
    
    # create the final data frame in long format
    myDf.long <- bind_rows(myDf.long, temp)
  } # end i for
} # end if dummyData

# Visualize the Raw Data --------------------------------------------------
ggplot(myDf.long) + geom_line(aes(
  x = time,
  y = value,
  color = (variable)
)) +
  theme_classic() +
  theme(legend.position = 'none') +
  ylab("relative abundance") +
  ggsave(paste0(figDir, "/origDataRelAbundance.png"))

ggplot(myDf.long %>%
         distinct(time) %>%
         arrange(time) %>%
         mutate(dt = time - lag(time))) + geom_line(aes(x = time,
                                                        y = dt)) +
  theme_classic() +
  theme(legend.position = 'none') +
  ylab("time since last observation") +
  ggsave(paste0(figDir, "/timeElapsed.png"))



# Detrend the original data to compare results ----------------------------
### produce the loessExample to show that detreend these data is not a great option...
vars <- unique(myDf.long$variable)

temp <- myDf.long %>%
  filter(variable == vars[17])
temp <- myDf.long

loessMod10 <-
  loess(value ~ time, data = temp, span = 0.1) # 10% smoothing span
loessMod25 <-
  loess(value ~ time, data = temp, span = 0.25) # 25% smoothing span
loessMod50 <-
  loess(value ~ time, data = temp, span = 0.50) # 50% smoothing span
# get smoothed output
smoothed10 <- predict(loessMod10)
smoothed25 <- predict(loessMod25)
smoothed50 <- predict(loessMod50)


library(fANCOVA)
FTSE.lo3 <-
  loess.as(
    temp$time,
    temp$value,
    degree = 1,
    criterion = c("aicc", "gcv")[2],
    user.span = NULL,
    plot = T
  )
FTSE.lo.predict3 <- predict(FTSE.lo3, data.frame(time = temp$time))

# Plot it
png(
  here::here("chapterFiles/resampling/figsCalledInDiss/loess.png"),
  width = 620,
  height = 450
)
plot(
  x = temp$time,
  y = temp$value,
  type = "l",
  main = "Loess Smoothing and Prediction",
  xlab = "time",
  ylab = paste0("relative abundance of ")
)
lines(smoothed10,
      x = temp$time,
      col = "red",
      lwd = 2)
lines(smoothed25,
      x = temp$time,
      col = "green",
      lwd = 2)
lines(smoothed50,
      x = temp$time,
      col = "blue",
      lwd = 2)
lines(
  FTSE.lo.predict3,
  x = temp$time,
  col = "yellow",
  lwd = 3,
  lty = 4
)
legend(
  -6800,
  1,
  legend = c(".025 span", ".05 span", ".10 span", ".05 (automatic) span"),
  col = c("red", "green", "blue", "yellow"),
  lty = 1:2,
  cex = 0.8
)
dev.off()

# Conduct reasmpling analysis --------------------------------------------------------
# This will run and save results to feathers. if you want specific results only, specify ews or fivi =TRUE/FALSE. This function will always calc and save distances to file. Can also specify to save the original data (origDat= TRUE)

# resamplingAnalysis(
#   myDf.long,
#   prop,
#   myMethods,
#   nDraws,
#   winMove = .20,
#   origData = TRUE,
#   ews = FALSE,
#   fivi = FALSE,
#   fi.method = "7.12" #7.12 is the derivatives method
# )


# Summarise the bootstraps ------------------------------------------------
## Distance results
summariseResults(
  dataDir = distDir,
  myMethods =  myMethods,
  prop = prop,
  summaryResultsDir = summaryResultsDir
)

## FIVI results
summariseResults(
  dataDir = fiviDir,
  myMethods =  myMethods,
  prop = prop,
  summaryResultsDir = summaryResultsDir
)



### I still havent figured out efficient way to visualize EWS data, so don't bother running it. 
# ## EWS results
# summariseResults(
#   dataDir = ewsDir,
#   myMethods =  myMethods,
#   prop = prop,
#   summaryResultsDir = summaryResultsDir
# )
## Be sure to summarise the directory (summaryResults) to summaryResults.zip in directory ~../diatoms





# END RUN -----------------------------------------------------------------
