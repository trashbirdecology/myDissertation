# install.packages(c("gapminder", "magick", "gganimate", "gifski", "transformr"))
# library(transformr)
# library(gapminder)
# library(magick)
# library(ggplot2)
# library(gganimate)
# library(gifski)
# library(tidyverse)


# Source function(s) ------------------------------------------------------
# source("./chapterFiles/fisherSpatial/04-chap-fisherSpatial_plotting_functions.R")

# Individual transects over space (x) by time (anim) ---------------------------------

# Distance
anim.SingleTsectOverTime(data = results_dist, 
                         metricType.ind = c("s", "dsdt"), 
                         direction.ind = "East-West", 
                         dirID.ind = 13, 
                         site.temp = "riley", 
                         fn.ind = "distance" , 
                         get.anim = FALSE, 
                         get.static = TRUE)

# Fisher Information 
anim.SingleTsectOverTime(data = results_ews, 
                         metricType.ind = c("FI_Eqn7.12"), 
                         direction.ind = "East-West", 
                         dirID.ind = 13, 
                         site.temp = "riley", 
                         fn.ind = "fisher",
                         get.anim = FALSE, 
                         get.static = TRUE)


# # Fisher Information + Variance Index
anim.SingleTsectOverTime(data = results_ews,
                         metricType.ind = c("FI_Eqn7.12", "VI"),
                         direction.ind = "East-West",
                         dirID.ind = 13,
                        site.temp = "riley",
                         fn.ind = "fiVi" ,
                        get.anim = FALSE, 
                        get.static = TRUE)

# Variance index + coefficient of variation
anim.SingleTsectOverTime(data = results_ews,
                         metricType.ind = c("CV","VI"),
                         direction.ind = "East-West",
                         dirID.ind = 13,
                         site.temp = "riley",
                         fn.ind = "viCv",
                         get.anim = FALSE, 
                         get.static = TRUE)


