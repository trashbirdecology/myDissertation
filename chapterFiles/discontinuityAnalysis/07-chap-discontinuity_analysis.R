# rm(list=ls())

library(tidyverse)


# Load and munge data -----------------------------------------------------
data <- read.csv(here::here("chapterFiles/discontinuityAnalysis/cpr_DA_transectData.csv")) %>% 
  as_tibble()

