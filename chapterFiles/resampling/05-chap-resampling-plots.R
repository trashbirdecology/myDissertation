# About -------------------------------------------------------------------
# this is the script for analysing data for the rsampling chapter, which  explores the impact of data quality and information on various RDMs
# Setup -------------------------------------------------------------------

rm(list=ls())

## Libs
library(tidyverse)
library(feather)
library(here)

# Source functions
source(paste0(here::here(),"/chapterFiles/resampling/05-chap-resampling-myFunctions.R"))

# What data are we plotting? ----------------------------------------------

# Which dataset to use
diatoms <- TRUE # do you want to analyze the Spanbuaer data? if not, program will create dummy data 

# Create directories to store results, figures, load data etc.
dirs <- createDirs(dirNameInd=ifelse(diatoms==TRUE, "diatoms","dummy")) # creates dirs if null and IDs
dataDir <- dirs$summaryResultsDir

# Load summarizes results ------------------------------------------------------------

my.ind <- "distances"
results.dist <- purrr::map_df(list.files(dataDir, full.names=TRUE, pattern = my.ind), read_feather)
my.ind <- "fivi"
results.fivi <- purrr::map_df(list.files(dataDir, full.names=TRUE, pattern = my.ind), read_feather)

# Create Plots ------------------------------------------------------------

# which metric to plot
metric.ind <- "s"
group.ind1 <- "observations" # a grouping variable
# group.ind1 <- "method" # a grouping variable
group.ind2 <- "prob" # a faceting var
n.col <- 1 # # facet cols

df <- results.dist
x <- "time"
y <-  paste0(metric.ind,".mean")
y.sd <- paste0(metric.ind, ".sd")

if(exists("group.ind1")) group1 <- paste0("as.factor(", group.ind1, ")")
if(exists("group.ind2")) group2 <- paste0("as.factor(", group.ind2, ")")


ggplot(data = df %>% 
         filter(
  time > min(time), 
  method == method.filter
                            )) +
  geom_line(aes_string(x = x, y = y), alpha = .6) +
    facet_wrap(facets = as.formula(paste0("~", group2)), ncol=n.col, scales="free_y")+
  theme_mine() +
  theme(legend.position = "top") 

df.temp <- df %>% 
  mutate(
    upper = !!sym(y)+1.96*!!sym(y.sd),
    lower = !!sym(y)-1.96*!!sym(y.sd)
  ) %>% 
  filter(method %in% method.filter) %>% 
  filter(!is.na(upper), 
         !is.na(lower)) %>% 
  mutate(prob = as.factor(100*as.numeric(as.character(prob))),
         method=as.factor(method))
# Change for labelling purposes
if(group2=="as.factor(prob)" | group2=="as.factor(prob)") levels(df.temp$prob) =  paste0(levels(df.temp$prob),"%")

## ribbons
p.ribbon <-
  ggplot(data = df.temp) +
  geom_line(aes_string(x = x, y = y)) +
  facet_wrap(facets = as.formula(paste0("~", group2)), ncol = n.col, scales="free_y") +
  geom_ribbon(aes_string(x = x,
                         ymin = 'lower',
                         ymax = 'upper' 
  ), na.rm = TRUE,
  color="grey", alpha=0.3)+
  theme_mine() +
  theme(legend.position = "top") +
  ylab("mean velocity")

fn <- paste0()
ggsave(plot=p.ribbon, device=".png",
       filename=)


  

