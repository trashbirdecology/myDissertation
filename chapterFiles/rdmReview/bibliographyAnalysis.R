require(tidyverse)
library(revtools)
rdm.dir <- paste0(here::here(), "/chapterfiles/rdmReview")

myFile <- paste0(revChapDir, "/regimeshift_20190225.bib")

bib <- read_bibliography(myFile)
class(bib)
summary(bib)
names(bib)

x=find_duplicates(data = bib, match_variable = "label")
screen_titles(bib)
screen_topics(bib)
