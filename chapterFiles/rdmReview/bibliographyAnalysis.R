require(tidyverse)
library(revtools)
rdm.dir <- paste0(here::here(), "/chapterfiles/rdmReview")

# Load data ---------------------------------------------------------------
# Load the WOS search results bib file
fn <- paste0(rdm.dir, "/reviewResults/wosRdmReview_20190304.bib")
bib.wos <- read_bibliography(fn)

# Load the GoogleScholar bib file
fn <- paste0(rdm.dir, "/reviewResults/googleScholarRdmReview_20190304.bib")
bib.gs <- read_bibliography(fn)

# Load the GoogleScholar bib file
fn <- paste0(rdm.dir, "/reviewResults/priorRdmReview_20190304.bib")
bib.prior <- read_bibliography(fn)


# Create soem filters -----------------------------------------------------
f1 <- paste("regime", "shift", "regime shift", "regime change", "method", "new", "novel", 
                       "abrupt", "stability", "transition", "threshold", "tipping point", "change point", "change-point", 
                       sep = "|"
  )

t = bib.wos %>%
  mutate(title = tolower(title), 
         abstract = tolower(abstract)) %>% 
  filter(str_detect(title, paste(f1))  |    
           str_detect(abstract, paste(f1)) 
                    )

f2 <- paste("new method", "novel method", "new approach", "novel approach", "new numerical", "novel numerical", 
            "new quantitative", "novel quantitative", "i introduce", "we introduce", 
            sep = "|")

t2 = bib.wos %>%
  mutate(abstract = tolower(abstract)) %>% 
  filter(str_detect(abstract, paste(f2)) 
  )

t3 = bib.wos %>%
  mutate(title = tolower(title)) %>% 
  filter(str_detect(title, paste(f2)) 
  )



# Munge the data ----------------------------------------------------------

(bib.wos)


# Some dissertation text -------------------------------------------------------

# Final number of methods papers used for each category. Thesea re called in the dissertation so do not remove!
n.wos <- nrow(bib.gs)
n.google <- nrow(bib.gs)
n.prior <- nrow(bib.prior)


