require(tidyverse)
library(revtools)
rdm.dir <- paste0(here::here(), "/chapterfiles/rdmReview")

# Get ecology and ecol methods journals ------------------------------------
tempFolder <- tempdir()

download.file("https://www.scimagojr.com/journalrank.php?category=2303&type=j&out=xls", 
              destfile = paste0(tempFolder,"/", "mySJRdata.csv"))#, method = "curl")
list.files(tempFolder)

ecolJrnls <-  read.csv(paste0(tempFolder, "/mySJRdata.csv"), header = T, sep=";") %>% 
  dplyr::select(Rank, Title, SJR.Quartile) %>% 
  rename(SO = Title) %>% 
  mutate(SO = toupper(SO)) %>% 
  mutate(SO = str_replace(SO, " AND ", " & ")) 

# Define other relevant jouranls not in top journals..
methJrnls <- c("METHODS IN ECOLOGY AND EVOLUTION", "ECOLOGICAL MODELLING")

# List of jornals to use to filter the WOS search
myJrnls <- c(ecolJrnls$SO, methJrnls)

# Create some helper funs -------------------------------------------------
dfIn = bib.prior
colName="abstract"
# Create a function for filtering variables
bibFilter.col <- function(dfIn, colName, f){
  dfOut <- dfIn %>%
    mutate(new = tolower(!! rlang::sym(colName))) %>% 
    filter(str_detect(new, paste(f))) %>% 
    select(-!! sym(colName)) %>% 
    rename(!! sym(colName) := new)
}

bibFilter.jrnl <- function(dfIn, jrnls){
  dfOut <- dfIn %>% 
    mutate(journal = str_replace(journal, "\\&", "&")) %>% 
    filter(journal %in% jrnls) }

# Create some filters -----------------------------------------------------

change.filter <- paste("regime", "shift", "regime shift", "regime change", "method", "new", "novel", 
            "abrupt", "stability", "transition", "threshold", "tipping point", "change point", "change-point", 
            sep = "|"
)

method.filter <- paste("new method", "novel method", "new approach", "novel approach", "new numerical", "novel numerical", 
                       "new quantitative", "novel quantitative", "i introduce", "we introduce", 
                       sep = "|")

# Load bibliogrpahies  ---------------------------------------------------------------
# Load the WOS search results bib file
fn <- paste0(rdm.dir, "/reviewResults/wosRdmReview_20190304.bib")
bib.wos <- read_bibliography(fn) %>% 
  filter(type == "article") %>%  # keep only articles
# Filter based on the journals 
  bibFilter.jrnl(myJrnls) %>% 
  bibFilter.col(colName = "abstract", f = paste(change.filter, method.filter, sep="|"))
# save a file which I can filter through 
write_csv(bib.wos, paste0(rdm.dir, "/reviewResults/", "wosRdmReview_20190304_toFilter.csv")) # save to file as .csv so I can filter titles, abstracts.


# Load the prior bib file
fn <- paste0(rdm.dir, "/reviewResults/priorRdmReview_20190304.bib")
bib.prior <- read_bibliography(fn) %>% 
  distinct(doi, .keep_all=T) %>% 
  filter(type == "article") %>%  # keep only articles
  # Filter based on the journals 
  # bibFilter.jrnl(myJrnls) %>% 
  # bibFilter.col(colName = "abstract", f = change.filter) %>%
  bibFilter.col(colName = "abstract", f = method.filter) 
  
# Remve overlap between wos
keep <- setdiff(bib.prior$doi, bib.wos$doi)
bib.prior <- bib.prior %>% filter(doi %in% keep)

# save a file which I can filter through 
write_csv(bib.prior, paste0(rdm.dir, "/reviewResults/", "priorRdmReview_20190304_toFilter.csv")) # save to file as .csv so I can filter titles, abstracts.

# Load the GoogleScholar bib file
fn <- paste0(rdm.dir, "/reviewResults/googleScholarRdmReview_20190304.bib")
bib.gs <- read_bibliography(fn) 

# Combine the filtered results to review abstracts and collect data. 
prior.fil <- read_bibliography(paste0(rdm.dir, "/reviewResults/", "priorRdmReview_20190304_filtered.csv")) %>% as_tibble()
wos.fil <- read_bibliography(paste0(rdm.dir, "/reviewResults/", "wosRdmReview_20190304_filtered.csv")) %>% as_tibble()


# Some dissertation text -------------------------------------------------------

# Final number of methods papers used for each category. Thesea re called in the dissertation so do not remove!
n.wos <- nrow(bib.gs)
n.google <- nrow(bib.gs)
n.prior <- nrow(bib.prior)




