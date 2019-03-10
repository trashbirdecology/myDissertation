
# SETUP -------------------------------------------------------------------

require(tidyverse)
require(revtools)

rdm.dir <- paste0(here::here(), "/chapterfiles/rdmReview")

# Create some helper funs -------------------------------------------------
# Create a function for filtering variables by abstrac,t title, etc. 
myFilterFun <- function(words, df, columnName){

  x <- sapply(words, function(x) grepl(x, df[,paste(columnName)]))
  idx <- which(apply(x, 1, any))
  
  return(df[idx,])
}

bibFilter.jrnl <- function(dfIn, jrnls){
  dfOut <- dfIn %>%
    mutate(journal = str_replace(journal, "\\&", "&")) %>%
    filter(journal %in% jrnls) }


# Define filters -----------------------------------------------------
regime.filter <- paste0('\\b', 
                        c("regime", "alternative state", "phase", "equilibrium", "stable state", "threshold"),
                        '\\b')

change.filter <- paste0('\\b', 
                        c("regime shift", "regime change", 
                          "abrupt shift", "abrupt change",
                          "step change", "stepped change",
                          "stark change", 
                          "tipping point", "change point", "change-point",
                          "regime shifts", "regime changes", 
                          "abrupt shifts", "abrupt changes",
                          "step changes", "stepped changes",
                          "stark changes", 
                          "tipping points", "change points", "change-points"), 
                       '\\b') %>% sort()
method.filter <- paste0('\\b',
  c("new method", "novel method", "new approach",
    "new practical method", "new simple method", "new multivariate method", 
    "new tool", "novel tool", "novel multivarte",
    "novel approach", "new numerical", "novel numerical",
                       "new quantitative", "novel quantitative", "i introduce", "we introduce"),
                        '\\b') %>% sort()

# Read in the systematic lit review results. ------------------------------
temp.dir <-paste0(rdm.dir, "/wosSearchResults_20190310/")
files <- dir(path = temp.dir, pattern = "*.bib")
files <- paste0(temp.dir, files)
wos.results <- files %>%
  map(revtools::read_bibliography) %>% 
  bind_rows() %>% 
  distinct(label, .keep_all=T) # not sure why but it is duplicating rows, distinct solves this.


# Filter the wos lit review results since there are wayyy too many! -------
# Filter for new methods
## For new methods
wos.filtered.meth.ti <- myFilterFun(words = method.filter, df = wos.results, columnName = "title")
wos.filtered.meth.abs <- myFilterFun(words = method.filter, df = wos.results, columnName = "abstract")
wos.filtered.meth <- full_join(wos.filtered.meth.abs, wos.filtered.meth.ti)
wos.filtered.regime.abs <- myFilterFun(words = regime.filter, df = wos.results, columnName = "abstract")
wos.filtered.meth <- inner_join(wos.filtered.meth, wos.filtered.regime.abs)

## For regime shift terms
wos.filtered.change.ti <- myFilterFun(words = change.filter, df = wos.results, columnName = "title")
wos.filtered.change.abs <- myFilterFun(words = change.filter, df = wos.results, columnName = "abstract")
wos.filtered.change <- full_join(wos.filtered.change.ti, wos.filtered.change.abs)
# Filter these by regime keywords

# Keep the intersection of these
wos.filtered <- full_join(wos.filtered.change, wos.filtered.meth)

# Read in methods of which I was already aware ----------------------------
prior.fil <- read_bibliography(paste0(rdm.dir, "/reviewResults/", "priorRdmReview_20190304_filtered.csv")) %>% as_tibble() %>% 
  # remove http:// and https:// from DOI
            mutate(doi = str_replace(doi, "https://doi.org/", ""))
wos.fil <- read_bibliography(paste0(rdm.dir, "/reviewResults/", "wosRdmReview_20190304_filtered.csv")) %>% as_tibble()

prior.dois <- c(prior.fil$doi, wos.fil$doi)

# Merge all data to see which are new to me -------------------------------

# Filter by labels on WOS results
label.temp <- setdiff(wos.filtered.meth$label, wos.fil$label)
# FIlter by DOIs
doi.temp <- setdiff(wos.filtered.meth$doi, prior.dois)

# Remove prior knowledge 
wos.withoutPrior <- wos.filtered.meth %>% filter(label %in% label.temp | 
                               doi %in% doi.temp)

# Save to file so I can edit by hand
write_csv(wos.withoutPrior, 
          path = paste0(temp.dir, "wos_withoutPrior.csv"))


# Read in the final list of results from WOS search.  ---------------------

read_csv(paste0(rdm.dir, "/wosSearchResults_20190310/wos_20190310_withoutPrior_filteredByHand.csv"))
