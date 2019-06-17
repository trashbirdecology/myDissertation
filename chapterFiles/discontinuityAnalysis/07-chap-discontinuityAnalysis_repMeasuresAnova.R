# Setup -------------------------------------------------------------------
## Clear memory
rm(list = ls())

## Load pkgs
library(cowplot)
library(tidyverse)

# Source files and functions ----------------------------------------------
## Source the script that returns the obkect `feathers.subset`, whcih si the BBS data + sampling gridded subsetted data...
source(
  here::here(
    "/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_getMungeBBSdata.R"
  )
)# Repeated-measures anova on data
## Source the helper functions
source(
  here::here(
    "/chapterFiles/discontinuityAnalysis/07-chap-discontinuityAnalysis_helperFunctions.R"
  )
)

# Define, create directories -------------------------------------
## discontinuity results location
reultsDir <-
  here::here("chapterFiles/discontinuityAnalysis/results/")

## figures
figDirTemp <-
  here::here("chapterFiles/discontinuityAnalysis/tempFigs/")
figDir <-
  here::here("chapterFiles/discontinuityAnalysis/figsCalledInDiss/")
tabDir <-
  here::here("chapterFiles/discontinuityAnalysis/tabsCalledInDiss/")
suppressWarnings(dir.create(figDirTemp))
suppressWarnings(dir.create(figDir))
suppressWarnings(dir.create(tabDir))


# Roberts et al regime shift locations -----------------------------------------------------------------------
## these are the locations and years of the regime shifts as presented in roberts dissertation.
rs.loc <- data.frame(year = as.factor(c(1970, 1985, 2000, 2015)),
                     lat = c(39, 39.5, 40, 40.5))


# Merge results and bbs route and other information -----------------------------------------------------------
# Import the discontinuity analysis results
gaps <- loadResultsDiscont()

# Join the results with the locations of the BBS routes
gaps <-
  left_join(gaps, bbsData.forAnalysis)


## Load the GRI 'constant power table'
pwr <-
  read_csv(here::here(
    "chapterFiles/discontinuityAnalysis/griConstantPowerTable.csv"
  ))

## Use linear apprxoimation to get richness for every integer between 20 and 300
pwr.approx <-
  approx(
    x = pwr$richness,
    y = pwr$threshold,
    xout = seq(from = 20, to = 300)
  ) %>%
  as_tibble() %>%
  rename(richness = x, powerConstant = y)


gaps.bbs <- gaps %>%
  group_by(countrynum, statenum, route, year) %>%
  ## Append species richenss to gap data
  mutate(richness = n_distinct(species)) %>% ungroup() %>%
  left_join(pwr.approx) %>%
  ungroup() %>%
  ## Add new column for GRI constant pwoer threshold level%>%
  mutate(isGap.powerConstant = ifelse(gap.percentile >= powerConstant, "yes", "no")) %>%
  mutate(isGap.percentile = ifelse(gap.percentile >= 0.90, "yes", "no"))


# Munge results -----------------------------------------------------------
all.gaps.bbs <- gaps.bbs %>%
  group_by(year, countrynum, statenum, route) %>%
  arrange(year, countrynum, statenum, route, log10.mass) %>%
  mutate(
    rank = 1:n(),
    edgeSpp  = ifelse(
      lag(isGap.percentile) == "yes" |
        isGap.percentile == "yes" ,
      "yes",
      "no"
    ),
    edgeSpp  = ifelse(
      log10.mass == min(log10.mass) |
        log10.mass == max(log10.mass),
      "yes" ,
      edgeSpp
    )
  ) %>%
  ungroup()

### Append a number for each aggregation
loc.ind <- unique(all.gaps.bbs$loc)
for (i in seq_along(loc.ind)) {
  if (i == 1)
    results <- NULL
  ## go along each country/state/route location
  temp1 <- all.gaps.bbs %>%
    filter(loc == loc.ind[i])
  
  year.ind <- unique(temp1$year)
  
  df.out <- NULL
  for (h in seq_along(year.ind)) {
    temp <-  temp1 %>%
      filter(year == year.ind[h])
    
    # setup for j loop
    x = temp$edgeSpp
    agg.vec  = rep(1, length.out = length(x))
    counter <- 1
    
    ## Create a vector of aggregation numbers for all rows in temp
    for (j in 2:length(x)) {
      # browser()
      agg.vec[j] <- counter
      
      if (j != length(x) &
          x[j] == "yes" &  x[j + 1] == "yes")
        counter <- counter + 1
      
      if (j == length(x))
        agg.vec[j] = counter  # ensure the last one takes on current counter...
    } # end j loop
    
    temp$aggNumber = agg.vec
    df.out <- bind_rows(temp, df.out)
  } # end h loop
  
  results <- bind_rows(results, df.out)
  
} # end i-loop
if (nrow(all.gaps.bbs) != nrow(results))
  stop("results arent same size as selectgapsbbs")

results.munged <- results %>%
  group_by(year, loc, aggNumber) %>%
  mutate(
    distEdge.left = abs(log10.mass - min(log10.mass)),
    distEdge.right = abs(log10.mass - max(log10.mass))
  ) %>%
  ungroup() %>%
  mutate(distEdge = ifelse(distEdge.left < distEdge.right, distEdge.left, distEdge.right)) %>%
  group_by(year, countrynum, statenum, route) %>%
  mutate(nAggs = n_distinct(aggNumber),
         nSpp  = n_distinct(scientificName)) %>%
  ungroup() %>%
  mutate(
    is.declining = ifelse(aou %in% decliningSpecies$aou, "yes", "no"),
    is.grassland = ifelse(aou %in% grassSpecies$aou, "yes", "no"),
    is.grassDeclining = ifelse((aou %in% decliningSpecies$aou &
                                  aou %in% grassSpecies$aou),
                               "yes",
                               "no"
    )
  ) %>%
  group_by(year, loc) %>% 
  mutate(distEdge.scaled = scale(distEdge, center=TRUE)) %>% 
  ungroup() %>% 
  mutate(regime="testing") %>% 
  mutate(
    regime = ifelse(year == rs.loc$year[1],  ifelse(lat <= rs.loc$lat[1], "South","North"),regime),
    regime = ifelse(year == rs.loc$year[2],  ifelse(lat <= rs.loc$lat[2], "South","North"),regime),
    regime = ifelse(year == rs.loc$year[3],  ifelse(lat <= rs.loc$lat[3], "South","North"),regime),
    regime = ifelse(year == rs.loc$year[4],  ifelse(lat <= rs.loc$lat[4], "South","North"),regime)
  ) %>% 
  group_by(loc) %>% 
  mutate(regimeShift = ifelse(n_distinct(regime)==1, "no","yes")) %>% 
  ungroup()

# View(results.munged %>% distinct(year, loc, regime) %>% arrange(year))
locs.keep <- results.munged %>% 
  distinct(loc, year) %>% 
  filter(n()>2)

results.munged <- results.munged %>% 
  filter(loc %in% locs.keep$loc)


# Visualize dist to edge  --------------------------------------------------------------
ggplot(results.munged)+
  geom_boxplot(aes(x=regime, y=distEdge.scaled))+
  facet_wrap(~year)

ggplot(results.munged)+
  geom_boxplot(aes(x=year, y=distEdge.scaled))+
  facet_wrap(~regime, ncol=1)

ggplot(results.munged)+
  geom_boxplot(aes(x=regime, y=distEdge.scaled))+
  facet_wrap(year~is.grassland, ncol=2)

ggplot(results.munged)+
  geom_boxplot(aes(x=regime, y=distEdge.scaled))+
  facet_wrap(year~is.declining, ncol=2)

ggplot(results.munged)+
  geom_boxplot(aes(x=regime, y=distEdge.scaled))+
  facet_wrap(year~nAggs, ncol=7)

ggplot(results.munged)+
  geom_boxplot(aes(x=is.grassland, y=distEdge.scaled))+
  facet_wrap(year~regime, ncol=2)

ggplot(results.munged)+
  geom_boxplot(aes(x=is.declining, y=distEdge.scaled))+
  facet_wrap(year~regime, ncol=2)

ggplot(results.munged)+
  geom_boxplot(aes(x=is.grassDeclining, y=distEdge.scaled))+
  facet_wrap(year~regime, ncol=2)



# Save tables to file for diss --------------------------------------------
## summary table to file for n routes per regime per year
saveTab(
  results.munged %>%  distinct(year, regime, loc) %>% group_by(year, regime) %>%  summarise(nLoc =
                                                                                       n()),
  fn = "nRtesPerRegimePerYear"
)

## summary of grassland and declinilng species in a table
saveTab(full_join(
  grassSpecies %>% mutate(id = "grass"),
  decliningSpecies %>% mutate(id = "declining")
),
fn = "grassDeclSppList")



# Correlation of nAggs with distEdge --------------------------------------
ggplot(results.munged)+
  geom_point(aes(x=nAggs, y=distEdge))+
  facet_wrap(year~regime, ncol=2)

# Munge DF for ANOVA ---------------------------------------------------------------
## ensure df has factors
vars <- c("year","regime","is.declining", "is.grassland", "is.grassDeclining","loc","commonName")
results.munged[vars] <- lapply(results.munged[vars], factor) 



# Visualize the scaled response  ---------------------------------------
ggplot(results.munged, aes(x=distEdge.scaled))+
  geom_histogram()

## Q-Q plot
ggplot(results.munged, aes(sample=distEdge.scaled))+
  geom_qq_line()+stat_qq()
  
ggplot(results.munged, aes(sample=distEdge.scaled, color=regime))+
  geom_qq_line()+stat_qq()

ggplot(results.munged, aes(sample=distEdge.scaled, color=is.grassland))+
  geom_qq_line()+stat_qq()

ggplot(results.munged, aes(sample=distEdge.scaled, color=is.declining))+
  geom_qq_line()+stat_qq()



# Check for overdispersion in response ------------------------------------
library(AER)
R> rd <- glm(scal ~ ., data = RecreationDemand, family = poisson)
R> dispersiontest(rd,trafo=1)

# Run ANOVA ---------------------------------------------------------------
library(nlme)
library(car)

## Question 1: Are declining and/or grassland species vulnerable to 'shifting regime' w.r.t. distance to edge?
model = lme(distEdge ~  is.declining + is.grassland + regime , 
            random = ~1|year,
            data=results.munged,
            method="REML")

Anova(model)





