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
  ## scale distanace to edge within each route-year
  group_by(year, loc) %>%
  mutate(distEdge.scaled = scale(distEdge, center = TRUE)) %>%
  ungroup() %>%
  mutate(regime = "testing") %>%
  mutate(
    regime = ifelse(
      year == rs.loc$year[1],
      ifelse(lat <= rs.loc$lat[1], "South", "North"),
      regime
    ),
    regime = ifelse(
      year == rs.loc$year[2],
      ifelse(lat <= rs.loc$lat[2], "South", "North"),
      regime
    ),
    regime = ifelse(
      year == rs.loc$year[3],
      ifelse(lat <= rs.loc$lat[3], "South", "North"),
      regime
    ),
    regime = ifelse(
      year == rs.loc$year[4],
      ifelse(lat <= rs.loc$lat[4], "South", "North"),
      regime
    )
  ) %>%
  group_by(loc) %>%
  mutate(regimeShift = ifelse(n_distinct(regime) == 1, "no", "yes")) %>%
  ungroup()

# View(results.munged %>% distinct(year, loc, regime) %>% arrange(year))
locs.keep <- results.munged %>%
  distinct(loc, year) %>%
  filter(n() > 2)

results.munged <- results.munged %>%
  filter(loc %in% locs.keep$loc)

## ensure df has factors
vars <-
  c("regime",
    "is.declining",
    "is.grassland",
    "is.grassDeclining",
    "loc",
    "commonName")
results.munged[vars] <- lapply(results.munged[vars], factor)

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




# Visualize the scaled response  ---------------------------------------
ggplot(results.munged, aes(x = distEdge.scaled)) +
  geom_histogram()

ggplot(results.munged, aes(y = distEdge.scaled)) +
  geom_boxplot()

## Q-Q plot
ggplot(results.munged, aes(sample = distEdge.scaled)) +
  geom_qq_line() + stat_qq() +
  xlab("standard normal quantiles") +
  ylab("data quantiles")

# ggplot(results.munged, aes(sample=distEdge.scaled, color=regime))+
#   geom_qq_line()+stat_qq()
# ggplot(results.munged, aes(sample=distEdge.scaled, color=is.grassland))+
#   geom_qq_line()+stat_qq()
# ggplot(results.munged, aes(sample=distEdge.scaled, color=is.declining))+
#   geom_qq_line()+stat_qq()


# Check for normality beyond QQ plots and Boxplots ------------------------
# Skewness
PerformanceAnalytics::skewness(results.munged$distEdge.scaled)
## 0.906 = right skew - moderate (0.5-1) to severe (>1) skewness.... troublesome
# Kurtosis
PerformanceAnalytics::kurtosis(results.munged$distEdge.scaled)
## 0.636 =  we cannot reach any conclusion about the kurtosis when it is betweeen -2 and 2. Excess kurtosis might be positive, negative, or zero....

# Transform the response var to achieve approximate normality ------------------------------------------------
hist(results.munged$distEdge)
hist(results.munged$distEdge.scaled)
hist((
  results.munged$distEdge.scaled + 1 - min(results.munged$distEdge.scaled)
) ^ (1 / 2))
hist(log2(
  results.munged$distEdge.scaled + 1 - min(results.munged$distEdge.scaled)
))
hist(log10(
  results.munged$distEdge.scaled + 1 - min(results.munged$distEdge.scaled)
))


interaction.plot(response = results.munged$distEdge.scaled,
                 results.munged$regime,
                 results.munged$year)
interaction.plot(response = results.munged$distEdge.scaled,
                 results.munged$year,
                 results.munged$regime)
interaction.plot(response = results.munged$distEdge.scaled,
                 results.munged$year,
                 results.munged$regimeShift)
interaction.plot(response = results.munged$distEdge,
                 results.munged$regime,
                 results.munged$is.grassland)
interaction.plot(response = results.munged$distEdge.scaled,
                 results.munged$regime,
                 results.munged$is.declining)


# Using `margins` package to visualize ------------------------------------

M.pairwise <- nlme::lme(
  distEdge.scaled ~
    regimeShift * is.declining +
    regime * is.grassland + regime * year ,
  random = ~ 1 | loc,
  data = results.munged,
  method = "REML"
)

summary(M.pairwise)
anova(M.pairwise)

plot(M.pairwise)


newdat <- expand.grid(
  regime = unique(results.munged$regime),
  is.grassland = unique(results.munged$is.grassland),
  is.declining = unique(results.munged$is.declining),
  year = unique(results.munged$year),
  distEdge.scaled = 0
)


# Visualize nlme::lme mixed model -----------------------------------------
(mm <- model.matrix(terms(M.pairwise), newdat))
# Calculate height based on the relevant effect sizes

newdat$distEdge <- mm %*% fixef(M.pairwise)

pvar.mm.distEdge <- diag(mm %*% tcrossprod(vcov(M.pairwise), mm))

# Add standard errors to the dataframe
(
  newdat <- data.frame(
    newdat,
    plo = newdat$distEdge - 1.96 * sqrt(pvar.mm.distEdge),
    phi = newdat$distEdge + 1.96 * sqrt(pvar.mm.distEdge)
  )
)




# Zurr protocol for mixed effects modelling -------------------------------
## from ZUrr et al. Mixed effects modelling book chapter 5

## Step 1a: Fit a linear regression of the fixed effects
M.lm <-
  lm(distEdge.scaled ~ regime * is.declining * is.grassland + regime * year,
     data = results.munged)
plot(M.lm, select = c(1))

## Step 1b. See if we can deal with heterogeneity in residuals
### Determine whether we need to include a term for loc (route)
E <- rstandard(M.lm)
boxplot(E ~ loc, data = results.munged, axes = FALSE)
abline(0, 0)
axis(2)
text(1:length(unique(results.munged$loc)), -2, levels(results.munged$loc))
#### since all the residuals overlap zero, we do not need to include the loc term
### Same for richness
E <- rstandard(M.lm)
boxplot(E ~ richness, data = results.munged, axes = FALSE)
abline(0, 0)
axis(2)
#### do not need to include richness

## Step 2: Fit model with GLS
Form <-
  formula(distEdge.scaled ~ is.declining * is.declining * is.grassland + regime *
            year)
# Form <- formula(distEdge.scaled ~ is.declining*is.grassland + regime*is.declining + regime*is.grassland + regime*year)

M.gls <- gls(Form, data = results.munged)

## Step 3: Choose variance structure
### It appears that richness and loc (route) do not need to be added as random effects...

## Step 4: Fit the lme model
M1.lme <-
  lme(Form,
      random = ~ 1 | loc,
      method = "REML",
      data = results.munged)

## Step 5: Compare old and new model
anova(M.gls, M1.lme)
## further, including loc as a random effect doesnt improve the gls model

### Fit the lme with year as random intercept
M2.lme <-
  lme(Form, ~ 1 | richness,   method = "REML", data = results.munged)
anova(M.gls, M2.lme)
## still, no need for a random intercept....

## Step 5b: REport model comparison as
anova(M.gls, M1.lme)
### L = 5.17x10^-6 (df=1, p=0.9982)

## Step 5c: define bevst model
M.best <- M.gls

## Step 6: Check residuaks of best model
E2 <- resid(M.best, type = "normalized")
F2 <- fitted(M.best)
op <- par(mfrow = c(2, 2))
plot(F2, E2)
boxplot(E2 ~ regime,
        main = "regime",
        ylab = "residuals",
        data = results.munged)
boxplot(E2 ~ is.declining,
        main = "is.declining",
        ylab = "residuals",
        data = results.munged)
boxplot(E2 ~ is.grassland,
        main = "is.grassland",
        ylab = "residuals",
        data = results.munged)
par(op)


## Step 7: Get optimal fixed structure
summary(M.best) ## look at the significance of the regression parameter and coefficient estiamtes
## Significant estiamtes are:
### is.declinling,  is.grassland1, is.declining1:is.grassland1
## See which interaction terms are significant
anova(M.best)
### regime:is.declining:is.grassland  & is.declining:is.grassland

### Step 7b: fit using ML by dropping insignificant interaction terms
regime:year
regime:is.declining
regime:is.grassland

M1.Full  <- gls(Form, method = "ML", data = results.munged)
M1.A <- update(M1.Full, . ~ . - regime:year) # drop regime:year
M1.B <- update(M1.Full, . ~ . - regime:is.declining)
M1.C <- update(M1.Full, . ~ . - regime:is.grassland)

### Step 7c: compare new models with M1.Full
anova(M1.Full, M1.A) ## this one is least sigificant of A,B,C -- so we will drop "regime:year: interaction term...
anova(M1.Full, M1.B)
anova(M1.Full, M1.C)

### Step 8: Build new model with one dropped interaction term...
Form2 <-
  formula(
    distEdge.scaled ~ is.declining * is.grassland + regime * is.declining + regime *
      is.grassland
  )
M2.Full <- gls(Form2, method = "ML", data = results.munged)

### Step 8b: drop the NS fixed terms and compare with full model
M2.A <- update(M2.Full, . ~ .-)