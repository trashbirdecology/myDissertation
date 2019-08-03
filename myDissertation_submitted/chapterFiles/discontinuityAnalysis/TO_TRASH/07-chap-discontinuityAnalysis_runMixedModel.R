library(ggplot2)


# Simple about dataset:
## a discontinuity analysis of avian body mass disitributions (using Barichievy et al. 2018 methods)
## testing the hypothesis of some collaborators re: 1) existence of "spatial regimes" and 2) that sensitive (here, grassland obligates and declining birds) birds are closer to the edge of a body mass distribution (i.e. LOWER `distEdge``) in areas undergoing 'regime shifts' (`regime`, `regimeShift`)

# Vars of interest/used in analysis
## `distEdge` = response of interest; a measure
## `distEdge.scaled`` = distEdge when scaled and centered when each `loc` and `year` combination
## `regime` = classified as South or North at each year `year` (by a paper in prep)
## `year` = year
## `is.declining`= LOGICAL. is the species declining (according to BBS pop trend estimates)
## `is.grassland`= LOGICAL. grassland obligate
## `loc` = bbs route location (countrynum_statenum_routenum)



# Other vars of interest
## `regimeShift` =  one of c(south, north, regimeShift). Did the location (`loc`; BBS route) undergo a 'regime shift' according to authors? If so, categorized as regimeShift. If not, categorized as either norht or south regime, corresponding to `regime`


# Load anlaysis data ------------------------------------------------------
dat <- readRDS(here::here("/chapterFiles/discontinuityAnalysis/anovaData.rds"))
glimpse(dat)



## more shit
ggplot(dat) +
  geom_boxplot(aes(x = regime, y = distEdge.scaled)) +
  facet_wrap( ~ year)

ggplot(dat) +
  geom_boxplot(aes(x = regimeShift, y = distEdge.scaled)) +
  facet_wrap( ~ year)

ggplot(dat) +
  geom_boxplot(aes(x = regime, y = distEdge.scaled)) +
  facet_wrap(year ~ is.grassland, ncol = 2)

ggplot(dat) +
  geom_boxplot(aes(x = regimeShift, y = distEdge.scaled)) +
  facet_wrap(year ~ is.grassland, ncol = 4)


# Transform the response var to achieve approximate normality ------------------------------------------------
hist(dat$distEdge)
hist(dat$distEdge.scaled)
hist((dat$distEdge.scaled + 1 - min(dat$distEdge.scaled)) ^ (1 / 2))
hist(log2(dat$distEdge.scaled + 1 - min(dat$distEdge.scaled)))
hist(log10(dat$distEdge.scaled + 1 - min(dat$distEdge.scaled)))

## i dont like any of these...

# Check for normality beyond QQ plots and Boxplots ------------------------
# Skewness
PerformanceAnalytics::skewness(dat$distEdge.scaled)
## 0.766 = right skew - moderate (0.5-1) to severe (>1) skewness.... troublesome
# Kurtosis
PerformanceAnalytics::kurtosis(dat$distEdge.scaled)
## 0.117 =  we cannot reach any conclusion about the kurtosis when it is betweeen -2 and 2. Excess kurtosis might be positive, negative, or zero....




## Build a mixed model using gls  to try to account for temporal autocorrelation
# M.gls <- nlme::gls(distEdge.scaled ~ regime*is.declining*is.grassland + regime*year ,
#                    # assuming compound symmetry over time
#                    correlation=corCompSymm(form=~year), ## this term specifies that the order of the data is by year variable..
#                    data=dat,
#                    method="REML")

### Compare this to the M.mixed
# anova(M.mixed, M.gls)
## this did not improve the model...
