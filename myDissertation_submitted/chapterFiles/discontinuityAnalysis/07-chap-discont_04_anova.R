# Init -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(ggthemes)


# META -------------------------------------------------------------------
# About dataset:
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


# 0. Load anlaysis data ------------------------------------------------------
dat <-
  readRDS(here::here(
    "/chapterFiles/discontinuityAnalysis/results/datForAnova.RDS"
  )) %>% 
  mutate(regimeShift = as.factor(regimeShift)) %>% 
  mutate(regime = as.factor(regime)) %>%
  # center teh year variable...
  mutate(year.center = year - round(mean(year)))%>% 
  mutate(
    sppGroup = "allOthers",
    sppGroup = ifelse(is.declining=="yes","declining",sppGroup),
    sppGroup = ifelse(is.grassland=="yes","grassObli",sppGroup),
    sppGroup = as.factor(ifelse(is.grassDeclining=="yes","grassDeclining", sppGroup) )
  ) %>%
# arrange data frame prior to analysis to account for year as a factor and as a rep. measure
  arrange(loc, aou, year)

dat$year.center %>% range()
dat$year        %>% range()

# 1. Munge analysis data -------------------------------------------------
# Reorder factor levels for easy interp
if(levels(dat$regime)[1]=="North") dat$regime = factor(dat$regime,levels(dat$regime)[c(2,1)])
if(!levels(dat$regimeShift)[1]=="no") dat$regimeShift = factor(dat$regimeShift,levels(dat$regimeShift)[c(2,1)])
levels(dat$regimeShift)
levels(dat$regime)

# 2. Source functions --------------------------------------------------------
to.source <- list.files(
  here::here("/chapterFiles/discontinuityAnalysis/"),
  pattern = "helper",
  full.names = TRUE
)
for (i in 1:length(to.source))source(to.source[i])

# 3. Define, create directories -------------------------------------
## discontinuity results location
resultsDir <-
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

################################################################################
## A. DATA EXPLORATION of  response variable
################################################################################
# 4. Check for normality beyond QQ plots and Boxplots ------------------------
# Skewness
PerformanceAnalytics::skewness(dat$distEdge.scaled)
## 0.752 = right skew - moderate (0.5-1) to severe (>1) skewness.... troublesome
# Kurtosis
PerformanceAnalytics::kurtosis(dat$distEdge.scaled)
## 0.177 =  we cannot reach any conclusion about the kurtosis when it is betweeen -2 and 2. Excess kurtosis might be positive, negative, or zero....

# 5. Visualize the original and scaled response  ---------------------------------------
h1 <- ggplot(dat, aes(x = distEdge)) +
  geom_histogram()
h2 <- ggplot(dat, aes(x = distEdge.scaled)) +
  geom_histogram()
cowplot::plot_grid(h1, h2)

b1 <- ggplot(dat, aes(y = distEdge)) +
  geom_boxplot()
b2 <- ggplot(dat, aes(y = distEdge.scaled)) +
  geom_boxplot()

cowplot::plot_grid(b1, b2)

## Q-Q plot of orig dat
ggplot(dat, aes(sample = distEdge)) +
  geom_qq_line() + stat_qq() +
  xlab("standard normal quantiles") +
  ylab("data quantiles")
## shit
ggplot(dat, aes(sample = distEdge.scaled)) +
  geom_qq_line() + stat_qq() +
  xlab("standard normal quantiles") +
  ylab("data quantiles")
## better but not amazing



# 6. Visualize dist to edge  --------------------------------------------------------------
## Set year ind for simple plotting 
year.ind <- c(1970, 1985, 2000, 2015)
## Reduced dataset for simpler plotting
dat2 <- dat %>% 
  filter(year %in% year.ind) 
## Determine whether we need random intercepts/slopes for loc and aou
ggplot(dat2) +
  geom_boxplot(aes(x = loc, y = distEdge.scaled)) +
  facet_wrap(~year  , ncol = 2)+
  xlab("NABBS route")+ ylab("scaled distance to edge")+
  theme_Publication()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())## eh.. not too much difference among locations....
saveFig(last_plot(), dir=figDir, fn="randEffect_loc")

ggplot(dat2) +
  geom_boxplot(aes(x = as.factor(aou), y = distEdge.scaled)) +
  facet_wrap(~year  , ncol = 2)+
  xlab("species")+ ylab("scaled distance to edge")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme_publication()
## eh.. not too much difference among locations....
saveFig(last_plot(), dir=figDir, fn="randEffect_aou")
  ## definitely need to account for AOU/species ID

ggplot(dat2) +
  geom_boxplot(aes(x = sppGroup, y = distEdge.scaled, color = regime)) +
  facet_wrap(~year  , ncol = 2)+
  xlab("species group")+ ylab("scaled distance to edge")+
  theme(axis.text.x = element_text(angle = 90))+
  ggthemes::scale_color_colorblind()
saveFig(last_plot(), dir=figDir, fn="randEffect_sppGroupByRegime")



ggplot(dat2 %>% group_by(year, loc, sppGroup) %>% summarise(y = mean(distEdge.scaled)))+ 
  geom_line(aes(x=year, y =y, group=loc), show.legend=F)+
  facet_wrap(~sppGroup)

ggplot(dat2 %>% group_by(year, sppGroup, regime) %>% summarise(y = mean(distEdge.scaled)))+ 
  geom_line(aes(x=year, y =y,group= regime), show.legend=F)+
  facet_wrap(~sppGroup)

ggplot(dat2 %>% group_by(year, aou, sppGroup) %>% summarise(y = mean(distEdge.scaled)))+ 
  geom_line(aes(x=year, y =y, group=aou), show.legend=F)+
  facet_wrap(~sppGroup)



# 7. Correlation of nAggs with distEdge --------------------------------------
ggplot(dat2, aes(x = nAggs, y = distEdge)) +
  geom_point() +
  facet_wrap(year ~ regime, ncol = 2)+
  geom_smooth(method="lm", se = T)
## unsurprisingly dist edge decreases with # aggs. so that's a good sign.
    ## it is NOT ery correlated swith disstEdge.scaled, though!
ggplot(dat2, aes(x = nAggs, y = distEdge.scaled)) +
  geom_point() +
  facet_wrap(year ~ regime, ncol = 2)+
  geom_smooth(method="lm", se = T)


# 8. Interaction plots -------------------------------------------------------
## Nothing too interesting here..
interaction.plot(response = dat2$distEdge.scaled,
                 dat2$regime,
                 dat2$year)
interaction.plot(response = dat2$distEdge.scaled,
                 dat2$year,
                 dat2$regime)
interaction.plot(response = dat2$distEdge.scaled,
                 dat2$regime,
                 dat2$is.grassland)
## Below, however, we see something goin gon...
saveFig(interaction.plot(response = dat2$distEdge.scaled,
                         dat2$year,
                         dat2$regimeShift, xlab = "Year", ylab="Mean distance-to-edge",
                         trace.label = "Regime\n   shift",  # label for legend
                         xpd = FALSE) ,#,  # 'clip' legend at border
        fn="intrxnRegimeShift", dir=figDir)


saveFig(interaction.plot(response = dat2$distEdge.scaled,
                         dat2$regimeShift,
                         dat2$is.declining,
                         fun = "mean",
                         xlab="Regime shift",
                         ylab="Mean distance-to-edge",
                         trace.label = "Declining\n   species",  # label for legend
                         xpd = FALSE) ,#,  # 'clip' legend at border
        fn="intrxnRegimeShiftDeclinSpp", dir=figDir)


################################################################################
## REGIME SHIFT lcoations according to roberts et al
################################################################################
# 10. Roberts et al regime shift locations -----------------------------------------------------------------------
## these are the locations and years of the regime shifts as presented in roberts dissertation.
rs.loc <- data.frame(year = as.factor(c(1970, 1985, 2000, 2015)),
                     lat = c(39, 39.5, 40, 40.5))


################################################################################
## ANOVA time
################################################################################
# 11. Ensure factors and best level order for interpretation purposes ----------------------------------------------
dat <- dat %>% 
  mutate(
    sppGroup = "allOthers",
    sppGroup = ifelse(is.declining=="yes","declining",sppGroup),
    sppGroup = ifelse(is.grassland=="yes","grassObli",sppGroup),
    sppGroup = as.factor(ifelse(is.grassDeclining=="yes","grassDeclining", sppGroup) )
  ) 

levels(dat$regime)
levels(dat$is.declining)
levels(dat$is.grassland)
levels(dat$is.grassDeclining)
levels(dat$sppGroup)


# 12. Run ANOVA ---------------------------------------------------------------
library(nlme)
# Set contrasts
options(contrasts = c("contr.sum", "contr.poly")) ## type 3


# Model wtih year as random slope 
M.mixed <-
  nlme::lme(
    distEdge.scaled ~ year.center*regime + regime*sppGroup ,
    random = ~ 1 | loc/aou,  
    correlation = corAR1(form = ~ 1 | loc / aou ),
    data = dat,
    method = "REML")

# Save the model to file
saveRDS(M.mixed,file = paste0(resultsDir, "/mixedModelResults.RDS"))

# Save the data used to feed the model, too
saveRDS(dat,file = paste0(resultsDir, "/mixedModel-dataUsed.RDS"))

summary(M.mixed)
M.mixed

levels(dat$sppGroup)
(t = intervals(M.mixed, which="fixed")$fixed %>% as.data.frame())

r2 <- row.names(M.mixed$contrasts$regime)[2]
spg <- row.names(M.mixed$contrasts$sppGroup) %>% str_to_title()
spg  = str_replace(spg, "Grassdeclining","Declining Grassland Obligates")
spg  = str_replace(spg, "Grassobli","Grassland Obligates")


row.names(t) = c(
  "(Intercept)",
  "Year",
  r2,
  spg[2],
  spg[3],
  spg[4],
  paste0("Year x ",  spg[2]),  
  paste0(r2, " x ", spg[2]),  
  paste0(r2, " x ", spg[3]),  
  paste0(r2," x ",  spg[4])  
  )

t %>% round(2)

# Save results to fiel 
saveTab(tab=M.mixed$coefficients, dir=tabDir, fn="lme_coefs")
saveTab(tab=t %>% round(2), dir=tabDir, fn="lme_varFix")


# Pretty visualizations?-----------------------------------------------------------------


# box-plots of residuals by loc
plot(M.mixed, loc ~ resid(.))
# observed versus fitted values by loc
plot(M.mixed, distEdge.scaled ~ fitted(.) | loc, abline = c(0,1))

saveFig(plot_model(M.mixed)+theme_Publication(), "simpleCoefEst", figDir)



# PLot body mass historgrams ----------------------------------------------

p<- ggplot(dat %>% distinct(year, aou, .keep_all = T), aes(x=log10.mass, y=sppGroup, fill=sppGroup))+
  ggridges::geom_density_ridges(scale=6, show.legend = FALSE)+#, jittered_points=T)+
scale_fill_colorblind()+
  theme_Publication()+
  scale_y_discrete(
    name="", 
    breaks=c(paste(levels(dat$sppGroup))),
    labels=c("Other",
          "Declining",
          "Declining \nGrassland\nObligates",
          "Grassland\nObligates"
        )
  )+
  # ylab("")
  xlab("log body mass")
p
saveFig(p,"bmDistPerSppGroup", figDir)

p2<- ggplot(dat %>% distinct(year, aou, regime, .keep_all = T), aes(x=log10.mass, y=sppGroup, fill=sppGroup))+
  ggridges::geom_density_ridges(scale=6, show.legend = FALSE)+#, jittered_points=T)+
  scale_fill_colorblind()+
  theme_Publication()+
  scale_y_discrete(
    name="", 
    breaks=c(paste(levels(dat$sppGroup))),
    labels=c("Other",
             "Declining",
             "Declining \nGrassland\nObligates",
             "Grassland\nObligates"
    )
  )+
  # ylab("")
  xlab("log body mass")+facet_wrap(~regime)
p2
saveFig(p2,"bmDistPerSppGroup-byRegime", figDir)


bcr.labs <- c(`11` = "Prairie Potholes", `22` = "Eastern Tallgrass Prairie", `19` = "Central Mixed Grass", 
              `18` = "Shortgrass Prairie", `17`= "Badlands & Prairies")

p3<- ggplot(dat %>% distinct(year, aou, bcr, .keep_all = T), aes(x=log10.mass, y=sppGroup, fill=sppGroup))+
  ggridges::geom_density_ridges(scale=6, show.legend = FALSE)+#, jittered_points=T)+
  scale_fill_colorblind()+
  theme_Publication()+
  scale_y_discrete(
    name="", 
    breaks=c(paste(levels(dat$sppGroup))),
    labels=c("Other",
             "Declining",
             "Declining Grassland Obligates",
             "Grassland Obligates"
    )
  )+
  xlab("log body mass")+
  facet_wrap(~bcr, labeller=labeller(bcr=bcr.labs), ncol=2)

p3
saveFig(p3,"bmDistPerSppGroup-byBCR", figDir)




# RE-RUN Anova with BCR instead of location -------------------------------

df <- dat %>% group_by(year,bcr) %>% 
  distinct(year.center, bcr, aou, regime, sppGroup, log10.mass, distEdge.scaled)

M.mixed.BCR <-
  nlme::lme(
    distEdge.scaled ~ year.center*regime + regime*sppGroup,
    random = ~ 1 | bcr/aou,  
    correlation = corAR1(form = ~ 1 | bcr / aou ),
    data = df,
    method = "REML")


(t = intervals(M.mixed.BCR, which="fixed")$fixed %>% as.data.frame())
r2 <- row.names(M.mixed.BCR$contrasts$regime)[2]
spg <- row.names(M.mixed.BCR$contrasts$sppGroup) %>% str_to_title()
spg  = str_replace(spg, "Grassdeclining","Declining Grassland Obligates")
spg  = str_replace(spg, "Grassobli","Grassland Obligates")


row.names(t) = c(
  "(Intercept)",
  "Year",
  r2,
  spg[2],
  spg[3],
  spg[4],
  paste0("Year x ",  spg[2]),  
  paste0(r2, " x ", spg[2]),  
  paste0(r2, " x ", spg[3]),  
  paste0(r2," x ",  spg[4])  
)

# Save results to fiel 
saveTab(tab=t %>% round(2), dir=tabDir, fn="lme_varFix-byBCR")


# Save the model to file
saveRDS(M.mixed.BCR,file = paste0(resultsDir, "/mixedModelResults-byBCR.RDS"))
