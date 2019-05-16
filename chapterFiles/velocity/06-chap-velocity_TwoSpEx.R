library(cowplot)
library(tidyverse)
library(tvdiff)
## source helper funs
source(here::here("chapterFiles/velocity/06-chap-velocity_helperFuns.R"))

theme_set(theme_Publication())


######### Individaul runs ##############
# Set parameters for getting and runng data -----------------------------------------------------------
# paramters for generating data
n = 100 ## length of the time series
tregime = .5 * n # location of shift
sd.perc_1a = 0.05 # percent of the mean will determine the sd_1a ... sd_2b
sd.perc_1b = 0.05
sd.perc_2a = 0.05 # percent of the mean will determine the sd_1a ... sd_2b
sd.perc_2b = 0.05
mu_1a = 10
mu_1b = 55
mu_2a = 15
mu_2b = 44
# alpha = 1 # this is the alpha value for hte TANH
alpha.vec = c(0.1, 0.5, 0.75, 1.0)

# paratmers for TVREGDIFF
tvdiff.iter = 10000
tvdiff.alpha = 1000

tvdiff.iter.vec <- c(100, 1000,10000)
tvdiff.alpha.vec <- c(1, 10, 100, 1000, 10000)

for(j in seq_along(tv.diff.alpha.vec))
  for(k in seq_along(tv.diff.iter.vec))
        tvdiff.alpha <- tv.diff.alpha.vec[j]
        tvdiff.iter <- tv.diff.iter.vec[k]
## Run for-loop over vector of alpha to pritn plots to file @ tempFigures
for(i in seq_along(alpha.vec)){ 
  # Get alpha (alpha is used as the smoothing param for the hyperbolic tan)
   alpha = alpha.vec[i]
  

# Create  a file name for printing figures to file -------------------------
fn1 <-
  paste(mu_1a,
    mu_1b,
    mu_2a,
    mu_2b,
    sd.perc_1a,
    sd.perc_1b,
    sd.perc_2a,
    sd.perc_2b,
    tvdiff.alpha,
    tvdiff.iter,
        sep = "-")
fn <- paste0("changeMeans_","alpha",alpha, "-",fn1)



# GEnerate the data -------------------------------------------------------
## Get the sim data
ts <- generateData(
  n = n,
  tregime = tregime,
  sd.perc_1a = sd.perc_1a,
  sd.perc_1b = sd.perc_1b,
  sd.perc_2a = sd.perc_2a,
  sd.perc_2b = sd.perc_2b,
  mu_1a = mu_1a,
  mu_1b = mu_1b,
  mu_2b = mu_2b,
  mu_2a = mu_2a,
  alpha = alpha
)

# Calculate distane -------------------------------------------------------
results <- calcDist(dat = ts, 
                    tvdiff.iter = tvdiff.iter,
                    tvdiff.alpha =   tvdiff.alpha
)

# Visualize ---------------------------------------------------------------
# plot observedd data
labs <-  c(
  expression(x[1] * " observed"),
  expression(x[1] * " true"),
  expression(x[2] * " observed"),
  expression(x[2] * " true")
)

## Plot observed and true data
results.long <-
  results %>% gather(key = "key",
                     value = "y",
                     -time,
                     -dx1,
                     -dx2,
                     -ds,
                     -s.obs,
                     -dsdt, 
                     -dsdt.tvdiff)
p.orig <- ggplot(data = results.long, aes(x = time, y = y, color = key)) +
  geom_line() +
  geom_vline(
    aes(xintercept = tregime),
    linetype = 3,
    color = "red",
    show.legend = FALSE
  ) +
  labs(x = "time", y = "values") +
  scale_color_manual(
    values = c("black", "darkblue", "grey40", "darkblue"),
    name = "",
    labels = labs
  ) +
  guides(color = guide_legend(override.aes = list(size = 4), ncol = 2))

# p.orig

saveFig(p.orig, fn = paste0(fn,"_origDat"))


# plot observedd data
labs <-  c(
  expression(x[1] * " observed"),
  expression(x[1] * " true"),
  expression(x[2] * " observed"),
  expression(x[2] * " true")
)

results.long <-
  results %>% gather(key = "key",
                     value = "y",
                     -time,
                     -x1_true,
                     -x2_true,
                     -x1_obs,
                     -x2_obs) %>%
  mutate(y = replace_na(y, 0))

## Plot dsdt of observed data
p.dsdt <- ggplot(data = results.long %>% filter(key == "dsdt")) +
  geom_line(
    aes(x = time, y = y), size=.75
  ) +
  geom_vline(
    aes(xintercept = tregime),
    linetype = 3,
    color = "red",
    show.legend = FALSE
  ) +
  labs(x = "time", y = expression(frac(Delta * "s", Delta * "t")))

# p.dsdt
# saveFig(p.dsdt, fn = paste0(fn,"_dsdt"))

## Plot dsdt of observed data USING TVDIFF
p.dsdt.tvdiff <- ggplot(data = results.long %>% filter(key == "dsdt.tvdiff")) +
  geom_line(
    aes(x = time, y = y), size=.75
  ) +
  geom_vline(
    aes(xintercept = tregime),
    linetype = 3,
    color = "red",
    show.legend = FALSE
  ) +
  labs(x = "time", y = expression(frac(Delta * "s", Delta * "t")*" tvdiff"))

# p.dsdt.tvdiff
# saveFig(p.dsdt.tvdiff, fn = paste0(fn,"_dsdttvdiff"))


## plot smoothed dsdst
p.dsdt.smooth <- ggplot(data = results.long %>% filter(key == "dsdt")) +
  geom_line(
    aes(x = time, y = y), size=.75
  ) +
  geom_smooth(method = "auto",
    aes(x = time, y = y), size=.75
  ) +
  geom_vline(
    aes(xintercept = tregime),
    linetype = 3,
    color = "red",
    show.legend = FALSE
  ) +
  labs(x = "time", y = expression(frac(Delta * "s", Delta * "t")))

# p.dsdt.smooth

# saveFig(p.dsdt.smooth, fn = paste0(fn,"_dsdt_smoothed"))


## Plot dsdt of observed data
p.s.obs <- ggplot(data = results.long %>% filter(key == "s.obs")) +
  geom_line(aes(x = time, y = y, size=.75), 
    size=1
  ) +
  geom_vline(
    aes(xintercept = tregime),
    linetype = 3,
    color = "red",
    show.legend = FALSE
  ) +
  labs(x = "time", y = "s")
# p.s.obs

p <- cowplot::plot_grid(p.s.obs+xlab(""), 
                   p.dsdt, ncol = 1, 
                   align="hv", axis="tblr",
                   labels = "AUTO"
                   )
# saveFig(p, fn = paste0(fn,"_stack2"))


## stack all three..
p.stack <- cowplot::plot_grid(
                        p.orig + theme(legend.position="none")+xlab(""),
                        p.s.obs+xlab(""), 
                        p.dsdt, ncol = 1, 
                        align="hv", axis="tblr",
                        labels = "AUTO"
)

# saveFig(p.stack, fn = paste0(fn,"_stack"))


# p.stack plus a smooth on dsdt..
p.stack.smooth <- cowplot::plot_grid(
    p.orig + theme(legend.position="none")+xlab(""),
    p.s.obs+xlab(""), 
    p.dsdt.smooth, 
    ncol = 1, 
    align="hv", axis="tblr",
    labels = "AUTO")

# saveFig(p.stack.smooth, fn = paste0(fn,"_stack.smooth"))


# p.stack plus tvdiff dsdt
p.stack.tvdiff <- cowplot::plot_grid(
  p.orig + theme(legend.position="none")+xlab(""),
  p.s.obs+xlab(""), 
  p.dsdt+xlab(""),
  p.dsdt.tvdiff, 
  ncol = 1, 
  align="hv", axis="tblr",hjust = -0.5,
  labels = "AUTO")
# p.stack.tvdiff

saveFig(p.stack.tvdiff, fn = paste0(fn,"_stack.tvdiff"))

print(paste("finish i-loop", i, " of ", length(alpha.vec)))
}

# END RUN -----------------------------------------------------------------


