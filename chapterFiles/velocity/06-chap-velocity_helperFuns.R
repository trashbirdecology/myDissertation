######### FUNCTIONS FOR 2-species and paleo examples #######
# Save figures to file ----------------------------------------------------
saveFig <- function(p=NULL, fn=NULL, dir=here::here("chapterFiles/velocity/tempFigures/"), dev=".png",
                    width=6, height=4.5){
  if(is.null(p))warning("no p specified. not printing plot.")
  ggsave(plot = p, filename=paste0(dir,fn, dev), width=width, height=height)
}


# Figure themes -----------------------------------------------------------

theme_Publication <- function(base_size=12, base_family="Helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(size=14, angle=90,vjust =2),
            axis.title.x = element_text(size=14, vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            # legend.key.size= unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="italic",size=16),
            legend.text.align = 0,
            plot.margin=unit(c(1,1,1,10),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))

}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# Approx results fun ------------------------------------------------------
approx_dist.sim <- function(t.mult){
  
  dist<-list()
  x_1 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean = 100, sd = 5, n=50))
  x_2 = c(rnorm(mean =25, sd = 5, n=50), rnorm(mean = 50, sd = 5, n=50))
  t = 1:length(x_1)
  dist[[1]] <- data.frame(t, x_1, x_2) %>% 
    gather(key="variable", value="value", -t)
  
  x_1 = approx(t, x_1, n = t.mult*length(t))
  x_2 = approx(t, x_2, n = t.mult*length(t))
  t=x_1$x
  
  df.wide = data.frame(t, x_1$y, x_2$y)
  
  df  = data.frame(t, x_1, x_2) %>% 
    tidyr::gather(key = "variable", value = "value", -t) 
  dist[[2]] <- df %>% 
    mutate(cellID = 1) %>% 
    rename(sortVar = t) %>% 
    regimeDetectionMeasures::calculate_distanceTravelled() %>% 
    dplyr::select(-cellID) %>% 
    rename(t = sortVar)
  
  return(dist)
}


######### FUNCTIONS FOR THE TANH example #######
# Geenrate data -----------------------------------------------------------
### generate a 2-var systems using smooth shift 
generateData <- function(n = 100, #length of time series
                         tregime = .5 * n,
                         sd.perc_1a = 0.05,
                         sd.perc_1b = 0.05,
                         sd.perc_2a = 0.05,
                         sd.perc_2b = 0.05,
                         mu_1a = NULL,
                         mu_1b = NULL,
                         mu_2a = NULL,
                         mu_2b = NULL,
                         alpha = 1){
  
  t = 1:n
  # browser()
  x_1_true <-
    kFun(
      t = t,
      alpha = alpha,
        k1 = mu_1a,
      k2 = mu_1b,
      tregime = tregime
    )
  x_2_true <-
    kFun(
      t = t,
      alpha = alpha,
      k1 = mu_2a,
      k2 = mu_2b,
      tregime = tregime
    )
  
  x_1_err <- c(
    rnorm(
      mean = 0,
      sd = mu_1a * sd.perc_1a,
      n = tregime
    ),
    rnorm(
      mean = 0,
      sd = mu_1b * sd.perc_1b,
      n = tregime
    )
  )
  x_2_err <- c(
    rnorm(
      mean = 0,
      sd = mu_2a * sd.perc_2a,
      n = tregime
    ),
    rnorm(
      mean = 0,
      sd = mu_2b * sd.perc_2b,
      n = tregime
    )
  )
  
  x_1_obs <- x_1_true + x_1_err
  x_2_obs <- x_2_true + x_2_err
  
  dat.out <- tibble(
    time = t,
    x1_true = x_1_true,
    x2_true = x_2_true,
    x1_obs = x_1_obs,
    x2_obs = x_2_obs
  )
  
  return(dat.out)
}


# kFun - smooth the abundance shift ---------------------------------------

## smoothing function for abundance shifts in two species
kFun <- function(t, alpha = alpha, k1, k2, tregime) {
  # k1 = the value before the shift and k2= after
  #tregime = time of regime shift
  k1 - 0.5 * (k1 - k2) * (tanh(alpha * (t - tregime)) + 1)
}


# Calculate distance and dsdst --------------------------------------------
### Calculate teh distnace stuff
calcDist <- function(dat, 
                     fn = NULL, ## fn index ued for saving plot
                     tvdiff.iter,# = 100,
                     tvdiff.alpha# = 10 
){
  
  dx.ind = dat %>% arrange(time) %>% 
    summarise(dt=mean(time-lag(time), na.rm=TRUE)) %>% 
    as.numeric()
  
  
  dist <- dat %>%
    arrange(time) %>%
    ## calc dist of observed data
    mutate(
      dx1.obs = x1_obs - lag(x1_obs),
      dx2.obs = x2_obs - lag(x2_obs),
      ds.obs = sqrt(dx1.obs ^ 2 + dx2.obs ^ 2),
      ds.obs = replace_na(ds.obs, 0),
      s.obs = cumsum(ds.obs),
      dsdt.obs = ((s.obs) - lag(s.obs)) / (time-lag(time))) %>%
    # Use tvdiff to get deriv
    mutate(dsdt.tvdiff =
             TVRegDiffR(
               data = s.obs,
               iter = tvdiff.iter,
               alph = tvdiff.alpha,
               scale = "small",
               ep = 1e-6,
               dx = dx.ind ## this may need to be updated if the ts is not sampled at REGULAR INTERVALS
             )[-1]) 
  # browser()
  dist2 <- dist %>% 
    ## temporarily add on a value for dt
    ## calc dist of true data
    mutate(
      dx1.true = x1_true - lag(x1_true),
      dx2.true = x2_true - lag(x2_true),
      ds.true = sqrt(dx1.true ^ 2 + dx2.true ^ 2),
      ds.true = replace_na(ds.true, 0),
      s.true = cumsum(ds.true),
      dsdt.true = ((s.true) - lag(s.true)) / (time-lag(time))) %>%
    # numerical differentiation
    mutate(dt = lead(time) - time,
           s.pred = s.obs[1] + cumsum(dsdt.tvdiff*dt)) %>% 
    dplyr::select(-dt)

  ## optional plots for debug/sensitivity
     p <- ggplot(dist2)+
        geom_line(aes(time,dsdt.true, color="black"), show.legend=TRUE)+
        geom_line(aes(time,dsdt.tvdiff, color="grey50"), linetype=1, show.legend=TRUE)+
        geom_point(aes(time,dsdt.obs, color="red"), alpha=.7, show.legend=TRUE)+
        labs(color="",linetype="", xlab=expression(frac(Delta * "s", Delta * "t")))+
        scale_color_manual(values= c("black", "grey50","red"),  
                           labels = c('True', "Numerical differentiation",'Observed'))+
       geom_text(aes(label = paste0("alpha==",tvdiff.alpha),y = .75*max(dist2$dsdt.true, na.rm=TRUE), 
                     x =  .15*max(dist2$time, na.rm=TRUE)), 
                 parse = T, colour = "black", size = 4)
      saveFig(p,paste0("compareTvdiff_dsdt_",fn))
      
      # browser()
      p2 <-ggplot(dist2)+
        # geom_line(aes(time,s.true, color="black"), show.legend=TRUE, width=1)+
        geom_line(aes(time,s.obs, color="grey50"),  show.legend=TRUE, width=1)+
        geom_line(aes(time,s.pred, color="red"), alpha=.5, show.legend=TRUE, linetype=2, width=1)+
        scale_color_manual(values= c("black", "grey50", "red"),  
                           labels = c('True', 'Observed', 'Pred (tvdiff)'))+
        labs(color="",linetype="")+
        geom_text(aes(label = paste0("alpha==",tvdiff.alpha),y = .75*max(dist2$s.pred, na.rm=TRUE), 
                      x =  .15*max(dist2$time, na.rm=TRUE)), 
                  parse = T, colour = "black", size = 4)+
        ylab("s")
     p2
     
     saveFig(p2,paste0("compareTvdiff_s_",fn))
     
     return(list(dist=dist, dist2=dist2))
  
}
