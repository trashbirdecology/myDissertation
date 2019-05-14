# Define plotting theme ---------------------------------------------------
theme_mine <- function(base_size = 12,
                       base_family = "Times") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 9),
      strip.text.y = element_text(size = 9),
      # axis.text.x = element_text(size=11),
      # axis.text.y = element_text(size=11,hjust=1.5),
      # axis.Cticks =  element_line(colour = "black"),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 12, angle = 90),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.spacing = unit(1.0, "lines"),
      plot.background = element_blank(),
      # axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color = "black", size = 1),
      plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines")
    )
}

# Load and munge summary results ------------------------------------------
loadSummaryResults <- function(dataDir,
                               distance = TRUE,
                               fivi = TRUE,
                               ews = TRUE) {
  results <- list()
  
  if (distance)
    results$distances <-
      purrr::map_df(list.files(dataDir, full.names = TRUE, pattern = "distances"),
                    read_feather) %>% 
              mutate(method=as.factor(method), 
                     prob=as.factor(prob))
  
  if (fivi)
    results$fivi <-
      purrr::map_df(list.files(dataDir, full.names = TRUE, pattern = "fivi"),
                    read_feather) %>% 
      mutate(method=as.factor(method), 
             prob=as.factor(prob))
  
  if (ews)
    results$ews <-
      purrr::map_df(list.files(dataDir, full.names = TRUE, pattern = "ews"),
                    read_feather)
  
  return(results)
}


# Indexing metrics and labels for plotting --------------------------------

setLabels <- function(metrics.to.plot, results) {
  myLabels <- list()
  
  cols.ignore <-
    c(
      "variable",
      "method",
      'prob',
      'winStart',
      'winStop',
      'winMove',
      'time',
      'skewMean.mean',
      'skewMean.sd'
    )
  
  if (any(c("distance", "distances", "dist") %in% metrics.to.plot)) {
    myLabels$distances <-
      names(results$distance)[!names(results$distance) %in% cols.ignore]
  }
  
  
  if ("fivi" %in% metrics.to.plot) {
    myLabels$fivi <-
      names(results$fivi)[!names(results$fivi) %in% cols.ignore]
  }
  
  if ("ews" %in% metrics.to.plot) {
    myLabels$ews <-
      names(results$ews)[!names(results$ews) %in% cols.ignore]
  }
  
  ## Remove all the .mean and .sd labels and return only the metrics, then keep distinct
  
  for (i in seq_along(myLabels)) {
    myLabels[[i]] <- gsub("\\.mean", "", myLabels[[i]])
    myLabels[[i]] <- gsub("\\.sd", "", myLabels[[i]])
    myLabels[[i]] <- myLabels[[i]] %>% unique()
    
  }

  return(myLabels)
   
  
}




# plot.bootstrappedFacetGroup ---------------------------------------------
plot.bootstrappedFacetGroup <- function(df,
                                        metric.ind,
                                        method.filter,
                                        n.col = 1,
                                        # number of facet columns
                                        savePlot = TRUE,
                                        preview = TRUE,
                                        baseline.prob = c(1, 100),
                                        add.baseline = TRUE,
                                        regime.breaks = NULL, 
                                        logFI=TRUE, 
                                        approx.metric=TRUE) {
  # stop if df is empty
  if (nrow(df) == 0)
    stop("data frame is empty -- this should not be")
  
  if ("time" %in% names(df))
    x <-  "time"
  if ("winStop" %in% names(df))
    x <-  "winStop"
  if (!("time" %in% names(df) |
        "winStop" %in% names(df)))
    stop(print("x variable time or winStop not found. Please check data frame."))
  
  y <-  paste0(metric.ind, ".mean")
  y.sd <- paste0(metric.ind, ".sd")
  
  
  # Get the uppper and lower CIs
  ## Get when log.FI==TRUE
  if (metric.ind == "FI" & logFI) {
  df2 <- df %>%
    filter(method %in% method.filter) %>%
    mutate(FI.mean = log(FI.mean + 1e-20)) %>% 
    rename(x = !!sym(x),
           y = !!sym(y),
           y.sd = !!sym(y.sd)) %>%
    mutate(upper = y + 1.96 * y.sd,
           lower = y - 1.96 * y.sd) %>%
    mutate(prob = as.factor(100 * as.numeric(as.character(prob))),
           method = as.factor(method))  %>%
    dplyr::select(x, y, method, prob, upper, lower)
    
  metric.ind <- "log(FI)"
  
  }
  ## For all non-FI metrics
  if(!metric.ind %in% c("log(FI)","FI")){
    df2 <- df %>%
      filter(method %in% method.filter) %>%
      rename(x = !!sym(x),
             y = !!sym(y),
             y.sd = !!sym(y.sd)) %>%
      mutate(upper = y + 1.96 * y.sd,
             lower = y - 1.96 * y.sd) %>%
      mutate(prob = as.factor(100 * as.numeric(as.character(prob))),
             method = as.factor(method))  %>%
      dplyr::select(x, y, method, prob, upper, lower)
  }
  
  
  # Subset the data for prob < baseline prob
  ribbon.data <- df2 %>%
    filter(!prob %in% baseline.prob) %>%
    filter(!is.na(upper),
           !is.na(lower)) %>%
    mutate(prob = paste0(prob, "%"))
  # This datawill be used ot plot a sigle line w/out ribbons..
  baseline.data <- df2 %>%
    filter(prob %in% baseline.prob) %>%
    rename(baseline = y) %>%
    dplyr::select(x, baseline)
  
  
  y_lab <- eval(bquote(expression(bar(.(
    metric.ind
  )))))
  
  ## Create the ribboned plot
  p.ribbon <-
    ggplot() +
    geom_line(data = ribbon.data,  mapping = aes(x = x, y = y)) +
    geom_ribbon(
      data = ribbon.data,
      mapping = aes(x = x,
                    ymin = lower,
                    ymax = upper),
      na.rm = TRUE,
      color = "grey",
      alpha = 0.3
    ) +
    theme_mine() +
    theme(legend.position = "top") +
    xlab("time") +
    ylab(y_lab) +
    facet_wrap(facets = ~ prob,
               ncol = n.col,
               scales = "free_y")
  
  # p.ribbon
  
  fn <-
    paste0(metric.ind,
           "_",
           method.filter,
           "_ribboned_facetByProb",
           ".png")
  
  # Add the baseline geom_line to compare results
  # if(add.baseline) p.ribbon <-
  p.ribbon <- p.ribbon +
    geom_line(
      data = baseline.data,
      mapping = aes(x = x, y = baseline),
      color = "red",
      linetype = 1,
      alpha = .4
    )
  
  # Save the plot
  if (savePlot)
    ggsave(plot = p.ribbon,
           path =  figDir,
           filename = fn)
  
  
  # Save plots for individual regimes
  if (!is.null(regime.breaks)) {
    for (m in seq_along(regime.breaks)) {
      x_lim <- regime.breaks[[m]]
      ribbon.data.regime <-
        ribbon.data %>% filter(x >= min(x_lim), x <= max(x_lim))
      baseline.data.regime <-
        baseline.data %>% filter(x >= min(x_lim), x <= max(x_lim))
      
      
      p.ribbon.regime <-
        ggplot() +
        geom_line(data = ribbon.data.regime,  mapping = aes(x = x, y = y)) +
        geom_ribbon(
          data = ribbon.data.regime,
          mapping = aes(
            x = x,
            ymin = lower,
            ymax = upper
          ),
          na.rm = TRUE,
          color = "grey",
          alpha = 0.3
        ) +
        theme_mine() +
        theme(legend.position = "top") +
        xlab("time") +
        ylab(y_lab) +
        facet_wrap(facets = ~ prob,
                   ncol = n.col,
                   scales = "free_y") +
        geom_line(
          data = baseline.data.regime,
          mapping = aes(x = x, y = baseline),
          color = "red",
          linetype = 1,
          alpha = .4
        )
      
      fn <-
        paste0(metric.ind,
               "_",
               method.filter,
               "_ribboned_facetByProb_regime",
               m,
               ".png")
      ggsave(plot = p.ribbon.regime,
             path =  figDir,
             filename = fn)
    }
    
    
  }
  
  # Export from function to screen or object
  if (preview)
    return(p.ribbon)
  
}



# Plot density ratio ------------------------------------------------------

## plots a density ratio faceted by method and colored by probability. Only one figure spits into Temp fig dir per METRIC (e.g., FI, VI, dsdt, ds)
plot.densityCV <- function(data = myDf.all,
                           mymetric,
                           figDir = figDir) {
  ## create some aesthetic and functional labels
  metric.ind <- paste0(mymetric, ".mean")
  metric.ind2 <- paste0(mymetric, ".sd")
  
  x.lab <- ifelse(
    mymetric == "FI",
    'Fisher Information',
    ifelse(
      mymetric == "VI",
      "Variance Index",
      ifelse(mymetric == "s", "distance travelled (s)",
             mymetric)
    )
  )
  x.lab <- paste0("CV of ", mymetric)
  
  
  
  ## subset the data
  temp.data <-
    myDf.all %>% filter(prob < 1 &
                          !(method %in% c("dominance", "Dominance", "DOMINANCE"))) %>%
    filter(!is.na(!!sym(metric.ind)),
           !is.na(!!sym(metric.ind2))) %>%
    mutate(cv = 100 * !!sym(metric.ind2) / !!sym(metric.ind))
  
  
  
  x.max <- ifelse(max(temp.data$cv) > 100,
                  100,
                  max(temp.data$cv))
  
  
  ## build and save plot
  p <- ggplot(data = temp.data) +
    geom_density(aes(x = abs(cv), color = as.factor(prob))) +
    coord_cartesian(xlim = c(0, x.max)) +
    facet_wrap( ~ method, scales = "free") +
    theme_mine() +
    theme(legend.position = "bottom") +
    scale_color_manual(
      name = "% data retained",
      values = c("black", 'grey60', "grey90"),
      labels = paste0(levels(as.factor(
        temp.data$prob * 100
      )), "%")
    ) +
    xlab(x.lab) +
    ylab("density\n")
  
  p
  my.fn <- paste0(figDir, "/", mymetric , "_cvDensity", ".png")
  ggsave(p,
         filename = my.fn ,
         width = 6,
         height = 4)
  print(paste0("printing density plot to file. See ", my.fn))
  
}
