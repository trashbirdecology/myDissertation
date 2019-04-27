## functions for PLOTTING

## My plotting theme
theme_mine <- function(base_size = 12,
                       base_family = "Times"
                       ) {
    # Starts with theme_grey and then modify some parts
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
            strip.background = element_blank(),
            strip.text.x = element_text(size = 9),
            strip.text.y = element_text(size = 9),
            axis.text.x = element_text(size=11),
            axis.text.y = element_text(size=11,hjust=1.5),
            axis.ticks =  element_line(colour = "black"),
            axis.title.x= element_text(size=11),
            axis.title.y= element_text(size=12,angle=90),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(1.0, "lines"),
            plot.background = element_blank(),
            axis.line.x = element_line(color="black", size = 1),
            axis.line.y = element_line(color="black", size = 1),
            plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines")
        )
}

# Load and munge summary results ------------------------------------------
loadSummaryResults <-function(dataDir, 
                              distance = TRUE, 
                              fivi=TRUE, 
                              ews=TRUE){
    
    results <- list()

    if(distance) results$distances <- purrr::map_df(list.files(dataDir, full.names=TRUE, pattern = "distances"), read_feather)
    
    if(fivi) results$fivi <- purrr::map_df(list.files(dataDir, full.names=TRUE, pattern = "fivi"), read_feather)
    
    if(ews) results$ews <- purrr::map_df(list.files(dataDir, full.names=TRUE, pattern = "ews"), read_feather)
    
    return(results)
}
                                                            

# Indexing metrics and labels for plotting --------------------------------

setLabels <-function(metrics.to.plot, results
){
    
    myLabels <- list()
    
    cols.ignore <- c("variable", "method", 'prob', 'winStart', 'winStop', 'winMove', 'time', 'skewMean.mean', 'skewMean.sd')
    
    if(any(c("distance", "distances", "dist") %in% metrics.to.plot)){
         myLabels$distances <- names(results$distance)[!names(results$distance) %in% cols.ignore]}
         
         
    if("fivi" %in% metrics.to.plot){ myLabels$fivi <- names(results$fivi)[!names(results$fivi) %in% cols.ignore]}
    
if("ews" %in% metrics.to.plot){ myLabels$ews <- names(results$ews)[!names(results$ews) %in% cols.ignore]}
    
    ## Remove all the .mean and .sd labels and return only the metrics, then keep distinct
    
    for(i in seq_along(myLabels)){
        
        myLabels[[i]]<-gsub("\\.mean","",myLabels[[i]]) 
        myLabels[[i]]<-gsub("\\.sd","",myLabels[[i]]) 
        myLabels[[i]]<- myLabels[[i]] %>% unique()
        
    }
        
    
        
        
        
        
    return(myLabels)

    
    
    
}



    
# plot.bootstrappedFacetGroup ---------------------------------------------
plot.bootstrappedFacetGroup <- function(df, 
                                        metric.ind, 
                                        method.filter, 
                                        n.col = 1, # number of facet columns
                                        savePlot = TRUE, 
                                        preview = TRUE, 
                                        baseline.prob = c(1, 100), 
                                        add.baseline=TRUE
    ){
    
    if("time" %in% names(df)) x <-  "time"
    if("winStop" %in% names(df)) x <-  "winStop"
    if(!( "time" %in% names(df) | "winStop" %in% names(df))) stop(print("x variable time or winStop not found. Please check data frame."))
        
    y <-  paste0(metric.ind,".mean") 
    y.sd <- paste0(metric.ind, ".sd")

    
    
    df2 <- df %>% 
        filter(method %in% method.filter) %>%  
        rename(x = !!sym(x), 
               y = !!sym(y), 
               y.sd = !!sym(y.sd)) %>% 
        mutate(
            upper = y+1.96* y.sd,
            lower = y-1.96*y.sd
        ) %>% 
        mutate(prob = as.factor(100*as.numeric(as.character(prob))),
               method=as.factor(method))  %>% 
        dplyr::select(x, y, method, prob, upper, lower)
    
        
    ribbon.data <- df2 %>% 
        filter(!prob %in% baseline.prob) %>% 
    filter(!is.na(upper), 
           !is.na(lower)) %>% 
        mutate(prob = paste0(prob, "%"))
    
    baseline.data <- df2 %>% 
        filter(prob %in% baseline.prob) %>% 
        rename(baseline = y) %>% 
        dplyr::select(x, baseline)

    y_lab <- eval(bquote(expression(bar(.(metric.ind)))))
    
    ## Create the ribboned plot
    p.ribbon <-
        ggplot() +
        geom_line(data=ribbon.data,  mapping = aes(x=x, y=y)) +
        geom_ribbon( data=ribbon.data, mapping=aes(x = x,
                               ymin = lower,
                               ymax = upper), na.rm = TRUE,
        color="grey", alpha=0.3)+
        theme_mine()+
        theme(legend.position = "top") +
      
        ylab(y_lab) +
        facet_wrap(facets = ~ prob, ncol = n.col, scales="free_y")+
        # ylab(expression(bar(y)))
            ylab(paste0(y))
    
    fn <- paste0(metric.ind,"_",  method.filter,  "_ribboned_facetByProb", ".png")
    
    # Add the baseline geom_line to compare results
    # if(add.baseline) p.ribbon <- 
    p.ribbon <- p.ribbon+
        geom_line(data = baseline.data, mapping = aes(x=x,y=baseline), color = "red", linetype = 1, alpha=.4) 
    
    # Save the plot
     if(savePlot) ggsave(plot=p.ribbon, path =  figDir, 
           filename = fn) 

    # Export from function to screen or object
    if(preview) return(p.ribbon)

    }    
