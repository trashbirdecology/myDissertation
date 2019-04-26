
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
         myLabels$distance <- names(results$distance)[!names(results$distance) %in% cols.ignore]}
         
         
    if("fivi" %in% metrics.to.plot){ myLabels$fivi <- names(results$fivi)[!names(results$fivi) %in% cols.ignore]}
    
if("ews" %in% metrics.to.plot){ myLabels$ews <- names(results$ews)[!names(results$ews) %in% cols.ignore]}
    
    return(myLabels)

}



    
# plot.bootstrappedFacetGroup ---------------------------------------------
plot.bootstrappedFacetGroup <- function(df, 
                                        metric.ind, 
                                        method.filter, 
                                        facet.var,
                                        n.col = 1, # number of facet columns
                                        x = "time",
                                        y = ".mean",
                                        y.sd = ".sd", 
                                        savePlot = TRUE, 
                                        preview = TRUE
    ){
    
    y <-  paste0(metric.ind,y)
    y.sd <- paste0(metric.ind, y.sd)

    
    if(exists("method.filter")) group1 <- paste0("as.factor(", method.filter, ")")
    if(exists("facet.var")) group2 <- paste0("as.factor(", facet.var, ")")
    
    
    df.temp <- df %>% 
        mutate(
            upper = !!sym(y)+1.96*!!sym(y.sd),
            lower = !!sym(y)-1.96*!!sym(y.sd)
        ) %>% 
        filter(method %in% method.filter) %>% 
        filter(!is.na(upper), 
               !is.na(lower)) %>% 
        mutate(prob = as.factor(100*as.numeric(as.character(prob))),
               method=as.factor(method))
    
    # Change for labelling purposes
    if(group2=="as.factor(prob)" | group2=="as.factor(prob)") levels(df.temp$prob) =  paste0(levels(df.temp$prob),"%")
    
    ## ribbons
    p.ribbon <-
        ggplot(data = df.temp) +
        geom_line(aes_string(x = x, y = y)) +
        facet_wrap(facets = as.formula(paste0("~", group2)), ncol = n.col, scales="free_y") +
        geom_ribbon(aes_string(x = x,
                               ymin = 'lower',
                               ymax = 'upper' 
        ), na.rm = TRUE,
        color="grey", alpha=0.3)+
        theme_mine() +
        theme(legend.position = "top") +
        ylab("mean velocity")
    
    fn <- paste0(metric.ind, "_ribboned_facetBy",facet.var, "_",  method.filter, ".png")
    
    if(preview) p.ribbon
    
 if(savePlot) ggsave(plot=p.ribbon, path =  figDir, 
           filename = fn) 

}    
