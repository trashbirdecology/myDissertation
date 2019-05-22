# My helper functions -----------------------------------------------------
## quick save figure to  dir of choice
saveFig <- function(p, fn, dir = figDirTemp, dev=".png"){
  fn <- paste0(dir, "/", fn, dev)
  suppressWarnings(ggsave(plot = p, filename = fn, width=6.5, height=5))
}

## plotting theme
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



# Load results ------------------------------------------------------------

loadResultsDiscont <-function(resultsDir=here::here("chapterFiles/discontinuityAnalysis/results/"),myPattern=".RDS"){
  files <- list.files(path = resultsDir, pattern = myPattern, full.names = TRUE)
  
  gaps <-  lapply(files, readRDS) %>% 
    bind_rows() %>% 
    mutate(isGap = as.factor(as.character(isGap)),
           loc = as.factor(as.character(loc)))
  
}



