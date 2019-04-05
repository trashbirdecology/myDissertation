# plotting function
myPlot <- function(lab = "driver", x = t,  y1 =S, y2 = D, size =1, hline = D, ann.pos.x = NULL, ann.pos.y = NULL){
  
  ggplot()+
    geom_line(aes(x,S), size = size, color = "black", linetype = 1)+
    geom_hline(yintercept = D, size = size, color = "blue", linetype = 1)+
    annotate("text", x = ann.pos.x, y = ann.pos.y, label = lab)+
    xlab("driver")+
    ylab("ecosystem state")
}

