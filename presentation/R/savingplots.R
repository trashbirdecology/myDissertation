saveFun <-function(p, fn) ggsave(p, path=paste0(here::here(), "/figures"), filename = paste0(fn, ".png"))
# 
# saveFun(p=p.sysEx, fn="exData")
# p.sysEx2 <-p.sysEx +
#   geom_point(data =df, aes(x = t, y = value, shape = variable, color=variable), size=4)+
#   xlim(c(1,2))+ylim(c(20,30))+
#   scale_color_manual(values=c("red","black"))+
#   theme(legend.position = 'none')+
#   theme_bw()+
#   ylab("x")
# saveFun(p=p.sysEx2, fn="exData_t1t2")
# 
# pz = ggplot(data = plotData,
#        aes(x = year.plot, y = value, fill = variable)) +
#   geom_area() +
#   geom_rug(sides = "b") +
#   scale_fill_brewer(palette = "Paired") +
#   theme_bw()+
#   theme(legend.position = "none") +
#   labs(x = "years before 1950", y = "relative abundance", fill = "variable")+
#   scale_x_reverse()
# 
# 
# saveFun(p=pz, fn="paleoturnover")
# 
# phasep <- ggplot(df %>% spread(key=variable, value = value))+
#   geom_point(aes(x_1, x_2, color = t ),size = 1)+
#   scale_color_gradient2(midpoint = 50, low = "red", mid="white" , high = "blue")+
#   annotate("text", x = 48, y = 25, label= "t < 50", size = 4.8)+
#   annotate("text", x = 75, y = 100, label= "t \u2265 50",size = 4.8)+
#   labs(font.face="bold.italic", size = 7)+
#   labs(x=expression(x[1]))+
#   labs(y=expression(x[2]))+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14), 
#         legend.position=c(1-0.15,1-0.68), 
#         legend.text = element_text(size = 8), 
#         legend.title = element_text(size = 14, face= "italic"))
#   
# phasep
# saveFun(p=phasep, fn="phaseplot")
# 
# dsdt <- dsdtPlot(dist=dist, df=df, div = 1)
# saveFun(p=dsdt,fn="dsdtplot")


ggplot(data = sysSol, aes(x = x1, y = x2)) +
  geom_bin2d(aes(fill = ..density..), color = "black", drop = T, bins = 20) +
  scale_fill_distiller(palette = "YlOrRd") +
  labs(x = "x_1", y = "x_2", fill = "p(x)") +
  coord_fixed()+
  theme_bw() ->p1
saveFun(p=p1, fn="predpreyhist")

