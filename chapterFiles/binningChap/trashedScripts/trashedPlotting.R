# # Plot the BBS routes, mil bases ------------------------------------------
# 
# plot_routes <- usplot +
#   xlim(min(routePts$long) - 2, max(routePts$long)) +
#   ylim(min(routePts$lat), max(routePts$lat) + 2) +
#   geom_point(
#     data = routePts,
#     aes(x = long, y = lat, color = "bbsRoutes"),
#     size = .4,
#     show.legend = TRUE
#   ) +
#   xlab ("longitude") + ylab("latitude") +
#   theme(legend.position = "bottom") +
#   # theme(legend.position= c(-75,35))+
#   scale_colour_manual(name = "",
#                       values = c(bbsRoutes = "black"))#, myline2="red"))
# plot_routes # plot of all bbs routes, simple map
# plot_routes_filename <- paste0(figDir, "/plot_routes.png")
# 
# plot_routes_milbases <- plot_routes +
#   geom_point(
#     data = coordinates(milBases) %>% as.data.frame(),
#     aes(
#       x = milBases@coords[, 1],
#       y = milBases@coords[, 2],
#       color = "milBase"
#     ),
#     size = .8,
#     show.legend = TRUE
#   ) +
#   xlab ("longitude") + ylab("latitude") +
#   theme(legend.position = "bottom") +
#   scale_colour_manual(name = "",
#                       values = c(bbsRoutes = "black", milBase  = "red")) +
#   xlim(-125, -69)
# 
# plot_routes_milbases
# plot_routes_milbases_filename <-
#   paste0(figDir, "/plot_routes_milbases.png")
# 
# ## Save the figures to file
# ggsave(filename = plot_routes_filename,
#        plot_routes,
#        width = 12,
#        height = 9)
# ggsave(
#   filename = plot_routes_milbases_filename,
#   plot_routes_milbases,
#   width = 12,
#   height = 9
# )
# 
# 
