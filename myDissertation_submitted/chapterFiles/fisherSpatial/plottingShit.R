
# Helper functions --------------------------------------------------------


# USA map base plot
usPlot <- ggplot()  +
    geom_polygon(
        data = us_states,
        mapping = aes(x = long, y = lat,
                      group = group),
        fill = "white",
        color = "black"
    )

# Plot the BBS routes, mil bases ------------------------------------------
plot_routes <- usPlot +
    xlim(min(routePts$long)-2, max(routePts$long)) +
    ylim(min(routePts$lat), max(routePts$lat)+2) +
    geom_point(
        data = routePts,
        aes(x = long, y = lat, color = "bbsRoutes"),
        size = .4,
        show.legend = TRUE
    ) +
    xlab ("longitude") + ylab("latitude") +
    theme(legend.position="bottom")+
    # theme(legend.position= c(-75,35))+
    scale_colour_manual(name = "",
                        values = c(bbsRoutes = "black"))#, myline2="red"))
plot_routes # plot of all bbs routes, simple map
plot_routes_filename <-paste0(figDir, "/plot_routes.png")

plot_routes_milbases <- plot_routes +
    geom_point(
        data = coordinates(milBases) %>% as.data.frame(),
        aes(
            x = milBases@coords[, 1],
            y = milBases@coords[, 2],
            color = "milBase"
        ),
        size = .8,
        show.legend = TRUE
    ) +
    xlab ("longitude") + ylab("latitude") +
    theme(legend.position="bottom")+
    scale_colour_manual(name = "",
                        values = c(bbsRoutes = "black", milBase  = "red")) +
    xlim(-125,-69)

plot_routes_milbases
plot_routes_milbases_filename <- paste0(figDir, "/plot_routes_milbases.png")

## Save the figures to file
ggsave(filename = plot_routes_filename, plot_routes, width = 12, height = 9)
ggsave(filename = plot_routes_milbases_filename, plot_routes_milbases, width = 12, height = 9)


# Plot example transect COL ----------------------------------------------

tmp <- routes_grid %>% 
    distinct(lat, long, .keep_all=T) %>% 
    filter(colID == 54)

tmpMilBases <- milBases@coords %>% 
    as_tibble() %>% 
    filter(coords.x1 > min(tmp$long) & coords.x1 < max(tmp$long)) %>% 
    filter(coords.x2 > min(tmp$lat) & coords.x2 < max(tmp$lat))

plot_tsect <- usPlot +
    geom_point(
        data = tmp,
        aes(x = long, y = lat, color = "bbsRoutes"),
        size = .6,
        show.legend = TRUE
    ) +
    xlab ("longitude") + ylab("latitude") +
    theme(legend.position="bottom")+
    scale_colour_manual(name = "", values = c(bbsRoutes = "black"))#, myline2="red"))

plot_tsect # plot of all bbs routes, simple map
plot_filename <-paste0(figDir, "/plot_tsect_colEx.png")
## Save the figures to file
ggsave(filename = plot_filename, plot_tsect, width = 12, height = 9)


# Plot transect example with military bases -------------------------------
tmpMilBases <- milBases@coords %>% 
    as_tibble() %>% 
    filter(coords.x1 > min(tmp$long) & coords.x1 < max(tmp$long)) %>% 
    filter(coords.x2 > min(tmp$lat) & coords.x2 < max(tmp$lat))

plot_tsect_mb <- plot_tsect +
    geom_point(
        data = tmpMilBases,
        aes(x = coords.x1, y = coords.x2, color = "mb"),
        size = .6,
        show.legend = TRUE
    ) +
    xlab ("longitude") + ylab("latitude") +
    theme(legend.position="bottom")+
    # theme(legend.position= c(-75,35))+
    scale_colour_manual(name = "",
                        values = c(mb = "red",bbsRoutes = "black"))#, myline2="red"))


plot_tsect_mb # plot of all bbs routes, simple map
plot_filename <-paste0(figDir, "/plot_tsect_colEx_mb.png")

## Save the figures to file
ggsave(filename = plot_filename, plot_tsect_mb, width = 12, height = 9)


# Plot example transect ROW ----------------------------------------------
tmp <- routes_grid %>% 
    distinct(lat, long, .keep_all=T) %>% 
    filter(rowID == 19)

plot_tsect <- ggplot()  +
    geom_polygon(
        data = us_states,
        mapping = aes(x = long, y = lat,
                      group = group),
        fill = "white",
        color = "black"
    ) +
    xlim(min(routePts$long)-2, max(routePts$long)) +
    ylim(min(routePts$lat), max(routePts$lat)+2) +
    geom_point(
        data = tmp,
        aes(x = long, y = lat, color = "bbsRoutes"),
        size = .6,
        show.legend = TRUE
    ) +
    xlab ("longitude") + ylab("latitude") +
    theme(legend.position="bottom")+
    # theme(legend.position= c(-75,35))+
    scale_colour_manual(name = "",
                        values = c(bbsRoutes = "black"))#, myline2="red"))

plot_tsect # plot of all bbs routes, simple map
plot_tsect_filename <-paste0(figDir, "/plot_tsect_rowEx.png")

## Save the figures to file
ggsave(filename = plot_tsect_filename, plot_tsect, width = 12, height = 9)


