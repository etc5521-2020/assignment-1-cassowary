# Get the world map
worldMap <- getMap()

# Member States of the European Union
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})
europeCoords <- do.call("rbind", europeCoords)

a <- gdpr_vio %>% group_by(name) %>% summarise (count = n()) %>%
  full_join(europeCoords, by = c("name" =  "region"))

P <- ggplot() + geom_polygon(data = a, aes(x = long, y = lat, group = name, fill = count),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

P <- P + scale_fill_gradient(name = "Number of violations", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")


P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
ggplotly(P)
