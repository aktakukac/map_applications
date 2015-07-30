setwd("G:/TEAM/EHUES_Üzleti_intelligencia/Közös/QV Solar/R terkep")

library(ggplot2)
library(ggmap)
library(dplyr)

Alapadat <- read.csv("./Alapadat.csv", header=TRUE, sep = ";", dec="," )

hungary_map <- get_map(location = "hungary", maptype = "terrain", zoom = 7)
#ggmap(hungary_map)

ggmap(hungary_map, extent = "device") + geom_point(aes(x = Longitude, y = Latitude), colour = "red", 
                                                 alpha = 0.1, size = 2, data = Alapadat, position="jitter")


ggmap(hungary_map, extent = "device") + 
  geom_density2d(data = Alapadat, aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = Alapadat, aes(x = Longitude, y = Latitude, 
                                      fill = ..level.., alpha = ..level..), 
                                      size = 0.01,bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


ggmap(hungary_map, extent = "device") + 
  geom_density2d(data = Alapadat, aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = Alapadat, aes(x = Longitude, y = Latitude, 
                                      fill = ..level.., alpha = ..level..), 
                 size = 0.01,bins = 20, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


balaton_map <- get_map(location = "balaton", maptype = "terrain", zoom = 8)
#ggmap(balaton_map)

Alapadat_02 <- Alapadat[!as.character(Alapadat$TELEPULES)=="BUDAPEST",]

ggmap(balaton_map, extent = "device") + 
  geom_density2d(data = Alapadat_02, aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = Alapadat_02, aes(x = Longitude, y = Latitude, 
                                      fill = ..level.., alpha = ..level..), 
                 size = 0.01,bins = 20, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)



