setwd("/Users/bradyfowler/Downloads/Census")

library(maptools)
library(ggplot2)

source("C:/Users/hexel/Documents/R/SYS6018/Case1/case1-crime/Scripts/2_match_crimes_and_prices.R")

tract <- readShapePoly('Census_Tracts__2010.shp')
cdata <- read.csv('Census_Tracts__2010.csv')

tract_geom <- fortify(tract, region = "GEOID")
tract_poly <- merge(tract_geom, cdata, by.x = "id", by.y = "GEOID")

FAGI2013 <- ggplot(tract_poly, aes(long, lat, group = group, fill = FAGI_MEDIAN_2013)) + 
  geom_polygon() +
  scale_fill_gradient(low = "skyblue1", high = "blue4",
                      breaks = c(25000, 50000, 75000, 100000, 125000),
                      labels = c("$25K", "$50", "$75", "$100K", "$125K"),
                      guide = guide_legend(title = "Median Income")) +
  coord_equal()


head(tract_geom)

sp.points = SpatialPoints(tract_poly[,c("long","lat")], proj4string=CRS(proj4string(dc)))

geom_ids<-data.frame(unique(tract_geom$id))
colnames(geom_ids)<-"unique.id"
lat.long.geoid <- data.frame()
for (i in 1:nrow(geom_ids)){
  current.id <- geom_ids[i,]
  tract_points_in_geom <- pnt.in.poly(lat.long[,c("longitude","latitude")], tract_geom[tract_geom$id==current.id, c("long","lat")])
  lat.long.geoid <- rbind(lat.long.geoid, cbind(tract_points_in_geom[tract_points_in_geom$pip==1,], current.id))
}
lat.long.geoid



FAGI2013 + 
  
  ggplot()+geom_point(data = lat.long.geoid, aes(x = longitude, y = latitude, group = current.id, color = current.id)) + coord_equal()
