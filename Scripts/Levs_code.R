# My code is towards the bottom on this file. 
# Basically I think we can use social data in a way Prof. Martinet explained last class as a classifier or sorts.
# I realize that Erik has made some major changes to the data filtering code... I can adapt mine to his if needed.
# - Lev Zadvinskiy

setwd("C:/Users/hexel/Documents/R/SYS6018/Case1/case1-crime/output/")

suppressWarnings(suppressMessages(library(dplyr)))
library(multidplyr)
suppressMessages(library(lubridate))
library(readr)
library(ggplot2)
library(geosphere)
library(SDMTools)
library(sp)
library(rgdal)

#################################################################
### Brady's code from 9/20/16, adapted to all uber categories ###
#################################################################

## import latitude and longitude locations for each coordinate location in uber data
lat.long <- read.csv("C:/Users/hexel/Documents/R/SYS6018/Case1/case1-crime/Data/awsLocations.csv")

load("uberpooled.Rda")

## join lat/long details into uberx
## remove unnecessary columns and cast the timestamps
uber.all  <- left_join(uber.pooled, lat.long, by=c("start_location_id" = "locations"))
uber.all$timestamp <- ymd_hms(uber.all$timestamp, tz = "UTC")
head(uber.all)

## roll uber data up to the nearest hour
uber.all$timestamp.half <- uber.all$timestamp
minute(uber.all$timestamp.half)<- floor(minute(uber.all$timestamp)/30)*30
second(uber.all$timestamp.half)<-0

# groupby timestamp and location and average surge/low/high
uber.all.roll <- uber.all %>% 
  dplyr::group_by(start_location_id, latitude, longitude) %>% 
  #dplyr::group_by(timestamp.half, start_location_id, latitude, longitude) %>% 
  dplyr::summarise(avg.surge = mean(avg_surge_multiplier, na.rm=TRUE),
                   avg.low   = mean(avg_low_estimate, na.rm=TRUE),
                   avg.high  = mean(avg_high_estimate, na.rm=TRUE)) 

## import crime data and fix column names
crime <- read.csv("C:/Users/hexel/Documents/R/SYS6018/Case1/case1-crime/Data/Crime_Incidents__2016.csv")
colnames(crime) <- tolower(c("long", "lat", colnames(crime)[3:length(crime)]))

# format dates:
crime$reportdatetime    <- ymd_hms(crime$reportdatetime, tz = "UTC")
crime$lastmodifieddate  <- ymd_hms(crime$lastmodifieddate, tz = "UTC")
crime$start_date        <- ymd_hms(crime$start_date, tz = "UTC")
crime$end_date          <- ymd_hms(crime$end_date, tz = "UTC")

## limit crime data to be inside uber dates:
crime <- crime[crime$reportdatetime>min(uber.all$timestamp.half) &
               crime$reportdatetime<max(uber.all$timestamp.half),]

nrow(crime) # 2,228 total crimes in timeframe

## look at offense scatter across DC
ggplot(crime, aes(x=long, y=lat, color=offense)) + geom_point() + coord_equal()

## look at offense type distribution
crime %>% 
  group_by(offense)  %>% 
  dplyr::summarise(count=n()) %>%
  mutate(freq = count / sum(count)) %>% 
  arrange(desc(count))
#                     offense count         freq
#                THEFT/OTHER   868 0.3895870736
#               THEFT F/AUTO   719 0.3227109515
#                    ROBBERY   218 0.0978456014
# ASSAULT W/DANGEROUS WEAPON   147 0.0659784560
#        MOTOR VEHICLE THEFT   135 0.0605924596
#                   BURGLARY   114 0.0511669659
#                  SEX ABUSE    15 0.0067324955
#                   HOMICIDE    10 0.0044883303
#                      ARSON     2 0.0008976661
# Probably only want to concentrate on non property crimes

# extract items of interest from crime table
crime_list <- crime %>% dplyr::select(long, lat, reportdatetime, objectid)
select(long, lat, reportdatetime, objectid)
nrow(crime_list) # 2228

######################################################################
## find nearest uber point for each crime
## use distm to create matrix of closest uber points
## add closest uber point to crime table along with its distance
######################################################################
for (i in 1:nrow(crime_list)) {
  crime.point <- crime_list[i,1:2]
  dists <- distm(crime.point, lat.long[,c("longitude","latitude")])
  min_uber_id <- which(dists==min(dists))
  min_uber_dist <- min(dists)
  min_uber_point <- lat.long[min_uber_id,]
  crime_list$uber.lat[i] <- min_uber_point[,1] 
  crime_list$uber.long[i] <- min_uber_point[,2] 
  crime_list$uber.location[i] <- min_uber_point[,3] 
  crime_list$uber.dist[i] <- min_uber_dist 
}

# whats the distribution of distances for the "nearest"
summary(crime_list$uber.dist)
hist(crime_list$uber.dist)
## most crimes can be matched to a uber call within 200 meters ~80%

## check point clustering with a few examples:
ggplot(subset(crime_list, uber.location==100), aes(x=long, y=lat, colour=factor(uber.location))) +
  geom_point() +coord_equal() + guides(colour=FALSE) +
  xlim(min(crime_list$long), max(crime_list$long))+
  ylim(min(crime_list$lat), max(crime_list$lat)) + 
  geom_point(data=subset(lat.long, locations==100), aes(x=longitude, y=latitude, colour="black"))

ggplot(crime_list, aes(x=long, y=lat, colour=factor(uber.location))) +
  geom_point() +coord_equal() + guides(colour=FALSE) 

############################################################
############################################################

uber.analysis <- dplyr::left_join(uber.all.roll, crime_list, by=c("start_location_id"="uber.location"))
uber.analysis$has.crime <- ifelse(is.na(uber.analysis$objectid)==TRUE,0,1)

# count number of crimes that occured
uber.analysis <- uber.analysis %>%
  dplyr::group_by(latitude, longitude, avg.surge, avg.low, avg.high, has.crime) %>%
  dplyr::summarise(num.crime = n())

# adapted from Lev's code:
tract <- readShapePoly('C:/Users/hexel/Documents/R/SYS6018/Case1/Census/Census_Tracts__2010.shp')
cdata <- read.csv('C:/Users/hexel/Documents/R/SYS6018/Case1/Census/Census_Tracts__2010.csv')
tract_geom <- fortify(tract, region = "GEOID")
tract_poly <- merge(tract_geom, cdata, by.x = "id", by.y = "GEOID")
# for each uber point, figure out which geoid it fits in
# create new dataframe with lat, long of uber and geo ID
geom_ids <- data.frame(unique(tract_geom$id))
colnames(geom_ids) <- "unique.id"

lat.long.geoid <- data.frame()
for (i in 1:nrow(geom_ids)){
  current.id <- geom_ids[i,]
  tract_points_in_geom <- pnt.in.poly(lat.long[,c("longitude","latitude")], tract_geom[tract_geom$id==current.id, c("long","lat")])
  lat.long.geoid <- rbind(lat.long.geoid, cbind(tract_points_in_geom[tract_points_in_geom$pip==1,], current.id))
}

# map uber analysis data to the geoid to pull in demographic info
uber.analysis <- left_join(uber.analysis, lat.long.geoid, by = c("latitude", "longitude"))
# cast geoid
cdata$GEOID <- factor(cdata$GEOID)
# join in demographic data
uber.analysis <- left_join(uber.analysis, cdata, by=c("current.id"="GEOID"))
# create fields of demographic inputs
uber.analysis$total_population <- uber.analysis$P0010001
uber.analysis$pct.minority <- (uber.analysis$P0010001-uber.analysis$P0020005)/uber.analysis$P0010001
uber.analysis$pct.over.18 <- uber.analysis$P0030001/uber.analysis$P0010001
uber.analysis$pct.vacant.homes <- uber.analysis$H0010003/uber.analysis$H0010001


################
### New Code ###
################

# Loading in night club and liquor licensed locations files
nclub <- read.csv('C:/Users/hexel/Documents/R/SYS6018/Case1/case1-crime/Data/Night_Club/Night_Club.csv')
liquor <- read.csv('C:/Users/hexel/Documents/R/SYS6018/Case1/case1-crime/Data/Liquor_License_Locations/Liquor_License_Locations.csv')

# Acquiring long/lat data for night clubs
nclub.points <- readShapePoints("C:/Users/hexel/Documents/R/SYS6018/Case1/case1-crime/Data/Night_Club/Night_Club.shp")
nclub$long <- coordinates(nclub.points)[,1]
nclub$lat <- coordinates(nclub.points)[,2]

# Acquiring long/lat data for liquor licensed locations
liquor.points <- readShapePoints("C:/Users/hexel/Documents/R/SYS6018/Case1/case1-crime/Data/Liquor_License_Locations/Liquor_License_Locations.shp")
liquor$long <- coordinates(liquor.points)[,1]
liquor$lat <- coordinates(liquor.points)[,2]

# Plots to check for general distribution
ggplot(nclub, aes(x=long, y=lat)) + geom_point() + coord_equal()
ggplot(liquor, aes(x=long, y=lat, color=TYPE)) + geom_point() + coord_equal()

# Find nearest uber point for each night club
for (i in 1:nrow(nclub)) {
  nclub.point <- nclub[i,7:8]
  dists <- distm(nclub.point, lat.long[,c("longitude","latitude")])
  min_uber_id <- which(dists == min(dists))
  min_uber_dist <- min(dists)
  min_uber_point <- lat.long[min_uber_id,]
  nclub$uber.lat[i] <- min_uber_point[,1] 
  nclub$uber.long[i] <- min_uber_point[,2] 
  nclub$uber.location[i] <- min_uber_point[,3] 
  nclub$uber.dist[i] <- min_uber_dist 
}

summary(nclub$uber.dist)
hist(nclub$uber.dist)

# Find nearest uber point for each liquor licensed location
for (i in 1:nrow(liquor)) {
  liquor.point <- liquor[i,23:24]
  dists <- distm(liquor.point, lat.long[,c("longitude","latitude")])
  min_uber_id <- which(dists == min(dists))
  min_uber_dist <- min(dists)
  min_uber_point <- lat.long[min_uber_id,]
  liquor$uber.lat[i] <- min_uber_point[,1] 
  liquor$uber.long[i] <- min_uber_point[,2] 
  liquor$uber.location[i] <- min_uber_point[,3] 
  liquor$uber.dist[i] <- min_uber_dist 
}

summary(liquor$uber.dist)
hist(liquor$uber.dist)

# Temporary data frames for future analysis
nclub.temp <- nclub[, c('ADDRESS', 'lat', 'long', 'uber.lat', 'uber.long', 'uber.location', 'uber.dist')]
nclub.temp$TYPE = 'Nightclub'
liquor.temp <- liquor[,c('ADDRESS', 'lat', 'long', 'uber.lat', 'uber.long', 'uber.location', 'uber.dist', 'TYPE')]

# Combining night club and liquor license data, and removing duplicates
social <- rbind(liquor.temp, nclub.temp)
social <- distinct(social)

# Adding categorical values for distinct social spots of interest
social$nightclub <- ifelse(social$TYPE == 'Nightclub', 1, 0)
social$tavern <- ifelse(social$TYPE == 'Tavern', 1, 0)
social$restaurant <- ifelse(social$TYPE == 'Restaurant', 1, 0)
social$club <- ifelse(social$TYPE == 'Club', 1, 0)

# Creating a dataframe to store only the social spots of interest
social.list <- social[(social$nightclub == '1')|(social$tavern == '1')|(social$restaurant == '1')|(social$club == '1'),]

# Combining Uber data with social data
uber.social <- dplyr::left_join(uber.all.roll, social.list, by = c("start_location_id" = "uber.location"))
uber.social <- uber.social[,c('latitude', 'longitude', 'TYPE', 'nightclub', 'tavern', 'restaurant', 'club')]
uber.social <- uber.social[!is.na(uber.social$TYPE),]

# Combining uber/social data with prior uber analysis dataframe
test <- dplyr::left_join(uber.analysis, uber.social, by = c('longitude', 'latitude'))
# Doesn't look right... creates many duplicates for non-social data.
# Maybe pull in the social data separately when doing regression analysis? 


