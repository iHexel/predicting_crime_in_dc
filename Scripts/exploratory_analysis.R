## load, clean and merge uber and crime data
## 9/10/2016
## db fowler

# load packages
library(dplyr); library(ggplot2); library(lubridate); library(geosphere); library(plyr)

## read uber data in. downloaded from:
## https://drive.google.com/drive/u/0/folders/0B-mutxqHY34rblhORk9raWxQQjQ
uber <- read.csv("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/case1-crime/uberdata/TractsSurgeDC2_Feb4_Mar2.csv")
nrow(uber)
# 53,695,775

## data doc:
    # "timestamp" : string, Date and Time (EST) when API was pinged
    # "surge_multiplier": float, The surge multiplier for the current time and location
    # "expected_wait_time": integer, The number of seconds rider may have to wait between requesting a car, and the car's arrival
    # "product_type": string, The type of car - uberTAXI, UberSUV, UberBLACK, uberX + Car Seat, uberX, uberXL, SUV + Car Seat, BLACK CAR + Car Seat
    # "low_estimate": integer, lower end of an estimated price of the ride (dollars)
    # "high_estimate": integer, upper end of an estimated price of the ride (dollars)
    # "start_location_id": integer, number between 0-275 that relates to our predetermined longitudes and latitudes across DC.
    # "end_location_id": integer, number between 0-275 that relates to our predetermined longitudes and latitudes across DC.
uber$surge_multiplier    <- as.numeric(uber$surge_multiplier)
uber$expected_wait_time  <- as.numeric(uber$expected_wait_time)
uber$low_estimate        <- as.numeric(uber$low_estimate)
uber$high_estimate       <- as.numeric(uber$high_estimate)

# check out product types
uber %>% 
  group_by(product_type) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count)) %>% 
  arrange(desc(count))
  ## pretty much evenly split. decide to use uberX only
      #         product_type   count      freq
      #                uberX 6974473 0.1298887
      #            UberBLACK 6973847 0.1298770
      #              UberSUV 6970760 0.1298195
      #               uberXL 6967755 0.1297636
      #             uberTAXI 6954906 0.1295243
      # BLACK CAR + Car Seat 6743141 0.1255805
      #       SUV + Car Seat 6718113 0.1251144
      #     uberX + Car Seat 5392780 0.1004321

# limit to uberX
uber.x <- uber %>% filter(product_type=="uberX")
save(uber.x, file="/Users/bradyfowler/Documents/Fall Semester/Mining_6018/case1-crime/output/uberx.Rda")

# pool together all types of ubers in case we decide to look at it this way
uber.pooled <- uber %>% 
  group_by(timestamp, start_location_id) %>% 
  summarise(avg_surge_multiplier=mean(surge_multiplier, na.rm=TRUE), 
            avg_expected_wait_time=mean(expected_wait_time, na.rm=TRUE),
            avg_low_estimate=mean(low_estimate, na.rm=TRUE), 
            avg_high_estimate = mean(high_estimate, na.rm=TRUE))
save(uber.pooled, file="/Users/bradyfowler/Documents/Fall Semester/Mining_6018/case1-crime/output/uberpooled.Rda")

# drop uber table to free up space
rm(uber)

## is start location always end location?
uber.x %>% filter(start_location_id!=end_location_id) %>% summarise(n())
## yep, we can probably drop it

####################################################################################
## begin matching
####################################################################################
setwd("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/case1-crime/output/")
load("uberx.Rda")

## pull in latitude and longitude locations for each coordinate location
lat.long <- read.csv("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/case1-crime/Data/awsLocations.csv")
#plot to see distribution
ggplot(lat.long, aes(x=longitude, y=latitude)) + 
  geom_point() + coord_equal()

## join lat/long into uberx
## remove unnecessary columns and cast the timestamps
uber.x2  <- left_join(uber.x, lat.long, by=c("start_location_id"="locations")) %>% 
  select(-end_location_id, -expected_wait_time, -product_type)
uber.x2$timestamp <- ymd_hms(uber.x2$timestamp, tz = "UTC")
head(uber.x2)

## import crime data and fix column names
crime <- read.csv("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/case1-crime/Data/Crime_Incidents__2016.csv")
colnames(crime)<-tolower(c("long", "lat", colnames(crime)[3:length(crime)]))
nrow(crime)
# 22,223

# fix dates:
crime$reportdatetime    <- ymd_hms(crime$reportdatetime, tz = "UTC")
crime$lastmodifieddate  <- ymd_hms(crime$lastmodifieddate, tz = "UTC")
crime$start_date        <- ymd_hms(crime$start_date, tz = "UTC")
crime$end_date          <- ymd_hms(crime$end_date, tz = "UTC")

## look at offense scatter across DC
ggplot(crime, aes(x=long, y=lat)) + 
  geom_point() + coord_equal()

## look at offense type distribution
crime %>% 
  group_by(offense)  %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count)) %>% 
  arrange(desc(count))
  #                     offense count         freq
  #                 THEFT/OTHER  8830 0.3973360932
  #                THEFT F/AUTO  6615 0.2976645817
  #                     ROBBERY  1966 0.0884669037
  #         MOTOR VEHICLE THEFT  1614 0.0726274580
  #  ASSAULT W/DANGEROUS WEAPON  1571 0.0706925258
  #                    BURGLARY  1356 0.0610178644
  #                   SEX ABUSE   181 0.0081447149
  #                    HOMICIDE    86 0.0038698646
  #                       ARSON     4 0.0001799937
  # Probably only want to concentrate on non property crimes


## create function to find the nearest crime (if any) to a surge price time
## also make sure its pretty soon after

# extract items of interest from crime table
crime_list<- crime[,c("long", "lat", "reportdatetime", "objectid")]

# standard city block is ~250 meters. we want to look within 4 blocks
nearest_soonest <- function(row_num){
  # details for current uber call
  deets <- uber.x2[row_num, c("timestamp", "latitude", "longitude")]
  # limit to crimes within an hour
  potential_crimes <- crime_list[abs(difftime(deets[,1], crime_list$reportdatetime,units="hours")) < 1,]
  # limit to crimes within 1000 meters
  potential_crimes <- potential_crimes %>% 
    mutate(dist = by(potential_crimes, 1:nrow(potential_crimes), function(row) { distm(c(deets[,2], deets[,3]), c(row$lat, row$long), fun = distHaversine)  })) %>% 
    filter(dist<1000)
  # return matched crimes
  return(list(potential_crimes$objectid))
}

## roll uber data up to the nearest hour
uber.x2$timestamp.half <- uber.x2$timestamp
minute(uber.x2$timestamp.half)<- floor(minute(uber.x2$timestamp)/30)*30
second(uber.x2$timestamp.half)<-0

uber.x2.roll <- uber.x2 %>% 
  dplyr::group_by(timestamp.half, start_location_id, latitude, longitude) %>% 
  dplyr::summarise(avg.surge = mean(surge_multiplier, na.rm=TRUE),
            avg.low   = mean(low_estimate, na.rm=TRUE),
            avg.high  = mean(high_estimate, na.rm=TRUE)) 

# couldnt get the other sapply or mutate approach to work
# so here ill apply it in a loop even if its not very eloquent
for (i in 1:nrow(uber.x2.roll)) {
  uber.x2.roll$dist[i] <- nearest_soonest(i) 
}


# look at clustering:
library(ks);  library(RColorBrewer)
points = crime[,c("X","Y")]
Hpi1 <- Hpi(x = points, pilot="dscalar", bgridsize =c(300,300))
plt <- kde(x = points, H = Hpi1, bgridsize = 2000)
r <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
image(plt$estimate, col = r, useRaster=TRUE, asp=1, axes=FALSE); 
