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
## begin matching uber data to crime data
####################################################################################
## import latitude and longitude locations for each coordinate location in uber data
  lat.long <- read.csv("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/case1-crime/Data/awsLocations.csv")
  #plot to see distribution
    ggplot(lat.long, aes(x=longitude, y=latitude)) + geom_point() + coord_equal()

## import uberx data
  setwd("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/case1-crime/output/")
  load("uberx.Rda")

## join lat/long details into uberx
## remove unnecessary columns and cast the timestamps
  uber.x2  <- left_join(uber.x, lat.long, by=c("start_location_id"="locations")) %>% 
    select(-end_location_id, -expected_wait_time, -product_type)
  uber.x2$timestamp <- ymd_hms(uber.x2$timestamp, tz = "UTC")
  head(uber.x2)

## roll uber data up to the nearest hour
  uber.x2$timestamp.half <- uber.x2$timestamp
  minute(uber.x2$timestamp.half)<- floor(minute(uber.x2$timestamp)/30)*30
  second(uber.x2$timestamp.half)<-0

  uber.x2.roll <- uber.x2 %>% 
    dplyr::group_by(timestamp.half, start_location_id, latitude, longitude) %>% 
    dplyr::summarise(avg.surge = mean(surge_multiplier, na.rm=TRUE),
                     avg.low   = mean(low_estimate, na.rm=TRUE),
                     avg.high  = mean(high_estimate, na.rm=TRUE)) 

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

## check date overlap: crime data should be longer timeframe than uber:
  crime %>% summarise(min(reportdatetime), max(reportdatetime))
  uber.x2.roll %>% summarise(min(timestamp.half), max(timestamp.half))

## limit crime data to be inside uber dates:
  crime<-crime[crime$reportdatetime>min(uber.x2.roll$timestamp.half) &
               crime$reportdatetime<max(uber.x2.roll$timestamp.half),]
  nrow(crime)
  # 2,228

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
  crime_list<- crime[,c("long", "lat", "reportdatetime", "objectid")]

######################################################################
## find nearest uber point for each crime
## use distm to create matrix of closest uber points
## add closest uber point to crime table along with its distance
######################################################################
  for (i in 1:nrow(crime_list)) {
    crime.point <- crime_list[i,1:2]
    dists <- distm(crime.point, lat.long[,c("longitude","latitude")])
    min_uber_dist <- which(dists==min(dists))
    min_uber_point <- lat.long[,][min_uber_dist,]
    crime_list$uber.lat[i] <- min_uber_point[,1] 
    crime_list$uber.long[i] <- min_uber_point[,2] 
    crime_list$uber.location[i] <- min_uber_point[,3] 
    crime_list$uber.dist[i] <- min_uber_dist 
  }

  # whats the distribution of distances for the "nearest"
  summary(crime_list$uber.dist)
  hist(crime_list$uber.dist)
  plot(ecdf(crime_list$uber.dist))
  ## most crimes can be matched to a uber call within 200 meters ~80%

######################################################################
## find the uber call that is soonest to crime report at that point
######################################################################
  for (i in 1:nrow(crime_list)) {
    # pull current crime details
    crime.point <- crime_list[i,c("uber.location", "reportdatetime")]
    # limit to crimes within an hour after
    potential_ubers <- uber.x2.roll[uber.x2.roll$start_location_id==crime.point[,1],] %>% 
      dplyr::mutate(diff = difftime(timestamp.half, crime.point[,2], units="hours")) %>% 
      dplyr::filter(diff>0 & diff<=1) 
    # select timestamp of soonest crime and assign to crime table
    crime_list$uber.time[i] <- potential_ubers[potential_ubers$diff==min(potential_ubers$diff), c("timestamp.half")]
    crime_list$uber.timediff[i]<-min(potential_ubers$diff)
  }
  crime_list$uber.time<-as.POSIXct(as.numeric(crime_list$uber.time), origin = "1970-01-01", tz = "UTC")
  head(crime_list)
 
  
# look at clustering:
library(ks);  library(RColorBrewer)
points = crime[,c("X","Y")]
Hpi1 <- Hpi(x = points, pilot="dscalar", bgridsize =c(300,300))
plt <- kde(x = points, H = Hpi1, bgridsize = 2000)
r <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
image(plt$estimate, col = r, useRaster=TRUE, asp=1, axes=FALSE); 
