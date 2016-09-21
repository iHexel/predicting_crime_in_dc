## combine uber data, crime data and location data
## find closest/soonest uber price point to a given crime
## 9/16/2016
## db fowler

# load packages
library(dplyr); library(ggplot2); library(lubridate); library(geosphere); library(plyr)

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
  uber.x2  <- left_join(uber.x, lat.long, by=c("start_location_id"="locations")) # %>% 
    #select(-end_location_id, -expected_wait_time, -product_type)
  uber.x2$timestamp <- ymd_hms(uber.x2$timestamp, tz = "UTC")
  head(uber.x2)

## roll uber data up to the nearest hour
  uber.x2$timestamp.half <- uber.x2$timestamp
  minute(uber.x2$timestamp.half)<- floor(minute(uber.x2$timestamp)/30)*30
  second(uber.x2$timestamp.half)<-0

  
  # groupby timestamp and location and average surge/low/high
  uber.x2.roll <- uber.x2 %>% 
    dplyr::group_by(start_location_id, latitude, longitude) %>% 
    #dplyr::group_by(timestamp.half, start_location_id, latitude, longitude) %>% 
    dplyr::summarise(avg.surge = mean(surge_multiplier, na.rm=TRUE),
                     avg.low   = mean(low_estimate, na.rm=TRUE),
                     avg.high  = mean(high_estimate, na.rm=TRUE)) 

## import crime data and fix column names
  crime <- read.csv("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/case1-crime/Data/Crime_Incidents__2016.csv")
  colnames(crime)<-tolower(c("long", "lat", colnames(crime)[3:length(crime)]))
  nrow(crime)
  # 22,223

  # format dates:
    crime$reportdatetime    <- ymd_hms(crime$reportdatetime, tz = "UTC")
    crime$lastmodifieddate  <- ymd_hms(crime$lastmodifieddate, tz = "UTC")
    crime$start_date        <- ymd_hms(crime$start_date, tz = "UTC")
    crime$end_date          <- ymd_hms(crime$end_date, tz = "UTC")

## check date overlap: crime data should be longer timeframe than uber:
  crime %>% summarise(min(reportdatetime), max(reportdatetime))
  uber.x2 %>% summarise(min(timestamp.half), max(timestamp.half))

## limit crime data to be inside uber dates:
  crime<-crime[crime$reportdatetime>min(uber.x2$timestamp.half) &
               crime$reportdatetime<max(uber.x2$timestamp.half),]
  nrow(crime)
  # 2,228 total crimes in timeframe

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
  crime_list<- crime %>% dplyr::select(long, lat, reportdatetime, objectid)
          #filter(offense %in% c("THEFT/OTHER", "ROBBERY", "ASSAULT W/DANGEROUS WEAPON", "BURGLARY")) %>%#, "SEX ABUSE", "HOMICIDE")) %>% 
          #filter(offense %in% c("ROBBERY")) %>% 
          select(long, lat, reportdatetime, objectid)
  nrow(crime_list)
  # 2228
  
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
  ggplot(subset(crime_list, uber.location==2), aes(x=long, y=lat, colour=factor(uber.location))) +
    geom_point() +coord_equal() + guides(colour=FALSE) +
    xlim(min(crime_list$long), max(crime_list$long))+
    ylim(min(crime_list$lat), max(crime_list$lat)) + 
    geom_point(data=subset(lat.long, locations==2), aes(x=longitude, y=latitude, colour="black"))

  ggplot(crime_list, aes(x=long, y=lat, colour=factor(uber.location))) +
    geom_point() +coord_equal() + guides(colour=FALSE) 
  
######################################################################
## find the uber call that is soonest to crime report at that point
######################################################################
  # for (i in 1:nrow(crime_list)) {
  #   # pull current crime details
  #   crime.point <- crime_list[i,c("uber.location", "reportdatetime")]
  #   # limit to crimes within a half hour
  #   potential_ubers <- uber.x2.roll[uber.x2.roll$start_location_id==crime.point[,1],] %>%
  #     dplyr::mutate(diff = difftime(timestamp.half, crime.point[,2], units="hours")) %>%
  #     dplyr::filter(diff>=0 & diff<=1)
  #   # select timestamp of soonest crime and assign to crime table
  #   crime_list$uber.time[i] <- potential_ubers[potential_ubers$diff==min(potential_ubers$diff), c("timestamp.half")]
  #   crime_list$uber.timediff[i]<-min(potential_ubers$diff)
  # }
  # crime_list$uber.time<-as.POSIXct(as.numeric(crime_list$uber.time), origin = "1970-01-01", tz = "UTC")
  # ## Two lat/longs are infinity time diff but there are no uber calls close enough
  # ## Swap with NA's
  # crime_list[!is.finite(crime_list$uber.timediff),]$uber.time <- NA
  # crime_list[!is.finite(crime_list$uber.timediff),]$uber.timediff <- NA
  # 
  # # whats the distribution of distances for the "soonest"
  # summary(crime_list$uber.timediff)
  # hist(crime_list$uber.timediff)

############################################################
############################################################
## match crimes to uber data
############################################################
############################################################
  uberx.analysis <- dplyr::left_join(uber.x2.roll,crime_list,
                                     by=c(#"timestamp.half"="uber.time",
                                       "start_location_id"="uber.location"))
  uberx.analysis$has.crime <- ifelse(is.na(uberx.analysis$objectid)==TRUE,0,1)
  
  # count number of crimes that occured
  uberx.analysis <- uberx.analysis %>%
   dplyr::group_by(latitude, longitude, avg.surge, avg.low, avg.high, has.crime) %>%
   dplyr::summarise(num.crime = n())

  # adapted from Lev's code:
      library(SDMTools); library(maptools)
      tract <- readShapePoly('/Users/bradyfowler/Downloads/Census/Census_Tracts__2010.shp')
      cdata <- read.csv('/Users/bradyfowler/Downloads/Census/Census_Tracts__2010.csv')
      tract_geom <- fortify(tract, region = "GEOID")
      tract_poly <- merge(tract_geom, cdata, by.x = "id", by.y = "GEOID")
      # for each uber point, figure out which geoid it fits in
      # create new dataframe with lat, long of uber and geo ID
      geom_ids<-data.frame(unique(tract_geom$id))
      colnames(geom_ids)<-"unique.id"
      lat.long.geoid <- data.frame()
      for (i in 1:nrow(geom_ids)){
        current.id <- geom_ids[i,]
        tract_points_in_geom <- pnt.in.poly(lat.long[,c("longitude","latitude")], tract_geom[tract_geom$id==current.id, c("long","lat")])
        lat.long.geoid <- rbind(lat.long.geoid, cbind(tract_points_in_geom[tract_points_in_geom$pip==1,], current.id))
      }
  
 
  # map uber analysis data to the geoid to pull in demographic info
  uberx.analysis <- left_join(uberx.analysis, lat.long.geoid, by = c("latitude", "longitude"))
  # cast geoid
  cdata$GEOID <- factor(cdata$GEOID)
  # join in demographic data
  uberx.analysis <- left_join(uberx.analysis, cdata, by=c("current.id"="GEOID"))
  # create fields of demographic inputs
  uberx.analysis$total_population <- uberx.analysis$P0010001
  uberx.analysis$pct.minority <- (uberx.analysis$P0010001-uberx.analysis$P0020005)/uberx.analysis$P0010001
  uberx.analysis$pct.over.18 <- uberx.analysis$P0030001/uberx.analysis$P0010001
  uberx.analysis$pct.vacant.homes <- uberx.analysis$H0010003/uberx.analysis$H0010001

  ## test that points are matched correctly:
  # test<- 160
  # ggplot(subset(tract_poly, OBJECTID==test), aes(long, lat, group = group, fill=OBJECTID)) +
  #   geom_polygon() + coord_equal() +
  #   xlim(min(tract_poly$long), max(tract_poly$long))+
  #   ylim(min(tract_poly$lat), max(tract_poly$lat)) +
  #   geom_point(data=subset(uberx.analysis, OBJECTID==test),
  #              aes(x=longitude, y=latitude, group = factor(current.id), color="white")) + guides(color=FALSE, fill=FALSE)
  # 
  
## add hour of day and day of week
  # uberx.analysis$hr  <- hour(uberx.analysis$timestamp.half)
  # uberx.analysis$dow <- wday(uberx.analysis$timestamp.half)
  
## try to map a linear regression
  train.indices = sample(1:nrow(uberx.analysis), as.integer(nrow(uberx.analysis) * 0.75))
  pairs(uberx.analysis[,c("num.crime","avg.surge", "pct.minority", "pct.over.18", "pct.vacant.homes", "FAGI_MEDIAN_2013")])
  lm = lm(num.crime ~ avg.surge+pct.minority+pct.over.18+pct.vacant.homes+FAGI_MEDIAN_2013, data = uberx.analysis[train.indices, ])
  summary(lm)
  
  # predict on held-out 25% and evaluate raw accuracy
  predictions = predict(lm, newdata = uberx.analysis[-train.indices, ], type="response")
  num.correct = sum((predictions-uberx.analysis[-train.indices,]$num.crime))
  accuracy = num.correct / nrow(uberx.analysis[-train.indices, ])
  accuracy

## TRY TWO ON LINEAR
  library(caret)
  train.indices = sample(1:nrow(uberx.analysis), as.integer(nrow(uberx.analysis) * 0.75))
  log.lm = glm(has.crime ~ avg.surge+pct.minority+pct.over.18+pct.vacant.homes+FAGI_MEDIAN_2013, 
           data = uberx.analysis[train.indices, ], family="binomial")
  summary(log.lm)
  probsTest <- predict(log.lm, newdata=uberx.analysis[-train.indices, ], type = "response")
  threshold <- 0.5
  pred      <- factor( ifelse(probsTest > threshold, 1, 0) )
  confusionMatrix(pred, uberx.analysis[-train.indices, ]$has.crime)
  
  sum(uberx.analysis[-train.indices, ]$has.crime)

# look at clustering:
# library(ks);  library(RColorBrewer)
# points = crime[,c("X","Y")]
# Hpi1 <- Hpi(x = points, pilot="dscalar", bgridsize =c(300,300))
# plt <- kde(x = points, H = Hpi1, bgridsize = 2000)
# r <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
# image(plt$estimate, col = r, useRaster=TRUE, asp=1, axes=FALSE); 
