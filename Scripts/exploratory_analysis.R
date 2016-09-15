
library(dplyr); library(ggplot2); library(lubridate)
# tracts 
uber <- read.csv("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/Assignment1/TractsSurgeDC2_Feb4_Mar2.csv")
nrow(uber)
# 53,695,775

# check out product types
uber %>% 
  group_by(product_type) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count)) %>% 
  arrange(desc(count))
## pretty much evenly split. decide to use uberX only

# limit to uberX
uber.x <- uber %>% filter(product_type=="uberX")
rm(uber)

head(uber.x)

## is start always end?
uber.x %>% filter(start_location_id!=end_location_id) %>% summarise(n())
## yep

## pull in latitude and longitude locations
lat.long <- read.csv("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/Assignment1/awsLocations.csv")

ggplot(lat.long, aes(x=longitude, y=latitude)) + 
  geom_point() + 
  coord_equal()

## join lat/long into uberx
## remove unnecessary columns
uber.x  <- left_join(uber.x, lat.long, by=c("start_location_id"="locations")) %>% 
  select(-end_location_id, -expected_wait_time, -product_type)
uber.x$timestamp <- ymd_hms(uber.x$timestamp, tz = "UTC")
head(uber.x)

## import crime data
crime <- read.csv("/Users/bradyfowler/Documents/Fall Semester/Mining_6018/Assignment1/Crime_Incidents__2016.csv")
colnames(crime)<-tolower(c("long", "lat", colnames(crime)[3:length(crime)]))

# fix dates:
crime$reportdatetime    <- ymd_hms(crime$reportdatetime, tz = "UTC")
crime$lastmodifieddate  <- ymd_hms(crime$lastmodifieddate, tz = "UTC")
crime$start_date        <- ymd_hms(crime$start_date, tz = "UTC")
crime$end_date          <- ymd_hms(crime$end_date, tz = "UTC")

nrow(crime)

## look at offense types
ggplot(crime, aes(x=X, y=Y)) + 
  geom_point() + 
  coord_equal()


library(ks);  library(RColorBrewer)
points = crime[,c("X","Y")]
Hpi1 <- Hpi(x = points, pilot="dscalar", bgridsize =c(300,300))
plt <- kde(x = points, H = Hpi1, bgridsize = 2000)
r <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
image(plt$estimate, col = r, useRaster=TRUE, asp=1, axes=FALSE); 

head(uber.x)
# third decimal place of lat/long is unnecessary if we are interested in neighborhood level:
library(spatstat); library(plyr)
row_num=2
crime_list<- crime[,c("long", "lat", "reportdatetime", "objectid")]
head(uber.x)
nearest_soonest <- function(row_num){
  deets <- uber.x[row_num, c("timestamp", "latitude", "longitude")]
  potential_crimes <- crime_list[abs(deets[,2]-crime_list$lat)  < .1 &
                                 abs(deets[,3]-crime_list$long) < .1 &
                                 abs(difftime(deets[,1],crime_list$reportdatetime,units="hours")) < 1,
                                 ]
  # potential_crimes <- crime_list[dist=crossdist(X = deets[2],
  #                                               Y = deets[3],
  #                                               x2= crime_list$lat,
  #                                               y2= crime_list$long) < 1 &
  #                                abs(difftime(deets[1],crime_list$timestamp,units="hours")) < 1,
  #                                ]
  #potential_crimes %>% plyr::mutate(dist=crossdist(X=latitude,Y=longitude,x2=lat,y2=long) %>% as.numeric())
}

difftime(deets[1],crime_list$reportdatetime[1],units="hours") < 0

head(potential_crimes)

crime_list[crime_list$lat-)<,]

options(sp)
deet2<- deets
deet2$latitude<-deet2$latitude+1
head(deets)
head(deet2)

xx<-left_join(deets, deet2, by=c("latitude.x-latitude.y"<1))

x<-head(potential_crimes)
x %>% mutate(dist=crossdist(X=latitude,Y=longitude,x2=lat,y2=long))


uber.x[2, c("timestamp")]
crossdist(X=38.90155, Y=-77.05967, x2= 38.91155, y2= -77.05967)
crossdist(X=38.91155, Y=-77.05967, x2= 38.91155, y2= -77.05967)
crossdist(X=38.93155, Y=-77.02967, x2= 38.91155, y2= -77.05967)
crossdist(X=38.90155, Y=-76.94967, x2= 38.91155, y2= -77.05967)
crossdist(X=38.90155, Y=-76.98967, x2= 38.91155, y2= -77.05967)
crossdist(X=38.91155, Y=-76.98967, x2= 38.91155, y2= -77.05967)

