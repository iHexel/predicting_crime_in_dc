## load and clean uber data
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