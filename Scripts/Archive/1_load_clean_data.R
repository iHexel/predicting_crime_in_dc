## load and clean uber data
## Read from raw CSV and produce binary Rda data files
## 9/10/2016
## db fowler

suppressWarnings(suppressMessages(library(dplyr)));

## read uber data in. downloaded from:
## https://drive.google.com/drive/u/0/folders/0B-mutxqHY34rblhORk9raWxQQjQ
uber <- read.csv("../Data/TractsSurgeDC2_Feb4_Mar2.csv")
## data doc:
# "timestamp" : string, Date and Time (EST) when API was pinged
# "surge_multiplier": float, The surge multiplier for the current time and location
# "expected_wait_time": integer, The number of seconds rider may have to wait between requesting a car, and the car's arrival
# "product_type": string, The type of car - uberTAXI, UberSUV, UberBLACK, uberX + Car Seat, uberX, uberXL, SUV + Car Seat, BLACK CAR + Car Seat
# "low_estimate": integer, lower end of an estimated price of the ride (dollars)
# "high_estimate": integer, upper end of an estimated price of the ride (dollars)
# "start_location_id": integer, number between 0-275 that relates to our predetermined longitudes and latitudes across DC.
# "end_location_id": integer, number between 0-275 that relates to our predetermined longitudes and latitudes across DC.


# limit to uberX
uber %>%
  filter(product_type=="uberX") %>%
  save(file="../output/uberx.Rda")

# pool together all types of ubers in case we decide to look at it this way
uber %>%
  group_by(timestamp, start_location_id) %>%
  summarise(avg_surge_multiplier=surge_multiplier %>% as.numeric %>% mean(na.rm=TRUE),
            avg_expected_wait_time=expected_wait_time %>% as.numeric %>% mean(na.rm=TRUE),
            avg_low_estimate=low_estimate %>% as.numeric %>% mean(na.rm=TRUE),
            avg_high_estimate=high_estimate %>% as.numeric %>% mean(na.rm=TRUE)) %>%
  save(file="../output/uberpooled.Rda")
