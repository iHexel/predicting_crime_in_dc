# Read from raw CSV and produce binary Rda data files
# NOTE: This makes use of multidplyr - a multicore toolset for dplyr
#       The result is a ~5 speed up of this operation on my machine (8 cores)
#       Most of the time is still spent on the synchronous action
#       for reading the csv

suppressWarnings(suppressMessages(library(dplyr)));
# # install.packages("devtools")
# devtools::install_github("hadley/multidplyr")
library(multidplyr)

paste("Starting Processing ...", Sys.time())

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

paste("Loaded CSV ...", Sys.time())

# limit to uberX
uber %>%
  filter(product_type=="uberX") %>%
  select(-product_type) %>%
  mutate(timestamp=as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S")) %>%
  saveRDS(file="../output/uberx.rds") # RDS serializes the data and prevents namespace pollution on readRDS


# pool together all types of ubers in case we decide to look at it this way
uber %>%
  mutate(timestamp=as.POSIXct(as.character(timestamp), format="%Y-%m-%d %H:%M:%S")) %>%
  partition(timestamp, start_location_id) %>%
  summarise(avg_surge_multiplier=mean(as.numeric(surge_multiplier), na.rm=TRUE),
            avg_expected_wait_time=mean(as.numeric(expected_wait_time), na.rm=TRUE),
            avg_low_estimate=mean(as.numeric(low_estimate), na.rm=TRUE),
            avg_high_estimate=mean(as.numeric(high_estimate ), na.rm=TRUE)) %>%
  collect() %>%
  saveRDS(file="../output/uberpooled.rds")

paste("Done", Sys.time())
