# Collect and combine the datasets
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(deldir))
suppressMessages(library(ks))
suppressMessages(library(purrr))
suppressMessages(library(mapproj))
# # install.packages("devtools")
# devtools::install_github("hadley/multidplyr")
library(multidplyr)
library(mapproj)
library(ggplot2)
library(readr)
library(SDMTools)
library(maptools)

################################################################################
#
# Load External Data
#
################################################################################
census.tracts <- readShapePoly('../Data/Census/Census_Tracts__2010.shp')
census.data <- read.csv('../Data/Census/Census_Tracts__2010.csv')
# uber <- readRDS("../Output/uber.rds")
uber.locations <- read_csv("../Data/awsLocations.csv")
crime.data <- read_csv("../Data/Crime_Incidents__2016.csv") %>%
  filter(
    start_date > min(uber.pooled$timestamp),
    start_date < max(uber.pooled$timestamp),
    !is.na(end_date)
  ) %>%
  select(longitude, latitude, offense, ward, district, census_tract, start_date, end_date)

################################################################################
#
# Census Location Tools
#
################################################################################
tract_geom <- fortify(census.tracts, region = "GEOID")
tract_poly <- merge(tract_geom, census.data, by.x = "id", by.y = "GEOID")

assign.census.tract.id <- function(crime.data.current,ids) {
  if (length(ids) < 1) return(crime.data.current)
  id.current <- ids %>% first
  id.tail <- ids %>% tail(length(ids)-1)

  pnt.in.poly(
      crime.data.current %>% select(longitude, latitude),
      tract_geom %>% filter(id==id.current) %>% select(long,lat)
    ) %>% select(pip) %>% unlist -> pip

  crime.data.current %>%
    mutate(census.tract=ifelse(pip==1,id.current,census.tract)) %>%
    assign.census.tract.id(id.tail)
}
add.census.tract <- function(long.lat.data) {
  tract_geom %>%
    get('id',.) %>%
    unique -> tract_geom.ids
  long.lat.data %>%
    mutate(census.tract=NA) %>%
    assign.census.tract.id(tract_geom.ids)
}

# super fast 'cheating'
crime.data.add.census.tract <- function(crime.data) {
  crime.data %>%
    mutate(census.tracts=paste('11001',census_tract,sep='')) %>%
    select(-census_tract)
}

################################################################################
#
# Combine and Group By Census Tracts
#
################################################################################
## Read in full Uber file
uber <- read_csv("../Data/TractsSurgeDC2_Feb4_Mar2.csv")

## Pull uber locations/census tract to full file
left_join(uber,
          uber.locations %>% add.census.tract,
          by=c("start_location_id"="locations")) %>%
  select(-start_location_id) -> uber.census

## group by hourly timestamp and census tract and roll up
uber.census %>%
   mutate(timestamp=update(timestamp,minutes=0,seconds=0)) %>%
   partition(timestamp, census.tract) %>%
   summarise(avg_surge_multiplier=mean(as.numeric(surge_multiplier), na.rm=TRUE),
            avg_expected_wait_time=mean(as.numeric(expected_wait_time), na.rm=TRUE),
            avg_low_estimate=mean(as.numeric(low_estimate), na.rm=TRUE),
            avg_high_estimate=mean(as.numeric(high_estimate ), na.rm=TRUE)) %>%
  collect() -> uber.pooled.census

uber.pooled.census %>% saveRDS(file="../output/uber.pooled.census.rds")

# ## something is wrong with this combination - it has multiple rows per timestamp/census tract
# uber.pooled.census %>% 
#   filter(timestamp =="2016-02-08 18:00:00", census.tract=="11001000300") %>% 
#   glimpse()
# ## i dont think we want to roll up the uber data until we have tagged it with census.tract
# ## because there are multiple uber locations within a neighborhood so we will want to keep it at the
# ## request level not request location level

crime.data %>% add.census.tract -> crime.data.census

# TODO: group by and counts and other values

# TODO: Remove these writes and only write out the final
crime.data.census %>% saveRDS(file="../output/crime.data.census.rds")
