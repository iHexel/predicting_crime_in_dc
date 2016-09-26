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
uber <- read_csv("../Data/TractsSurgeDC2_Feb4_Mar2.csv")
uber.locations <- read_csv("../Data/awsLocations.csv")
crime.data <- read_csv("../Data/Crime_Incidents__2016.csv") %>%
  filter(
    start_date > min(uber$timestamp),
    start_date < max(uber$timestamp),
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

# add census tract to crime data
crime.data %>% add.census.tract -> crime.data.census
crime.data.census %>% saveRDS(file="../output/crime.data.census.rds")

################################################################################
#
# Combine and Group By Census Tracts
#
################################################################################
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

rm(uber)
rm(uber.census)

################################################################################
#
# Import Demographics Data
#
################################################################################
census.data %>%
  mutate(geoid            = as.character(GEOID),
         total.population = P0010001,
         pct.minority     = (P0010001-P0020005)/P0010001,
         pct.over.18      = P0030001/P0010001,
         pct.vacant.homes = H0010003/H0010001,
         med.income.2013  = FAGI_MEDIAN_2013,
         tot.income.2013  = FAGI_TOTAL_2013) %>%
  select(geoid, total.population, pct.minority, pct.over.18,
         pct.vacant.homes, med.income.2013, tot.income.2013) -> census.data

left_join(uber.pooled.census,
          census.data,
          by=c("census.tract"="geoid")) -> uber.pooled.census

################################################################################
#
# Import Liquor License Data
#
################################################################################
# read in tables and extract coordinates
#liquor stores
liquor <- data.frame(readShapePoints("../Data/Liquor_License_Locations/Liquor_License_Locations.shp")) %>%
  mutate(nightclub  = ifelse(TYPE == 'Nightclub', 1, 0),  tavern  = ifelse(TYPE == 'Tavern', 1, 0),
         restaurant = ifelse(TYPE == 'Restaurant', 1, 0), club    = ifelse(TYPE == 'Club', 1, 0),
         liquor.st  = ifelse(TYPE == 'Retail - Liquor Store', 1, 0)) %>%
  filter(nightclub+tavern+restaurant+club+liquor.st > 0 ) %>%
  select(address=ADDRESS, longitude=coords.x1, latitude=coords.x2,
         type = TYPE, nightclub, tavern, restaurant, club, liquor.st) %>%
  add.census.tract

# Plots to check for general distribution
  ggplot(liquor, aes(x=longitude, y=latitude, color=type)) + geom_point() + coord_equal()

# Group by census tract
liquor %>%
  group_by(census.tract) %>%
  summarise(nightclub  = sum(nightclub),
            tavern     = sum(tavern),
            restaurant = sum(restaurant),
            club       = sum(club),
            liquor.st  = sum(liquor.st)) -> liquor.count

# add to uber
left_join(uber.pooled.census,
          liquor.count,
          by=c("census.tract")) -> uber.pooled.census

uber.pooled.census %>% saveRDS(file="../output/uber.pooled.census.rds")
