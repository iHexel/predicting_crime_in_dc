# Collect and combine the datasets

suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(deldir))
suppressMessages(library(ks))
suppressMessages(library(purrr))
library(multidplyr)
library(mapproj)
library(ggplot2)
library(readr)


uber.x <- readRDS("../Output/uberx.rds")
uber.pooled <- readRDS("../Output/uberpooled.rds")
uber.locations <- read_csv("../Data/awsLocations.csv")
crime.data <- read_csv("../Data/Crime_Incidents__2016.csv") %>%
  filter(
    start_date > min(uber.pooled$timestamp),
    start_date < max(uber.pooled$timestamp),
    !is.na(end_date)
  ) %>%
  select(longitude, latitude, offense, ward, district, census_tract, start_date, end_date)


left_join(uber.pooled,
          uber.locations,
          by=c("start_location_id"="locations")) %>%
  select(-start_location_id) ->
  uber.pooled.long.lat


crime.data %>%
  select(longitude,latitude,start_date) %>%
  rename(timestamp=start_date) %>%
  mutate(timestamp=as.numeric(timestamp)) ->
  source.pts.crime
# source.pts.crime %>% head


uber.pooled.long.lat %>%
  select(longitude, latitude, timestamp) %>%
  ungroup %>%
  mutate(timestamp=as.numeric(timestamp)) ->
  eval.pts.uber
# eval.pts.uber %>% head


h <- Hpi(x = source.pts.crime,
         pilot="dscalar")
# This naive use spends ~90 minutes on my box to finish
k <- kde(x = source.pts.crime,
         H = (h + t(h))/2, # NOTE: the h matrix from Hpi isn't quite symetric; this forces it to be
         eval.points = eval.pts.uber)


k %>% saveRDS(file="../output/kde.evals.rds")



# data.frame(a=1:50,b=50:1) %>%
#   partition() %>%
#   mutate(c=a+1) %>%
#   collect() %>%
#   arrange(a)

# NOTE: !!! This is not working!
#       dmvnorm mean and sigma have non-conforming size error
# eval.pts.fn <- function(eval.pts) {
#   k <- kde(x = source.pts.crime,
#            H = (h + t(h))/2, # NOTE: the h matrix from Hpi isn't quite symetric; this forces it to be
#            eval.points = eval.pts %>% select(-PARTITION_ID))
#   cbind(eval.pts,data.frame(estimate=k$estimate))
# }
#
# eval.pts.uber %>%
#   partition() %>%
#   eval.pts.fn %>%
#   collect() %>%
#   saveRDS(file="../output/temp.evals.rds")
