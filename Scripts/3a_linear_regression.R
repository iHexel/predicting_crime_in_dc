# Analyse datasets
suppressMessages(library(dplyr))
library(ggplot2)
library(readr)
library(car)
library(lubridate)

uber.pooled.census <- readRDS("../Output/uber.pooled.census.rds")
crime.data.census <- readRDS("../Output/crime.data.census.rds") %>% 
  mutate(timestamp=update(start_date,minutes=0,seconds=0)) 
# uber.pooled.census$tmp <- paste(month(uber.pooled.census$timestamp),'-',day(uber.pooled.census$timestamp),'-',hour(uber.pooled.census$timestamp))
# crime.data.census$tmp <- paste(month(crime.data.census$timestamp),'-',day(crime.data.census$timestamp),'-',hour(crime.data.census$timestamp))

# join crimes into uber data on the hourly level
left_join(uber.pooled.census,
          crime.data.census %>% 
            group_by(timestamp, census.tract) %>% 
            summarise(cnt.crimes=n()),
          by=c("timestamp", "census.tract")) -> uber.crime

## this will make sure crimes are only counted once
sum(uber.crime$cnt.crimes, na.rm=TRUE) == nrow(crime.data.census)
# not exactly joined 1:1 for some reason but close enough for now..

#################################
# create linear model
#################################
  ## format crime counts
  uber.crime$cnt.crimes <- replace(uber.crime$cnt.crimes, is.na(uber.crime$cnt.crimes), 0)
  uber.crime$has.crime <- ifelse(uber.crime$cnt.crimes>0,1,0)

  ## select training set
  train.indices = sample(1:nrow(uber.crime), as.integer(nrow(uber.crime) * 0.75))
  pairs(uber.crime[,c("has.crime","avg_surge_multiplier", "avg_expected_wait_time")])

  ## run model
  log.reg <- glm(has.crime ~ 
               avg_surge_multiplier+
               avg_expected_wait_time+
               factor(census.tract), data=uber.crime[train.indices, ], family="binomial")
  summary(log.reg)

# predict on held-out 25% and evaluate confusion matrix
  library(caret)
  predictions = predict(log.reg, newdata = uber.crime[-train.indices, ], type="response")
  threshold <- 0.1
  pred      <- factor( ifelse(predictions > threshold, 1, 0) )
  matrix <- confusionMatrix(pred, uber.crime[-train.indices,]$has.crime)
  matrix$table