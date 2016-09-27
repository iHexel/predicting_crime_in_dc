# Analyse datasets
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(maptools))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(maptools))
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(ROCR))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(caret))


########################################################################
#
# import data and join crimes to uber info in hour before
#
########################################################################
## add lagged timestamp for crime to compare 9 pm uber surge to 10 pm crime
crime.data.census <- readRDS("../Output/crime.data.census.rds") %>%
  mutate(timestamp=update(start_date,minutes=0,seconds=0)  + hours(1))

uber.pooled.census <- readRDS("../Output/uber.pooled.census.rds")

# join crime into uber data on the hourly level
left_join(uber.pooled.census,
          crime.data.census %>%
            group_by(timestamp, census.tract) %>%
            summarise(cnt.crime=n()),
          by=c("timestamp", "census.tract")) -> uber.crime

## add day / evening / night
uber.crime$tod <- ifelse(hour(uber.crime$timestamp) < 6,                                     "Late Night",     uber.crime$timestamp)
uber.crime$tod <- ifelse(hour(uber.crime$timestamp) < 12 & hour(uber.crime$timestamp) >= 6,  "Morning",   uber.crime$tod)
uber.crime$tod <- ifelse(hour(uber.crime$timestamp) < 18 & hour(uber.crime$timestamp) >= 12, "Afternoon", uber.crime$tod)
uber.crime$tod <- ifelse(hour(uber.crime$timestamp) < 24 & hour(uber.crime$timestamp) >= 18, "Evening",   uber.crime$tod)

## set any NA's to 0 so they arent excluded in regression
uber.crime[is.na(uber.crime)] <- 0

## add summed nightlife variable
uber.crime %>%
  mutate(nightlife = nightclub+tavern+club+liquor.st) -> uber.crime

## num obs by week : determine train v. test proportion
uber.crime %>%
  group_by(week(timestamp)) %>%
  summarise(cnt = n()) %>%
  mutate(cnt*100/sum(cnt))
  # want to use weeks 5-8 as training

########################################################################
#
# create logistic model
#
########################################################################
## format crime counts
uber.crime$cnt.crime <- replace(uber.crime$cnt.crime, is.na(uber.crime$cnt.crime), 0)
uber.crime$has.crime <- ifelse(uber.crime$cnt.crime>0,1,0)
uber.crime$hour <- hour(uber.crime$timestamp)
uber.crime$hour.factor <- factor(uber.crime$hour)

## select training and testing set by month
train = uber.crime %>% filter(week(timestamp)<9)
test  = uber.crime %>% filter(week(timestamp)==9)

# Alternative to pairs function
chart.Correlation(train[1:1000,c("has.crime", "hour", "avg_surge_multiplier","avg_expected_wait_time","pct.minority","pct.over.18","pct.vacant.homes","med.income.2013","tot.income.2013","nightclub","tavern","restaurant","club","liquor.st")],
                  method="spearman",
                  histogram=TRUE,
                  pch=16)
  ## High pos. correlation between net and median income variables
  ## High neg. correlation between pct. minority and both income variables
  ## Noticeable pos. correlation between pct. over 18 and income variables
  ## Noticeable correlation of restaurant variable with other demographic factors

# Alternative model selection method using a stepwise selection process (based on AIC)
  ## AIC shows the tradeoff between goodness of fit and complexity of the model, a mean of model selection
model.null <- glm(has.crime ~ 1, data = train, family = binomial(link = "logit"))

model.full <- glm(has.crime ~
                    avg_surge_multiplier+
                    avg_expected_wait_time+
                    pct.minority+
                    pct.over.18+
                    pct.vacant.homes+
                    med.income.2013+
                    tot.income.2013+
                    nightlife+
                    hour.factor,
                  data = train, family = binomial(link = "logit"))

step(model.null,
     scope = list(upper = model.full),
     direction = "both",
     test = "Chisq",
     data = train)

# based on stepwise model selection, the final model is the following
model.final <- glm(formula = has.crime~hour.factor+nightlife+pct.vacant.homes+tot.income.2013+
                     pct.minority+pct.over.18+avg_expected_wait_time,
                   family = binomial(link = "logit"), data = train)
summary(model.final) # AIC: 15439

Anova(model.final, type = "II", test = "Wald")
#                        Df     Chisq     Pr(>Chisq)
# hour.factor            23   392.4592    < 2.2e-16 ***
# nightlife               1   225.1550    < 2.2e-16 ***
# pct.vacant.homes        1    25.4115    4.631e-07 ***
# tot.income.2013         1    26.5169    2.612e-07 ***
# pct.minority            1    23.8466    1.043e-06 ***
# pct.over.18             1     9.6056      0.00194 **
# avg_expected_wait_time  1     4.0573      0.04398 *
## All variables are statistically significant

vif(model.final)
#                            GVIF   Df    GVIF^(1/(2*Df))
# hour.factor            1.255068   23        1.004951
# nightlife              1.333504    1        1.154774
# pct.vacant.homes       1.116485    1        1.056638
# tot.income.2013        2.346010    1        1.531669
# pct.minority           3.809870    1        1.951889
# pct.over.18            2.899222    1        1.702710
# avg_expected_wait_time 1.603318    1        1.266222
## no apparent multicollinearity issues

# Residuals vs fitted values plot
plot(fitted(model.final), rstandard(model.final), col = c("blue", "red"), main = "Residuals VS Fitted", xlab = "Fitted Values", ylab = "Studentized Residuals")
abline(h = 0, lty = 2, col = "grey")
lines(lowess(fitted(model.final), rstandard(model.final)), col = "green", lwd = 2)

## run model
log.reg.no.hr <- glm(has.crime ~
                       avg_surge_multiplier+
                       avg_expected_wait_time+
                       pct.minority+
                       pct.over.18+
                       pct.vacant.homes+
                       tot.income.2013+
                       nightlife,
                     data=train, family="binomial")
summary(log.reg.no.hr) # AIC: 15878

log.reg.plus.hr <- glm(has.crime ~
               avg_expected_wait_time+
               pct.minority+
               pct.over.18+
               pct.vacant.homes+
               tot.income.2013+
               nightlife+
               hour.factor,
               data=train, family="binomial")
summary(log.reg.plus.hr) # AIC: 15441

# code for outputting tables:
Anova(log.reg.no.hr, type = "II", test = "Wald")
Anova(log.reg.plus.hr, type = "II", test = "Wald")
## Shows that avg. surge multiplier is insignificant when time is included

vif(log.reg.no.hr)
vif(log.reg.plus.hr)[,1]
## VIF with a cutoff of 5 does not show that any multicolinearity
## exists in either model - we do know that the model including hour is better, however

################################################################################
#
## run predictions of both models
#
################################################################################
# create ROC curve on logit models
par(mfrow = c(1,2))
roc.pred <- prediction(predict(log.reg.no.hr, newdata = test, type = "response"), test$has.crime)
roc.perf <- performance(roc.pred, "tpr", "fpr")
auc <- performance(roc.pred, "auc")@y.values[[1]]
plot(roc.perf, main = paste("Model Excluding Hour\nROC (AUC=", round(auc,2), ")", sep = ""))
abline(0, 1, lty = "dashed")

roc.predalt <- prediction(predict(log.reg.plus.hr, newdata = test, type = "response"), test$has.crime)
roc.perfalt <- performance(roc.predalt, "tpr", "fpr")
aucalt <- performance(roc.predalt, "auc")@y.values[[1]]
plot(roc.perfalt, main = paste("Model Including Hour\nROC (AUC=", round(aucalt,2), ") Alt", sep = ""), col = "red")
abline(0, 1, lty = "dashed", col = "red")
par(mfrow = c(1,1))

## create matrix using best threshold
predictions <- predict(log.reg.plus.hr, newdata = test, type="response")

# simulate different cutoffs
pos<-vector()
neg<-vector()
for (i in seq(0,1,.01)) {
  t.1 <- i
  pred        <- factor(ifelse(predictions > t.1, 1, 0))
  matrix      <- confusionMatrix(pred, test$has.crime)
  pos <- c(pos,matrix$byClass["Pos Pred Value"])
  neg <- c(neg,matrix$byClass["Neg Pred Value"])
}
plot(neg,pos)

threshold   <- 0.1
pred        <- factor(ifelse(predictions > threshold, 1, 0))
matrix      <- confusionMatrix(pred, test$has.crime)
matrix$table


## add predictions for last hour of data:
last.day <- test %>% filter(timestamp==max(test$timestamp))
last.day$predictions <- data.frame(predict(log.reg.plus.hr, newdata = last.day, type="response"))[,1]

## plot prediction next to actual
tract <- fortify(readShapePoly('../Data/Census/Census_Tracts__2010.shp'), region = "GEOID")
tract_poly <- merge(tract, last.day, by.x = "id", by.y = "census.tract")

# plot geoshapes colored by prediction
pred <- ggplot(tract_poly, aes(long, lat, group = group, fill = predictions))  +
  geom_polygon(colour="#ecf0f1", size=.2) + coord_equal() +
  ggtitle("Logistic Crime Prediction") +
  scale_fill_gradient(low = "skyblue1", high = "blue4",
                      guide = guide_legend(title = "Log Odds")) +
  theme(legend.position = "bottom",
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# plot actual
act <- ggplot(tract_poly, aes(long, lat, group = group, fill = cnt.crime)) +
  geom_polygon(colour="#ecf0f1", size=.2) + coord_equal() +
  ggtitle("Actual Crime Count") +
  scale_fill_gradient(low = "skyblue1", high = "blue4", breaks=c(0,1,2,3,4),
                      guide = guide_legend(title = "# Crimes")) +
  theme(legend.position = "bottom",
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Plot side by side
main=textGrob("Predicted vs. Actual Crime by Neighborhood\n2016-03-03 08:00:00 UTC",gp=gpar(fontsize=15,font=3))
grid.arrange(pred, act, ncol=2, top = main)


########################################################################
#
# CALCULATE SURVEILLANCE PLOT
#
########################################################################
## this will evaluate how our model performs as a tool for cops to determine how to prioritize neighborhoods
## we want to rank the neighborhoods in terms of descending probability
## count the # crimes that occured in those neighborhoods as # total crimes
predictions <- predict(log.reg.plus.hr, newdata = test, type="response")
test$predictions <- data.frame(predict(log.reg.plus.hr, newdata = test, type="response"))[,1]

# rank the predictions by highest to lowest
# find cumulative pct crime captured by neighborhood by ranked prediction
test %>%
  group_by(timestamp) %>%
  mutate(predict.rank=rank(-predictions, ties.method = 'first'),
         pct.crime = cnt.crime*100/sum(cnt.crime),
         pct.surveilled = round(predict.rank*100/max(predict.rank),0)) %>%
  arrange(predict.rank) %>% mutate(tot.pct.crime = cumsum(pct.crime)) -> test

# create summary of pct surveilled by time of day
# all times together
surveil.summary <- test %>%
    mutate(tod="All Times") %>%
    group_by(tod, pct.surveilled) %>%
    summarise(avg.crime.caught = mean(tot.pct.crime, na.rm=TRUE))

# by time of day
surveil.summary.tod <- test %>%
    group_by(tod, pct.surveilled) %>%
    summarise(avg.crime.caught = mean(tot.pct.crime, na.rm=TRUE))

# plot for all times
all.time <- ggplot(surveil.summary, aes(x=pct.surveilled, y=avg.crime.caught, colour=tod)) +
  theme(legend.position = "bottom") + geom_point() + geom_line() +coord_equal() +
  scale_color_manual(values="#2ecc71", guide = guide_legend(title = "")) +
  ggtitle("All Times of Day")+ geom_abline(aes(intercept=0, slope=1)) +
  scale_x_continuous(name="Percent Surveilled", breaks=seq(0,100,5)) +
  scale_y_continuous(name="Avg % Incidents Caught", breaks=seq(0,100,10))

# plot by time
by.time <- ggplot(surveil.summary.tod, aes(x=pct.surveilled, y=avg.crime.caught, colour=tod)) +
  theme(legend.position = "bottom") + geom_point() + geom_line() +coord_equal() +
  scale_color_manual(values=c("#95a5a6", "#3498db", "#34495e", "#e74c3c"), guide = guide_legend(title = "")) +
  ggtitle("All Times of Day")+ geom_abline(aes(intercept=0, slope=1)) +
  scale_x_continuous(name="Percent Surveilled", breaks=seq(0,100,5)) +
  scale_y_continuous(name="Avg % Incidents Caught", breaks=seq(0,100,10))

# plot both together
main=textGrob("Surveillance Plot by Time of Day",gp=gpar(fontsize=15,font=3))
grid.arrange(all.time, by.time, ncol=2, top = main)
