# Analyse datasets
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(lubridate))
suppressMessages(library(maptools))
suppressMessages(library(car))
library(ggplot2)
library(readr)
library(car)
library(lubridate)
library(maptools)

## notes from LEE:
  ## hypothesis: more people more crime
  ## train on month one and then test on the other month
  ## cant use data from the next moment in creating prediction for this moment so compare current hour stats to next hour's crime
  # discuss the assumptions and tests for multi colinearity:
    # heterogeinity
    # independence
    # normality
    # test for outliers
  # if you add historical crime rates into the model then it could affect the validity
      ## discuss why we didnt keep last hour's crime into the model
  ## USE VIF TO TEST THE INCLUSION OF VARIABLES
    ## discuss why k-fold cross validation does not apply here because of the temporality
  ## try a surveillance plot or ROC curve to show result
  ## keep in mind that crime is a rare event so data is highly skewed so have to accomodate
    ## when choosing training and testing and in choosing threshhold

########################################################################
#
# import data and join crimes to uber info in hour before
#
########################################################################

# setwd("C:/Users/hexel/Documents/R/SYS6018/Case1/v2/case1-crime-brady_dev")
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

## select training and testing set by month
train = uber.crime %>% filter(week(timestamp)<9)
test  = uber.crime %>% filter(week(timestamp)==9)
pairs(train[1:1000,c("cnt.crime", "has.crime", "avg_surge_multiplier","avg_expected_wait_time","pct.minority","pct.over.18","pct.vacant.homes","med.income.2013","tot.income.2013","nightclub","tavern","restaurant","club","liquor.st")])
  ## might not want both income factors in
  ## caution with pct minority and median income included together

# Alternative to pairs function
library(PerformanceAnalytics)
chart.Correlation(train[1:1000,c("cnt.crime", "has.crime", "avg_surge_multiplier","avg_expected_wait_time","pct.minority","pct.over.18","pct.vacant.homes","med.income.2013","tot.income.2013","nightclub","tavern","restaurant","club","liquor.st")],
                  method="spearman",
                  histogram=TRUE,
                  pch=16)
  ## High pos. correlation between net and median income variables
  ## High neg. correlation between pct. minority and both income variables
  ## Noticeable pos. correlation between pct. over 18 and income variables
  ## Noticeable correlation of restaurant variable with other demographic factors

# Alternative model selection method using a stepwise selection process (based on AIC)
  ## AIC shows the tradeoff between goodness of fit and complexity of the model, a mean of model selection
model.null <- glm(has.crime ~ 1,
                  data = train, family = binomial(link = "logit"))

model.full <- glm(has.crime ~
                    avg_surge_multiplier+
                    avg_expected_wait_time+
                    pct.minority+
                    pct.over.18+
                    pct.vacant.homes+
                    med.income.2013+
                    tot.income.2013+
                    nightlife,
                  data = train, family = binomial(link = "logit"))

step(model.null,
     scope = list(upper = model.full),
     direction = "both",
     test = "Chisq",
     data = train)

# based on stepwise model selection, the final model is the following
model.final <- glm(formula = has.crime ~ nightlife + pct.vacant.homes + tot.income.2013 +
                     pct.minority + pct.over.18 + avg_expected_wait_time + avg_surge_multiplier,
                   family = binomial(link = "logit"), data = train)

summary(model.final) # AIC: 15878

Anova(model.final, type = "II", test = "Wald")
#                           Df   Chisq Pr(>Chisq)
#   nightlife               1 233.112  < 2.2e-16 ***
#   pct.vacant.homes        1  24.700  6.700e-07 ***
#   tot.income.2013         1  28.895  7.641e-08 ***
#   pct.minority            1  24.673  6.792e-07 ***
#   pct.over.18             1  24.337  8.088e-07 ***
#   avg_expected_wait_time  1  12.728  0.0003601 ***
#   avg_surge_multiplier    1   6.126  0.0133211 *
  ## All variables are statistically significant

vif(model.final)
# nightlife       pct.vacant.homes        tot.income.2013           pct.minority
# 1.342967               1.112263               2.338107               3.807767
# pct.over.18 avg_expected_wait_time   avg_surge_multiplier
# 2.853191               1.297919               1.010500
  ## no apparent multicollinearity issues

# Residuals vs fitted values plot
plot(fitted(model.final), rstandard(model.final), col = c("blue", "red"), main = "Residuals VS Fitted", xlab = "Fitted Values", ylab = "Studentized Residuals")
abline(h = 0, lty = 2, col = "grey")
lines(lowess(fitted(model.final), rstandard(model.final)), col = "green", lwd = 2)
  ## According to the plot, data exhibits lack of heterogeinity, interdependence and lack of normality
  ## There are also many outliers
  ## All of these issues are likely due to the temporal and mixed nature of our data
  ## Cannot base model selection on residual analysis, will rely on VIF and summary statistics


## run model
log.reg <- glm(has.crime ~
               avg_surge_multiplier+
               avg_expected_wait_time+
               pct.minority+
               pct.over.18+
               pct.vacant.homes+
               med.income.2013+
               tot.income.2013+
               nightlife,
               #factor(census.tract),
               data=train, family="binomial")
summary(log.reg) # AIC: 15880

Anova(log.reg, type = "II", test = "Wald")
#                           Df    Chisq Pr(>Chisq)
#   avg_surge_multiplier    1   6.1436  0.0131887 *
#   avg_expected_wait_time  1  12.7018  0.0003653 ***
#   pct.minority            1  16.3356  5.306e-05 ***
#   pct.over.18             1  24.2389  8.510e-07 ***
#   pct.vacant.homes        1  24.2301  8.549e-07 ***
#   med.income.2013         1   0.0324  0.8571918
#   tot.income.2013         1  24.4038  7.811e-07 ***
#   nightlife               1 233.1964  < 2.2e-16 ***
  ## med.income is statistically insignificant

vif(log.reg)
# avg_surge_multiplier avg_expected_wait_time   pct.minority          pct.over.18
# 1.011094               1.298632               5.478816               2.861129
# pct.vacant.homes      med.income.2013        tot.income.2013        nightlife
# 1.124398               4.600962               2.857838               1.343451
  ## Apparent multicollinearity issue with pct. minority

log.reg.2 <- glm(has.crime ~
                 avg_surge_multiplier+
                 avg_expected_wait_time+
                 # pct.minority+
                 pct.over.18+
                 pct.vacant.homes+
                 med.income.2013+
                 tot.income.2013+
                 nightlife,
               #factor(census.tract),
               data=train, family="binomial")
summary(log.reg.2) # AIC: 15894

Anova(log.reg.2, type = "II", test = "Wald")
#                         Df    Chisq Pr(>Chisq)
# avg_surge_multiplier    1   6.2897  0.0121445 *
# avg_expected_wait_time  1  11.8328  0.0005820 ***
# pct.over.18             1  11.2698  0.0007878 ***
# pct.vacant.homes        1  24.4012  7.822e-07 ***
# med.income.2013         1   8.7404  0.0031124 **
# tot.income.2013         1  17.8684  2.367e-05 ***
# nightlife               1 241.9360  < 2.2e-16 ***
  ## All variables are statistically significant

vif(log.reg.2)
# avg_surge_multiplier avg_expected_wait_time   pct.over.18       pct.vacant.homes
# 1.010746               1.306282               2.095746               1.133208
# med.income.2013        tot.income.2013        nightlife
# 3.182878               2.676318               1.332628
  ## No apparent multicollinearity

# Residuals vs fitted values plot
plot(fitted(log.reg.2), rstandard(log.reg.2), col = c("blue", "red"), main = "Residuals VS Fitted", xlab = "Fitted Values", ylab = "Studentized Residuals")
abline(h = 0, lty = 2, col = "grey")
lines(lowess(fitted(log.reg.2), rstandard(log.reg.2)), col = "green", lwd = 2)
  ## According to the plot, data exhibits lack of heterogeinity, interdependence and lack of normality
  ## There are also many outliers
  ## All of these issues are likely due to the temporal and mixed nature of our data
  ## Cannot base model selection on residual analysis, will rely on VIF and summary statistics

# Model which removes pct.minority and med.income.2013, based on previous VIF values
log.reg.3 <- glm(has.crime ~
                 avg_surge_multiplier+
                 avg_expected_wait_time+
                 # pct.minority+
                 pct.over.18+
                 pct.vacant.homes+
                #  med.income.2013+
                 tot.income.2013+
                 nightlife,
               #factor(census.tract),
               data=train, family="binomial")
summary(log.reg.3)

Anova(log.reg.3, type = "II", test = "Wald")
#                        Df    Chisq          Pr(>Chisq)
# avg_surge_multiplier   1     5.983161       1.444310e-02
# avg_expected_wait_time 1     12.140357      4.934234e-04
# pct.over.18            1     6.161192       1.305832e-02
# pct.vacant.homes       1     28.987264      7.285574e-08
# tot.income.2013        1     8.781420       3.043142e-03
# nightlife              1     239.705777     4.558894e-54
  ## All variables are statistically significant, but not quite as much

vif(log.reg.3)
# avg_surge_multiplier
# 1.01010499245473
# avg_expected_wait_time
# 1.30766218549044
# pct.over.18
# 1.83749772557633
# pct.vacant.homes
# 1.12561466869971
# tot.income.2013
# 1.4361581769739
# nightlife
# 1.34273233551098
  ## All under 2, no apparent multicollinearity

# Residuals vs fitted values plot
plot(fitted(log.reg.3), rstandard(log.reg.3), col = c("blue", "red"), main = "Residuals VS Fitted", xlab = "Fitted Values", ylab = "Studentized Residuals")
abline(h = 0, lty = 2, col = "grey")
lines(lowess(fitted(log.reg.3), rstandard(log.reg.3)), col = "green", lwd = 2)
## According to the plot, data exhibits lack of heterogeinity, interdependence and lack of normality
## There are also many outliers
## All of these issues are likely due to the temporal and mixed nature of our data
## Similar to 1&2, 3 offers no real improvement
## Cannot base model selection on residual analysis, will rely on VIF and summary statistics



################################################################################
# Keeping log.reg.2 as the preferred model

# predict on held-out 25% and evaluate confusion matrix
  library(caret)
  predictions <- predict(log.reg.2, newdata = test, type="response")
  threshold   <- 0.1
  pred        <- factor(ifelse(predictions > threshold, 1, 0))
  matrix      <- confusionMatrix(pred, test$has.crime)
  matrix$table

# create ROC curve on logit models
  library(ROCR)
  par(mfrow = c(1,2))
  roc.pred <- prediction(predict(log.reg.2, newdata = test, type = "response"), test$has.crime)
  roc.perf <- performance(roc.pred, "tpr", "fpr")
  auc <- performance(roc.pred, "auc")@y.values[[1]]
  plot(roc.perf, main = paste("ROC (AUC=", round(auc,2), ")", sep = ""))
  abline(0, 1, lty = "dashed")

  roc.predalt <- prediction(predict(model.final, newdata = test, type = "response"), test$has.crime)
  roc.perfalt <- performance(roc.predalt, "tpr", "fpr")
  aucalt <- performance(roc.predalt, "auc")@y.values[[1]]
  plot(roc.perfalt, main = paste("ROC (AUC=", round(auc,2), ") Alt", sep = ""), col = "red")
  abline(0, 1, lty = "dashed", col = "red")
  par(mfrow = c(1,1))

## add predictions for last hour of data:
  last.day <- test %>% filter(timestamp==max(test$timestamp))
  last.day$predictions <- data.frame(predict(log.reg.2, newdata = last.day, type="response"))[,1]
  last.day %>% glimpse()

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
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
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
predictions <- predict(log.reg.2, newdata = test, type="response")
test$predictions <- data.frame(predict(log.reg.2, newdata = test, type="response"))[,1]

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
