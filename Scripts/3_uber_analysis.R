# Join computed and existing data sets to perform analysis
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))

uber.data <- readRDS("../output/kde.evals.rds") %>% get('estimate',.)
uber.pooled <- readRDS("../Output/uberpooled.rds")


uber.pooled %>%
  ungroup %>%
  mutate(
    crime.estimate=uber.data,
    wday=wday(timestamp),
    hour=hour(timestamp)
    ) -> uber.joined
# uber.joined %>% glimpse

partition.df <- function(df,fraction=0.7,seed=0451) {
  set.seed(seed)
  df %>%
    sample_frac(fraction) -> train
  train %>%
    rownames %>%
    as.numeric -> train.rows
  list(
    train=train,
    test=df %>% slice(-train.rows)
    )
}


uber.joined %>% partition.df -> uber.joined.sets
# uber.joined.sets$train %>% glimpse


test.formula <- function(fm){
  fm %>%
    glm(data=uber.joined.sets$train,
                 family="binomial") -> log.fit
  log.fit %>%
    predict.glm(newdata = uber.joined.sets$test, type="response") %>%
    `-`(uber.joined.sets$test$crime.estimate) %>%
    `^`(2) %>%
    sum -> residuals.sum
  list(
    summary=log.fit %>% summary,
    residuals.sum=residuals.sum
    )
}


""
"   ================================================================ "
"Crime Estimate vs Average Surge Multiplier"
test.formula(crime.estimate ~ avg_surge_multiplier) -> results.basic
results.basic$summary
paste("Using the test set, the sum of residuals is:",results.basic$residuals.sum)

""
"   ================================================================ "
"Crime Estimate vs Average Surge Multiplier AND Average Expected Wait Time"
test.formula(crime.estimate ~ avg_surge_multiplier + avg_expected_wait_time) -> results.combo
results.combo$summary
paste("Using the test set, the sum of residuals is:",results.combo$residuals.sum)

""
"   ================================================================ "
"Crime Estimate vs Average Expected Wait Time"
test.formula(crime.estimate ~ avg_expected_wait_time) -> results.wait_time
results.wait_time$summary
paste("Using the test set, the sum of residuals is:",results.wait_time$residuals.sum)

""
"   ================================================================ "
"Crime Estimate vs ratio of Average Surge Multiplier to Average Expected Wait Time"
test.formula(crime.estimate ~ avg_surge_multiplier/avg_expected_wait_time) -> results.ratio
results.ratio$summary
paste("Using the test set, the sum of residuals is:",results.ratio$residuals.sum)
