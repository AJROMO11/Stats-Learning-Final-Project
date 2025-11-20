library(rpart)
library(rpart.plot)

df <- read.csv("nyc-rolling-sales_initalcleaning.csv")

#convert cat vars to factors
df$BOROUGH <- factor(df$BOROUGH)
df$NEIGHBORHOOD <- factor(df$NEIGHBORHOOD)
df$BUILDING.CLASS.CATEGORY <- factor(df$BUILDING.CLASS.CATEGORY)
df$ZIP.CODE <- factor(df$ZIP.CODE)
df$TAX.CLASS.AT.TIME.OF.SALE <- factor(df$TAX.CLASS.AT.TIME.OF.SALE)
df$BUILDING.CLASS.AT.TIME.OF.SALE <- factor(df$BUILDING.CLASS.AT.TIME.OF.SALE)

df$GROSS.SQUARE.FEET <- as.numeric(df$GROSS.SQUARE.FEET)

#basic tree
treeModel <- rpart(
  GROSS.SQUARE.FEET ~ .,
  data = df,
  method = "anova"
)
print(treeModel)

rpart.plot(
  treeModel,
  extra = 1,
  main = "Decision Tree Predicting Gross Square Feet"
)

#tree came out very weird with too many factors. I am trying to condense factor levels

#showing how many factor levels in each categorical variable
sapply(df, function(x) if(is.factor(x)) length(levels(x)))

#factor level proportion of observations, looking to condense so each factor is at lest 5% of observations
lapply(df[, sapply(df, is.factor)], function(x) {
  prop.table(table(x))*100
})

library(forcats)

#condense building class at time of sale
df$BUILDING.CLASS.AT.TIME.OF.SALE <-
  substr(as.character(df$BUILDING.CLASS.AT.TIME.OF.SALE), 1, 1)

df$BUILDING.CLASS.AT.TIME.OF.SALE <- factor(df$BUILDING.CLASS.AT.TIME.OF.SALE)

df$BUILDING.CLASS.AT.TIME.OF.SALE <-
  fct_lump_prop(df$BUILDING.CLASS.AT.TIME.OF.SALE, prop = 0.02)

#condense building class
df$BUILDING.CLASS.CATEGORY <- as.character(df$BUILDING.CLASS.CATEGORY)

df$BUILDING.CLASS.CATEGORY[
  grepl("RENTAL", df$BUILDING.CLASS.CATEGORY, ignore.case = TRUE)
] <- "RENTALS"

df$BUILDING.CLASS.CATEGORY <- factor(df$BUILDING.CLASS.CATEGORY)

df$BUILDING.CLASS.CATEGORY <-
  fct_lump_prop(df$BUILDING.CLASS.CATEGORY, prop = 0.02)

#borough condense
levels(df$BOROUGH)[levels(df$BOROUGH) %in% c("2", "4")] <- "4"

#https://thewanderlover.com/zip-code-ny-new-york/ link for proof of zip code clusters
#zipcodes
df$ZIP.CODE <- substr(as.character(df$ZIP.CODE), 1, 3)
df$ZIP.CODE <- factor(df$ZIP.CODE)
df$ZIP.CODE <-
  fct_lump_prop(df$ZIP.CODE, prop = 0.02)

#neighborhoods is too complicated to condense for this dataset so i dropped it
df$NEIGHBORHOOD <- NULL

#reconfirming that my condensing worked
sapply(df, function(x) if(is.factor(x)) length(levels(x)))
lapply(df[, sapply(df, is.factor)], function(x) {
  prop.table(table(x))*100
})

df$GROSS.SQUARE.FEET <-  log(df$GROSS.SQUARE.FEET)
df$SALE.PRICE <-  log(df$SALE.PRICE)

trainIndex <- sample(seq_len(nrow(df)), size = 0.7*nrow(df))
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]


#pruned decision tree
treeModel <- rpart(
  GROSS.SQUARE.FEET ~ .,
  data = train,
  method = "anova",
  control = rpart.control(
    minsplit = 20,
    minbucket = 30,
    cp = 0.01,
    maxdepth = 5
  )
)

print(treeModel)

rpart.plot(
  treeModel,
  extra = 1,
  main = "Pruned Decision Tree Predicting Gross Square Feet",
  box.palette = "Blues",
  shadow.col = "gray",
)

library(Metrics)
pred_rpart_log <- predict(treeModel, newdata = test)
pred_rpart_orig <- exp(pred_rpart_log)

rmse_rpart_orig <- rmse(test$GROSS.SQUARE.FEET, pred_rpart_orig)

cat("RMSE of Decision Tree: ", round(rmse_rpart_orig, 2), "\n")


#boosted tree
library(gbm)
boostModel <- gbm(
  formula = GROSS.SQUARE.FEET ~ .,
  data = train,
  distribution = "gaussian",
  n.trees = 1000,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 20,
  cv.folds = 5,
  verbose = FALSE
)

#best iteration
bestIter <- gbm.perf(boostModel, method = "cv")
bestIter

summary(boostModel, n.trees = bestIter)
df$predictedSqFt <- predict(boostModel, newdata = df, n.trees = bestIter)

pred_boost_log <- predict(boostModel, newdata = test, n.trees = bestIter)
pred_boost_orig <- exp(pred_boost_log)

rmse_boost_orig <- rmse(test$GROSS.SQUARE.FEET, pred_boost_orig)

cat("RMSE of Boosted Tree: ", round(rmse_boost_orig, 2), "\n")

