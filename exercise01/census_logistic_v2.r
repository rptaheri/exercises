library(gmodels)
library(caret)
library(pROC)

# read from CSV file
records = read.csv(records_log) 


# recode ? values to missing
records$native_country[records$native_country == '?'] <- NA
records$occupation[records$occupation == '?'] <- NA
records$working_class[records$working_class == '?'] <- NA

# create new variable to indicate if country of origin is US, otherwise 0
# instead of country_of_origin
records$us_flg[records$native_country == 'United-States'] <- 1
records$us_flg[records$native_country != 'United-States'] <- 0

mosaicplot(table(records$working_class, records$over_50k))
table(records$working_class)

# logically group Never-worked and Without-pay to just Without-pay
records$working_class[records$working_class == 'Never-worked'] <- 'Without-pay'
table(records$working_class)


# re-factor categorical variables after removing question marks and setting missing values
records$occupation <- factor(records$occupation)
records$native_country <- factor(records$native_country)
records$us_flg <- factor(records$us_flg)
records$working_class <- factor(records$working_class)

inTrain = createDataPartition(records$over_50k, p=7/10, list=FALSE)

# split data into training (70%) and validation (30%)
train = records[inTrain,]
validate = records[-inTrain,]

# attach training dataset 
attach(train)

logit <- glm(over_50k ~ age + capital_loss + capital_gain + education_num + hours_week + us_flg
             + marital_status + occupation + relationship + race + 
               gender + working_class, family=binomial(logit))

# run logistic regression - removed capital_gain due to linear separation problem
logit <- glm(over_50k ~ age + capital_loss + education_num + hours_week + us_flg
             + marital_status + occupation + relationship + race + 
               gender + working_class, family=binomial(logit))
summary(logit) # AIC 22,648
Concordance(logit) # concordance = 0.886
logit.roc <- roc(logit$y, logit$fitted)
logit.roc # roc = 0.8853

# removing race, all of the levels are insignificant
logit <- glm(over_50k ~ age + capital_loss + education_num + hours_week + us_flg
             + marital_status + occupation + relationship  + 
               gender + working_class, family=binomial(logit))
summary(logit) # AIC 22,650
Concordance(logit) # concordance = 0.8859
logit.roc <- roc(logit$y, logit$fitted)
logit.roc # roc = 0.8852

plot(logit.roc)
detach(train)

# score validation set
score <- as.data.frame(predict(logit, validate, type="response"))

valid_score <- cbind(validate$over_50k,score)

# change names to make it easier to reference them
colnames(valid_score) <- c("over_50k", "predicted_prob")

# choose two cutoffs maximized for specificity
valid_score$cutoff50[valid_score$predicted_prob >= 0.5] <- 1
valid_score$cutoff50[valid_score$predicted_prob < 0.5] <- 0

valid_score$classification_50[valid_score$cutoff50 == valid_score$over_50k] <- 1
valid_score$classification_50[valid_score$cutoff50 != valid_score$over_50k] <- 0


colMeans(valid_score, na.rm = TRUE) # classification rate at 50% cutoff is 83.3% in validation

