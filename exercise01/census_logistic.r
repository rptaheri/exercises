library(foreign)
library(gmodels)
library(vcd)
library(epiR)
library(NSM3)
library(psych)
library(MASS)
library(rms)
library(contrast)
library(aod)
library(ggplot2)
library(aod)

records = read.csv(records_loc) #read csv file

# -------------------------  TO-DO --------------------------- #
# 1. find/write reliable function for partitioning data 70/30 and 
#    small test sample
# 2. find Type 3 Analysis of Effects equivalent
# 3. run box-tidwell test on continuos variables
# 4. find relationship for graph
# 5. 1/2 page write-up
# 6. look into impute methods, 3620 records not used due to 
#    missing values
# 7. figure out how to read crosstable in R
# ------------------------------------------------------------ #

# create a data frame that contains only continuous variables
continuous <- as.data.frame(cbind(records$over_50k, records$age, records$capital_gain,
                                  records$capital_loss, records$hours_week, records$education_num))
# histogram of target variable
hist(records$over_50k)

# rename columns in newly created data frame, continuous
colnames(continuous) <- c("over_50k","age","capital_gain","capital_loss","hours_week","education_num")

# continuous variable correlations
mixed.cor(continuous) # medium relationship between education_num and over_50k (0.46)

# look @ mosaic plot education and over_50k
mosaicplot(table(records$education_num,records$over_50k))

# histograms of continuous variables
hist(records$age)
hist(records$capital_gain)
hist(records$capital_loss)

# cross tabs with categorical variables
CrossTable(records$over_50k, records$gender)
CrossTable(records$over_50k, records$marital_status)
CrossTable(records$over_50k, records$occupation)
CrossTable(records$over_50k, records$relationship)
CrossTable(records$over_50k, records$working_class)

table(records$education_num)

barplot(table(native_country))
barplot(table(highest_education_lvl))

# change education into a categorical variable
records$education_num <- factor(records$education_num)

# look for missing values in categorical variables
table(records$native_country)
table(records$occupation)
table(records$race)
table(records$relationship)
table(records$gender)
table(records$working_class)

# recode ? values to missing
records$native_country[records$native_country == '?'] <- NA
records$occupation[records$occupation == '?'] <- NA
records$working_class[records$working_class == '?'] <- NA

# re-factor categorical variables after removing question marks and setting missing values
records$occupation <- factor(records$occupation)
records$native_country <- factor(records$native_country)
records$working_class <- factor(records$working_class)

# look for possible multi-collinearity between education_num and 
# highest_education_lvl
table(records$highest_education_lvl,records$education_num)

# highest_education_lvl and education_num are perfectly collinear, using only education_num 
# moving forward

# attach records dataset so we don't have to type records$ before every variable
attach(records)

# run logistic regression
logit <- glm(over_50k ~ age + education_num + capital_gain + capital_loss + hours_week + native_country
              + marital_status + occupation + relationship + race + 
               gender + working_class, family=binomial(logit))
detach(records)

summary(logit)

# AIC: 29496

# odds ratios 
or <- exp(coef(logit)[-1])
or

# copied function for calculating concordant and discordant pairs
Concordance = function(GLM.binomial) {
  outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # Equate the length of the event and non-event tables
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  ones_and_zeros = data.frame(ones, zeros)
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    # This tests for concordance
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
     disc[i] = 0
     ties[i] = 0}
    # This tests for a tie
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    # This should catch discordant pairs.
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0
    }
  }
  # Here we save the various rates
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
  
}

Concordance(logit)
# 90% concordant and 10% discordant

# calculate ROC
logit.roc <- roc(logit$y, logit$fitted)

# plot ROC
plot(logit.roc)

# ROC = 0.9058
logit.roc

# write our file to a csv for plotting in D3
write.csv(records, file=outloc)


