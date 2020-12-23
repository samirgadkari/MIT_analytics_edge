# Linear regression to predict wine values from Rainfall, etc.
# wine$AGST = Average Growing Season Temperature

wine = read.csv('datasets/wine.csv')
str(wine)
summary(wine)

# lm() is used to build a Linear Regression model
model1 = lm(Price ~ AGST, data=wine) # Dependent_Variable ~ Independent_Variable
summary(model1)
# Shows the coefficients in the Estimate column.
# Adjusted R^2:
#   The adjusted R^2 adjusts R^2 to account for the number of variables used
#   compared to the number of data points. It penalizes R^2 if you add a variable
#   that does not help the prediction much. You can see which variables are useful
#   by adding them one at a time and checking the change in the adjusted R^2 value.
# Estimate:
#   These are the coefficients values. If the coefficients are close to 0,
#   the feature is not very important in predicting the dependent variable
# Std. Error:
#   How much the coefficient is likely to vary from the Estimate value
# t value:
#   This is the Estimate/(Std. Error). It will have the same sign of the Estimate.
#   The larger the absolute t value, the more significant the coefficient
# Pr(>|t|):
#   This gives the probability that the coefficient is actually zero.
#   It will be large if the absolute t value is small, and 
#   it will be small if the absolute t value is large.
#   We want small values here.
# Stars at the end of each row:
#   The more the stars, the more that feature is significant.
#   The star coding scheme is at the bottom of the summary.
#   This is the best way to find out the most significant features.
model1$residuals  # Residuals are store in model1$residuals
SSE = sum(model1$residuals^2)  # Get the sum of squared errors
SSE

model2 = lm(Price ~ AGST + HarvestRain, data=wine) # Price depends on AGST and HarvestRain
summary(model2)
# The adjusted R^2 is much larger. The model is much better than the previous model
SSE = sum(model2$residuals^2)
SSE
# This shows that the SSE is much smaller than the previous model, since it is better.

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
# The adjusted R^2 is even better than the previous models
SSE = sum(model3$residuals^2)
SSE
# The SSE is even better than before

# From the model3 summary, we can see that the Age and FrancePop are not significant.
# Let's start by removing just FrancePop.
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
# Our adjusted R^2 increased when we removed FrancePop.
# Earlier, Age was not significant, but now it is. Why did this happen?
# This is because Age and FrancePopulation were highly collinear.
# By removing one of then, the other becomes more significant.
plot(wine$FrancePop, wine$Age) # Scatter plot
cor(wine$FrancePop, wine$Age)  # Correlation of two features
# Results in:
# [1] -0.9944851
# which is a very high negative correlation
cor(wine)  # Correlation of all features with each other
# Multi-collinearity only applies to independent variables.
# We want feature highly correlated with the target.

# Let's remove Age and FrancePop at the same time since they're
# not highly correlated to the target.
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
# This drops our adjusted R^2 value from 0.79 to 0.71.
# Older wines are more costlier. Also this is similar to FrancePop
# since the population grows over time, but the model captures Age
# better than the population growth correlation to the Price.
# So we should add Age in.
# We will stick with model4 as our final model.

# Use the dataset wine.csv to create a linear regression model 
# to predict Price using HarvestRain and WinterRain as 
# independent variables, like you did in the previous quick question. 
# Using the summary output of this model, answer the following questions:
#   1. Is the coefficient for HarvestRain significant?
#   2. Is the coefficient for WinterRain significant?
testmodel1 = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(testmodel1)
# From the output the coefficient for HarvestRain is significant,
# but the coefficient for WinterRain is not significant.

# It's important to build the model which predicts correctly on new data.
# Accuracy on test data is called out-of-sample accuracy.
# Let's see how our model does on wine_test.csv
wineTest = read.csv('datasets/wine_test.csv')
str(wineTest)
predictTest = predict(model4, newdata=wineTest) # predict price for test data
predictTest  # Shows the predicted prices as a vector of numerics.
# Let's see how our R^2 values are for the test set.
# R^2 = 1 - (SSE / SST)
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE / SST
# Returns:
# [1] 0.7944278
# This is a good R^2 value. But, there were only 2 points. We should use a
# test set with more points.
# We should compare the Model R^2 on the training set, and the test set R^2
# values. We want both those values to be high.
# Test set R^2 can be negative if the model does worse on the test set.
# Training set R^2 can never be negative.
# Test set R^2 can cannot be > 1.0 since both SSE and SST are > 0 and 
# R^2 = 1 - (SSE / SST)

# Remove all dataframes and values
rm(model1, model2, model3, model4, model5, quickQuestionModel1,
   testmodel1, wine, wineTest, predictTest, SSE, SST)

