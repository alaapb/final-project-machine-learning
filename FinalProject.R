getwd()
setwd("/Users/alaapbharadwaj/desktop/CS555")
library("dplyr")
library("corrplot")
require("scatterplot3d")
library("ggstatsplot")
library("tidyverse")
library("rstatix")
library("ggpubr")
library("olsrr")

# Loading Dataset

df <- read.csv("taxitripdata.csv")

# Exploring Dataset

summary(df)

head(df)

## Creating new column to describe exact time taken for a trip since pickup and dropoff are in datetime format
## Keep in mind the column being added, timeTaken, has units of minutes
df$timeTaken <- paste(difftime(df$tpep_dropoff_datetime,df$tpep_pickup_datetime, units="mins"))
df$timeTaken <- as.numeric(df$timeTaken)
head(df)

## We will be dropping some columns as they are all IDs and will not help in the model since they are categorical variables
new_df <- df %>% select(-c(store_and_fwd_flag, tpep_pickup_datetime, tpep_dropoff_datetime, PULocationID, DOLocationID, VendorID, RatecodeID, payment_type))

### Outlier Handling

boxplot(new_df)

#### We have now plotted our dataset, so we will remove our outliers first and then choose our sample size
#### With the boxplot we can see which variables have outliers and now we will remove them all

new_df <- new_df[-which(new_df$timeTaken %in% boxplot.stats(new_df$timeTaken)$out), ]
new_df <- new_df[-which(new_df$fare_amount %in% boxplot.stats(new_df$fare_amount)$out), ]
new_df <- new_df[-which(new_df$trip_distance %in% boxplot.stats(new_df$trip_distance)$out), ]
new_df <- new_df[-which(new_df$total_amount %in% boxplot.stats(new_df$total_amount)$out), ]
new_df <- new_df[-which(new_df$tolls_amount %in% boxplot.stats(new_df$tolls_amount)$out), ]
new_df <- new_df[-which(new_df$passenger_count %in% boxplot.stats(new_df$passenger_count)$out), ]
new_df <- new_df[-which(new_df$mta_tax %in% boxplot.stats(new_df$mta_tax)$out), ]
new_df <- new_df[-which(new_df$tip_amount %in% boxplot.stats(new_df$tip_amount)$out), ]
new_df <- new_df[-which(new_df$airport_fee %in% boxplot.stats(new_df$airport_fee)$out), ]
new_df <- new_df[-which(new_df$congestion_surcharge %in% boxplot.stats(new_df$congestion_surcharge)$out), ]


boxplot(new_df)

#### We can see from this that the majority of outliers have been removed, now we can go ahead with our investigation

### Choosing random 500 rows

complete_df <- sample_n(new_df, 500)

summary(complete_df)

attach(complete_df)

### Scatter Plot

pairs(complete_df, upper.panel=NULL)

#### This output shows: 
#### Some linear relationship between trip distance and time taken
#### No Linear relationship between passenger count and time taken, airport fee and time taken, congestion surhcarge and time taken, improvement surcharge and time taken, tolls amount and time taken, mta tax and time taken, extra and time taken
#### Some Linear relationship between tip amount and time taken, but with some noise
#### Strong Linear relationship between total amount and time taken, fare amount and time taken

#### We can also get a closer look at the ones we think is linear from the initial plot

#### trip distance and time taken
plot(complete_df$trip_distance, complete_df$timeTaken)

#### We can confirm that is relationship is linear 

#### total amount and time taken
plot(complete_df$total_amount, complete_df$timeTaken)

#### We can confirm that this is also linear

#### fare amount and time taken
plot(complete_df$fare_amount, complete_df$timeTaken)

#### We can see that there is a strong linear relationship

#### tip amount and time taken
plot(complete_df$tip_amount, complete_df$timeTaken)

#### We can see that there is a very slight linear relationship while there is a lot of noise

# Testing

## Simple Linear Regression

### Trip Distance to Time Taken Linear Regression
#### Model
##### Now we will create the linear model and then print out a summary to check the R-squared and p-value to check the goodness of this model
lmdistance <- lm(timeTaken~trip_distance, data=complete_df)
summary(lmdistance)
##### We can see that:
##### p-value is < 0.05, therefore, it shows that the variable is significant
##### slope and intercept p-value is < 0.05, therefore, it is clear both coefficients is significant
##### this model with distance as predictor explains about 59% variability of the target
##### residual standard error is 3.868

### Total Amount to Time Taken Linear Regression
#### Model
lmamount <- lm(timeTaken~total_amount, data=complete_df)
summary(lmamount)
##### We can see that:
##### p-value is < 0.05, therefore, it shows that the variable is significant
##### slope and intercept p-value is < 0.05, therefore, it is clear both coefficients is significant
##### this model with total amount as predictor explains about 80% variability of the target
##### residual standard error is 2.686

### Fare Amount to Time Taken Simple Linear Regression
#### Model
lmfare <- lm(timeTaken~fare_amount, data=complete_df)
summary(lmfare)
##### We can see that:
##### p-value is < 0.05, therefore, it shows that the variable is significant
##### slope and intercept p-value is < 0.05, therefore, it is clear both coefficients is significant
##### this model with fare amount as predictor explains about 87% variability of the target
##### residual standard error is 2.143

### Tip Amount to Time Taken Simple Linear Regression
#### Model
lmtip <- lm(timeTaken~tip_amount, data=complete_df)
summary(lmtip)
##### We can see that:
##### p-value is < 0.05, therefore, it shows that the variable is significant
##### slope and intercept p-value is < 0.05, therefore, it is clear both coefficients is significant
##### this model with fare amount as predictor explains about 11% variability of the target, meaning it does not do a good job in predicting the time taken value
##### residual standard error is 5.72

#### From the Simple Linear Regression that we ran above, we can say the fare amount gave us the best variability so we will plot the scatterplot along with the regression line

plot(fare_amount, timeTaken)

abline(lmfare, col="blue")

#### As you can see the line does not fully explain the variability within time taken, so maybe we could try multiple linear regression and see if more predictors help improve the line

### Correlation

res <- cor.test(timeTaken, tip_amount)
res

#### correlation is 0.333, since the correlation is not high we can say that this won't create a lot of noise

res <- cor.test(timeTaken, trip_distance)
res

#### correlation of 0.770 means this variable could cause a lot of noise

res <- cor.test(timeTaken, fare_amount)
res

#### We get a correlation of 0.935 so we can say it can cause a lot of noise as well

res <- cor.test(timeTaken, total_amount)
res
#### The correlation for this is 0.896 so it will have a lot of noise when added to the model

#### After running the correlation tests, we can see what correlation we get, while some are high this could cause noise in our MLR model or it could help make it more accurate

### Check for Multi-Colinearity
res <- cor.test(fare_amount, tip_amount)
res
#### We get a correlation of 0.3656 therefore it should not have a lot of noise when added together

res <- cor.test(fare_amount, total_amount)
res
#### We get a correlation of 0.961 therefore this could create a lot of noise when added together in the model

res <- cor.test(fare_amount, trip_distance)
res
#### We get a correlation of 0.937 therefore this could create a lot of noise when added together in the model

res <- cor.test(total_amount, tip_amount)
res
#### We get a correlation of 0.608 therefore this could create noise as well within our model

res <- cor.test(total_amount, trip_distance)
res
#### We get a correlation of 0.909 therefore this could create a lot of noise as well within our model

res <- cor.test(trip_distance, tip_amount)
res
#### We get a correlation of 0.371 therefore there is less chances of this creating noise in our model

#### We can see that some of the correlation values are high which could mean when added to a multiple linear regression model it could add a lot of noise, however, it could also not add noise but rather help the significance of the model instead

## Multiple Linear Regression
### Now that we have created our linear regression models, cleaned our data, and identified which variables are significant and have linear relationship with the time taken
### We will create the multiple regression model by adding different variables and seeing how the p-value is affected so that we create a model with the least p-value and a good r-squared value as well

### Fare Amount + Total Amount
#### Model
lm_fare_total = lm(timeTaken~fare_amount+ total_amount, data=complete_df)
summary(lm_fare_total)
##### p-value < 0.05, therefore, the model is significant
##### p-value of coefficients for the intercept and the fare amount is < 0.05, however, total amount coefficient is > 0.05 
##### The model for fare amount and total amount explains a 87% variability of time taken, however, this is lower than the variability shown in the simple linear regression by just the fare amount
##### residual standard error is 2.144 
##### therefore we will not add total amount to the model since it reduces the variability

### Fare Amount + Trip Distance
#### Model
lm_fare_trip = lm(timeTaken~fare_amount+trip_distance, data=complete_df)
summary(lm_fare_trip)
##### p-value < 0.05, therefore, the model is significant
##### p-value for all the coefficients is < 0.05, meaning that the slope and intercept coefficients are statistically significant
##### This model with fare amount and trip distance explains about 96.7% variability of time taken
##### residual standard error is 1.086
##### this is model is showing us good results, maybe we could try adding another variable to the model and see how it responds

#### Anova
##### Now we must test whether the improvement in variability in Adjusted R squared is significant or not
##### We can test this by using ANOVA, having a null hypothesis as improvement in adjusted r squared is not statistically significant, and the alternative hypothesis is the improvement in adjusted r squared is statistically significant
anova(lmfare, lm_fare_trip)
##### the p-value from the anova shows us that we can reject the null hypothesis and accept the alternative hypothesis
##### so the improvement for the adjusted r squared is statistically significant

### Fare Amount + Trip Distance + Tip Amount
#### Model
lm_fare_trip_tip = lm(timeTaken~fare_amount+trip_distance+tip_amount, data=complete_df)
summary(lm_fare_trip_tip)
##### p-value < 0.05, therefore, the model is statistically significant
##### p-value for all the coefficients is < 0.05, meaning that the slope and intercept coefficients are statistically significant
##### This model with fare amount, trip distance and tip amount explains about 96.8% variability of time taken which is slightly higher than the model with trip distance and fare amount
##### This variability is higher than the previous model, therefore, making the model slightly more accurate
##### residual standard error is 1.082

#### Anova
##### Now we must test whether the improvement in variability in Adjusted R squared is significant or not
##### We can test this by using ANOVA, having a null hypothesis as improvement in adjusted r squared is not statistically significant, and the alternative hypothesis is the improvement in adjusted r squared is statistically significant
anova(lm_fare_trip, lm_fare_trip_tip)
##### p-value is < 0.05, therefore, we can reject the null hypothesis and accept the alternative hypothesis
##### the improvement for the adjusted r squared is statistically significant

### Fare Amount + Trip Distance + Tip Amount + Total Amount
#### Model
lm_fare_trip_total = lm(timeTaken~fare_amount+trip_distance+total_amount, data=complete_df)
summary(lm_fare_trip_total)
##### p-value < 0.05, therefore, the model is statistically significant
##### p-value for all the coefficients (slope and intercept) is < 0.05, therefore, making them significant
##### This model with fare amount, total amount, tip amount, trip distance explains about 97% variability of time taken
##### residual standard error is 1.037 which is lower than the previous model as well

#### Anova
##### Now we must test whether the improvement in variability in Adjusted R squared is significant or not
##### We can test this by using ANOVA, having a null hypothesis as improvement in adjusted r squared is not statistically significant, and the alternative hypothesis is the improvement in adjusted r squared is statistically significant
anova(lm_fare_trip, lm_fare_trip_total)
##### p-value is < 0.05, therefore, we can reject the null hypothesis and accept the alternative hypothesis
##### the improvement for the adjusted r squared is statistically significant

### Residual Plot
#### Checking Linearity Assumption
plot(lm_fare_trip_total, 1)
##### The red line is pretty much horizontal and linear indicating that linearity assumption holds well
##### Residual fluctuates between the -4 to 4 bound, which shows the fitted model is good prediction to certain extent
##### Since we have a few points outside the bounds, 484, 337, 40, these could be outliers since they are very from other points
##### Large residuals tell us that there could indicate that either the variance is not constant (heterosceadasticity) or the true relationship between the variables is non linear
##### While the variance is being violated as shown on the residual plot, however, the transformation only marginally improved the model so we won't transform due to the increased complexity

#### Here we try transoforming to get a better variance
new_df <- complete_df
new_df$timeTaken <- log(new_df$timeTaken)
new_df <- new_df[is.finite(rowSums(new_df)),]

lm_fare_trip_total_transf <- lm(timeTaken~fare_amount+trip_distance+total_amount, data=new_df)
summary(lm_fare_trip_total_transf)
#### As you can see the transformation has not helped the model at all, in fact it has made our model worse with an 86% variability now while before we had a 97% variability
#### Therefore, we will choose not to transform it so that our model is more accurate

#### Checking Distribution of Residuals
hist(resid(lm_fare_trip_total))
##### As you can see from the histogram the residuals are normally distributed which is a good thing

#### Checking F-stat with f-dist value
summary(lm_fare_trip_total)
qf(0.95, 4, 495)
#### F-stat is > 2.390 with 4136 so we can proceed to reject the null hypothesis and say that it is statistically significant

## Conclusion
### In the statistical analysis I have used Correlation Tests, Anova, Simple Linear Regression, and Multiple Linear Regression. 
### After using simple linear regression to model all the variables individually with the time taken as the predictor variable to see which models are significant and can be expanded on by using the MLR model
### I then tested the correlations for these variables with time taken to see which or if any variables has high correlation and could create noise in MLR model when combined
### I then tested the correlations between the independent variables to see if there exists any multicolinearity as this could add noise to our model. Correlation between some independent variables was extremely high, however, adding these variables together in the model helped its overall accuracy as we saw with the r squared value and other diagnostic methods I applied when checking the goodness of the model.
### I also run some other diagnostics to check the linear relationship of the variables such plotting a scatterplot of the variable with the most significance and added a linear regression line to it to see how well it fits the data, so that I can visually observe
### After the diagnostics of the simple linear regression, we then move onto the multiple linear regression portion of this and we start with the variable with most significant model and add other variables to it to see which combination provides us with the most accurate model
### The MLR model that shows the most significance, with a good p-value, high r squared value, high f-stat, and normally distributed residuals, that I found was when time taken is predicted by fare amount, total amount, trip distance, and tip amount
### To check the fit of the MLR model we ran diagnostics such as checking F-stat, checking the distribution of the residuals, checking the residual vs fitted plot to see if the model is actually linear or not
### The residual vs fitted plot also showed that the constant variance factor was being violated, however, when I tried to transform using the log(n) method it affected the model negatively, therefore, we did not transform and went ahead using the original model
### To conclude we can say that the fare amount, trip distance, total amount, and tip amount as multiple linear regression helps best explain the changes and variability in the time taken for each taxi trip