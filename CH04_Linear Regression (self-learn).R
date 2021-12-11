library(tidyverse)
bikes <- read.csv(file = "bikes.csv")
bikes <- read_csv("bikes.csv", col_types = "Dffffddddd")
glimpse (bikes)
cov(bikes$humidity,bikes$rentals)
sd(bikes$humidity)
sd(bikes$rentals)
sd(bikes$humidity)
cor(bikes$humidity, bikes$rentals)
cor(bikes$windspeed, bikes$rentals)
cor(bikes$temperature, bikes$rentals)
bikenumeric <- bikes %>%
  select(-date)
library(corrplot)
bike_correlations <- cor(bikenumeric)
corrplot(bike_correlations)
corrplot.mixed(bike_correlations)
## linear regression model: rentals (yi) = B0 + B1*xi(temperature)
B1 <- cov(bikes$temperature, bikes$rentals) / var(bikes$temperature)
B0 <- mean(bikes$rentals) - B1*mean(bikes$temperature)
## using the lm() function
bikes_mod1 <- lm(data=bikes, rentals~temperature)
## details of the model
summary(bikes_mod1)
## multiple regression with 3 independent variables
## model: rentals (yi) = B0 + B1*x1i(temperature) + B2*x2i(humidity) + B3*x3i(windspeed)
## call library stats package
library(stats)
bikes_mod2 <- lm(data=bikes, rentals ~ humidity + windspeed + temperature)
summary(bikes_mod2)

## to plot all four graphs into one plot, use the par function
par(mfrow=c(2,2))
plot(bikes_mod2)
## Residual diagnostics - key steps to verify the model relevance and degree of fitness
## check for residual has a mean of zero
mean (bikes_mod2$residuals)
## test of normality of residuals (i.e. the residuals follow a normal distribution)
## use the ols_plot_resid_his() function in R
library (olsrr)
ols_plot_resid_hist (bikes_mod2)

## test for no collinearity between the independent variables
## collinearity means there exists a simple relation between two independent variables

## use ols_plot_resid_fit, examine if the scattered plot shows residuals are approx within horizontal band
ols_plot_resid_fit(bikes_mod2)
## the Residual vs Fitted Values show some degree of heteroscedasticity 

##
## test for Residual autocorrelation - if any, the residual has a time difference correlation, e.g AR1
## make use of the Durbin-Watson statistics test: 0-2 positive autocorrelation, 2=zero, 2-4 negative
library (car)
durbinWatsonTest(bikes_mod2)
## D-W statistic value=0.404 (positive autocorrelate). The model needs additional predictors to fit better
## D-W stats affect the hypothesis test, a lower bound dL and upper bound dU to be computed based on confidence level, e.g. 5%
## for 3 dependent variables, dL = 1.738, dU = 1.79 (n>200). In this case, the null hypothesis of no auto-corr to be rejected

## Influential Point Analysis - to identify outliers as they have influences to the estimators
## use Cook's Distance measures to examine which are the outliers
ols_plot_cooksd_chart(bikes_mod2)
outliers <- bikes[,c("rentals","humidity","windspeed","temperature")]
outliers$cd = cooks.distance(bikes_mod2)
## if an observation has a cook distance greater than 4/(n-k-1); n=no. of samples, k=no. of predictors, 4/726=0.006 
## In this example data, observation 69 has a value of 0.005 > 0.006
## obtain a list of outliers
## cooks_outliers <- ols_plot_cooksd_chart(bikes_mod2)$outliers
## cooks_Dist <- cooks.distance(bikes_mod2)
library(dplyr)
arrange(outliers, desc(cd))
## manual plot 
plot(cooks.distance(bikes_mod2), pch="*", cex=2, main="Influential Obs by Cook's distance Using Official Thre
shold (4/N)") 
abline(h = 4/731, col="red") # add cutoff line
## obtain a list of outliers with cook's distance > 4/N
large_cd<-subset(outliers, cd > (4/731)) 
View(large_cd)
df_large_cd <- data.frame(large_cd)
library(psych)
describe(df_large_cd$cd)
hist(large_cd$cd)
quantile(large_cd$cd, probs = seq(0, 1, 0.05)) # from 0 to 1, by 0.05
## a big jump from 75th to 80th percentile and upwards, these are the outliers, cutoff point at 75%
large_cd2<-subset(outliers, cd > 0.010164468)
# Use the View() function to examine observations in this object
View(large_cd2)
## cook's distance plot with cutoff point 
cd1 <- cooks.distance(bikes_mod2)
plot(cooks.distance(bikes_mod2), pch="*", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 0.010164468 , col="red") # add cutoff line
text(x=1:length(cd1), y=cd1, labels=ifelse(cd1>0.010164468,names(cd1),""), col="red", pos = 4) # add labels
# pos = 4 to position at the right 

## fit a model without the outliers, cutoff from 0.010164468
bikes_mod3 <- lm(rentals~ humidity + windspeed + temperature,
                 data=subset(outliers, cd<0.010164468))
summary(bikes_mod2) 
summary(bikes_mod3)  # compare without the outliers

## Influence Plot - bubble size represents influence
library(car)
influencePlot(bikes_mod3, id=list(method="noteworthy"), # "identify": for interactive labeling
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

## The following section is about Using leverage
outliers$lev <- hatvalues(bikes_mod2)
plot(outliers$lev, 
 xlab="Index",
 ylab="leverage",
 pch=19)
abline(lm(lev~rentals, data=outliers), col="red") # Add a fitted line

plot(bikes_mod2,1)
plot(bikes_mod2, which=5) # residuals vs leverage, visual examine which outliers to be omitted
## obtain a subset of outliers with leverage > 0.02
leverage1<-subset(outliers,lev > .02)
View(leverage1)

## test for multicollinearity between predictors
## compute the total VIF value (variance inflation factor)
ols_vif_tol(bikes_mod2)
## if the VIF of one ore more predictors is above 5.0, it has the collinearity problem
## resolutions (1) drop the variable, or (2) combine 2 or more correlated variables into a single variable
## model with polynomial 
bikes2 <- bikes %>%
  mutate (humidity2 = humidity^2) %>%
  mutate (windspeed2 = windspeed^2) %>%
  mutate (temperature2 = temperature^2)
bikes_mod4 <- 
  lm(data=bikes2, rentals ~ humidity + windspeed + temperature + 
       humidity2 + windspeed2 + temperature2)
summary(bikes_mod4)
## windspeed square is not needed
bikes_mod5 <- 
  lm(data=bikes2, rentals ~ humidity + windspeed + temperature + 
       humidity2 + temperature2)
summary(bikes_mod5)
summary(bikes_mod2)

## model with categorical variables

summary(bikes2[,c("season","holiday","weekday","weather")])
library(plyr)
bikes2$season <- as.factor(bikes$season)
bikes2$holiday <- as.factor(bikes$holiday)
bikes2$weekday <- as.factor(bikes$weekday)
bikes2$weather <- as.factor(bikes2$weather)
bikes2 <- bikes2 %>%
  mutate(season=revalue(season, c("1"="Winter", "2"="Spring", "3"="Summer","4"="Fall"))) %>%
  mutate(holiday=revalue(holiday,c("0"="No", "1"="Yes"))) %>%
  mutate(weekday=revalue(weekday,c("0"="Sunday","1"="Monday","2"="Tuesday","3"="Wednesday",
                                   "4"="Thursday","5"="Friday","6"="Saturday"))) %>%
  mutate(weather=revalue(weather,c("1"="Clear","2"="Light rain","3"="Heavy rain")))

bikes_mod6 <- 
  lm(data=bikes2, rentals ~ humidity + windspeed + temperature + 
       humidity2 + temperature2 + season)
summary(bikes_mod6)
## When including a categorical variable, the important thing is that a baseline is established first, then 
## each value other than that of the baseline will have its regressed coefficient, which is to be interpreted as 
## the average value from the baseline ... e.g. winter is the baseline, there is no coefficient for it, unlike 
## other values such as seasonSummer ... which contains 1 if summer. The coefficient of seasonSummer will be the 
## average from the Winter baseline.

## The process of selecting variables into the linear regression model requires a systematic approach. There could be
## Forward Selection = to start with single predictor, then adding in dual predictors, trio, etc, verify with the 
## R2 and p-values to decide which combination is to be dropped

## A Backward selection is the start from including all predictors, and remove those that are insignificant
##
## A mixed seclection is a combination of both. R has a library ols_step_both_p() which will step through all possible 
## combination of predictors, and allow us to see which will have the lowest R2, and the predictors are statistically
## significant (p-value is very small for the given t-statistics)
##
## In the bikes data, we would need to transform the date variable into day, month, year as possible predictors 
## library (lubridate) is such a function 

library(lubridate)
bikes2 <- bikes2 %>%
  mutate (day=as.numeric(date-min(date))) %>%
  mutate (month=as.factor(month(date))) %>%
  mutate (year=as.factor(year(date))) %>%
  select (-date)  # remove the date variable
summary(bikes2)

## use the Stepwise function to perform forward and backward regression on each predictor variable

ols_step_both_p(
  model = lm(
    data=bikes2,
    rentals ~ humidity + weekday + holiday +
      temperature + humidity2 + temperature2 + season +
      windspeed * weather + realfeel + day + month + year
  ),
  pent = 0.2,
  prem = 0.01, 
  progress = TRUE,
  details = FALSE
)



