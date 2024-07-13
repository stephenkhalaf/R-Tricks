########################## R statistics  ##############################
matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow = TRUE) #to create a matrix

df <- read.csv(file.choose())  #to import data
edit(variable / df)  #to edit a variable or df
write.csv(df,"C:/Users/H P/Desktop/df.csv",row.names = FALSE) #to export data frame to a specific working directory

rm(variable name)  / rm(list=ls()) #to remove variable from the workspace
names(df)  #to get the feature names
unique(df$feature)  #to get unique values of a feature
distinct(df)  #to remove duplicates in a dataframe such as drop_duplicates in python

df[!complete.cases(df$feature),]   #to get the dataframe for missing values in a feature

df$date <- as.Date(df$date,format='%m/%d/%y')  #to convert a date string to real date
weekdays(df$date) #to get the days like mon,tues,wed...
months(df$date)   #to get the month 

install.packages()
remove.packages()
packageVersion('module') #to check the version of a module
    
tapply(df$numeric, df$cat1, mean,na.rm=TRUE)                  #to get the mean of numeric feature based on a categorical variable
tapply(df$numeric, list(df$cat1,df$cat2), mean,na.rm=TRUE)    #to get the mean of numeric feature based on two categorical variables

cor(x,y) #to calculate the correlation between x and y features

quantile(x, probs = c(0,0.2,0.5,0.8,1))  #to generate the quantiles

boxplot(y~x)  #to create a boxplot between a numeric variable y and a categorical variable x
boxplot(y~(x1*x2), col=c('blue','red'),las=2)  #to create boxplot between a numerical variable y and two categorical variables x1 and x2

x_cat <- cut(x,breaks=c(),labels=c())  #to convert a numerical variable into a categorical variable

barplot(table(f1,f2),beside=T,legend.text = T) #to draw a barplot for two categorical features f1 and f2

########################## T-test and Anova test(F-test)  ##############################
#t-test is done on one target numerical feature against two categorical features
#anova is done on one target numerical feature against two or more categorical/numerical features for regression
#anova is done on one target categorical feature against two or more categorical/numerical features for classification

t.test(numeric_feature ~ categorical_feature, data = df)   #to perform t-test 

model <- aov(numeric_feature ~ categorical_feature, data = df)   #to perform anova
summary(model)

########################## DPLYR    ##############################
# f1,f2 <- features
# v1,v2 <- values

#fiter
filter(df, f1==v1,f2==v2)   #the ',' servers as 'and'
filter(df, f1==v1 | f2==v2) #the '|' servers as 'or'
filter(df, f1 %in% c(v1,v2))

#select
select(df,f1,f2)
select(df,f1:f4, contains('name1'), starts_with('name2'))  #selects items between f1 and f4,variable name that contains/starts_with name1
select(df, -f1), #to omit f1 in the dataframe
select(df, !is.na(f1)) #returns a data frame that excludes the null values of feature f1

#arrange
arrange(df,f1)           #to arrange a data frame by f1 feature in an ascending way
arrange(df,desc(f1))     #to arrange a data frame by f1 feature in a descending way

#recode
df$sex <- recode(df$sex, 'male'='man', 'female'='woman')  #to rename the values of factor variables

#if_else
df['new_feature'] = if_else(df$feature < 1 , 'yes' ,'no')  #performing if_else statement in dplyr

#substr
df['new_feature'] = if_else(df$feature == '', 'None', substr(df$feature,1,1))  #usage of substr function

#fill in missing values with the median
new_feature <- if_else(is.na(df$feature),median(df$feature,na.rm = TRUE),df$feature )

#ordered categorical variable
df$Pclass <- ordered(df$Pclass, levels=c('3','2','1'))   #to make an ordinal variable in which 3 < 2 < 1 

###########  How to perform long and wide on dataframe using reshape2 module   ################
library(reshape2)
df_long = melt(df_wide,id.vars='your preferred feature')   #the df_wide is just an ordinary data frame with many features and it will be                                                                # turned into a dataframe with just 'your  preferred feature' with 'vaiable' and                                                            # 'value' as feature

df_wide_again <- dcast(df_long, your preferred feature ~ variable)  #to convert the long form into wide again

###########  How to perform SimpleImputer using KNN   ################
library(caret)
library(RANN)
impute <- preProcess(df , method = c("knnImpute"))
df <- predict(impute,df)

#########   How to plot in base R  #############
plot(x,y,type="l",col="red",lwd="2",main="Title of Plot",xlab='x-label',ylab='y-label')
points(x,y,pch=10)
text(x=20,y=20,labels="text added")
abline(lm(y~x),col='blue',lty='dashed',lwd=2)

par(mfrow=c(2,2)) #to split the plots

##########  How to plot ggplot  ##########

ggplot(data=df,aes(x=x,y=y)) +
geom_point(aes(color=""),alpha="")  #to make scatter plots
facet_wrap(~categorical variable) #breaks the plots into the variable categories
geom_jitter() #scatterplot to reduce overlap
geom_histogram() #histogram
geom_density() #density plot
geom_boxplot() #boxplot
geom_violin() #violin plot (combination of boxplot and density plot)
geom_bar() #bar graph
geom_line() #line graph
geom_errorbar() #Add error bar
geom _smooth() #To add best line of fit(lm function)
geom_abline() #Add line with specified slope and intercept
xlab('x label')
ylab('y label')
ggtitle('title of the plot')

#############################################    Linear Regression  ###################################### 
model1 <- lm(y ~ f1 + f2 + f3 + f3, data=df)  #A linear regression model
summary(model1)  #to check the summary perfomance of the features in the model
model2 <- update(model1, ~ . - f1 ) #to remove a feature from the df incase it's p-value is less than 0.05
anova(model1, model2)  #to check if there's any improvement when a feature is removed by checking if the p-value is less than 0.5

AIC/BIC #Akaike and Bayesian information citeria are used instead of anova. The lower the value, the better the model
AIC(model1, model2)
BIC(model1,model2)

predict(model, test_data) #to do predictions



#############################################  Probability and Statistics  ###########################################################
choose(n,r)  #binomial distribution
sample(vec,n, replace, prob) #to randomly select n numbers from a vector
dbinom #binomial distribution nCr(p)^r(q)^n-r
pbinom #cummulative binomial distribution  nCr(p)^r(q)^n-r + nCr(p)^r(q)^n-r .......
pnorm(30,mean=25,sd=2).... p(X<=30) #PDF for normal distribution... 

x <- seq(from=0, to=6, length.out=100)
plot(x,dnorm(x, mean=3, sd=1))  #to plot the normal distribution
plot(x, dunif(x, min=2,max=4),type='l')  #to plot uniform distibution

########## To customize a probability density function of p(1<=z<=2)
x <- seq(-3,3,length.out=100)
y = dnorm(x)
plot(x,y, main='Standard Normal Distribution', type='l')
abline(h=0)
region.x <- x[1 <= x & x <= 2]
region.y <- y[1 <= x & x <= 2]
region.x <- c(region.x[1], region.x, tail(region.x,1))
region.y <- c( 0, region.y, 0)
polygon(region.x, region.y, density=-1, col='red') #you can also set density=10

summary(df$f1)  #A quick summary of a dataset
summary(table(df$f1,df$f2)) #to check if there is a relationship between two categorical variables by checking the p-value(Chi-Square test)
scale(df) #to normalize a data into z-score.... Standard Scaling

x <- rnorm(50, mean=100, sd=15) #to take some samples(50) from a normal distibution of mean 100 and sd of 15
t.test(x,mu=m) #to check if the sample mean of x is equal to the population mean of m
#As t.test is for mean, wilcox.test is for the median
wilcox.test(x, conf.int = T)  #to check the confidence level for the median
shapiro.test(x)  #it is used to check if a sample came from a normal distribution, p-value > 0.05 means it's from a normal distribution
ks.test(x,y)  #it is used to check if two samples comes from the same distribution, p-value > 0.05 means they're from the same distribution
cor.test(x,y) #if the p-value is less than 0.05, it means it's significant

###################################################### Time Series ####################################################################
library(zoo) #to load th zoo package for time series
ts <- zoo(x, dt) #x is the numeric vectors, dt is the date vectors
prices <- c(132.45, 130.85, 130.00, 129.55, 130.85)
dates <- as.Date(c("2010-01-04", "2010-01-05", "2010-01-06","2010-01-07","2010-01-08"))
ts <- zoo(prices, dates)
coredata(ts)  # to extract the x values
index(ts)  #to extract the dt values
window(ts, start=as.Date('2010-01-05'), end=as.Date('2010-01-07')) #to select some index of the time series
plot(ts)  #to visualize the time series
merge(ts1,ts2)  #to merge two time series together
merge(ts1,ts2,all=FALSE) #inner join to remove NA values
na.locf(merge(ts1,ts2))  #to replace the NaN's value with the most recent observation...locf(last observation carried forward)
lag(ts,k=+1, na.pad=T) #to shift the time series data forward
lag(ts,k=-1, na.pad=T) #to shift the time series data backward
acf(ts) #Auto Correlation function to check significant data in the time series that can be used for ARIMA model
pacf(ts)  #Partial Auto Correlation is also similar to Auto Correlation
### Significant values are the one above the dashed line apart from the first value
Box.test(ts)  #it is also used to check for auto-correlation, p-value < 0.05 means there are significant auto-correlatons
Box.test(ts, type="Ljung-Box")  # Ljung–Box test is used to check auto-correations for small dataset

#####A#####################################    ARIMA MODEL   ######################################################
# Arima model consist of (p,d,q) where p means Auto Regressive(AR), d means Differencing and q means Moving Average Coefficient
# library(forecast)
model <- auto.arima(df$x) #the x is the numeric vector
model <- arima(df$x, order=c(p,d,q)) #use the arima function if you already know the p,d,q values
confint(model) #if the confidence interval range consist of zero, then it is insignificant
#if there is any insignificant coefficient in the model, use the fixed parameter in arima to remove it. 0 for remove, NA for keep
model <- arima(df$Sales_k, order=c(p,d,q), fixed = c(NA,0,NA,NA)) #After checking the confint, 0 to remove the coefficient, NA will keep it
tsdiag(model) #it used to run diagnostic on the model, it will give three graphs

##to make predictions. it returns the prediction($pred) and standard error($se)
predict(model)  #to predict the next observation
predict(model,10) # to predit the next 10 observations


#####################################    PRACTICAL STEPS ON A TIME SERIES DATA    #########################################################
library(forecast)
library(tseries)
#after loading the data frame(df)
df_ts <- ts(df$x, start=c(1972), frequency=12)  #frequency of 12 means monthly
autoplot(df_ts) #to check if it's stationary or not
adf.test(df_ts, k=12) #Dickey-Fuller test to check if it's staionary, if p-value < 0.01, then it is stationary...k=12, because it's monthly
##Keep performing difference on the df_ts, until it becomes stationary. Hence, your number of difference is your 'd'
df_ts1 <- diff(df_ts, differences = 1)  #to perform difference, hence, d=1
df_ts2 <- diff(df_ts, differences = 2)  # d=2....keep trying it, until it becomes stationary
autoplot(df_ts2) # to check if it is now stationary

###############################  Direct method   ##################################
df_ts <- ts(df$x, start=c(1972), frequency=12)  #frequency of 12 means monthly
auto.arima(df_ts) #to get the p,d,q... it might consist of seasonal data also
model<- arima(df_ts, order=c(1,0,0),seasonal = list(order=c(0,1,2)))  #to create the model afer getting the p,d,q values
conf.int(model)  #to check coefficient that are insignificant
tsdiag(model) #it prints out three clean graphs to check the models performance
forecast(model, h=12) #it predicts the next 12 months data
autoplot(forecast(model, h=12))  #to visualize the next 12 months forecasting
