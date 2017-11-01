####################### PROJECT  - IIA  - DVD Sales Dataset #####################

# Create a Linear Regression Model for DVD sales data set. The data set contains the following
# details:
# Advertising: The budget spent on advertising.
# Sales: Number of copies sold
# Plays: Number of plays on Radio Mirchi
# Attractiveness: Attractiveness of the brand (rating scale from 1 to 10; 1 being the worst
#                                              and 10 being the best)


#Problem Statement
#Imagine that the CEO of a DVD player sales company approaches you in order to predict the
#sale of DVDs. He also provides you the data such as the advertising budget (in thousands), sales
#(in thousands), number of times the song is played on the radio channel, Radio Mirchi per week
#and the attractiveness of the brand (rated on a scale of 1 to 10 by an independent agency).

# Ans:
                 ### PREDICTIVE MODELLING TECHNIQUE  ### 
                    ### SIMPLE LINEAR REGRESSION ###

#Simple linear regression is a statistical method that allows us 
#to summarize and study relationships between two continuous 
#(quantitative) variables:
  
# One variable, denoted x, is regarded as the predictor, explanatory, 
# or independent variable.
# The other variable, denoted y, is regarded as the response, outcome, 
# or dependent variable.

# Here we are interested in statistical relationships, in which the 
# relationship between the variables is not perfect wherein the plot 
# exhibits some "trend," but it also exhibits some "scatter." 

# A simple linear regression model that describes the relationship 
# between two variables x and y can be expressed by the following equation. 
# wherein the numbers ?? and ?? are called parameters, and ?? is the error term.
# as given below:-----------------

y = ?? + ??x+ ??



                   ##################################

(package="datasets")
options(stringsAsFactors=FALSE) # to prevent strings or characters being converted to Factors
options(scipen=999) # To prevent the numbers from being converted to exponential form
                    
                   ################################   

# install.packages("data.table")
library(data.table)
# install.packages("readxl")
library(readxl)
# install.packages("gdata")
library(gdata)
# install.packages("xlsx")
library(xlsx)
# install.packages("XLConnect")
library(XLConnect)
# install.packages("corrplot")
library(corrplot)
# install.packages("gclus")
library(gclus)
# install.packages("caTools")
library(caTools)
# install.packages("Hmisc")
library(Hmisc)
# install.packages("e1071")
library(e1071)
# install.packages("lmtest")
library(lmtest)
# install.packages("car")
library(car)
# install.packages("MASS")
library(MASS)
# install.packages("ggpubr")
library(ggpubr)
# install.packages("caret")
library(caret)
# install.packages("DAAG")
library(DAAG)
# install.packages("Metrics")
library(Metrics)
# install.packages("usdm")
library(usdm)
#install.packages("hexbin")
#library(hexbin)
# install.packages("earth")
library(earth)
# install.packages("gvlma")
library(gvlma)

######################################################

# To import .csv file in R by using fread(), we need to first invoke the 
# library named data.table as given below:--

sales_dataset<-fread("E://Back up 1/ACADGILD/Project - II/Sales_dataset.csv",header = TRUE,sep=",", stringsAsFactors=FALSE)
View(sales_dataset)

# To understand the given dataset/table in R environment, a few Basic Codes needs 
# to be executed as follows:------------

class(sales_dataset) # to understand the structure of the given datset

str(sales_dataset) # To understand the data structures of the given variables in the dataset
              
summary(sales_dataset) # To understand the Descriptive Statistics of the given variables in the dataset


# Before performing any Predictive Modelling Technique, a few basic tests for the 
# Sample Dataset needs to be carried out as follows:-

# I) Normality Test - To check/assess for Normality of Data:

# If the sample size is large enough (n > 30), we can ignore the distribution 
# of the data and use Parametric tests.
# The central limit theorem tells us that the sampling distribution tends to 
# be normal if the sample is large enough (n > 30).

# However, to be consistent, normality can be checked by visual inspection
# using the following techniques:
# a) Normal plots (Histogram)
# b) Q-Q plot (quantile-quantile plot) or 
# c) Significance tests

# In the situations where the assumptions are violated, Non-Paramatric Tests 
# are recommended.

# a) Normal Plots (Histogram):
  hist(sales_dataset$advertise)
  kurtosis(sales_dataset$advertise)
  skewness(sales_dataset$advertise)
  
  hist(sales_dataset$plays)
  kurtosis(sales_dataset$plays)
  skewness(sales_dataset$plays)
  
  hist(sales_dataset$attractiveness)
  kurtosis(sales_dataset$attractiveness)
  skewness(sales_dataset$attractiveness)
  
  hist(sales_dataset$sales)
  kurtosis(sales_dataset$sales)
  skewness(sales_dataset$sales)
  
# b) Q-Q plot (quantile-quantile plot) 
# To assess the Outliers in the given dataset--
  layout(matrix(c(1),1)) # Layout of the plots
  
  qqPlot(sales_dataset, main="QQ Plot") #qq plot for studentized residuals 
  
  qqplot(x = sales_dataset$advertise, y = sales_dataset$sales,main="QQ Plot")
  
  qqplot(x = sales_dataset$plays, y = sales_dataset$sales, main="QQ Plot")
  
  qqplot(x = sales_dataset$attractiveness, y = sales_dataset$sales, main="QQ Plot")
  
  require(ggpubr) 
  ggqqplot(sales_dataset$advertise)
  ggqqplot(sales_dataset$plays)
  ggqqplot(sales_dataset$attractiveness)
  ggqqplot(sales_dataset$sales)
  
# As all the points fall approximately along this reference line, 
# we can assume normality.

# c) Significance Test----
# In significance test, we compare the sample distribution to a normal one 
# in order to ascertain whether data show or not a serious deviation from normality.

# There are several methods for Normality test such as: 
# i) Kolmogorov-Smirnov (K-S) normality test and 
# ii) Shapiro-Wilk's test.

# Shapiro-Wilk's method is widely recommended for normality test and 
# it provides better power than K-S. It is based on the correlation 
# between the data and the corresponding normal scores.

# The Null hypothesis of such tests conveys that ???sample distribution is normal???. 
# If the test is significant, the distribution is non-normal.

# The R function shapiro.test() can be used to perform the Shapiro-Wilk 
# test of normality for one variable (univariate).

shapiro.test(sales_dataset$advertise)
skewness(sales_dataset$advertise)

shapiro.test(sales_dataset$plays)
skewness(sales_dataset$plays)

shapiro.test(sales_dataset$attractiveness)
skewness(sales_dataset$attractiveness)

shapiro.test(sales_dataset$sales)
skewness(sales_dataset$sales)

# To understand the Confidence Levels---

t_crit_val_advertise <- quantile(sales_dataset$advertise,probs = c(0.684,0.945,0.99))
t_crit_val_advertise

t_crit_val_plays <- quantile(sales_dataset$plays,probs = c(0.684,0.945,0.99))
t_crit_val_plays

t_crit_val_attractiveness <- quantile(sales_dataset$attractiveness,probs = c(0.684,0.945,0.99))
t_crit_val_attractiveness

t_crit_val_sales <- quantile(sales_dataset$sales,probs = c(0.684,0.945,0.99))
t_crit_val_sales

# Shapiro.test tests the NULL hypothesis that the given samples are collected
# or taken from a Normal distribution. 

# This means that if p-value is < or= 0.05, then the NULL hypothesis is rejected. 
# the samples have been taken from a Normal distribution.

# Normality test is passed.

# II) Density Plot---

# Density Plot is used to check if the Response/Dependant i.e. y variable is 
# close to normality or not

library(e1071)
#par(mfrow=c(1, 3))  # divide graph area in 3 columns

plot(density(sales_dataset$advertise), main="Density Plot: Advertise", ylab="Sales", sub=paste("Skewness:", round(e1071::skewness(sales_dataset$advertise), 2)))  # density plot for advertise
polygon(density(sales_dataset$advertise), col="tan")

plot(density(sales_dataset$plays), main="Density Plot: Plays", ylab="Sales", sub=paste("Skewness:", round(e1071::skewness(sales_dataset$plays), 2)))  # density plot for plays
polygon(density(sales_dataset$plays), col="beige")

plot(density(sales_dataset$attractiveness), main="Density Plot: Attractiveness", ylab="Sales", sub=paste("Skewness:", round(e1071::skewness(sales_dataset$attractiveness))))  # density plot for attractiveness
polygon(density(sales_dataset$attractiveness), col="light green")

# III) To generate Box-plots-----

boxplot(sales~advertise,data=sales_dataset, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="DVD_Sales_Data", 
        xlab="advertise", ylab="sales")

boxplot(sales~plays,data=sales_dataset, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="DVD_Sales_Data", 
        xlab="plays", ylab="sales")

boxplot(sales~attractiveness,data=sales_dataset, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="DVD_Sales_Data", 
        xlab="attractiveness", ylab="sales")


# IV) To generate Scatter Plots---------
# To generate a Scatter Plot----
plot(sales_dataset$advertise,sales_dataset$sales,xlab="Advertisements",ylab="DVD Sales")

# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(sales_dataset$sales ~ sales_dataset$advertise))

# To generate a Scatter Plot ----
plot(sales_dataset$plays, sales_dataset$sales, xlab="plays", ylab="DVD Sales")

# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(sales_dataset$sales ~ sales_dataset$plays))

# To generate a Scatter Plot ----
plot(sales_dataset$attractiveness, sales_dataset$sales, xlab="attractiveness", ylab="DVD Sales")

# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(sales_dataset$sales ~ sales_dataset$attractiveness))

# Basic Scatterplot Matrix with all the variables in the given dataset----
pairs(~sales+advertise+plays+attractiveness,data=sales_dataset, 
      main="Simple Scatterplot Matrix")
dvd_corr<-abs(cor(sales_dataset))# to get correlations
dvd_corr
dvd_col<-dmat.color(dvd_corr) # get colors(sales_dataset))
dvd_col

# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dvd_corr)
dta.o

cpairs(sales_dataset, dta.o, panel.colors=dvd_col, gap=.5,
       main="Variables Ordered and Colored by Correlation")

# V) High Density Scatterplot with Binning

sales <- rnorm(1000)
advertise <- rnorm(1000)
sales_dataset<-hexbin(sales, advertise, xbins=50) 
sales_dataset
plot(sales_dataset, main="Hexagonal_Binning_Advertisements")

sales <- rnorm(1000)
plays <- rnorm(1000)
sales_dataset<-hexbin(sales, plays, xbins=50) 
sales_dataset
plot(sales_dataset, main="Hexagonal_Binning_Plays")

sales <- rnorm(1000)
attractiveness <- rnorm(1000)
sales_dataset<-hexbin(sales, attractiveness, xbins=50) 
sales_dataset
plot(sales_dataset, main="Hexagonal_Binning_Attractiveness")


# Thus by executing the aforesaid Statistical Tests, we have checked for
# normality of the Sampling data.

####################################


# VI) Correlation:
# To compute Correlation Matrix---
Mx<-cor(sales_dataset,method=c("pearson", "kendall", "spearman"))
Mx
Ro<-(round(Mx,2))
Ro
cor.test(sales_dataset$sales,sales_dataset$advertise, method=c("pearson", "kendall", "spearman"))
cor.test(sales_dataset$sales, sales_dataset$plays, method=c("pearson", "kendall", "spearman"))
cor.test(sales_dataset$sales, sales_dataset$attractiveness,method=c("pearson", "kendall", "spearman"))
rcorr(as.matrix(sales_dataset)) 

# Correlogram is used for generating Correlation Matrix.
# Corrplot() is used to plot the graph of Correlation Matrix

corrplot(Mx, method="pie")
corrplot(Mx, method="circle")
corrplot(Mx, method="square")


corrplot(Mx, method="color")
corrplot(Mx, method="shade")
corrplot.mixed(Mx, tl.pos = "lt", diag = "u")


corrplot(Mx, method="number")

corrplot.mixed(Mx, tl.pos = "lt")
corrplot.mixed(Mx, tl.pos = "lt", diag = "u")
corrplot.mixed(Mx, tl.pos = "lt", diag = "l")
corrplot.mixed(Mx, tl.pos = "n")

# Positive correlations are displayed in blue and negative correlations in 
# red color. Color intensity and the size of the circle are proportional to 
# the correlation coefficients.

# Reordering the correlation matrix---
# correlogram with hclust reordering
corrplot(Mx, order="hclust")

# Combining correlogram with the significance test &
# Computing the p-value of correlations

# To compute the matrix of p-value, a custom R function is used :


# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(sales_dataset)
p.mat
head(p.mat[, 1:4])

# To Add significance level to the correlogram

# Specialized the Insignificant values or to Strike-Off Insignificant Values 
# according to the significant level---------

corrplot(Mx, type="full", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

# OR----
# Leave blank on no significant coefficient
corrplot(Mx, type="full", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")

# correlations with p-value > 0.01 are considered as insignificant.
#In this case the correlation coefficient values are either left blank or 
# crosses are added.

# To Customize the correlogram ----------------------

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(Mx, method="color", col=col(200),  
         type="full", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="maroon", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

# Thus the correlation of variables within the given sampling dataset  has been 
# checked for and noted.
 

####################

# To create Training (Development) and Test (Validation) data from the 
# original dataset----

## Set the seed to make the dataset partition reproductible----
set.seed(123)

# Sample Indexes
# Split dataset into 70% and 30%
indexes = sample(1:nrow(sales_dataset), size=0.3*nrow(sales_dataset))
test_data = sales_dataset[indexes,]
dim(test_data)  
train_data = sales_dataset[-indexes,]
dim(train_data) 

# OR-------
require(caTools)
sample = sample.split(sales_dataset$sales, SplitRatio = .70)
train_data = subset(sales_dataset, sample == TRUE)
test_data  = subset(sales_dataset, sample == FALSE)
dim(train_data)
dim(test_data)

# OR ---

require(caret)
inTraining <- createDataPartition(sales_dataset$sales, p = .70, list = FALSE)
train_data <- sales_dataset[ inTraining,]
test_data  <- sales_dataset[-inTraining,]
dim(train_data)
dim(test_data)

# Variable Selection for Linear Regression:---
# By using using the stepAIC( ) function from the MASS package, stepwise selection 
# can be performed comprising of 3 types namely:-
# a) Forward Selection
# b) Backward Elimination
# c) Forward-Backward Selection / Step-wise Regression

# a) Forward Selection -
# Step() can be used to perform variable selection. 
# To perform forward selection we need to begin by specifying a starting model 
# and the range of models which we want to examine in the search. 

null=lm(sales~1, data=train_data)
null

full=lm(sales~., data=train_data)
full

# To perform forward selection using the command:
step(null, scope=list(lower=null, upper=full), direction="forward")

# By doing so, it tells R to start with the null model and search through models 
# lying in the range between the null and full model using the forward selection 
# algorithm.

# b) Backward Elimination ---
step(full, data=train_data, direction="backward")

# c) Forward-Backward Selection / Step-wise Regression

step(null, scope = list(upper=full), data=train_data, direction="both")

# Herein, algorithms give rise to results that are equivalent to the 
# forward selection 

# By applying lm() to all the variables in the given dataset in order to
# do the fitting of the data-points---

require(MASS)
fit <- lm(sales~advertise + plays + attractiveness,data=train_data)
fit

# AIC (Akaike's information criterion):
# It is a measure of measures of the goodness of fit of an estimated 
# statistical model and can also be used for model selection.

AIC_forward_step <- stepAIC(fit, direction="forward") # AIC value is 1088.75
AIC_forward_step$anova

AIC_backward_step <- stepAIC(fit, direction="backward") # AIC value is 1088.75
AIC_backward_step$anova

AIC_stepwise <- stepAIC(fit, direction="both") # AIC value is 1088.75
AIC_stepwise$anova # display results


# BIC (Bayesian information criterion):
# Similar to AIC, this test is a measure of measures of the goodness of fit 
# of an estimated statistical model and can also be used for model selection.

BIC_forward_step<- BIC(fit) # BIC value is 1508.51
BIC_forward_step
# BIC_forward_step$anova

# BIC_backward_step<- BIC(fit, direction="backward") # BIC value is 2130.94
# BIC_backward_step$anova

# BIC_stepwise <- BIC(fit, direction="both") # BIC value is 
# BIC_stepwise$anova # display results


# Detection of Outliers within the given dataset ----

# Bonferroni Outlier Test is used which reports the 
# Bonferroni p-values for Studentized residuals in linear and generalized 
# linear models

outlierTest(fit,cutoff = Inf,n.max = Inf)
leveragePlots(fit) # leverage plots

#######################

# Estimated Simple Regression Equation-----

# To Apply the simple linear regression model for the data set sales_dataset, 
# and to estimate the DVD Sales,lm function is applied as follows:- 

# a) Model Preparation/Building of Algorithm for the Linear Regression Model: 

#df <- data.frame(matrix(unlist(l), nrow=132, byrow=T),stringsAsFactors=FALSE)

dvdmod1=lm(sales~.,train_data)
dvdmod1
summary(dvdmod1)
#dvd.fitted1<-data.frame(train_data,"fitted"=fitted(dvdmod1),"residual"=resid(dvdmod1))
#View(dvd.fitted1)

plot(dvdmod1)
#plot(dvd.fitted1)

# To predict the DVD Sales:-----
SalesPred_Mod_1<-predict(dvdmod1,test_data)

#Summary
summary(SalesPred_Mod_1)

# ----OR-----

require(earth)
new_model <-earth(formula=sales ~ plays + advertise + attractiveness,data = sales_dataset)
evimp(new_model)

#b) Alternative model/Alternative Algorithm for the Linear Regression Model:---
dvdmod2 = lm(sales ~ advertise + plays, data=train_data)
dvdmod2

# Thereafter, the parameters of the estimated regression equation with the
# coefficients function are extracted as follows:

coeffs = coefficients(dvdmod2); coeffs
summary(dvdmod2)

# To predict the DVD Sales:-----
SalesPred_Mod_2<-predict(dvdmod2,test_data)

#Summary
summary(SalesPred_Mod_2)
plot(dvdmod2)

# Based on the outcome i.e. Summary of 2 algorithms i.e. dvdmod1 and dvdmod2,
#we can decipher as follows:

# a) dvdmod1
# Multiple R-squared:  0.6866,	Adjusted R-squared:  0.6798 
# Residual standard error: 46.66
# p-value: < 0.00000000000000022

# b) dvdmod2
# Multiple R-squared:  0.6452,	Adjusted R-squared:  0.6401
# Residual standard error: 49.47
# p-value: < 0.00000000000000022

# a) Based on the outcome, we can see that the 1st algorithm (dvdmod1) consisting of all the 
# variables, R square=0.68 and Adjusted R square=0.67.
# b) Based on the outcome, we can see that the 2nd algorithm (dvdmod2) consisting of selected 
# variables, R square=0.64 and Adjusted R square=0.64.

# So we can observe that R square and Adjusted R square values decrease while 
# opting for selected variables, which is not a healthy sign for a robust model.
# Higher the R2 and Adjusted R2 the better, hence we retain the first alogritm (dvdmod1)

# c) There is no change in p value for both the proposed models i.e. 
# p-value: < 0.00000000000000022.

# d) Residual standard error: 46.66 (dvdmod1) and
#    Residual standard error: 49.47 (dvdmod2)

# Residual error increases upon specified or select variables which is again not
# a healthy sign for a good, robust model. Moreover, the standard error should be
# closer to zero the better.

# Based on the outcomes of the aforesaid 2 algorithms, it is found that the
# first algorithm  is considered to be a robust and better algorithm for
# the proposed model.
# Hence, we retain only the 1st algorithm (i.e. dvdmod1) and discard the 2nd
# algorithm for the proposed model(i.e. dvdmod2.)


# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(sales ~ plays,advertise))

# Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(dvdmod1)

# To revert back to the default method of Graphs/Plots:
layout(matrix(c(1),1))

# Calculate prediction accuracy and error rates:
#A simple correlation between the actuals and predicted values can be used
# as a form of accuracy measure. A higher correlation accuracy implies that 
# the actuals and predicted values have similar directional movement, 
# i.e. when the actuals values increase the predicteds also increase and vice-versa.

# To make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=test_data$sales, predicteds=SalesPred_Mod_1))  
actuals_preds
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# To calculate Min-Max Accuracy:
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# 64.5% of Min-Max accuracy.
# Higher the Min-Max Accuracy the better.


# MAPE (Mean Absolute Processing Error):
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

# k- Fold Cross validation: For this purpose, we perform MSE(Mean Square Error) calculation.
# It is important to rigorously test the models performance as much as possible. 
# One way is to ensure that the model equation you have will perform well, when it 
# is built on a different subset of training data and predicted on the remaining 
# data.
# For this purpose, Split your data into k-mutually exclusive random sample 
# portions. Keeping each portion as test data, we build the model on the remaining
# (k-1 portion) data and calculate the mean squared error of the predictions. This
# is done for each of the k-random sample portions. Then finally, the average of 
# these mean squared errors (for k portions) is computed. We can use this metric 
# to compare different linear models.

# By doing this, we need to check two things:
  
# i)  If the model's prediction accuracy isn't varying too much for any one 
# particular sample, and
# ii) If the lines of best fit don't vary too much with respect the the slope 
# and level.
# In other words, they should be parallel and as close to each other as possible. 


cvResults <- suppressWarnings(CVlm(df=test_data, form.lm=(test_data$sales ~ test_data$advertise+ test_data$plays+test_daata$attractiveness), m=5, 
                                   dots=FALSE, seed=29, legend.pos="topleft",  
                                   printit=FALSE, main="Small symbols are predicted
                                   values while bigger ones are actuals."));  # performs the CV

cvResults <- CVlm(df=sales_dataset, form.lm=sales ~ ., m=5, 
                                   dots=FALSE, seed=29, legend.pos="topleft",  
                                   printit=FALSE, main="Small symbols are predicted
                                   values while bigger ones are actuals")  # performs the CV


pd <- predict(train_data , training.data)

mean((train_data - predict(dvdmod1))^2)

# Calculating MSE (Mean Standard Error)
# x <- test$x
# p <- predict(model, data.frame(x)) # or: predict(model, test)

sales <- test_data$sales_dataset
p <- predict(dvdmod1, data.frame(sales_dataset))
p<-predict(dvdmod1,test_data)
p


#MSE

mean((test_data$sales - predict.lm(dvdmod1, test_data)) ^ 2)

#mse(actual, predicted)
pd <- predict(dvdmod1 , train_data)
#mse(training.data$pd)
mse(train_data$sales,pd)

mean((test_data - predict(dvdmod1))^2)

###############################

#  To check for the validity of Assumptions of Linear Regression:
# 1) To rule out Heteroscedascity &  establish the presecence of 
# Homoscesdacity in a model- Breusch-Pagan test is used.
# To test/conformance for Homoscesdacity in a given linear regression model-----

library(lmtest)

Breusch_Pagan_Test<-bptest(dvdmod1, ~.,  data=sales_dataset, studentize = TRUE)
Breusch_Pagan_Test


# So if p_val < 0.05 (or your chosen alpha value); you reject the Null Hypothesis 
# and infer the presence of Heteroscedasticity.

# If p_val > 0.05 (or your chosen alpha value); you accept the Null Hypothesis and
# conclude the presence of Homoscedasticity.
# In this case, p value (0.1365) > 0.05, hence we accept Null Hypothesis and conclude 
# the presence of Homoscedasticity  and therefore the absence of any 
# Heterscedasticity within the given dataset, thereby in conformance with the 
# assumptions of Linear Regression.

# 2) Multicollinearity---

# Multicollinearity exists when two or more of the predictors in a regression model 
# are moderately or highly correlated. Unfortunately, when it exists, it can wreak 
# havoc on our analysis and thereby limit the research conclusions we can draw. 
# All the variables having VIF > 2.5 are faced with a problem of multicollinearity and
# hence it is better to discard such variables.

# To detect Multicollinarity in the given dataset, a function called as
# Variance Inflation Factors (VIF) is used. 
# As the name suggests, a # variance inflation factor (VIF) quantifies 
# how much the variance is inflated. 

require(car)
multicollinearity_test<-vif(dvdmod1)
summary(multicollinearity_test)

# 3) Additional Diagnostic Help:-
# The gvlma( ) function in the gvlma package, performs a global validation of 
# linear model assumptions as well separate evaluations of 
# i)  skewness
# ii) kurtosis and 
# iii) heteroscedasticity.

gvmodel <- gvlma(dvdmod1) 
summary(gvmodel)

