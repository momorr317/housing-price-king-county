#========================================================#
# STAT 6021: Linear Models for Data Science              |
# Final Project                                          |
#========================================================#
library(tidyverse)
library(perturb)
library(car)
library(caret)

#========================================================#
# Basic exploration                                      |
#========================================================#
# Read in raw dataset
raw <- read.csv("kc_house_data.csv", sep=',')  # 21613    21

# Check missing value 
miss = sapply(raw, function(x) length(which(is.na(x))))
miss.df = data.frame(miss[miss>0])
names(miss.df) = "number_of_missing_values"
miss.df # no missing value

# Since the problem does not involve time series analysis, we drop the "date" column
# id is not useful in our model, we drop it as well
# Note that we already have location information by longitude and latitude. Feature zipcode 
# indicates location info as well with 69 levels. To avoid redundance and multiple dimentions, 
# we exclude zipcode
dat <- subset(raw, select = -c(date, id, zipcode))  # 21613    18

# Drop duplicates
dat <- unique(dat)   # 21608    18

# Brief summary 
summary(dat) 
# price            bedrooms        bathrooms      sqft_living       sqft_lot           floors     
# Min.   :  75000   Min.   : 0.000   Min.   :0.000   Min.   :  290   Min.   :    520   Min.   :1.000  
# 1st Qu.: 321838   1st Qu.: 3.000   1st Qu.:1.750   1st Qu.: 1429   1st Qu.:   5040   1st Qu.:1.000  
# Median : 450000   Median : 3.000   Median :2.250   Median : 1910   Median :   7620   Median :1.500  
# Mean   : 540098   Mean   : 3.371   Mean   :2.115   Mean   : 2080   Mean   :  15110   Mean   :1.494  
# 3rd Qu.: 645000   3rd Qu.: 4.000   3rd Qu.:2.500   3rd Qu.: 2550   3rd Qu.:  10690   3rd Qu.:2.000  
# Max.   :7700000   Max.   :33.000   Max.   :8.000   Max.   :13540   Max.   :1651359   Max.   :3.500  
# waterfront            view          condition         grade          sqft_above   sqft_basement   
# Min.   :0.000000   Min.   :0.0000   Min.   :1.000   Min.   : 1.000   Min.   : 290   Min.   :   0.0  
# 1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:3.000   1st Qu.: 7.000   1st Qu.:1190   1st Qu.:   0.0  
# Median :0.000000   Median :0.0000   Median :3.000   Median : 7.000   Median :1560   Median :   0.0  
# Mean   :0.007543   Mean   :0.2342   Mean   :3.409   Mean   : 7.657   Mean   :1788   Mean   : 291.5  
# 3rd Qu.:0.000000   3rd Qu.:0.0000   3rd Qu.:4.000   3rd Qu.: 8.000   3rd Qu.:2210   3rd Qu.: 560.0  
# Max.   :1.000000   Max.   :4.0000   Max.   :5.000   Max.   :13.000   Max.   :9410   Max.   :4820.0  
# yr_built     yr_renovated          lat             long        sqft_living15    sqft_lot15    
# Min.   :1900   Min.   :   0.00   Min.   :47.16   Min.   :-122.5   Min.   : 399   Min.   :   651  
# 1st Qu.:1951   1st Qu.:   0.00   1st Qu.:47.47   1st Qu.:-122.3   1st Qu.:1490   1st Qu.:  5100  
# Median :1975   Median :   0.00   Median :47.57   Median :-122.2   Median :1840   Median :  7620  
# Mean   :1971   Mean   :  84.33   Mean   :47.56   Mean   :-122.2   Mean   :1987   Mean   : 12770  
# 3rd Qu.:1997   3rd Qu.:   0.00   3rd Qu.:47.68   3rd Qu.:-122.1   3rd Qu.:2360   3rd Qu.: 10083  
# Max.   :2015   Max.   :2015.00   Max.   :47.78   Max.   :-121.3   Max.   :6210   Max.   :871200  

# It seems that many columns have outliers and variables are in different scale
# price, bedrooms, bathrooms, sqft_living, sqft_lot and sqft_basement have outliers

# Check range, standard deviation
sapply(dat, range)       # Many variables are in different scales
# price bedrooms bathrooms sqft_living sqft_lot floors waterfront view condition grade sqft_above
# [1,]   75000        0         0         290      520    1.0          0    0         1     1        290
# [2,] 7700000       33         8       13540  1651359    3.5          1    4         5    13       9410
# sqft_basement yr_built yr_renovated     lat     long sqft_living15 sqft_lot15
# [1,]             0     1900            0 47.1559 -122.519           399        651
# [2,]          4820     2015         2015 47.7776 -121.315          6210     871200

sapply(dat, sd)  # The standard deviations of different variables vary much
                 # Probably this is also due to the fact that they are measured in a large scale
# price      bedrooms     bathrooms   sqft_living      sqft_lot        floors    waterfront          view 
# 3.671272e+05  9.300618e-01  7.701632e-01  9.184409e+02  4.142051e+04  5.399889e-01  8.651720e-02  7.663176e-01 
# condition         grade    sqft_above sqft_basement      yr_built  yr_renovated           lat          long 
# 6.507430e-01  1.175459e+00  8.280910e+02  4.425750e+02  2.937341e+01  4.016792e+02  1.385637e-01  1.408283e-01 
# sqft_living15    sqft_lot15 
# 6.853913e+02  2.730418e+04 


# Correlation 
cor(dat)     
# It seems that price and sqft_living have high correlation
# sqft_living have high correlation with price, bathrooms, grade, sqft_above and sqft_living15
# sqft_above has correlation with grade
# sqft_above has correlation with sqft_living15

# Categorcial data
# To avoid curse of dimensonality, we only treat waterfront, condition and yr_renovated as categorical data
# Since most of value in yr_renovated is 0, we create another dummy variable to indicate the presence of renovation

dat$isRenovated <- ifelse(dat$yr_renovated>0, 1, 0)
dat <- subset(dat, select = -c(yr_renovated))  

# Since waterfront has only 0 and 1, it is not necessary to change it to dummy variable
dat$condition <- as.factor(dat$condition)

# No abnormal feature appears so far

# Plot
par(mfrow = c(1,1))
hist(dat$price, main="Distribution of Price", xlab="price", ylab="Frequency")  # The distribution of price is right-skewed

# Pairs graph
# Since it is too slow to perform pairs plot on original price, we change the scale
dat$price.new <- dat$price/1000
# pairs(dat[,c(2:7, 19)]) 
# pairs(dat[,c(8:12, 19)])
# pairs(dat[,c(13:18, 19)])

# Scatter plot
par(mfrow=c(2,2))
plot(dat$bedrooms, dat$price.new,  main="bedrooms vs price", xlab="bedrooms", ylab="price/1000")
plot(dat$bathrooms, dat$price.new, main="bathrooms vs price", xlab="bathrooms", ylab="price/1000")
plot(dat$sqft_living, dat$price.new, main="sqft_living vs price", xlab="sqft_living", ylab="price/1000")
plot(dat$floors, dat$price.new, main="floors vs price", xlab="floors", ylab="price/1000")
plot(dat$grade, dat$price.new, main="grade vs price", xlab="grade", ylab="price/1000")
plot(dat$sqft_above, dat$price.new, main="sqft_above vs price", xlab="sqft_above", ylab="price/1000")
plot(dat$sqft_basement, dat$price.new, main="sqft_basement vs price", xlab="sqft_basement", ylab="price/1000")
plot(dat$lat, dat$price.new, main="lat vs price", xlab="lat", ylab="price/1000")
plot(dat$long, dat$price.new, main="long vs price", xlab="long", ylab="price/1000")
plot(dat$sqft_lot15, dat$price.new, main="sqft_lot15 vs price", xlab="sqft_lot15", ylab="price/1000")
par(mfrow=c(1,1))

# According to pairs plot and scatter plot, it seems price has a relationship with bedrooms, bathrooms,
# sqft_living, sqft_lot, condition and grade

#========================================================#
# Basic exploration                                      |
#========================================================#
#first try create dummy variable for condition
df <- dat
dummy_condi <- dummyVars(~ condition, data = df)
output = predict(dummy_condi, newdata=df)
head(output, n=3)

output <- as.data.frame(output)
df <- df[, !(colnames(df) %in% c("condition", "price.new"))]
df <- cbind(df, output[,-1])  #mz - added -1 here to drop one level

# fit full model
fit <- lm(price~., data=df)
summary(fit) # coefficient for sqft_basement is NA
alias( fit ) # check dependency in the terms of the model
# Complete :
#   (Intercept) bedrooms bathrooms sqft_living sqft_lot floors waterfront view grade sqft_above
# sqft_basement  0           0        0         1           0        0      0          0    0    -1        
# yr_built lat long sqft_living15 sqft_lot15 isRenovated condition.2 condition.3 condition.4
# sqft_basement  0        0   0    0             0          0           0           0           0         
# condition.5
# sqft_basement  0            

# based on above output, sqft_basement depends on sqft_living and sqft_above - remove sqft_basement in model
fit <- lm(price~.-sqft_basement, data=df)
summary(fit)
vif(fit)
# bedrooms     bathrooms   sqft_living      sqft_lot        floors    waterfront          view 
# 1.649584      3.362056      8.639979      2.104766      2.006841      1.203971      1.423532 
# grade    sqft_above      yr_built           lat          long sqft_living15    sqft_lot15 
# 3.429339      6.954914      2.434464      1.127939      1.501821      2.958025      2.136011 
# isRenovated   condition.2   condition.3   condition.4   condition.5 
# 1.156049      6.700471    166.055099    141.273639     53.542539 

# A VIF exceeding 5 or 10 indicates the presence of multicollinearity. 
# VIF of sqft_living, sqft_above and condition dummy variables indicate multicollinearity.

# From summary of the model we can see that t value for sqft_living is smaller than t value for
#sqft_above. Because of multi-collinearity, we will remove sqft_above.
# Also, remove condition.3 (the one with highest VIF), fit the model again

# update df
df <- subset(df, select=-c(sqft_basement, sqft_above, condition.3))
names(df)
# [1] "price"         "bedrooms"      "bathrooms"     "sqft_living"   "sqft_lot"      "floors"       
# [7] "waterfront"    "view"          "grade"         "yr_built"      "lat"           "long"         
# [13] "sqft_living15" "sqft_lot15"    "isRenovated"   "condition.2"   "condition.4"   "condition.5" 

# refit model 
fit.2 <- lm(price~., data=df) 
summary(fit.2)
vif(fit.2)
# bedrooms     bathrooms   sqft_living      sqft_lot        floors    waterfront          view 
# 1.648726      3.282687      5.012480      2.102152      1.615315      1.202212      1.383752 
# grade      yr_built           lat          long sqft_living15    sqft_lot15   isRenovated 
# 3.347803      2.430258      1.110649      1.440785      2.889582      2.135114      1.155591 
# condition.2   condition.4   condition.5 
# 1.024054      1.229171      1.191803 

# VIF improved, and only sqft_living has VIF >5, keep for now, use feature selection methods later
# condition.2 has insignificant p -value - remove condition.2
df <- subset(df, select=-c(condition.2))  # 21608    17
names(df)
# [1] "price"         "bedrooms"      "bathrooms"     "sqft_living"   "sqft_lot"      "floors"       
# [7] "waterfront"    "view"          "grade"         "yr_built"      "lat"           "long"         
# [13] "sqft_living15" "sqft_lot15"    "isRenovated"   "condition.4"   "condition.5"  

# Condition indice and variance decomposition proportions
colldiag(fit.2, scale=TRUE, center=TRUE, add.intercept=FALSE)
# Condition indice greater than 30 indicates the presence of multicollinearity.
# Based on condition indice, no multicollinearity involved in the model.

#========================================================#
# Influential Points Diagnostics                         |
#========================================================#
dat.lm <-lm(price~., data=df)

# Used the function "influence.measures" to calculate the standard influence diagnostics
options(max.print=1000000)
#influence.measures(dat.lm)

# Use the function "cooks.distance" to calculate the cook's distance:
dat.cook <- cooks.distance(dat.lm)
#dat.cook

# The associated cutoff value for identifying an influential observation is
cut.inf1 <- 1 

# The results from comparing against the cutoff value are displayed as follows
a <- data.frame(cbind(dat.cook, dat.cook > cut.inf1))
names(a) <- c("dat.cook","cut.inf1.compare")
filter(a,a$cut.inf1.compare == 1)
# Based on the result of Cook's distance, there's no influential points that we need to remove.

##########
# DFFITS #
##########
# Use the function "dffits" to calculate the value of DFFITS
dat.dffits <- dffits(dat.lm)
#dat.dffits

# The associated cutoff value for identifying an influential observation is
n <- dim(dat)[1]
p <- length(coefficients(dat.lm))
cut.inf2 <- 2*sqrt(p/n)  # 0.05609797

# The results from comparing against the cutoff value are displayed as follows
b <- data.frame(cbind(dat.dffits, abs(dat.dffits) > cut.inf2))
names(b) <- c("dat.dffits","cut.inf2.compare")
filter(b,b$cut.inf2.compare == 1)  # There are 1117 influential points found by DFFITS
b2 <- rownames(b[b$cut.inf2.compare==1,])
df_dffits <- df[-as.numeric(b2),]  # Delete influential points found by DFFITS
#df_dffits                         # 20491, 17

# Calculate R student residual and fitted values on original data
R.stud.res <- rstudent(dat.lm)
y.hat <- fitted(dat.lm)

# Calculate R student residual and fitted values on data after deleting the influential points
dat.lm.dffits <- lm(price~., data=df_dffits)
R.stud.res.dffits <- rstudent(dat.lm.dffits)
y.hat.dffits <- fitted(dat.lm.dffits)

# Residual plots for comparing these datasets
par(mfrow = c(2, 2), mai=c(0.7,0.7,0.2,0.1)) # mai=c(bottom, left, top, right)
qqnorm(R.stud.res, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals (original)")
qqline(R.stud.res)
plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot (original)")
abline(h=0, lty=1, lwd=3)
qqnorm(R.stud.res.dffits, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals (dffits)")
qqline(R.stud.res.dffits)
plot(y.hat.dffits, R.stud.res.dffits, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot (dffits)")
abline(h=0, lty=1, lwd=3)
par(mfrow = c(1, 1))

summary(dat.lm)
summary(dat.lm.dffits)

# By using DFFITS we have 1117 influential points. 
# After removing those points the normality probability and residual-by-fitted-value points are 
# much better because the number of outliers reduced a lot.
# Also the Adjutsed R-squared for original model is 0.6949, and the Adjusted R-squared for DFFITS model 
# is 0.7026 which improves a lot.

###########
# DFBETAS #
###########
# Use the function "dfbates" to calculate the value of dfbates
dat.dfbetas <- dfbetas(dat.lm)
dat.dfbetas

# The associated cutoff value for identifying an influential observation is
n <- dim(dat)[1]
cut.inf3 <- 2/sqrt(n)  # 0.01360576

# Because we have 17 columns, and all of those columns are very important to consider, 
# we need to sum up the number of columns whose value is larger than cutoff value and 
# choose the sum rows which have the sum equals to 17. Only by doing so we can pretty sure 
# that those points we selected are 100% influential points. 

# The results from comparing against the cutoff value are displayed as follows
c <- data.frame(cbind(dat.dfbetas, abs(dat.dfbetas) > cut.inf3))
d <- c[,18:34,]
e <- data.frame(rowSums(d))
names(e) <- c("cut.inf3.compare")
filter(e,e$cut.inf3.compare == 17)
e$index <- rownames(e)
e[which(e$cut.inf3.compare ==17),2]
# [1] "1449" "7036" "7253"

# Based on dfbetas, we find 3 influential points and delete them
df_dfbetas <- df[-c(1449,7036,7253),]  # 21605    17

# Calculate the R student residual and fitted values based on the data after deleting the influential points
dat.lm.dfbetas <- lm(price~., data=df_dfbetas)
R.stud.res.dfbetas <- rstudent(dat.lm.dfbetas)
y.hat.dfbetas <- fitted(dat.lm.dfbetas)

# Residual plots for comparing these datasets
par(mfrow = c(2, 2), mai=c(0.7,0.7,0.2,0.1)) # mai=c(bottom, left, top, right)
qqnorm(R.stud.res, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals (original)")
qqline(R.stud.res)
plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot (original)")
abline(h=0, lty=1, lwd=3)
qqnorm(R.stud.res.dfbetas, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals (dfbetas)")
qqline(R.stud.res.dfbetas)
plot(y.hat.dfbetas, R.stud.res.dfbetas, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot (dfbetas)")
abline(h=0, lty=1, lwd=3)
par(mfrow = c(1, 1))

summary(dat.lm)
summary(dat.lm.dfbetas)

# By using DFBETAS we only have 3 influential points. 
# After removing these three points the normality probability and residual-by-fitted-value points are 
# almost the same since there are 21605 rows, three outliers does not make much impact on the whole dataset.
# Also the Adjusted R-squared for original model is 0.6947, and the Adjusted R-squared for DFBETAS model is 0.6956
# which does not improve a lot.

############
# COVRATIO #
############

# The values of COVRATIO may be calculated using the function "covratio":
dat.covratio <- covratio(dat.lm)
#dat.covratio

# The associated cutoff value for identifying an influential observation is:
n <- dim(dat)[1]
p <- length(coefficients(dat.lm))
cut.inf4.lo <- 1 - 3*p/n         # 0.9976398
cut.inf4.hi <- 1 + 3*p/n         # 1.00236

# The results from comparing against the cutoff values are displayed as follows:
f <- data.frame(cbind(dat.covratio, (dat.covratio < cut.inf4.lo) | (dat.covratio > cut.inf4.hi)))
names(f) <- c("dat.covratio","cut.inf4.compare")
filter(f,f$cut.inf4.compare == 1)
f2 <- rownames(f[f$cut.inf4.compare==1,]) # There are 1324 influential points found by covratio
df_covratio <- df[-as.numeric(f2),]    # 20284    17

# Calculate R student residual and fitted values on data after deleting these 1323 influential points
dat.lm.covratio <- lm(price~., data=df_covratio)
R.stud.res.covratio <- rstudent(dat.lm.covratio)
y.hat.covratio <- fitted(dat.lm.covratio)

# Residual plots for comparing these datasets
par(mfrow = c(2, 2), mai=c(0.7,0.7,0.2,0.1)) # mai=c(bottom, left, top, right)
qqnorm(R.stud.res, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals (original)")
qqline(R.stud.res)
plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot (original)")
abline(h=0, lty=1, lwd=3)
qqnorm(R.stud.res.covratio, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals (covratio)")
qqline(R.stud.res.covratio)
plot(y.hat.covratio, R.stud.res.covratio, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot (covratio)")
abline(h=0, lty=1, lwd=3)
par(mfrow = c(1, 1))

summary(dat.lm)
summary(dat.lm.covratio)

# By using COVERATO we have 1324 influential points. 
# After removing those points the normality probability and residual-by-fitted-value points are 
# better, the number of outliers reduced a lot.
# Also the Adjusted R-squared for original model is 0.6947, and the Adjusted R-squared for covratio model 
# is 0.7008 which also improves a lot.

# After comparing those four methods, we conclude that DFFITS method give us best result and we will remove
# those 1117 influential points in order to get a better model with higher Adjusted R-squared.

#========================================================#
# Residual Analysis                                      |
#========================================================#

## No transformation ##
par(mfrow = c(2, 2))
# plot normal probability - curvy shape suggests flaw in assumption for linear regression, need tranformation
R.stud.res <- rstudent(fit.2)
qqnorm(R.stud.res, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals (original) ")
qqline(R.stud.res)

# plot the residuals against fitted value - funnel shape, need transformation (try transforming response first)
y.hat <- fitted(fit.2)
plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot (original) ")
abline(h=0, lty=1, lwd=3) 

## Log transformation ##
fit.3 <- lm(log(price)~., data=df) 
summary(fit.3) 
# R2 improved from 0.6949 to 0.7674 and all regressors significant (including condition.2, which was not significant in fit.2)
# plot normal probability - much better 
R.stud.res <- rstudent(fit.3)
qqnorm(R.stud.res, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals")
qqline(R.stud.res)
# plot the residuals gainst fitted value - better random pattern
y.hat <- fitted(fit.3)
plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")
abline(h=0, lty=1, lwd=3) 

#========================================================#
# Box-Cox transformation                                 |
#========================================================#
par(mfrow = c(2, 2))
boxcox(price~., data=df, lambda=seq(from=0, to=1, by=0.01))
# Based on the plot, there is no need to transform the response variable under box-cox. 
# This means that the transformations under box-cox are not the best transformation to address nonnormality and/or nonconstant.

# Double check by constructing the sum of squared residual value list against lambda
# using original price as the response variable.
# The result shows that lambda= 1 is the best, which means there is no need to transform the response variable.  
box.cox.power.trans <- function(lambda, resp) {
  y.dot <- exp(mean(log(resp)))
  if (lambda == 0) {
    resp.trans <- y.dot*log(resp)
  } else {
    resp.trans <- (resp^lambda - 1) / (lambda*y.dot^(lambda-1))
  }
  return(resp.trans)
}
# Find SS.Res for several values of lambda               
lambda.list <- c(-2, -1, -0.5, 0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 1, 2)
n.lambda <- length(lambda.list)
SS.Res.list <- numeric(length=n.lambda)
for (i.lambda in 1:n.lambda) {
  lambda <- lambda.list[i.lambda]
  y.trans <-  box.cox.power.trans(lambda, resp=df$price)
  dat.new <- data.frame(df, y.trans)
  dat.lm.new <- lm(y.trans ~.-price, data=dat.new)
  SS.Res.list[i.lambda] <- anova(dat.lm.new)[17,2]
}
cbind(lambda.list, SS.Res.list)
# Select the best among these values                  
SS.Res.min <- min(SS.Res.list)
i.lambda <- which(SS.Res.list == SS.Res.min)
lambda.hat <- lambda.list[i.lambda]
# Use this value to calculate a confidence interval for lambda                                             |
alpha <- 0.05
chisq.crit <- qchisq(alpha, df=1, lower.tail=FALSE)
SS.Res.crit <- SS.Res.min*exp(chisq.crit/n)
par(mfrow=c(1,1))
plot(lambda.list, SS.Res.list, type="l", lty=1, lwd=3, xlab="lambda", ylab="residual sum-of-square", main = "Selecting a Box-Cox transformation")
abline(h=SS.Res.crit, lty=1, lwd=1)
# Model with lambda = 0 gets the lowest resdsidual sum of square

# Tranform the response variable with lambda = 0
df$y.trans <- box.cox.power.trans(0, df$price)
boxcox.lm <- lm(y.trans~.-price, data=df)
summary(boxcox.lm)
# Since the model performance is not improved after transformation, we decided not to apply
# Box-Cox transformation.
df <- subset(df, select = -c(y.trans))
names(df)
# [1] "price"         "bedrooms"      "bathrooms"     "sqft_living"   "sqft_lot"      "floors"       
# [7] "waterfront"    "view"          "grade"         "yr_built"      "lat"           "long"         
# [13] "sqft_living15" "sqft_lot15"    "isRenovated"   "condition.4"   "condition.5" 

#========================================================#
# Box-Tidwell transformation                             |
#========================================================#

# Since the continuous variables are sqft_living, sqft_lot, sqft_living 15, sqft_lot 15, 
# We will apply polynomial regression to these variables and see if we can achieve better results. 
# Box-Tidwell only accept positive variables.
# If we use log(price), then the lambda would change a lot.
boxTidwell(price ~ sqft_lot, data=df)            # lambda: 0.03463495  
boxTidwell(price ~ sqft_living, data=df)         # lambda: 1.764
boxTidwell(price ~ sqft_living15, data=df)       # lambda: 1.9889
boxTidwell(price ~ sqft_lot15, data=df)          # lambda: 0.007364, maximum iterations exceeded
boxTidwell(log(price) ~ sqft_lot, data=df)       # lambda: 0.24666  
boxTidwell(log(price) ~ sqft_living, data=df)    # lambda: 0.80879
boxTidwell(log(price) ~ sqft_living15, data=df)  # lambda: 0.92808
boxTidwell(log(price) ~ sqft_lot15, data=df)     # lambda: 0.24156

# Based on result from Box-Tidwell transformation with log transformed price, 
# we transform sqft_lot and sqft_lot15 with power of 1/4 and refit the model.
fit.4 <- lm(log(price) ~. + I(sqft_lot^(1/4)) + I(sqft_lot15^(1/4)), data=df) # Refit model with power transformed variables as the additives
fit.5 <- lm(log(price) ~. + I(sqft_lot^(1/4)) + I(sqft_lot15^(1/4)) - sqft_lot15 - sqft_lot, data=df) # Refit model with only power transformed variables
summary(fit.4)  # Adjusted-R2: 0.770
summary(fit.5)  # Adjusted-R2: 0.767
# Since the adjusted R-squared of fit.4 is slightly better than that of fit.5, we choose to perform further analysis
# on fit.4.

par(mfrow = c(2, 2))
# plot the distribution of residuals
R.stud.res <- rstudent(fit.4)
qqnorm(R.stud.res, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals")
qqline(R.stud.res)

# plot the residuals against fitted value
y.hat <- fitted(fit.4)
plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")
abline(h=0, lty=1, lwd=3)

# The distribution of residuals is heavy-tailed and the plot residuals against fitted values still has a double bow pattern
# It seems the power transformation doesn't help improve the model a lot

#========================================================#
# Polynomial Regression                                  |
#========================================================#
# squared sqft_living, squared bedrooms and interactions of sqft_living and bedrooms
df.poly <- df
df.poly$sqft_living2 <- df.poly$sqft_living^2
df.poly$bedrooms2 <- df.poly$bedrooms^2
df.poly$living_bedrooms <- df.poly$sqft_living * df.poly$bedrooms
poly.fit <- lm(log(price) ~., data=df.poly)
summary(poly.fit)

# Both the quadratic terms are siginificant, while the interaction term is not significant.
# We still keep it since it should be kept in high-order polynomial regression
# The Adjusted R-squared does not have improvement.
# We leave the terms for feature selection.

# squared sqft_lot2, squared bedrooms and interactions of sqft_living and bedrooms
df.poly <- df
df.poly$sqft_lot2 <- df.poly$sqft_lot^2
df.poly$bathrooms2 <- df.poly$bathrooms^2
df.poly$lot_bathrooms <- df.poly$sqft_lot * df.poly$bathrooms
poly.fit <- lm(log(price) ~., data=df.poly)
summary(poly.fit)
# sqft_lot 2 is not significant. we will keep only quadratic terms of bathrooms.




#========================================================#
# Feature Elemination                                    |
#========================================================#

# Functions

get.model.str <- function(var.in, resp.name, reg.names) {
  var.in.idx <- which(var.in)
  model.str <- paste(resp.name, "~")
  first.in <- TRUE
  for (iVAR in var.in.idx) {
    if (first.in) {
      model.str <- paste(model.str, reg.names[iVAR])
      first.in <- FALSE
    } else {
      model.str <- paste(model.str, "+", reg.names[iVAR])
    }
  }
  return(model.str)
}

eval.lm <- function(model.str, data.name) {
  lm.call.str <- paste("reg.lm <- lm(", model.str, ", data=", data.name, ")")
  eval(parse(text=lm.call.str))
  return(reg.lm)
}

forward.step <- function(curr.var.in, alpha.in, resp.name, reg.names, data.name) {
  curr.var.out.idx <- which(!curr.var.in)
  enter.idx <- NA
  if (length(curr.var.out.idx) > 0) {
    k <- length(reg.names)
    pval.seq <- rep(x=Inf, times=k)
    for (iVAR in curr.var.out.idx) {
      cand.var.in <- curr.var.in
      cand.var.in[iVAR] <- TRUE
      cand.model.str <- get.model.str(cand.var.in, resp.name, reg.names)
      cand.model.lm <- eval.lm(cand.model.str, data.name)
      iROW <- which(row.names(summary(cand.model.lm)$coefficients) == reg.names[iVAR])
      pval.seq[iVAR] <- summary(cand.model.lm)$coefficients[iROW,4]
    }
    enter.idx <- which.min(pval.seq)
    if (pval.seq[enter.idx] < alpha.in) {
      print(paste("Variable ", reg.names[enter.idx], " enters the model (pval=", sprintf("%6.4f", pval.seq[enter.idx]), ")", sep=""))
    } else {
      print("No variables enter the model")
      enter.idx <- NA
    }
  } else {
    print("No variables available to enter the model")
  }
  return(enter.idx)
}

backward.step <- function(curr.var.in, alpha.out, resp.name, reg.names, data.name) {
  curr.var.in.idx <- which(curr.var.in)
  leave.idx <- NA
  if (length(curr.var.in.idx) > 0) {
    k <- length(reg.names)
    pval.seq <- rep(x=-Inf, times=k)
    curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
    curr.model.lm <- eval.lm(curr.model.str, data.name)
    for (iVAR in curr.var.in.idx) {
      iROW <- which(row.names(summary(curr.model.lm)$coefficients) == reg.names[iVAR])
      pval.seq[iVAR] <- summary(curr.model.lm)$coefficients[iROW,4]
    }
    leave.idx <- which.max(pval.seq)
    if (pval.seq[leave.idx] >= alpha.out) {
      print(paste("Variable ", reg.names[leave.idx], " leaves the model (pval=", sprintf("%6.4f", pval.seq[leave.idx]), ")", sep=""))
    } else {
      print("No variables leave the model")
      leave.idx <- NA
    }
  } else {
    print("No variables available to leave the model")
  }
  return(leave.idx)
}

forward.selection <- function(alpha.in, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=FALSE, times=k)
  stop <- FALSE
  while(!stop) {
    enter.idx <- forward.step(curr.var.in, alpha.in, resp.name, reg.names, data.name)
    if (is.na(enter.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[enter.idx] <- TRUE
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}

backward.elimination <- function(alpha.out, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=TRUE, times=k)
  stop <- FALSE
  while(!stop) {
    leave.idx <- backward.step(curr.var.in, alpha.out, resp.name, reg.names, data.name)
    if (is.na(leave.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[leave.idx] <- FALSE
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}

stepwise.selection <- function(alpha.in, alpha.out, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=FALSE, times=k)
  stop <- FALSE
  while(!stop) {
    enter.idx <- forward.step(curr.var.in, alpha.in, resp.name, reg.names, data.name)
    if (is.na(enter.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[enter.idx] <- TRUE
      leave.idx <- backward.step(curr.var.in, alpha.out, resp.name, reg.names, data.name)
      if (!is.na(leave.idx)) {
        curr.var.in[leave.idx] <- FALSE
        if (leave.idx == enter.idx) {
          stop <- TRUE
        }
      }
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}

# add log.price and quadratic terms
df$log.price <- log(df$price)
df$sqft_living2 <- df$sqft_living^2
df$bedrooms2 <- df$bedrooms^2
df$living_bedroom <- df$sqft_living * df$bedrooms
df$bathrooms2 <- df$bathrooms^2

# remove price
df <- subset(df, select=-c(price))

reg_name <- colnames(df)
# exclude categorical data since they are not accepted in the backward, forward and stepwise selection
reg_name <- reg_name[!(reg_name %in% c("log.price"))]

# backward, forward and stepwise selection
resp.name <- "log.price"
reg.names <- reg_name
data.name <- "df"
alpha.in <- 0.25
alpha.out <- 0.10

# forward elimination
dat.forward <- forward.selection(alpha.in, resp.name, reg.names, data.name)
summary(dat.forward)
# [1] "Final model: log.price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + 
# view + grade + yr_built + lat + long + sqft_living15 + sqft_lot15 + isRenovated + condition.4 + 
# condition.5 + sqft_living2 + bedrooms2"

# Backward elimination
dat.backward <- backward.elimination(alpha.out, resp.name, reg.names, data.name)
# [1] "Final model: log.price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + 
# view + grade + yr_built + lat + long + sqft_living15 + sqft_lot15 + isRenovated + condition.4 + 
# condition.5 + sqft_living2 + bedrooms2"

# Stepwise selection
dat.step <- stepwise.selection(alpha.in, alpha.out, resp.name, reg.names, data.name)
# [1] "Final model: log.price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + 
# view + grade + yr_built + lat + long + sqft_living15 + sqft_lot15 + isRenovated + condition.4 + 
# condition.5 + sqft_living2 + bedrooms2"

# The quadratic term of bathrooms is removed in all three selection methods

final.model<-lm(log.price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + 
                view + grade + yr_built + lat + long + sqft_living15 + sqft_lot15 + isRenovated + condition.4 + 
                condition.5 + sqft_living2 + bedrooms2, data=df)
summary(final.model)
