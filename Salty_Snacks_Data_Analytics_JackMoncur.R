
'
Jack Moncur - Data Analyics Final Project
Exploratory Data Work
Understanding the Data
'

### Libraries ###

library(dplyr)
library(ggplot2)
library(class)
library("readxl")
library(lubridate)
library(caret)
library(randomForest)
library(e1071)
library(TTR)
library(forecast)

### Import Data ###
salty <- read_excel('/Users/jackmoncur23/Desktop/Last Sem/Data Analytics/Final Project/Data/salty_sales.xlsx')
head(salty)
View(salty)

pepsi <- read.csv('/Users/jackmoncur23/Desktop/Last Sem/Data Analytics/Final Project/Data/PEP.csv')
mond <- read.csv('/Users/jackmoncur23/Desktop/Last Sem/Data Analytics/Final Project/Data/MDLZ.csv')
kh <- read.csv('/Users/jackmoncur23/Desktop/Last Sem/Data Analytics/Final Project/Data/KHC.csv')
kel <- read.csv('/Users/jackmoncur23/Desktop/Last Sem/Data Analytics/Final Project/Data/K.csv')

pepsi$Date <- format(as.Date(pepsi$Date, format = "%m/%d/%y"), "%Y-%m-%d")
mond$Date <- format(as.Date(mond$Date, format = "%m/%d/%y"), "%Y-%m-%d")
kh$Date <- format(as.Date(kh$Date, format = "%m/%d/%y"), "%Y-%m-%d")
kel$Date <- format(as.Date(kel$Date, format = "%m/%d/%y"), "%Y-%m-%d")

### EDA WORK ###
# Set the seed for reproducibility
set.seed(1)

head(kel$Date)
print(class(kel$Date))

# Joining the Stock Data together
stock_values <- pepsi %>%
  left_join(mond, by = "Date") %>%
  left_join(kh, by = "Date") %>%
  left_join(kel, by = "Date")

salty_test <- data.frame(salty)

distinct_count <- salty %>%
  distinct(week_end_date) %>%
  n_distinct()

print(distinct_count)
#66

#######################  Cleaning & Feature Creation ############################
'Lets get the week_end_date column fixed to match that of the stock data'
freq_weeks <- table(salty_test$week_end_date)
print(freq_weeks)

salty_test$week_end_date <- gsub("WE ", "", salty_test$week_end_date)

salty_test$week_end_date <- as.Date(salty_test$week_end_date, format="%b %d %y")

salty_test$week_end_date <- salty_test$week_end_date - 5

'
Apr 03 21 Apr 10 21 Apr 17 21 Apr 24 21 Aug 07 21 Aug 14 21 Aug 21 21 Aug 28 21 Aug 29 20 Dec 05 20 Dec 12 20 Dec 19 20 Dec 26 20 Feb 06 21 Feb 13 21 
Feb 20 21 Feb 27 21 Jan 02 21 Jan 09 21 Jan 16 21 Jan 23 21 Jan 30 21 Jul 03 21 Jul 10 21 Jul 17 21 Jul 24 21 Jul 31 21 Jun 05 21 Jun 12 21 Jun 19 21 
Jun 26 21 Mar 06 21 Mar 13 21 Mar 20 21 Mar 27 21 May 01 21 May 08 21 May 15 21 May 22 21 May 29 21 Nov 06 21 Nov 07 20 Nov 13 21 Nov 14 20 Nov 20 21 
Nov 21 20 Nov 27 21 Nov 28 20 Oct 02 21 Oct 03 20 Oct 09 21 Oct 10 20 Oct 16 21 Oct 17 20 Oct 23 21 Oct 24 20 Oct 30 21 Oct 31 20 Sep 04 21 Sep 05 20 
Sep 11 21 Sep 12 20 Sep 18 21 Sep 19 20 Sep 25 21 Sep 26 20 
'

#SALTY CLEAN UP WEEKS
salty$week_end_date <- gsub("WE ", "", salty$week_end_date)

salty$week_end_date <- as.Date(salty$week_end_date, format="%b %d %y")

salty$week_end_date <- salty$week_end_date - 5


# Split the dataframe into a train and test set
train_test_split <- createDataPartition(
  data = salty,
  p = 0.8, # 80% train, 20% test
  list = FALSE
)

train_test_split <- sample_frac(salty, 0.8)

train_set <- filter(salty, train_test_split)
test_set <- filter(salty, !train_test_split)

train_set <- salty[train_test_split,]
test_set <- salty[-train_test_split,]

### IMPORTANT
salty$sales_without_promo <- as.numeric(salty$sales_without_promo)
salty$sales_with_promo <- as.numeric(salty$sales_with_promo)
salty$promo_indicator <- ifelse(is.na(salty$sales_with_promo), 0, 1)




# Understanding the features
colnames(salty)
'
 [1] "year"                         "week_num"                     "week_end_date"                "category"                    
 [5] "segment"                      "manufacturer"                 "brand"                        "sales"                       
 [9] "sales_with_promo"             "sales_without_promo"          "sales_display_feature_tpr"    "sales_display_feature_wo_tpr"
[13] "sales_feature_tpr"            "sales_display_only"           "sales_feature_only"           "sales_tpr_only"              
[17] "units"                        "units_with_promo"             "units_without_promo"          "units_display_feature_tpr"   
[21] "units_display_feature_wo_tpr" "units_feature_tpr"            "units_display_only"           "units_feature_only"          
[25] "units_tpr_only"               "pounds"       
'

summary(salty)

# Manufacturers - Total number of brands sold by each manufacturer
freq <- table(salty$manufacturer)
print(freq)

'
TOTAL LIST OF ALL MANUFACTURERS AND NUMBER OF PRODUCTS
AO MNFR - 1177
BIMBO CANADA - 103
BORDEN NEW VENTURES - 123
CALBEE AMERICA - 335
CAMPBELL - 319
CAPE COD POTATO CHIP - 65
CL - 654
COMPLETE CHILI KTCHN - 131
CONAGRA - 271
CONFISERIES EMANUELL - 65
CORNFIELDS INC - 114
COVERED BRIDGE - 134
FOODSHOULD TASTEGOOD - 196
FRITO LAY 2140
GENERAL MILLS - 107
HAIN CELESTIAL - 263
HERSHEY - 263
KELLOGG - 156
KETTLE FOODS - 134
KRAFT HEINZ - 205
LES CRSTLLES YUM YUM - 263
M C SNACK - 1
MEDITERRANEAN BAKERY - 28
MONDELEZ - 166
MRS PALMERS PANTRY - 1
N Y STYLE BAGEL CHIP - 66
NALLEYS - 66
NEAL BROTHERS - 269
OLD DUTCH FOODS - 1198
P&G - 1
PENNEX GOOD HEALTH - 164
POPCHIPS INC - 65
POPCORN INDIANA - 21
QUE PASA MEXICAN FDS - 196
SHULTZ FOOD - 65
SNACK FACTORY - 65
SNYDERS OF HANOVER - 66
SPECTRUM FOODS - 66
TWIGZ - 65
W T HAWKINS - 131
'

total_freq <- length(unique(salty$manufacturer))
cat("Total Frequency (Total Unique Values):", total_freq, "\n")
# Total is 40

years <- table(salty$year)
print(years)
#Years 2020 and 2021 - Maybe explore how sales changed from pandemic 2020 to more open 2021


sales_total <- sum(unique(salty$sales))
print(sales_total)

ggplot(salty, aes(x = manufacturer, y = sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Sales by Brand", x = "Brand", y = "Sales")

'
Percentage of sales w/ promo and without
Pounds of sales correlate with sales? units?

PREDICT further sales by using promotion
PREDICT future stock perfromance based on seasonal sales
'


### Feature Engineering ###


'Lets create some categorical features'


#Growth Rate
salty$promo_sale_rate <- (salty$sales_with_promo - salty$sales_without_promo) / salty$sales_without_promo
salty$promo_sale_rate <- replace(salty$promo_sale_rate, is.na(salty$promo_sale_rate), 0)


#Price Per Unit
salty$unit_price <- salty$sales / salty$units





################################################################################
#
## Model Creation
#
################################################################################


### Logistic Regression ###
traintestsetup <- sample(nrow(salty), 0.8 * nrow(salty))
salty_train <- salty[traintestsetup, ]
salty_test <- salty[-traintestsetup, ]

#Select Correct Cols
salty_train <- salty_train %>%
  select(sales, promo_indicator, promo_sale_rate, units)

salty_test <- salty_test %>%
  select(sales, promo_indicator, promo_sale_rate, units)


lr_model <- glm(promo_indicator ~ promo_sale_rate, data = salty_train, family = binomial())

predicitons <- predict(lr_model, newdata = salty_test, type = "response")
confusion_matrix <- table(salty_test$promo_indicator, predicitons)
accuracy <- mean(diag(confusion_matrix))

print(confusion_matrix)
print(accuracy)
print(predicitons)

prediction_data <- data.frame(
  actual = salty$promo_indicator,
  predicted = predicitons
)


ggplot(predicitons, aes(x = salty$promo_indicator))


################### Lays VS Pringles affect on Stock ####################################
# Using Linear Regression to measure the effect of sales on their respective stock price



#Lays
value_counts <- table(salty$brand)
lays <- value_counts["LAYS"]
print(lays)
#130

lays_df <- salty[salty$brand == "LAYS", ]
lays_df <- lays_df[lays_df$segment == "POTATO", ]
lays_df <- lays_df %>%
  select(week_end_date, sales)
lays_mean <- mean(lays_df$sales)
lays_new <- c("2021-11-22", lays_mean)
lays_df <- rbind(lays_df, lays_new)

colnames(pepsi)[colnames(pepsi) == "Date"] <- "week_end_date"
pepsi <- pepsi %>%
  select(week_end_date, Close)

lays_merge <- merge(lays_df, pepsi, by = "week_end_date")
lays_merge$sales <- as.numeric(lays_merge$sales)

correlations_lays <- cor(lays_merge$sales, lays_merge$Close)
print(correlations_lays)

lays_merge <- lays_merge %>%
  mutate(lagged_sales_1 = lag(sales, order_by = week_end_date),
         lagged_sales_2 = lag(sales, order_by = week_end_date, 2))

lays_model <- lm(Close ~ sales + lagged_sales_1 + lagged_sales_2, data = lays_merge)

summary(lays_model)


#Pringles
pringles <- value_counts["PRINGLES"]
print(pringles)

pringles_df <- salty[salty$brand == "PRINGLES", ]
pringles_df <- pringles_df[pringles_df$segment == "POTATO", ]
pringles_df <- pringles_df %>%
  select(week_end_date, sales)
pringles_mean <- mean(pringles_df$sales)
pringles_new <- c("2021-11-22", pringles_mean)
pringles_df <- rbind(pringles_df, pringles_new)

colnames(kel)[colnames(kel) == "Date"] <- "week_end_date"
kel <- kel %>%
  select(week_end_date, Close)

pringles_merge <- merge(pringles_df, kel, by = "week_end_date")
pringles_merge$sales <- as.numeric(pringles_merge$sales)

correlations_pringles <- cor(pringles_merge$sales, pringles_merge$Close)
print(correlations_pringles)

pringles_merge <- pringles_merge %>%
  mutate(lagged_sales_1 = lag(sales, order_by = week_end_date),
         lagged_sales_2 = lag(sales, order_by = week_end_date, 2))

pringles_model <- lm(Close ~ sales + lagged_sales_1 + lagged_sales_2, data = pringles_merge)

summary(pringles_model)



############################# Random Forest #####################################
# Using Random Forest to Predict Sales

right <- data.frame(salty)
right <- right %>%
  select(sales, units, pounds, promo_indicator, promo_sale_rate, unit_price)
right$brand <- as.factor(right$brand)
head(right)

indices <- sample(1:nrow(right), 0.8 * nrow(right))
train_rf <- right[indices, ]
test_rf <- right[-indices, ]

rf_salty <- randomForest(sales ~ ., data = train_rf, ntree = 100)
rf_pred <- predict(rf_salty, newdata = test_rf)

rf_mse <- mean((test_rf$sales - rf_pred) ^ 2)
r_squared_rf <- 1 - rf_mse / var(test_rf$sales)

print(paste("MSE:", rf_mse))
print(paste("R-Squared:", r_squared_rf))

resi <- test_rf$sales - rf_pred

#Predicted vs Actual Plot
plot(test_rf$sales, rf_pred,
     main = "Predicted Vs Actual",
     xlab = "Actual Sales",
     ylab = "Predicted Sales",
     pch = 16, col = "green"
)
abline(a=0, b=1, col = "red", lty =2)

# Residuals Plot
plot(rf_pred, resi,
     main = "Residual Plot",
     xlab = "Predictes Sales",
     ylab = "Residuals",
     pch = 16, col = "blue"
)
abline(h=0, col = "red", lty = 2)

lines(smooth.spline(rf_pred, resi), col = "orange")

####################################################################################
#
### RandomForest - Predicting unit price based on sales, units, and pounds
#
####################################################################################

# Outlier Identification & Elimination
summary(right$sales)

Q1 <- quantile(right$sales, 0.25)
Q3 <- quantile(right$sales, 0.75)
IQR_value <- Q3 - Q1

thresh <- 1.5

outliers <- right$sales < (Q1 - thresh * IQR_value) | right$sales > (Q3 + thresh * IQR_value)

which(outliers)

right_tight <- right[!outliers, ]

summary(right_tight)


# Model Creation 
predictor_variables <- c("sales", "units", "pounds")

rf_right <- randomForest(unit_price ~ sales + units + pounds, data = right_tight, ntree = 100)

print(rf_right)

predictor <- predict(rf_right, newdata = right_tight)


# Validation
mse_rt <- mean((right_tight$unit_price - predictor) ^ 2)
print(paste("MSE: ", mse_rt))
r2_rt <- 1 - sum((right_tight$unit_price - predictor) ^ 2) / sum((right_tight$unit_price - mean(right_tight$unit_price)) ^ 2)
print(paste("R2: ", r2_rt))


# Scatter Plot
tightplots <- data.frame(Observed = right_tight$unit_price, Predicted = predictor)

ggplot(tightplots, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  
  labs(title = "Observed vs Predicted Unit Prices",
       x = "Observed Unit Price",
       y = "Predicted Unit Price") +
  theme_minimal()

###################### SVM to Predict Units ####################################
# Another attempt with SVM

svm_df <- data.frame(salty)
svm_df <- svm_df %>%
  select(brand, sales, units, pounds, promo_indicator, promo_sale_rate, unit_price)
svm_df$brand <- as.factor(svm_df$brand)
svm_df$units <- as.numeric(svm_df$units)
head(svm_df)

N <- 2
top_brands <- names(tail(sort(tapply(svm_df$units, svm_df$brand, sum)), N))

svm_filt <- svm_df[svm_df$brand %in% top_brands, ]


indices_svm <- sample(1:nrow(svm_filt), 0.8 * nrow(svm_filt))
train_svm <- svm_df[indices_svm, ]
test_svm <- svm_df[-indices_svm, ]

#Feature Scaling
scaled_train_svm <- scale(train_svm[-1])
scaled_test_svm <- scale(test_svm[-1])

svm_model <- svm(units ~ ., data = train_svm, kernel = "linear", cost = 1, scale = FALSE)
pred_svm <- predict(svm_model, newdata = test_svm)

summary(svm_model)
plot(svm_model, train_svm)





svm_mse <- mean((test_svm$units - pred_svm) ^ 2)
svm_r2 <- 1 - svm_mse / var(test_svm$units)

print(paste("MSE:", svm_mse))
print(paste("R-Squared:", svm_r2))

#Plot
plot_data <- data.frame(
  Actual = test_svm$units,
  Predicted = pred_svm
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "SVM Regression: Predicted vs Actual Units",
       x = "Actual Units",
       y = "Predicted Units") +
  theme_minimal()



################################################################################
# Another SVM Attempt
'
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y
# We begin by checking whether the classes are linearly separable.
plot(x, col=(3-y))
# They are not. Next, we fit the support vector classifier.
# Note that in order for the svm() function to perform classification
# we must encode the response as a factor variable.
# We now create a data frame with the response coded as a factor.
dat <- data.frame(x = x,y = as.factor(y))
svmfit <- svm(y ~., data=dat, kernel="linear", cost=10,scale=FALSE)
# The argument scale=FALSE tells the svm() function not to scale each feature to
# have mean zero or standard deviation one;
# depending on the application, one might prefer to use scale=TRUE.
# We can now plot the support vector classifier obtained:
plot(svmfit , dat)
# Note that the two arguments to the plot.svm() function are the output of the call to svm(), #as well as the data used in the call to svm().
# The region of feature space that will be assigned to the −1 class is shown in light blue, # and the region that will be assigned to the +1 class is shown in purple.
'











################################################################################
# MAs,  

pepsi <- pepsi %>%
  select(Date, Close)
pepsi$Date <- as.Date(pepsi$Date)


frito <- salty[salty$manufacturer == "FRITO LAY", ]
frito <- frito %>%
  select(week_end_date, sales)

frito$week_end_date <- as.Date(frito$week_end_date)
frito_summed <- frito %>%
  group_by(week_end_date) %>%
  summarise(sum_sales = sum(sales))

frito_summed <- frito_summed %>%
  rename(Date = date)

pep_frito <- inner_join(pepsi, frito_summed, by = "Date")

head(pep_frito)
pep_frito <- pep_frito[-nrow(pep_frito), ]

### LIKETHIS ###
plot(pep_frito$Date, pep_frito$Close, type = "l", col = "blue", xlab = "Date", ylab = "Closing Price", main = "PepsiCo Performance")
lines(pep_frito$Date, pep_frito$sum_sales/1e6, col = "red")
legend("topright", legend = c("Closing Price"), col = c("blue"), lty = 1)


### GGPLOT ###
ggplot(pep_frito, aes(x = Date)) +
  geom_line(aes(y = sum_sales/1e6, color = "Total Sales (in millions)"), size = 1) +
  geom_line(aes(y = rolling_avg_sales/1e6, color = "Rolling Avg Sales (in millions)"), size = 1, linetype = "dashed") +
  labs(title = "Total Sales and Rolling Avg Sales Over Time", x = "Date", y = "Value (in millions)") +
  scale_color_manual(values = c("Closing Price" = "blue", "Total Sales (in millions)" = "red", "Rolling Avg Sales (in millions)" = "green")) +
  theme_minimal()


corel <- cor(pep_frito$Close, pep_frito$sum_sales)
print(corel)
summary(pep_frito$Close)
summary(pep_frito$sum_sales)

pep_frito$rolling_avg_sales <- SMA(pep_frito$sum_sales, n = 3)

lr <- lm(sum_sales ~ Close + rolling_avg_sales, data = pep_frito)
summary(lr)

lr1 <- lm(sum_sales ~ rolling_avg_sales, data = pep_frito)
summary(lr1)



future_dates <- data.frame(Date = seq(from = max(pep_frito$Date) + 1, by = "weeks", length.out = n)) #Makes a DF of lots of future weeks
future_rolling_avg_values <- predict(lr1, newdata = future_dates)

forecast_values <- forecast(lr1, newdata = data.frame(rolling_avg_sales = future_rolling_avg_values))


summary(pep_frito$Close)
plot(pep_frito$Close)
tsdata <- ts(pep_frito$Close, frequency = 1)
ddata <- decompose(tsdata, "multiplicative")
plot(ddata)


plot(pep_frito$Close)
abline(reg = lm(pep_frito$Close ~ time(pep_frito$Close)))

boxplot(pep_frito$Close, xlab = "Date", ylab = "Close Value", main = "$PEP Close Boxplot")

arima <- auto.arima(pep_frito$Close)
arima

plot.ts(arima$residuals)

myforecast <- forecast(arima, level = c(95), h = 10 *12)
plot(myforecast)

dorito <- salty[salty$brand == "DORITOS", ]
dorito <- dorito[dorito$segment == "FTC", ]

plot(dorito$sales)
tsdata1 <- ts(dorito$sales, frequency = 7)
dddata <- decompose(tsdata1, "multiplicative")
plot(dddata)

plot(dorito$sales)
abline
