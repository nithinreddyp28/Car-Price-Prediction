# Module 6 - Group Project

# Loading the libraries
library(ggplot2)
library(tidyr)
library(tidyverse)
library(janitor)
library(psych)
library(VIM)
library(modeltools)
library(corrplot)
options(scipen = 999)

#Installing libraries
# install.packages('rpart')
# install.packages('caret')
# install.packages('rpart.plot')
# install.packages('rattle')
# install.packages('readxl')
# install.packages("randomForest")
# install.packages("glmnet")
# install.packages("Matrix")
# install.packages("Metrics")
#install.packages("gbm")
#install.packages("leaps")
#install.packages("vif")


#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)
library(readxl)
library(randomForest)
library(glmnet)
library(Matrix)
library(Metrics)
library(gbm)
library(leaps)
library(car)
library(e1071)
library(dplyr)


# Reading the dataset
df <- read_csv('AustralianVehiclePrices.csv')
dim(df)
head(df)


# Examining the Descriptive Statistics
describe(df)

# Checking the null values
sum(is.na(df))

# Data Cleaning
df$CylindersinEngine <- gsub(" cyl", "", df$CylindersinEngine)
df$Doors <- gsub(" Doors","",df$Doors)
df$Seats <- gsub(" Seats","",df$Seats)
df$CylindersinEngine <- gsub(" L", "", df$CylindersinEngine)
df$FuelConsumption <- gsub(" L / 100 km","",df$FuelConsumption)
df$Engine <- gsub("4 cyl, ","",df$Engine)
df$Engine <- gsub("8 cyl, ","",df$Engine)
df$Engine <- gsub("6 cyl, ","",df$Engine)
df$Engine <- gsub("3 cyl, ","",df$Engine)
df$Engine <- gsub("10 cyl, ","",df$Engine)
df$Engine <- gsub("12 cyl, ","",df$Engine)
df$Engine <- gsub("5 cyl, ","",df$Engine)
df$Engine <- gsub(" L","",df$Engine)
colnames(df)[which(names(df) == "Car/Suv")] <- "CarType"


# Data Transformation
df$CylindersinEngine <- as.numeric(df$CylindersinEngine)
df$Doors <- as.numeric(df$Doors)
df$Seats <- as.numeric(df$Seats)
df$Kilometres <- as.numeric(df$Kilometres)
df$Engine <- as.numeric(df$Engine)
df$FuelConsumption <- as.numeric(df$FuelConsumption)


df <- separate(df, Location, into = c("City", "Province"), sep = ", ")

summary(df)

# Imputing Price NA's
predictors <- c("Price", "Engine", "CarType", "Doors","Model","Year","UsedOrNew","FuelType")
imputation_data <- df[, c(predictors, 'Price')]

knn_imputed_price <- kNN(imputation_data, 
                         variable = "Price", 
                         k = sqrt(length(df)))
df$Price <- knn_imputed_price$Price
summary(df$Price)


# Imputing FuelConsumption NA's
predictors_fuel <- c("FuelConsumption", "CarType", "Model","Engine","Transmission","FuelType","DriveType")
imputation_fuel <- df[, c(predictors_fuel, 'FuelConsumption')]

knn_imputed_FuelConsumption <- kNN(imputation_fuel, 
                                   variable = "FuelConsumption", 
                                   k = sqrt(length(df)))
df$FuelConsumption <- knn_imputed_FuelConsumption$FuelConsumption
summary(df$FuelConsumption)

# Imputing Kilometres NA's
predictors_km <- c("Kilometres", "CarType", "UsedOrNew")
imputation_km<- df[, c(predictors_km, 'Kilometres')]

knn_imputed_Kilometres <- kNN(imputation_km, 
                              variable = "Kilometres", 
                              k = sqrt(length(df)))
df$Kilometres <- knn_imputed_Kilometres$Kilometres
summary(df$Kilometres)

df <- drop_na(df)
summary(df)

df <- df[df$FuelType != "-",]

# Box Plot for Price
ggplot(df, aes(y = Price)) +
  geom_boxplot() +
  labs(x = "Price", y = "Value", title = "Box Plot for Price")+
  theme_minimal()


#Price outliers##

# Calculate the quartiles and IQR for the price variable
q1 <- quantile(df$Price, 0.25)
q3 <- quantile(df$Price, 0.75)
iqr <- q3 - q1

# Define the lower and upper bounds for outliers
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Identify outliers
outliers <- df$Price[df$Price < lower_bound | df$Price > upper_bound]

# Display details of outliers
outliers

df_filtered <- df[!(df$Price < lower_bound | df$Price > upper_bound), ]


#Checking box plot
ggplot(df_filtered, aes(y = Price)) +
  geom_boxplot() +
  labs(x = "Price", y = "Value", title = "Box Plot for Price")+
  theme_minimal()

# Distribution of Price
ggplot(df_filtered,aes(x=Price))+
  geom_histogram(fill='red',alpha=0.8)+
  theme_classic()+
  labs(title = "Distribution of Price",x='Price',y='Frequency')

# Fuel Type Distribution
df_fuelType <- df_filtered %>% group_by(FuelType) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

ggplot(df_fuelType, aes(x=FuelType,y=count))+
  geom_bar(stat = 'identity',fill='blue')+
  theme_minimal()+
  labs(title = 'Distribution of Fuel Type',x='Fuel Type',y='Number of Vehicles')


# Drive Type Distribution
df_driveType <- df_filtered %>% group_by(DriveType,Transmission) %>% 
  summarise(count = n()) %>% 
  filter(Transmission != '-')

ggplot(df_driveType, aes(x=DriveType,y=count,fill=Transmission))+
  geom_bar(stat = 'identity')+
  theme_minimal()+
  labs(title = 'Distribution of Drive Type',x='Drive Type',y='Number of Vehicles')

# Time Series Plot for Price from year 2000
df_price_year <- df_filtered %>% group_by(Year) %>% 
  summarise(Price = mean(Price)) %>% 
  filter(Year >= 2000)

ggplot(df_price_year,aes(x=Year,y=Price))+
  geom_line(color='red') +
  labs(title = 'Year wise trend of Price',x='Year',y='Price')+
  theme_minimal()


# Box Plot - FuelType and Price
ggplot(df_filtered, aes(x = FuelType, y = Price, fill = FuelType)) +
  geom_boxplot() +
  labs(x = "Category", y = "Value", title = "Box Plot by Category")

#Distribution Province wise
df_province <- df_filtered %>% 
  group_by(Province) %>% 
  summarise(count = n())

ggplot(df_province,aes(x=Province,y=count))+
  geom_bar(stat = 'identity',fill='lightblue')+
  theme_minimal()+
  labs(title = "Distribution of Cars by Province",x='Province',y='Count')

# Correlation Plot
df_corr <- subset(df_filtered, select=c("Price","Doors","Seats","Engine","Kilometres","FuelConsumption","CylindersinEngine"))
M = cor(df_corr)
corrplot(M, method = 'number') # colorful number

# Scatter Plot ( Relation between Price and Kilometres)
ggplot(df_filtered,aes(x=Kilometres,y=Price))+
  geom_point(color='orange')+
  geom_smooth(method='lm',se=FALSE,color='red')+
  theme_minimal()+
  labs(title = "Relation between Price and Kilometres",x='Kilometres',y='Price')

df_filtered$CarType <- as.numeric(as.factor(df_filtered$CarType))
df_filtered$Model <- as.numeric(as.factor(df_filtered$Model))
df_filtered$Brand <- as.numeric(as.factor(df_filtered$Brand))
df_filtered$DriveType <- as.numeric(as.factor(df_filtered$DriveType))
df_filtered$UsedOrNew <- as.numeric(as.factor(df_filtered$UsedOrNew))
df_filtered$Transmission <- as.numeric(as.factor(df_filtered$Transmission))

##Finding best subset

subsets_model <- regsubsets(Price ~ Brand+Year+Model+CarType+
                              DriveType+UsedOrNew+Transmission+Doors+
                              CylindersinEngine+Seats+Kilometres, 
                            data = df_filtered, nbest=3)
summary(subsets_model)

set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(df_filtered), 0.75 * nrow(df_filtered))
train_data <- df_filtered[train_index, ]
test_data <- df_filtered[-train_index, ]

#linear regression model##

lm_model <- lm(Price ~ Brand+Year+Model+
                 DriveType+UsedOrNew+Kilometres+
                 Doors+CylindersinEngine+Seats, data = train_data)

predictions_lm <- predict(lm_model, newdata = test_data)

# Calculate evaluation metrics
MAE_lm <- mean(abs(predictions_lm - test_data$Price))
MAE_lm
MSE_lm <- mean((predictions_lm - test_data$Price)^2)
MSE_lm
RMSE_lm <- sqrt(MSE_lm)
RMSE_lm
RSquared_lm <- summary(lm_model)$r.squared
RSquared_lm


#Rgeression Diagnostics 

plot(lm_model)

#Evaluating multi-collinearity

vif(lm_model) 
sqrt(vif(lm_model)) > 2


#Lasso and Ridge regression#

###Ridge and Lasso###

##As glmnet requires a matrix, converting the data in to matrix
train_x <- model.matrix(Price ~ Brand+Year+Model+
                          DriveType+UsedOrNew+Kilometres+
                          Doors+CylindersinEngine+Seats, data = train_data)[, -1]
test_x <- model.matrix(Price ~ Brand+Year+Model+
                         DriveType+UsedOrNew+Kilometres+
                         Doors+CylindersinEngine+Seats, data = test_data)[,-1]

##Assigning grade to train and test variable
train_y<-train_data$Price
test_y<-test_data$Price


##Finding best lambda values 
set.seed(123)
cv.lasso <- cv.glmnet(train_x, train_y, nfolds=10)
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se

log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

##Fitting models based in Lambda##

#building model on training data using lambda.min

#alpha =1 <we specify that we are running a Lasso model>
#alpha=0 <we specify that we are running Ridge model>

##Lasso Regression##
lasso_train_model_min <- glmnet(train_x, train_y, alpha=1, lambda = cv.lasso$lambda.min)
lasso_train_model_min 

#display coefficients
coef(lasso_train_model_min )

#building model on training data using lambda.1se
lasso_train_model_1se <- glmnet(train_x, train_y, alpha=1, lambda = cv.lasso$lambda.1se)
lasso_train_model_1se 

#display coefficients
coef(lasso_train_model_1se)

# Predicting based on train data using lambda.1se
predit_train_lasso_1se <- predict(lasso_train_model_1se, newx = train_x)
rmse_train_lasso_1se <- rmse(train_y, predit_train_lasso_1se)
rmse_train_lasso_1se

# Predicting based on test data using lambda.1se
predit_test_lasso_1se <- predict(lasso_train_model_1se, newx = test_x)
rmse_test_lasso_1se <- rmse(test_y, predit_test_lasso_1se)
rmse_test_lasso_1se

## Predicting based on train data using lambda.min
predit_train <- predict(lasso_train_model_min ,newx=train_x)
rmse_train <- rmse(train_y,predit_train )
rmse_train 

## Predicting based on test data using lambda.min

predit_test <- predict(lasso_train_model_min ,newx=test_x)
rmse_test <- rmse(test_y,predit_test)
rmse_test

##Ridge Regression###

#building model on training data using lambda.1se

regid_train_model_1se <- glmnet(train_x, train_y, alpha=0, lambda = cv.lasso$lambda.1se)
regid_train_model_1se 

#display coefficients

coef(regid_train_model_1se)

#building model on training data using lambda.min

regid_train_model_min <- glmnet(train_x, train_y, alpha=0, lambda = cv.lasso$lambda.min)
regid_train_model_min 

#display coefficients

coef(regid_train_model_1se)

## Predicting based on train data using lambda.1se
predit_train2 <- predict(regid_train_model_1se ,newx=train_x)
rmse_train2 <- rmse(train_y,predit_train2)
rmse_train2
## Predicting based on test data using lambda.1se
predit_test2 <- predict(regid_train_model_1se ,newx=test_x)
rmse_test2 <- rmse(test_y,predit_test2)
rmse_test2
## Predicting based on train data using lambda.min
predit_train3 <- predict(regid_train_model_min ,newx=train_x)
rmse_train3 <- rmse(train_y,predit_train3)
rmse_train3
## Predicting based on test data using lambda.min
predit_test3 <- predict(regid_train_model_min ,newx=test_x)
rmse_test3 <- rmse(test_y,predit_test3)
rmse_test3

##Decission Tree###

tree_model <- rpart(Price ~ Brand+Year+Model+
                      DriveType+UsedOrNew+Kilometres+
                      Doors+CylindersinEngine+Seats, data = train_data)
rpart.plot(tree_model)
predictions <- predict(tree_model, newdata = test_data)


# Calculate evaluation metrics
MAE <- mean(abs(predictions - test_data$Price)) #Mean Abs Error
MAE
MSE <- mean((predictions - test_data$Price)^2) ##Mean Sqr Error
MSE
RMSE <- sqrt(MSE) #Root Mean Sq Error
RMSE
RSquared <- 1 - sum((test_data$Price - predictions)^2) / sum((test_data$Price - mean(test_data$Price))^2)
RSquared

#Gradient Boosting algorithm 

model <- gbm(Price ~ Brand+Year+Model+
               DriveType+UsedOrNew+Kilometres+
               Doors+CylindersinEngine+Seats, 
             data = train_data, distribution = "gaussian", 
             n.trees = 150, interaction.depth = 4, shrinkage = 0.1)

predictions_gbm <- predict(model, newdata = test_data, n.trees = 150)

MAE_gbm <- mean(abs(predictions_gbm - test_data$Price)) #Mean Abs Error
MAE_gbm
MSE_gbm <- mean((predictions_gbm - test_data$Price)^2) ##Mean Sqr Error
MSE_gbm
RMSE_gbm <- sqrt(MSE_gbm) #Root Mean Sq Error
RMSE_gbm
RSquared_gbm <- 1 - sum((test_data$Price - predictions_gbm)^2) / sum((test_data$Price - mean(test_data$Price))^2)
RSquared_gbm

#Random Forest Regression#
set.seed(100)
rf_model <- randomForest(Price ~ Brand+Year+Model+
                           DriveType+UsedOrNew+Kilometres+
                           Doors+CylindersinEngine+Seats,
                         data = train_data,ntree = 100)
rf_model
# Make predictions on test data
predictions_rf <- predict(rf_model, newdata = test_data, )

MAE_RF <- mean(abs(predictions_rf - test_data$Price))
MAE_RF
MSE_RF <- mean((predictions_rf - test_data$Price)^2)
MSE_RF
RMSE_RF <- sqrt(MSE_RF)
RMSE_RF
RSquared_RF <- 1 - sum((test_data$Price - predictions_rf)^2) / sum((test_data$Price - mean(test_data$Price))^2)
RSquared_RF


# Tuning Random Forest model based on tree depth

set.seed(100)
rf_model_TR <- randomForest(Price ~ Brand+Year+Model+
                           DriveType+UsedOrNew+Kilometres+
                           Doors+CylindersinEngine+Seats,
                         data = train_data, ntree = 250, max_depth = 5)
rf_model_TR
# Make predictions on test data
predictions_rf_TR <- predict(rf_model_TR, newdata = test_data, )

MAE_RF_TR <- mean(abs(predictions_rf_TR - test_data$Price))
MAE_RF_TR
MSE_RF_TR <- mean((predictions_rf_TR - test_data$Price)^2)
MSE_RF_TR
RMSE_RF_TR <- sqrt(MSE_RF_TR)
RMSE_RF_TR
RSquared_RF <- 1 - sum((test_data$Price - predictions_rf)^2) / sum((test_data$Price - mean(test_data$Price))^2)
RSquared_RF

#SVM

set.seed(100)

svm_model <- svm(Price ~ Brand+Year+Model+
                   DriveType+UsedOrNew+Kilometres+
                   Doors+CylindersinEngine+Seats, 
                 data = train_data, 
                 type = 'eps-regression', 
                 kernel = 'radial')
print(svm_model)


#SVM Plot

# Predicting on the training data
train_data$Predicted_Price <- predict(svm_model, train_data)

# Plotting Predicted vs. Actual Price
plot(train_data$Price, train_data$Predicted_Price,
     xlab = "Actual Price", ylab = "Predicted Price",
     main = "Actual vs. Predicted Price (Train Data)",
     col = "blue")
abline(0, 1, col = "red")  # Add a diagonal line for reference

# Plotting Predicted vs. Actual Price for the test data
test_data$Predicted_Price <- predict(svm_model, newdata = test_data)
plot(test_data$Price, test_data$Predicted_Price,
     xlab = "Actual Price", ylab = "Predicted Price",
     main = "Actual vs. Predicted Price (Test Data)",
     col = "green")
abline(0, 1, col = "red")  # Add a diagonal line for reference

# Predict and evaluate the model
predictions_svm <- predict(svm_model, test_data)
MAE_svm <- mean(abs(predictions_svm - test_data$Price))
MAE_svm
MSE_svm <- mean((predictions_svm - test_data$Price)^2)
MSE_svm
RMSE_svm <- sqrt(MSE_svm)
RMSE_svm 
RSquared_svm <- 1 - sum((test_data$Price - predictions_svm)^2) / 
  sum((test_data$Price - mean(test_data$Price))^2)
RSquared_svm

