# Predicting Car Prices in Australia - 2023 Dataset Analysis

Welcome to the repository! This project focuses on analyzing car price data from the Australian market for the year 2023. By examining a wide range of factors such as brand, model, condition, and features, we aim to identify the key drivers behind car prices in Australia and predict those prices using machine learning techniques.

## 🚗 **Project Overview**

This project provides an in-depth analysis of over 16,000 car listings in Australia, containing information about various car brands, models, types, and features. By performing Exploratory Data Analysis (EDA) and employing multiple machine learning algorithms, we aim to understand how different features contribute to the pricing of cars.

### **Key Data Points:**
- Brand
- Model
- Car type (Car/SUV)
- Condition (New/Used)
- Transmission type
- Engine specifications
- Fuel type and fuel consumption
- Mileage
- Location (City and Province)
- Body type, number of doors, and seats

## 🔍 **Business Question**

**What factors most significantly influence the pricing of cars in the Australian market, and how accurately can we predict these prices using machine learning algorithms?**

We explore various predictive techniques like Linear Regression, Decision Trees, Random Forests, and more to answer this question.

## 📊 **Exploratory Data Analysis (EDA)**

We begin by loading and cleaning the data, including:
- Handling missing values
- Cleaning columns for consistency
- Transforming categorical features into numeric formats

Afterward, we perform various visualizations, including:
- Price distribution histograms
- Fuel type and drive type distribution
- Correlation between price and other numeric features
- Price trends over time

## 🤖 **Machine Learning Models**

We apply different machine learning models to predict car prices, including:
1. **Linear Regression**: Basic yet powerful method to predict car prices based on features like brand, year, and mileage.
2. **Lasso and Ridge Regression**: Regularization techniques to prevent overfitting and select the most relevant features.
3. **Decision Trees**: A visual method to understand how features like brand and year impact car prices.
4. **Random Forests**: An ensemble method to improve prediction accuracy and handle non-linear relationships.
5. **Gradient Boosting**: An advanced technique to boost prediction power by iteratively improving weak learners.

## ⚙️ **Data Preprocessing & Model Evaluation**

Each model is evaluated using various metrics such as:
- **Mean Absolute Error (MAE)**
- **Mean Squared Error (MSE)**
- **Root Mean Squared Error (RMSE)**
- **R-squared (R²)**

The results provide insights into how well the models predict car prices, and we compare these results to determine the best approach for pricing prediction.

## 📈 **Model Performance & Insights**

- **Linear Regression**: Explained 52.72% of the variance in car prices, with an RMSE of around $10,793.
- **Lasso and Ridge Regression**: Both showed similar performance with RMSE values between $10,571 and $10,833.
- **Decision Tree**: Achieved an R² of 0.57, suggesting moderate predictive power with an RMSE of approximately $10,332.
- **Gradient Boosting**: Achieved an impressive R² of 0.80, explaining 80% of the variance in car prices, with an RMSE of around $10,667.
- **Random Forest**: Provided strong performance, with low errors and high variance explanation.

## Dataset link: 
https://www.kaggle.com/datasets/nelgiriyewithana/australian-vehicle-prices/data
