# 2020-US-Election-Predictive-Modelling
*For STU33002 - Statistical Analysis III in Trinity College Dublin*

This repository contains the R code and comprehensive report for a predictive analysis of the 2020 United States Presidential Election. The project utilizes advanced modeling techniques, specifically Generalized Linear Models (Logistic Regression), to predict voting outcomes based on demographic and socioeconomic variables.

## Project Overview
The primary objective of this project was to construct a robust model capable of predicting the "majority" variable (Trump vs. Biden) across US counties/states. By analyzing a diverse set of demographic variables, the model identifies underlying patterns in voter behavior and electoral strength.

### Key Methodologies:
- **Data Preprocessing:** Handled missing values via mean imputation and row deletion to mitigate bias.
- **Class Imbalance Handling:** Explored stratification, resampling (weighted by population density), and state-level data aggregation.
- **Modeling:** Generalized Linear Model (GLM) / Logistic Regression.
- **Evaluation:** 80/20 train-test split, Accuracy, Precision, Recall, ROC Curves, and AUC scores.

## Optimal Model & Results
The most optimal model was built using **state-levelled aggregated data** with mean imputation and a curated subset of predictors. 
- **Accuracy:** 90%
- **Precision:** 100% (No false positives)
- **Recall:** 75%
- **AUC Score:** 0.952 (95.24% success rate in distinguishing classes)

### Selected Predictors:
The final model utilizes the following 8 predictors to minimize multicollinearity while capturing electorate nuances:
1. `average_age65andolder_pct`
2. `average_lesscollege_pct`
3. `average_lesscollege_whites_pct`
4. `average_rural_pct`
5. `average_foreignborn_pct`
6. `average_white_pct`
7. `average_clf_unemploy_pct`
8. `median_hh_inc`
