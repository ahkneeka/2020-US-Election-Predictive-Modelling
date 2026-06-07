#Statistical Analysis III - Final Project - Anica Azad (21363939)
setwd("C:/Users/anica/Documents/Stat III/Stat3 Final Project") # Set working directory to location of the original dataset, mine is my project file
load("data_election_2020.RData")
ls()#After loading in the data_election_2020 dataset, we must check if it's properly loaded

#Part One: Data Handling
any(is.na(data_election_2020)) #Checking for missing values in the dataset
# Scanning over the dataset briefly reveals that the missing values take on values of "0" already 
#but for our analysis this is not ideal
colSums(is.na(data_election_2020))#shows us what columns have missing values, majority of them are the numerical predictors

#In order to progress into prediction of the majority variables we must handle the missing values accordingly,
#getting rid of them is one step but we might do mean imputation. 
#Mean imputation essentially substitutes the missing values with the calculated mean for each respective column

# List of variables for mean imputation 
variables_to_impute <- c(
  
  "demsen16", "repsen16", "othersen16", "demhouse16", "rephouse16", "otherhouse16",
  "total_population", "cvap", "white_pct", "black_pct", "hispanic_pct", "nonwhite_pct",
  "foreignborn_pct", "female_pct", "age29andunder_pct", "age65andolder_pct",
  "median_hh_inc", "clf_unemploy_pct", "lesshs_pct", "lesscollege_pct",
  "lesshs_whites_pct", "lesscollege_whites_pct", "rural_pct"
)

# Mean imputation function
mean_impute <- function(x) {
  mean_value <- mean(x, na.rm = TRUE)
  x[is.na(x)] <- mean_value
  return(x)
}

# Apply mean imputation to selected variables
data_election_2020[variables_to_impute] <- lapply(data_election_2020[variables_to_impute], mean_impute)

#De-comment this to do row-deletion
#data_election_2020 <- data_election_2020[complete.cases(data_election_2020[variables_to_impute]), ]

#Part Two: Data Exploration

#2016 Election Results
total_votes <- sum(data_election_2020$trump16, data_election_2020$clinton16, data_election_2020$otherpres16)
proportion_trump <- sum(data_election_2020$trump16) / total_votes
proportion_clinton <- sum(data_election_2020$clinton16) / total_votes
proportion_other <- sum(data_election_2020$otherpres16) / total_votes
cat("Proportion of votes for Trump:", proportion_trump, "\n")
cat("Proportion of votes for Clinton:", proportion_clinton, "\n")
cat("Proportion of other votes:", proportion_other, "\n")

#Looking at Senate votes from 2016
senate_votes <- data.frame()
senate_votes <- data.frame(
  Party = c("Democratic", "Republican", "Other"),
  Votes = c(sum(data_election_2020$demsen16), sum(data_election_2020$repsen16), sum(data_election_2020$othersen16))
)
senate_votes$TotalVotesScaled <- senate_votes$Votes / 10000
barplot(senate_votes$TotalVotesScaled, names.arg = senate_votes$Party, col = c("orangered4", "royalblue4", "papayawhip"),
        main = "Total Votes for Senate Parties in 2016 Presidential Elections",
        xlab = "Party",
        ylab = "Total Votes")


#Looking at House votes from 2016
house_votes <- data.frame()
house_votes <- data.frame(
  Party = c("Democratic", "Republican", "Other"),
  Votes = c(sum(data_election_2020$demhouse16), sum(data_election_2020$rephouse16), sum(data_election_2020$otherhouse16))
)
house_votes$TotalVotesScaled <- house_votes$Votes / 10000
barplot(house_votes$TotalVotesScaled, names.arg = house_votes$Party, col = c("orangered4", "royalblue4", "papayawhip"),
        main = "Total Votes for House Parties in 2016 Presidential Elections",
        xlab = "Party",
        ylab = "Total Votes")


#2012 Election Results
total_votes_2012 <- sum(data_election_2020$obama12, data_election_2020$romney12, data_election_2020$otherpres12)
proportion_obama <- sum(data_election_2020$obama12) / total_votes_2012
proportion_romney <- sum(data_election_2020$romney12) / total_votes_2012
proportion_other_2012 <- sum(data_election_2020$otherpres12) / total_votes_2012
cat("Proportion of votes for Obama (2012):", proportion_obama, "\n")
cat("Proportion of votes for Romney (2012):", proportion_romney, "\n")
cat("Proportion of other votes (2012):", proportion_other_2012, "\n")

# Calculate how much Obama won Romney by, not taking into consideration votes for other candidates of this year
obama_win <- sum(data_election_2020$obama12) - sum(data_election_2020$romney12) 
print(obama_win)
#Answer = 4,639,044

#Conclusion on brief election exploration: Democratic Candidates in the lead for past two elections! 
#Furthermore in 2016, Republicans were favoured in the Senate and Democratics were more favoured as House Represntatives by a narrow margin to Republicans. 
#However, despite winning popular vote in 2016, Clinton failed to succeed in the election by electoral votes.

#Analysing the majority variable:
proportion_biden <- sum(data_election_2020$majority == "Biden") / sum(data_election_2020$majority %in% c("Biden", "Trump"))
proportion_trump <- sum(data_election_2020$majority == "Trump") / sum(data_election_2020$majority %in% c("Biden", "Trump"))
cat("Proportion of votes for Biden:", proportion_biden, "\n")
cat("Proportion of votes for Trump:", proportion_trump, "\n")
table(data_election_2020$majority)
# Trump shows to have an exceptionally high allocation of votes on a per county basis. 


#Attempts at data alteration to counteract class imbalance identified through the distribution of outcomes in "majority"

# 1) Stratifying the data by county and state
library(dplyr)
data_stratified <- data_election_2020 %>%
  group_by(county,state)

#2) Resampling the data such that the minority class in "majority" has an equitable standing
library(ROSE)
#calculating counts for voting outcomes, calculating the sum of population who voted for each candidate
biddy <- sum(data_election_2020$majority == "Biden") 
total_population_biden <-sum(data_election_2020$total_population[biddy])
trummy <-sum(data_election_2020$majority == "Trump")
total_population_trump <- sum(data_election_2020$total_population[trummy])

#creating weights from counts and population
re_proportion_biden <- total_population_biden/ (total_population_biden + total_population_trump )
re_proportion_trump <- total_population_trump/ ( total_population_trump +total_population_biden )
p_trump <- re_proportion_trump
p_biden <- re_proportion_biden

#using proportion to reweight the data such that it takes into account a more level playing field
formula_resample <- as.formula("majority ~ .")
set.seed(220307)
data_resampled <- ROSE(formula_resample, data = data_election_2020, p = c(p_biden))$data

#3) State levelled Data: consolidating each of the counties and its respective data back into its state and thus subsetting the data
#For this to reproduce the exact outcomes as me, this method of altering the data is ran separate to the code above after loading in "data_election_2020" , and missing values are handled after the 
#new dataframe "state_level_data" is created . It is possible to not do this but this is how the results outlined in my report were achieved. 

#Clear console, load in original dataset and then load this method separate to above
library(dplyr)
library(tidyr)

# Group by 'state' and 'majority', then count the occurrences of each combination
state_majority_counts <- data_election_2020 %>%
  group_by(state, majority) %>%
  summarise(count = n()) %>%
  ungroup()

# Find the majority candidate for each state based on the count of counties
state_majority_winner <- state_majority_counts %>%
  group_by(state) %>%
  arrange(desc(count)) %>%
  slice(1) %>%
  select(state, majority = majority)

# Group by 'state' and summarize the data
state_level_data <- data_election_2020 %>%
  group_by(state) %>%
  summarise(
    total_trump16 = sum(trump16),
    total_clinton16 = sum(clinton16),
    total_otherpres16 = sum(otherpres16),
    total_romney12 = sum(romney12),
    total_obama12 = sum(obama12),
    total_otherpres12 = sum(otherpres12),
    total_demsen16 = sum(demsen16),
    total_repsen16 = sum(repsen16),
    total_othersen16 = sum(othersen16),
    total_demhouse16 = sum(demhouse16),
    total_rephouse16 = sum(rephouse16),
    total_otherhouse16 = sum(otherhouse16),
    total_population = mean(total_population),  
    total_cvap = mean(cvap),                      
    average_white_pct = mean(white_pct),
    average_black_pct = mean(black_pct),
    average_hispanic_pct = mean(hispanic_pct),
    average_nonwhite_pct = mean(nonwhite_pct),
    average_foreignborn_pct = mean(foreignborn_pct),
    average_female_pct = mean(female_pct),
    average_age29andunder_pct = mean(age29andunder_pct),
    average_age65andolder_pct = mean(age65andolder_pct),
    median_hh_inc = mean(median_hh_inc),
    average_clf_unemploy_pct = mean(clf_unemploy_pct),
    average_lesshs_pct = mean(lesshs_pct),
    average_lesscollege_pct = mean(lesscollege_pct),
    average_lesshs_whites_pct = mean(lesshs_whites_pct),
    average_lesscollege_whites_pct = mean(lesscollege_whites_pct),
    average_rural_pct = mean(rural_pct)
  )

# Merge the majority_winner information with state_level_data
state_level_data <- state_level_data %>%
  left_join(state_majority_winner, by = "state")

#Handling missing values once again, exact same as above, except since our optimal model uses mean imputation we will only use that here:

variables_to_impute <- c("total_demsen16", "total_repsen16", "total_othersen16", "total_demhouse16", "total_rephouse16", "total_otherhouse16",
  "total_population",  "average_white_pct", "average_black_pct", "average_hispanic_pct", "average_nonwhite_pct",
  "average_foreignborn_pct", "average_female_pct", "average_age29andunder_pct", "average_age65andolder_pct",
  "median_hh_inc", "average_clf_unemploy_pct", "average_lesshs_pct", "average_lesscollege_pct",
  "average_lesshs_whites_pct", "average_lesscollege_whites_pct", "average_rural_pct")

mean_impute <- function(x) {
  mean_value <- mean(x, na.rm = TRUE)
  x[is.na(x)] <- mean_value
  return(x)
}

state_level_data[variables_to_impute] <- lapply(state_level_data[variables_to_impute], mean_impute)


#Moving onto correlation analysis to identify strong contenders for predictors, using state_level_data
library(corrplot)
# Create a correlation matrix for the selected variables
correlation_matrix <- print(cor(state_level_data[,c(-1,-31)]))
corrplot(correlation_matrix, method = "color", tl.srt=45, addCoef.col = "black", diag=FALSE)
print(correlation_matrix) #In hindsight the high number of variables makes the corrplot difficult to interpret


#Part Three: GLM construction and analysis

#The report outlines my efforts in creating models for all three methods of data alteration and using a different technique of data handling each time. For the sake of clarity and concisesness,
#the depiction of all those models will be omitted here. Instead using state_level_data, I will outline the steps taken for each of the models made. However, those models can be validated with the same
#code presented here by switching the appropriate dataframe and by performing the respective data handling technique and by using a threshold of 0.5. Duly noted, where in Figure 3, "less variables" are indicated for a model, it is implied that
#variables with insignificant log-odds were removed from the selected list of predictors for a particular model. 

#Iteration Zero: Using all the numerical variables as predictors

# Selected predictors identified from correlation analysis
selected_variables <- c(
  "total_trump16", "total_clinton16","total_otherpres16", "total_romney12", "total_obama12",
  "total_otherpres12", "total_demsen16", "total_repsen16","total_othersen16", "total_demhouse16", "total_rephouse16",
  "total_otherhouse16", "total_population","average_white_pct", "average_black_pct", "average_hispanic_pct",
  "average_nonwhite_pct", "average_foreignborn_pct", "average_female_pct","average_age29andunder_pct", "average_age65andolder_pct", "median_hh_inc",
  "average_clf_unemploy_pct", "average_lesshs_pct", "average_lesscollege_pct","average_lesshs_whites_pct", "average_lesscollege_whites_pct", "average_rural_pct"
)

# Convert "majority" into a binary outcome variable: 1 if Biden won, 0 otherwise
state_level_data$Biden_Win <- as.factor(ifelse(state_level_data$majority == "Biden", 1, 0))
# Subset the data with selected predictor variables
subset_data <- state_level_data[, c(selected_variables, "Biden_Win")]

# Split the data into training and test sets (8/20 ratio)
#With how small the dataset has become now, the test set makes predictions on only 10 observations, I do acknowledge that this does skew model accuracy and that a larger test set could be beneficial.
set.seed(21)  # for reproducibility
indices <- sample(1:nrow(subset_data), 0.8 * nrow(subset_data))
train_data <- subset_data[indices, ]
test_data <- subset_data[-indices, ]

# Fit a generalized linear model (GLM) on the stratified data
model_zero <- glm(Biden_Win ~ ., data = train_data, family = binomial)

# Make predictions on the test set: These predictions outline the predicted probabilities for each observation in the test
#Essentially, how likely is the outcome in occurring 
predictions_zero <- predict(model_zero, newdata = test_data, type = "response")
# Store predicted probabilities directly
predicted_probabilities_zero <- predictions_zero

# Visualizing the ROC curve and calculating the AUC
library(pROC)
roc_curve_zero <- roc(test_data$Biden_Win, predicted_probabilities_zero)
plot(roc_curve_zero, main = "ROC Curve | Model Zero", col = "lightpink", lwd = 3)
auc_score <- auc(roc_curve_zero)
cat("AUC-ROC:", auc_score, "\n")

# Convert predicted probabilities to binary outcomes based on a threshold 
threshold_zero<- 0.7
predicted_outcome_zero <- ifelse(predicted_probabilities_zero > threshold_zero, 1, 0)

#Create a confusion matrix
confusion_matrix_zero <- table(predicted_outcome_zero, test_data$Biden_Win)
print("Confusion Matrix:")
print(confusion_matrix_zero)

# Calculate accuracy, precision, recall, and F1 score for the stratified model based on the true positives, false negatives etc. found in the confusion matrix
accuracy_zero <- sum(diag(confusion_matrix_zero)) / sum(confusion_matrix_zero)
precision_zero <- confusion_matrix_zero[2, 2] / sum(confusion_matrix_zero[, 2])
recall_zero <- confusion_matrix_zero[2, 2] / sum(confusion_matrix_zero[2, ])
f1_score_zero <- 2 * (precision_zero * recall_zero) / (precision_zero + recall_zero)

cat("Accuracy :", accuracy_zero, "\n")
cat("Precision :", precision_zero, "\n")
cat("Recall :", recall_zero, "\n")
cat("F1 Score :", f1_score_zero , "\n")

# Extract coefficients from the logistic model 
coefficients_zero <- coef(model_zero)
# Create a data frame with predictors and their coefficients
coefficients_df <- data.frame(predictor = names(coefficients_zero), coefficient = coefficients_zero)
# Calculate log odds for each predictor
coefficients_df$log_odds <- coefficients_df$coefficient

#Formatting and printing results
num_digits <- 4
coefficients_df$coefficient_formatted <- sprintf("%.*f", num_digits, coefficients_df$coefficient)
coefficients_df$log_odds_formatted <- sprintf("%.*f", num_digits, coefficients_df$log_odds)
print(coefficients_df[, c("predictor", "coefficient_formatted", "log_odds_formatted")])
#summary(model_zero) #Analysing GLM output


#Iteration 1: Using curated predictors assessed from correlation analysis
#(Code is the exact same as above, with the exception of different predictors, therefore not commented)

selected_predictors<- c(
  "average_age65andolder_pct", "average_lesscollege_pct", 
  "average_lesscollege_whites_pct", "average_rural_pct","median_hh_inc",
  "average_foreignborn_pct", "average_white_pct","average_clf_unemploy_pct"
)

state_level_data$Biden_Win <- as.factor(ifelse(state_level_data$majority == "Biden", 1, 0))
subset_data <- state_level_data[, c(selected_predictors, "Biden_Win")]

set.seed(21)
index_train <- sample(1:nrow(subset_data), 0.8 * nrow(subset_data))
index_test <- setdiff(1:nrow(subset_data), index_train)
train_data <- subset_data[index_train, ]
test_data <- subset_data[index_test, ]

logistic_model_one <- glm(Biden_Win ~ ., data = train_data, family = binomial)

predictions <- predict(logistic_model_one, newdata = test_data, type = "response")
predicted_probabilities <- predictions

library(pROC)
roc_curve <- roc(test_data$Biden_Win, predicted_probabilities)
plot(roc_curve, main = "ROC Curve | Model One", col = "red", lwd = 3)
auc_score <- auc(roc_curve)
cat("AUC-ROC:", auc_score, "\n")

threshold <- 0.7
predicted_outcome <- ifelse(predicted_probabilities > threshold, 1, 0)

confusion_matrix <- table(predicted_outcome, test_data$Biden_Win)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

coefficients_one <- coef(logistic_model_one)
coefficients_df2 <- data.frame(predictor = names(coefficients_one), coefficient = coefficients_one)
coefficients_df2$log_odds <- coefficients_df2$coefficient
num_digits <- 4
coefficients_df2$coefficient_formatted <- sprintf("%.*f", num_digits, coefficients_df2$coefficient)
coefficients_df2$log_odds_formatted <- sprintf("%.*f", num_digits, coefficients_df2$log_odds)
print(coefficients_df2[, c("predictor", "coefficient_formatted", "log_odds_formatted")])
#summary(logistic_model_one)

#Iteration 2 : Removing "median_hh_inc" to potentially refine the model.

refined_predictors<- c(
  "average_age65andolder_pct", "average_lesscollege_pct", 
  "average_lesscollege_whites_pct", "average_rural_pct",
  "average_foreignborn_pct", "average_white_pct","average_clf_unemploy_pct"
)

state_level_data$Biden_Win <- as.factor(ifelse(state_level_data$majority == "Biden", 1, 0))
subset_data <- state_level_data[, c(refined_predictors, "Biden_Win")]

set.seed(21)
index_train <- sample(1:nrow(subset_data), 0.8 * nrow(subset_data))
index_test <- setdiff(1:nrow(subset_data), index_train)
train_data <- subset_data[index_train, ]
test_data <- subset_data[index_test, ]

logistic_model_one <- glm(Biden_Win ~ ., data = train_data, family = binomial)

predictions <- predict(logistic_model_one, newdata = test_data, type = "response")
predicted_probabilities <- predictions

library(pROC)
roc_curve <- roc(test_data$Biden_Win, predicted_probabilities)
plot(roc_curve, main = "ROC Curve | Model Two", col = "navy", lwd = 3)
auc_score <- auc(roc_curve)
cat("AUC-ROC:", auc_score, "\n")

threshold <- 0.7
predicted_outcome <- ifelse(predicted_probabilities > threshold, 1, 0)

confusion_matrix <- table(predicted_outcome, test_data$Biden_Win)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

coefficients_one <- coef(logistic_model_one)
coefficients_df2 <- data.frame(predictor = names(coefficients_one), coefficient = coefficients_one)
coefficients_df2$log_odds <- coefficients_df2$coefficient
num_digits <- 4
coefficients_df2$coefficient_formatted <- sprintf("%.*f", num_digits, coefficients_df2$coefficient)
coefficients_df2$log_odds_formatted <- sprintf("%.*f", num_digits, coefficients_df2$log_odds)
print(coefficients_df2[, c("predictor", "coefficient_formatted", "log_odds_formatted")])
#summary(logistic_model_one)
