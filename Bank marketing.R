#read CSV file
bank_Data <- read.csv("./bank-full.csv", header=T, na.strings=c(""), stringsAsFactors = T)
summary(bank_Data)
str(bank_Data)


# Let's summarise the dataset a little and check it out
summary_bank_Data <- t(summary(bank_Data))
print(summary_bank_Data)


#Checkpoint for the Null values
missing_values <- colSums(is.na(bank_Data))
print(missing_values)## No null value##

# Let's check the categorical features within the dataset
library(dplyr)

categorical_cols <- bank_Data %>%
  select_if(is.factor) %>%
  names()

cat("Categorical Columns:\n")
for (col in categorical_cols) {
  cat("- ", col, "\n")
}

# Next we check the Numerical features within the dataset
library(dplyr)

numeric_cols <- bank_Data %>%
  select_if(is.numeric) %>%
  names()

cat("Numeric Columns:\n")
for (col in numeric_cols) {
  cat("- ", col, "\n")
}
----------------------------------------------------------------------------
#Visualization
library(ggplot2)
library(gridExtra)

##1.histogram plots for numeric columns
for (col in numeric_cols) {
  hist_plot <- ggplot(bank_Data, aes(x = get(col))) +
    geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram for", col),
         x = col,
         y = "Frequency")
  
  print(hist_plot)
}


##2. Create bar plots for categorical columns
for (col in categorical_cols) {
  bar_plot <- ggplot(bank_Data, aes(x = factor(get(col)), fill = get(col))) +
    geom_bar(alpha = 0.7, position = "dodge") +
    labs(title = paste("Bar plot for", col),
         x = col,
         y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle
  
  print(bar_plot)
}

##3.Box plot for Numeric columns
box_plots <- list()

# Create box plots for numeric columns
for (col in numeric_cols) {
  box_plot <- ggplot(bank_Data, aes(x = 1, y = get(col))) +
    geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Box plot for", col),
         x = "",
         y = col)
  
  print(box_plot)
}

##4.Density plot for Numeric columns
library(ggplot2)
for (col in numeric_cols) {
  density_plot <- ggplot(bank_Data, aes(x = get(col))) +
  geom_density(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = paste("Density plot for", col),
       x = col,
       y = "Density")

# Display density plot in a separate window
print(density_plot)
}


#------------------------------------------------------------------
#heatmap
  

library(ggplot2)
library(corrplot)

# Calculate the correlation matrix

numeric_features <- bank_Data[, numeric_cols]

correlation_matrix <- cor(numeric_features)

corrplot(correlation_matrix, method = "color",bg = "red", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

#------------------------------------------------------------------
##Removing outliers from numeric columns
str(bank_Data)
remove_outliers <- function(data, column) {
    Q1 <- quantile(data[[column]], 0.25)
    Q3 <- quantile(data[[column]], 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    return(data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ])
  }

# Remove outliers from numeric columns in bank_Data
for (col in numeric_cols) {
  bank_Data_no_outliers <- remove_outliers(bank_Data, col)
}

# Check the modified data frame without outliers
print(head(bank_Data_no_outliers))
str(bank_Data_no_outliers)
dimensions <- dim(bank_Data_no_outliers)
print(dimensions)
----------------
#after removing outliers, 'pdays' and 'previous' columns only have -1 and 0 value. all other values are outliers. 

#-----------------------------------------------------------------------
##6.Box plot after removing outliers for Numeric columns

# Create box plots for numeric columns
  print(dimensions)
  for (col in numeric_cols) {
    box_plot_2 <- ggplot(bank_Data_no_outliers, aes(x = 1, y = get(col))) +
      geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
      labs(title = paste("Box plot for", col),
           x = "",
           y = col)
    
    print(box_plot_2)
  }

#-------------------------------------------------------------------------
#Lable encode the categorical columns
categorical_cols_to_encode <- c("job", "marital", "education","default","housing","loan","contact","month","poutcome")
bank_Data_final[, categorical_cols_to_encode] <- lapply(bank_Data_final[, categorical_cols_to_encode], function(x) as.numeric(factor(x)))
bank_Data_no_outliers[, categorical_cols_to_encode] <- lapply(bank_Data_no_outliers[, categorical_cols_to_encode], function(x) as.numeric(factor(x)))

# Print the encoded dataframe
str(bank_Data_final)

unique_values <- lapply(bank_Data_final, unique)
print(unique_values)
#--------------------------------------------------------------------------
#Binary encoding for target variable
  
#bank_Data_final$y <- ifelse(bank_Data_final$y == 'e', 0, 1)
#head(bank_Data_final)
#bank_Data_no_outliers$y <- ifelse(bank_Data_no_outliers$y == 'e', 0, 1)



#------------------------KNN-------------------------------------------------------


library(caret)

set.seed(23112573)

bank_Data_no_outliers$y <- as.numeric(bank_Data_no_outliers$y) - 1

split_index <- sample(1:nrow(bank_Data_no_outliers), 0.8 * nrow(bank_Data_no_outliers))
train_data <- bank_Data_no_outliers[split_index, ]
test_data <- bank_Data_no_outliers[-split_index, ]


# Load the required library
library(class)

# Assuming 'y' is the response variable in your dataset
response_variable <- "y"

# Define the features (exclude the response variable)
features <- setdiff(names(train_data), response_variable)

# Train the KNN model
knn_model <- knn(train = train_data[, features],
                 test = test_data[, features],
                 cl = train_data[[response_variable]],
                 k = 5)  # You can adjust the value of 'k' as needed

# Make predictions on the test set
predictions <- knn_model

# Convert predicted labels to binary (if needed)
threshold <- 0.5
binary_predictions <- ifelse(predictions == "yes", 1, 0)  # Adjust based on your classes

# Confusion matrix
conf_matrix <- table(Actual = test_data[[response_variable]], Predicted = binary_predictions)
print(conf_matrix)


accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# Calculate metrics: Precision, Recall, F1 Score
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))


#----------------------------------------Random Over/Under Sampling-----------------------------

library(ROSE)
oversampled_data <- ROSE(y ~ ., data = train_data, seed = 23112573, p = 0.5, N = 2 * nrow(train_data)) 
oversampled_data <- data.frame(oversampled_data)
oversampled_data$y <- as.factor(oversampled_data$y)

# Train KNN model on the oversampled data
knn_model_oversampled <- train(y ~ ., data = oversampled_data$data, method = "knn")

# Evaluate the model (you can use your own evaluation metrics)
predictions_oversampled <- predict(knn_model_oversampled, newdata = test_data)
conf_matrix_oversampled <- confusionMatrix(predictions_oversampled, reference = test_data$y)
print(conf_matrix_oversampled)


#---------------------------------------------------------------------------
#Model XGBoost

library(xgboost)
library(caret)
library(dplyr)

# Define the target variable
target_variable <- "y"

# Train the XGBoost model
xgb_model <- xgboost(data = as.matrix(train_data[, !colnames(train_data) %in% target_variable]),
                     label = train_data[[target_variable]],
                     objective = "binary:logistic",
                     nrounds = 100,  # Number of boosting rounds
                     print_every_n = 10,  # Print progress every 10 rounds
                     early_stopping_rounds = 10,  # Stop if performance doesn't improve for 10 rounds
                     eval_metric = "logloss",  # Evaluation metric
)

# Make predictions on the test set
predictions <- predict(xgb_model, as.matrix(test_data[, -ncol(test_data)]))
#predict(xgb_model, as.matrix(test_data[, !colnames(test_data) %in% target_variable]))

# Convert predicted probabilities to binary predictions
threshold <- 0.5
binary_predictions <- ifelse(predictions > threshold, 1, 0)
unique(binary_predictions)

#confusion matrix
library(caret)
x_conf_matrix <- table(Actual = test_data$y, Predicted = binary_predictions)
print(x_conf_matrix)

# Evaluate the model (e.g., accuracy)
accuracy <- sum(diag(x_conf_matrix)) / sum(x_conf_matrix)
#sum(binary_predictions == test_data[[target_variable]]) / length(test_data[[target_variable]])
print(paste("Accuracy:", accuracy))

precision <- x_conf_matrix[2, 2] / sum(x_conf_matrix[, 2])
recall <- x_conf_matrix[2, 2] / sum(x_conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))
## accuracy is 1 and in confusion matrix there is no False Positives or False Negatives
##because class is not balanced in data. "Yes" instance present only in around 5000 rows
## and 39000 around are "No" instance. 

