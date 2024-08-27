library(ggplot2)
library(dplyr)
library(tidyr)

adult_df <- read.csv("./adult.csv", header = FALSE, na.strings = c("?", " ?"))

# Column name definition
colnames(adult_df) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status",
                  "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss",
                  "hours_per_week", "native_country", "income")

str(adult_df)

# Summary statistics
summary(adult_df)
View(adult_df)


# Let's just add a theme here
theme_dt <- function(){
  theme_minimal() %+replace%
    theme(
      axis.line    = element_line(colour = "black", lineend = "square", linetype = "solid"  , size = 0.3),
      axis.text.x  = element_text(size = 9, colour = "darkseagreen1", family = "sans"),
      axis.text.y  = element_text(size = 9, colour = "darkseagreen1", family = "sans"),
      axis.title.x = element_text(size = 12, colour = "darkseagreen1", family = "sans", margin = ggplot2::margin(0.5,0,0,0, "cm")),
      axis.title.y = element_text(size = 12, colour = "darkseagreen1", family = "sans", margin = ggplot2::margin(0,0.5,0,0, "cm"), angle = 90),
      axis.ticks   = element_line(colour = "darkseagreen1"),
      
      legend.background     = element_rect(fill = "darkslategray"), 
      legend.key            = element_rect(fill = "darkslategray"),
      legend.box.background = element_rect(fill = "darkslategray"),
      legend.text           = element_text(colour = "darkseagreen1", size = 9, family = "serif"),
      legend.title          = element_text(colour = "darkseagreen1", size = 12, family = "sans", margin = ggplot2::margin(0, 0.1, 0, 0, "cm")),
      legend.position       = "right",
      legend.text.align     = 0.01,
      legend.title.align    = 0.5,
      
      panel.background = element_rect(fill = "darkslategray"), 
      panel.border     = element_rect(fill = NA, colour = NA), 
      panel.grid.major = element_line(colour = "darkolivegreen1", lineend = "square", linetype = "dotted" , size = 0.2),
      panel.grid.minor = element_line(colour = NA),
      panel.ontop = FALSE,
      
      strip.background = element_rect(fill = "darkslategray"), 
      strip.text       = element_text(size = 11, colour = "darkseagreen1", family = "sans"),
      strip.text.y     = element_text(angle = -90),
      
      plot.background =  element_rect(fill = "darkslategray"),
      plot.title      =  element_text(size = 15, colour = "darkseagreen1", family = "sans", margin = ggplot2::margin(0,0,0.5,0, "cm")),
      plot.margin     =  unit(c(1,1,0.5,0.5), "lines")
    )
}


# -----------------------------The theme ends here

# Upon careful consideration of the dataset it is evident that the Dataset contains question marks instead of 
# null values. So these data are supposed to be considered as Null. Let's handle that here.
# Replace "?" marks with NA
adult_df[adult_df == "?"] <- NA

# Checkpoint for missing values
sum(is.na(adult_df))

missing_values <- colSums(is.na(adult_df))

# Display the results
print(missing_values)

# Let's remove the null value instances from the dataset as per the the
# native_country column

adult_df <- subset(adult_df, !is.na(native_country))
# Lets check the null values again.

missing_values <- colSums(is.na(adult_df))

# Display the results
print(missing_values)

# Looks Good. Those null values are removed. let's now handle the other ones.

# let's handle the missing values for Occupation column. Replace the missing values with mode
mode_occupation <- names(sort(table(adult_df$occupation), decreasing = TRUE)[1])
adult_df$occupation[is.na(adult_df$occupation)] <- mode_occupation

# Alright, Now lets check again!
missing_values <- colSums(is.na(adult_df))

# Display the results
print(missing_values)

# perfect. The mode values are added into the null instances. Let's do the same for workclass
mode_workclass <- names(sort(table(adult_df$workclass), decreasing = TRUE)[1])
adult_df$workclass[is.na(adult_df$workclass)] <- mode_workclass

# Alright now lets grab the categorical values and 
# convert them into factors
adult_df$workclass <- as.factor(adult_df$workclass)
adult_df$education <- as.factor(adult_df$education)
adult_df$marital_status <- as.factor(adult_df$marital_status)
adult_df$occupation <- as.factor(adult_df$occupation)
adult_df$relationship <- as.factor(adult_df$relationship)
adult_df$race <- as.factor(adult_df$race)
adult_df$sex <- as.factor(adult_df$sex)
adult_df$native_country <- as.factor(adult_df$native_country)
adult_df$income <- as.factor(adult_df$income)

str(adult_df)


categorical_cols <- c("workclass", "education", "marital_status", "occupation", "relationship", "race", "sex", "native_country","income")
numeric_columns <- c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")

encoded_df <- adult_df

# Label encoding for selected columns
for (col in categorical_cols) {
  levels <- unique(adult_df[[col]])
  encoded_df[[col]] <- as.integer(factor(adult_df[[col]], levels = levels))
}


# Display the first few rows of the encoded dataframe
head(encoded_df)

graphics.off()

# Load required library
library(ggplot2)

# COuntplot for Income
ggplot(encoded_df, aes(x = income)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Count Plot for Income", x = "Income", y = "Count") +
  theme_classic()

# Countplot for Age
ggplot(encoded_df, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#3498db", color = "#2c3e50") +
  labs(title = "Count Plot for Age", x = "Age", y = "Count") +
  theme_classic()

# Correlation Matrixx

library(corrplot)

cat_features <- encoded_df[, categorical_cols]

correlation_matrix <- cor(cat_features)

corrplot(correlation_matrix, method = "color",bg = "lightgrey", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Occupatoin vs income
ggplot(adult_df, aes(x = occupation, fill = factor(income))) +
  geom_bar(position = "dodge") +
  theme_dt() +
  labs(title = "Bar Chart of Occupation vs Income", x = "Occupation", y = "Count", fill = "Income") +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +  # Specify colors for income levels
  theme(legend.title = element_blank())

# We can conclude that mostly within the occupations, the no. of people with income less than 50 k is 3 times
# more than the no.of people with more than 50k earning.

# Workclass vs Income

ggplot(adult_df, aes(x = workclass, fill = factor(income))) +
  geom_bar(position = "dodge") +
  theme_dt() +
  labs(title = "Bar Chart of Workclass vs Income", x = "Workclass", y = "Count", fill = "Income") +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +  # Specify colors for income levels
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# Self employed people have a larger bracket of people with an earning greater than 50k.

# marital status vs Income

ggplot(adult_df, aes(x = marital_status, fill = factor(income))) +
  geom_bar(position = "dodge") +
  theme_dt() +
  labs(title = "Bar Chart of Marital Status vs Income", x = "Marital Status", y = "Count", fill = "Income") +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +  # Specify colors for income levels
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# Education vs Income
ggplot(adult_df, aes(x = education, fill = factor(income))) +
  geom_bar(position = "dodge") +
  theme_dt() +
  labs(title = "Bar Chart of Education vs Income", x = "Education", y = "Count", fill = "Income") +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +  # Specify colors for income levels
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# Hours per week of work distribution
ggplot(encoded_df, aes(x = hours_per_week)) +
  geom_line(stat = "count") +
  labs(title = "Hours Per Week Distribution", x = "Hours per Week", y = "Count") +
  theme_minimal()


# Correlation Matrix of the whole data now, as all the data is now converted to numeric datatype.
cor_matrix <- cor(encoded_df)

# Create a correlation matrix heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, col = colorRampPalette(c("#3498db", "#ffffff", "#e74c3c"))(100))


# As observed from the correlation matrix, the column 'fnlwgt' is extremely less correlated with the target variable 'income'
# So hence it can be dropped.

encoded_df <- encoded_df[, !colnames(encoded_df) %in% c("fnlwgt")]

colSums(encoded_df)

ggplot(data = encoded_df, aes(x = as.factor(income), y = hours_per_week, col = as.factor(income))) +
  geom_boxplot() +
  theme_dt() + theme(legend.position = "none") +
  labs(x = "Income", y = "Hours per Week", 
       title = "Boxplot for Income & Hours per Week")

ggplot(data = encoded_df, aes(x = as.factor(income), y = age, col = as.factor(income))) +
  geom_boxplot() +
  theme_dt() + theme(legend.position = "none") +
  labs(x = "Income", y = "Age", 
       title = "Boxplot for Income & Age")

f1score = function(precision, recall){
  2 * precision * recall / (precision + recall)
}

# ------------------------------------------------------------------------------------------------------------
#                                          OUTLIERS

identify_outliers_zscore <- function(column, threshold = 3) {
  # Calculate z-scores
  z_scores <- scale(column)
  
  # Identify outliers
  outliers <- which(abs(z_scores) > threshold)
  
  return(outliers)
}

# Identify outliers for all numeric columns using z-scores
outliers_per_column <- lapply(encoded_df, function(column) identify_outliers_zscore(column))

# Display the result
print(outliers_per_column)

#------------------------Handling outliers HEre properly-----------------------------
# Handlinig outliers of continuous values
# Capital Gain Outlier handling
z_scores <- scale(encoded_df$capital_gain)

# Identify outliers
outliers <- which(abs(z_scores) > 3)
new_encoded <- encoded_df[-outliers, ]

# Capital Loss Outlier handling
z_scores <- scale(new_encoded$capital_loss)

# Identify outliers
outliers <- which(abs(z_scores) > 3)
well_encoded <- new_encoded[-outliers, ]

# Capital Loss Outlier handling
z_scores <- scale(well_encoded$education_num)

# Identify outliers
outliers <- which(abs(z_scores) > 3)
final_df <- well_encoded[-outliers, ]

#----------------------------------Normalization--------------------------------

normalize_minmax <- function(column) {
  min_val <- min(column, na.rm = TRUE)
  max_val <- max(column, na.rm = TRUE)
  scaled_column <- (column - min_val) / (max_val - min_val)
  return(scaled_column)
}

columns_to_normalize_minmax <- c("age", "capital_gain", "capital_loss")

df_minmax <- lapply(final_df[columns_to_normalize_minmax], normalize_minmax)

column_indices_to_drop <- which(names(final_df) %in% columns_to_normalize_minmax)

# Drop the columns
df_rest <- final_df[, -column_indices_to_drop]

df_scaled <- cbind(df_minmax, df_rest)

df_scaled$income <- as.numeric(df_scaled$income) - 1
#levels(df_scaled$income) <- c(0, 1)
#df_scaled$income <- as.integer(df_scaled$income)
#df_scaled$age <- as.integer(df_scaled$age)
#df_scaled$capital_gain <- as.integer(df_scaled$capital_gain)
#df_scaled$capital_loss <- as.integer(df_scaled$capital_loss)
# ----------------------------------------------------------------------------------





######################### Train/Test Split #############################

#------------------------- Logistic Regression-----------------------------
df_scaled <- as.data.frame(df_scaled)
set.seed(23112573)  # for reproducibility
splitIndex <- createDataPartition(df_scaled$income, p = 0.7, list = FALSE)
train_data <- df_scaled[splitIndex, ]
test_data <- df_scaled[-splitIndex, ]

logistic_model <- glm(income ~ ., data = train_data, family = binomial(link = "logit"))

#_-------------------------Evaluation Metrics for Logistic Regression--------------------------

library("data.table")

lr_pred = data.table(target = append(train_data$income, test_data$income),
                     prob   = append(predict.glm(logistic_model, train_data, type = "response"), predict.glm(logistic_model, test_data, type = "response")))
lr_pred[, pred := ifelse(prob > 0.5, 1, 0)]
lr_TruePos  = lr_pred[target == 1 & pred == 1, .N]
lr_FalsePos = lr_pred[target == 0 & pred == 1, .N]
lr_FalseNeg = lr_pred[target == 1 & pred == 0, .N]
lr_Precision = lr_TruePos / (lr_TruePos + lr_FalsePos)
lr_Recall    = lr_TruePos / (lr_TruePos + lr_FalseNeg)
lr_score     = f1score(lr_Precision, lr_Recall); rm(lr_TruePos, lr_FalsePos, lr_FalseNeg, lr_Precision, lr_Recall)

accuracy <- sum(lr_pred$target == lr_pred$pred) / nrow(lr_pred)

print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

######################## EVALUATE EACh Class#######################

conf_matrix <- table(lr_pred$target, lr_pred$pred)

# Calculation
accuracy_class0 <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
accuracy_class1 <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

# Accuracies for each class
print(paste("Accuracy for Class 0:", round(accuracy_class0 * 100, 2), "%"))
print(paste("Accuracy for Class 1:", round(accuracy_class1 * 100, 2), "%"))


# PRECISION-----------
precision_class1 <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(precision_class1)
# RECALL ------------
recall_class1 <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(recall_class1)

# Model performance using F1 Score
print(lr_score)

#--------------------------RANDOM FOREST block----------------------------------------y

library(randomForest)
train_data$income <- as.factor(train_data$income)

rf_model <- randomForest(income ~ ., data = train_data, ntree = 100)

predictions <- predict(rf_model, newdata = test_data)
train_data$income <- as.numeric(train_data$income)
confusion_matrix <- table(predictions, test_data$income)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

######################## EVALUATE EACh Class#######################


# Calculation
accuracy_class0 <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
accuracy_class1 <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])

# Accuracies for each class
print(paste("Accuracy for Class 0:", round(accuracy_class0 * 100, 2), "%"))
print(paste("Accuracy for Class 1:", round(accuracy_class1 * 100, 2), "%"))


# Evaluation Metrics -

precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuracy:", round(accuracy, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(f1_score, 4)))


#--------------Scaling of dataset-----------------

target_column <- df_scaled$income

# Select only the numeric columns for scaling
numeric_columns <- sapply(df_scaled, is.numeric)
numeric_data <- df_scaled[, numeric_columns, drop = FALSE]

# Scale the numeric features (excluding the target column)
scaled_numeric_data <- scale(numeric_data)

# Combine the scaled numeric features with the non-numeric columns and the target column
df_scaled <- cbind(scaled_numeric_data, df_scaled[!numeric_columns], income = target_column)


#-------------------PCA begins here--------------------


pca_result <- prcomp(
  train_data[, -ncol(train_data)],  # Exclude the target variable (income) from PCA
  center = TRUE,
  scale. = TRUE
)

# Explore the summary of PCA
summary(pca_result)

plot(cumsum(summary(pca_result)$importance[, 2]), type = "b", pch = 19, main = "Cumulative Proportion of Variance")

eigenvalues <- summary(pca_result)$importance[, 2]
num_components <- sum(eigenvalues > 1)
print(num_components)

pca_result <- prcomp(df_scaled, scale = TRUE)

# Plot the scree plot
plot(summary(pca_result)$importance[, 2], type = "b", pch = 19, main = "Scree Plot")

# Choose the number of components based on Kaiser's Rule (eigenvalue > 1)
num_components <- sum(summary(pca_result)$importance[, 2] > 1)

# Use the selected number of components for further analysis
selected_pca <- pca_result$x[, 1:num_components]

print(num_components)


#-----------------------------------------------------------------------------------------------------
features <- train_data[, -which(names(train_data) == "income")]

# Perform PCA
pca_result <- prcomp(features, scale. = TRUE)

# Plot the scree plot
plot(1:length(pca_result$sdev), pca_result$sdev^2, type = "b", main = "Scree Plot", xlab = "Principal Component", ylab = "Eigenvalue")


k <- 5

# Perform PCA with the specified number of components
pca_result <- prcomp(features, scale. = TRUE)

# Extract the first 'k' principal components
reduced_data <- predict(pca_result, newdata = features)[, 1:k]

# Combine with the target variable 'income'
reduced_data <- cbind(reduced_data, income = train_data$income)

#-------------------- XGB Modeling---------------------------------------------------------------

set.seed(23112573)
reduced_data <- as.data.frame(reduced_data)
splitIndex <- createDataPartition(reduced_data$income, p = 0.7, list = FALSE)
xgb_train_data <- reduced_data[splitIndex, ]
xgb_test_data <- reduced_data[-splitIndex, ]


# Train an XGBoost model
xgb_model <- xgboost(data = as.matrix(xgb_train_data[, -ncol(xgb_train_data)]),
                     label = xgb_train_data$income,
                     nrounds = 100,  # You can adjust the number of rounds
                     objective = "binary:logistic",  # For binary classification
                     eval_metric = "logloss")  # You can choose a different evaluation metric

# Make predictions on the test set
xgb_pred <- predict(xgb_model, as.matrix(xgb_test_data[, -ncol(xgb_test_data)]))

xgb_pred_class <- ifelse(xgb_pred > 0.5, 1, 0)

# Evaluate model performance
conf_matrix <- table(Actual = xgb_test_data$income, Predicted = xgb_pred_class)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
print(paste("Precision:", round(precision * 100, 2), "%"))
print(paste("Recall:", round(recall * 100, 2), "%"))
print(paste("F1 Score:", round(f1_score, 4)))

