
library(ggplot2)
library(DT)
library(dplyr)
library(viridis)



steel_df <- read.csv("./Steel_industry_data.csv")

# Check if there are any numeric columns
numeric_columns <- sapply(steel_df, is.numeric)

# Display the first few rows with a styled data table if there are numeric columns
if (any(numeric_columns)) {
  datatable(head(steel_df), options = list(dom = 't', paging = FALSE), rownames = FALSE) %>%
    formatStyle(names(steel_df)[numeric_columns], background = styleColorBar(steel_df[, numeric_columns], color = "lightblue"))
} else {
  # If no numeric columns, display data table without styling
  datatable(head(steel_df), options = list(dom = 't', paging = FALSE), rownames = FALSE)
  
}

numeric_columns <- sapply(steel_df, is.numeric)
numeric_df <- steel_df[, numeric_columns]

# Calculate the correlation matrix
corr_matrix <- cor(numeric_df)

# Convert the correlation matrix to a data frame for ggplot2
corr_df <- as.data.frame(as.table(corr_matrix))
colnames(corr_df) <- c("Variable1", "Variable2", "Correlation")

# Plot the correlation matrix using ggplot2
ggplot(corr_df, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  theme_minimal()
  
null_sum <- colSums(is.na(steel_df))

# Display the sum of null values in each column
print(null_sum)

cat_columns <- names(steel_df[sapply(steel_df, is.character)])

for (col in cat_columns) {
  cat(col, "\n")
  print(table(steel_df[[col]]))
}


colnames(steel_df) <- sub("Lagging_Current_Reactive.Power_kVarh", "Lagging_Reactive_Power_kVarh", colnames(steel_df))
colnames(steel_df) <- sub("Leading_Current_Reactive_Power_kVarh", "Leading_Reactive_Power_kVarh", colnames(steel_df))
colnames(steel_df) <- sub("Lagging_Current_Power_Factor", "Lagging_Power_Factor", colnames(steel_df))
colnames(steel_df) <- sub("Leading_Current_Power_Factor", "Leading_Power_Factor", colnames(steel_df))
colnames(steel_df) <- sub("CO2.tCO2.", "CO2", colnames(steel_df))

# Display the updated data frame
head(steel_df)


ggplot(steel_df, aes(x = Lagging_Reactive_Power_kVarh)) +
  geom_histogram(fill = "#3F7F7F", color = "red", bins = 100) +
  geom_density(alpha = 0.5, fill = "red") +
  theme_minimal() +
  labs(x = "Lagging Reactive Power (kVarh)", y = "Frequency") +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 12))



colors <- c("#dc1e1e", "#dbba78", "black", "#dbba78", "#bb9c55", "#909195", "#dc1e1e", "#a02933", "#716807", "#717cb4")

par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))

# Scatter plot 1
plot(steel_df$Usage_kWh, steel_df$Lagging_Reactive_Power_kVarh, pch = 20, col = colors[1], main = "Usage kWh vs Lagging Reactive Power kVarh", xlab = "Usage (kWh)", ylab = "Lagging Reactive Power (kVarh)")

# Scatter plot 2
plot(steel_df$Usage_kWh, steel_df$Leading_Reactive_Power_kVarh, pch = 20, col = colors[8], main = "Usage kWh vs Leading Reactive Power kVarh", xlab = "Usage (kWh)", ylab = "Leading Reactive Power (kVarh)")

# Scatter plot 3
plot(steel_df$Usage_kWh, steel_df$Lagging_Power_Factor, pch = 20, col = colors[3], main = "Usage kWh vs Lagging Power Factor", xlab = "Usage (kWh)", ylab = "Lagging Power Factor")

# Scatter plot 4
plot(steel_df$Usage_kWh, steel_df$Leading_Power_Factor, pch = 20, col = colors[9], main = "Usage kWh vs Leading Power Factor", xlab = "Usage (kWh)", ylab = "Leading Power Factor")

# Scatter plot 5
plot(steel_df$Lagging_Reactive_Power_kVarh, steel_df$Leading_Reactive_Power_kVarh, pch = 20, col = colors[2], main = "Lagging Reactive Power (kVarh) vs Leading Reactive Power (kVarh)", xlab = "Lagging Reactive Power (kVarh)", ylab = "Leading Reactive Power (kVarh)")

# Scatter plot 6
plot(steel_df$Lagging_Power_Factor, steel_df$Leading_Power_Factor, pch = 20, col = colors[4], main = "Lagging Power Factor vs Leading Power Factor", xlab = "Lagging Power Factor", ylab = "Leading Power Factor")

# Scatter plot 7
plot(steel_df$Lagging_Reactive_Power_kVarh, steel_df$Lagging_Power_Factor, pch = 20, col = colors[5], main = "Lagging Reactive Power (kVarh) vs Lagging Power Factor", xlab = "Lagging Reactive Power (kVarh)", ylab = "Lagging Power Factor")

# Scatter plot 8
plot(steel_df$Lagging_Reactive_Power_kVarh, steel_df$Leading_Power_Factor, pch = 20, col = colors[4], main = "Lagging Reactive Power (kVarh) vs Leading Power Factor", xlab = "Lagging Reactive Power (kVarh)", ylab = "Leading Power Factor")

# Reset par settings
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)




library(plotly)
library(patchwork)

# Count plot
count_plot <- ggplot(steel_df, aes(x = Load_Type)) +
  geom_bar(fill = "#dbba78") +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Load_Type", fontsize = 20, color = '#dbba78', font = 'Times New Roman', pad = 30) +
  theme_minimal()

# Pie chart 
pie_chart <- plot_ly(steel_df, labels = ~Load_Type, type = 'pie', hole = 0.4) %>%
  layout(title = list(text = "Load_Type", font = list(size = 20, color = '#dbba78', family = 'Times New Roman', pad = 30)))

# We can plot both histogram and piechart. However I prefer pie chart as plotly allows an amazing dynamic
# ability that allows us to see the count and the percentage of each section in the pie chart!

count_plot
pie_chart



# Create the count plot
count_plot <- ggplot(steel_df, aes(x = WeekStatus, fill = Load_Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Usage kWh by Load Type", color = "Lightpink") +
  xlab("Load_Type") +
  theme_minimal()

# Display the plot
print(count_plot)

custom_colors <- c("Type1" = "black", "Type2" = "brown", "Type3" = "#dbba78")
scatter_plot <- ggplot(steel_df, aes(x = Usage_kWh, y = Lagging_Reactive_Power_kVarh, color = Load_Type)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Usage kWh vs Lagging Reactive Power kVarh by Load Type", color = "Lightpink") +
  xlab("Usage kWh") +
  ylab("Lagging Reactive Power kVarh") +
  scale_color_manual(values = colors)

print(scatter_plot)

scatter_plot <- ggplot(steel_df, aes(x = Usage_kWh, y = Leading_Reactive_Power_kVarh, color = Load_Type)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Usage kWh vs Lagging Reactive Power kVarh by Load Type", color = "Lightpink") +
  xlab("Usage kWh") +
  ylab("Lagging Reactive Power kVarh") +
  scale_color_manual(values = colors)

# Display the plot
print(scatter_plot)



# Skewness handling
library(psych)
library(moments)

vars <- c('Usage_kWh', 'Lagging_Reactive_Power_kVarh', 'Leading_Reactive_Power_kVarh', 'Lagging_Power_Factor', 'Leading_Power_Factor')

# Function to print statistics and create histograms
print_statistics_and_histogram <- function(col) {
  cat("\033[91m\033[1m")
  cat("Skewness:", col, "=", round(skewness(steel_df[[col]]), 3), "\n")
  cat("Kurtosis:", col, "=", round(kurtosis(steel_df[[col]]), 2), "\n")
  cat("Mean:", col, "=", round(mean(steel_df[[col]]), 2), "\n")
  cat("Max:", col, "=", round(max(steel_df[[col]]), 2), "\n")
  cat("Min:", col, "=", round(min(steel_df[[col]]), 2), "\n")
  cat("Median:", col, "=", round(median(steel_df[[col]]), 2), "\n")
  cat("Std:", col, "=", round(sd(steel_df[[col]]), 2), "\n")
  cat("Var:", col, "=", round(var(steel_df[[col]]), 2), "\n")
  
  hist_title <- paste(col, " Distribution", sep = "")
  hist(steel_df[[col]], main = hist_title, col = "red", border = "black", xlab = col, breaks = 50)
  
  cat("\033[30m\033[1m")
  cat("=====","=====","=====","=====","=====","=====","=====","=====","=====","=====","\n")
}

for (col in vars) {
  print_statistics_and_histogram(col)
}

# ============================ Data Cleaning ===============================


library(dplyr)
library(purrr)
library(ggplot2)
library(e1071)

# Identify numeric columns
numeric_columns <- sapply(steel_df, is.numeric)

# Calculate skewness for numeric columns
old_skew <- sapply(steel_df[, numeric_columns], skewness)
old_skew <- sort(old_skew, decreasing = TRUE)

# Function to apply log transformation and visualize distribution
log_transformation <- function(data, feature) {
  # Apply log transformation
  data_log <- data %>%
    mutate(across(all_of(feature), ~log1p(.), .names = "log_{.col}"))
  
  # Plot distribution before and after transformation
  plot_before <- ggplot(data, aes(x = .data[[feature]])) +
    geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
    labs(title = "Distribution before Transformation", x = feature, color = "red") +
    theme_minimal()
  
  plot_after <- ggplot(data_log, aes(x = .data[[paste0("log_", feature)]])) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Distribution after Transformation", x = paste0("log_", feature), color = "blue") +
    theme_minimal()
  
  print(gridExtra::grid.arrange(plot_before, plot_after, ncol = 2))
  
  # Print skewness before and after transformation
  cat(paste("Skewness was", round(old_skew[feature], 2), "before & is", round(skewness(data_log[[paste0("log_", feature)]]), 2), "after Log transformation.\n"))
}

# Apply log transformation to 'Lagging_Reactive_Power_kVarh'
log_transformation(steel_df, feature = "Lagging_Reactive_Power_kVarh")



# Skewness Handling

options(repr.plot.width=18, repr.plot.height=4)

ggplot(steel_df, aes(x = Usage_kWh, fill = Load_Type)) +
  geom_density(alpha = 0.7) +
  labs(title = "Kernel Density Plot of Usage_kWh by Load_Type", x = "Usage_kWh") +
  theme_minimal()

# Label Encoding for categorical columns
library(forcats)

# Identify categorical columns
categorical_columns <- sapply(steel_df, is.character)

# Apply label encoding to categorical columns
steel_df <- steel_df %>%
  mutate(across(all_of(names(steel_df)[categorical_columns]), as.factor)) %>%
  mutate(across(all_of(names(steel_df)[categorical_columns]), ~as.integer(fct_recode(., !!!levels(.)))))

# Display the first few rows
head(steel_df)

# Correlation Analysis

corr_matrix <- cor(steel_df)

# Convert the correlation matrix to a data frame for ggplot2
corr_df <- as.data.frame(as.table(corr_matrix))
colnames(corr_df) <- c("Variable1", "Variable2", "Correlation")

# Plot the correlation matrix using ggplot2
ggplot(corr_df, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  theme_minimal()




#--------------------------------------------------PCA----------------------------------------------------------

steel_df$Load_Type <- as.numeric(as.character(steel_df$Load_Type)) - 1

# Exclude non-numeric and target variable
numeric_vars <- sapply(steel_df, is.numeric)
numeric_vars <- numeric_vars[!names(numeric_vars) %in% c("date", "Load_Type")]


numeric_df <- steel_df[, numeric_vars]
numeric_df <- numeric_df[, !names(numeric_df) %in% "Load_Type"]
# Standardize the numeric features (optional but recommended for PCA)
scaled_numeric_df <- scale(numeric_df)

# Perform PCA
pca_result <- prcomp(scaled_numeric_df, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca_result)
loadings <- pca_result$rotation

# Display the loadings
print(loadings)
# Scree plot to visualize the variance explained by each principal component
screeplot(pca_result, type = "lines")


principal_components <- pca_result$x

captured_components <- principal_components[, 1:7]

#--------------------ML ON PCA--------------------------------------
library(ranger)
data_pca <- cbind.data.frame(captured_components, Load_Type = steel_df$Load_Type)

# Convert 'Load_Type' to a factor
data_pca$Load_Type <- as.factor(data_pca$Load_Type)

#-----------------------------------Decision Tree on PCA---------------------------------------

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
split_index <- sample(1:nrow(data_pca), 0.8 * nrow(data_pca))
train_data <- data_pca[split_index, ]
test_data <- data_pca[-split_index, ]

# Define the ranger model
tree_model <- ranger(Load_Type ~ ., data = train_data, num.trees = 500)

# Make predictions on the test set
test_predictions_tree <- predict(tree_model, data = test_data)$predictions

# Evaluate the ranger model
conf_matrix_tree <- table(test_predictions_tree, as.character(test_data$Load_Type))
accuracy_tree <- sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)

# Display the confusion matrix and accuracy for the ranger model
print(conf_matrix_tree)
cat("Accuracy (Decision Tree on PCA data - Ranger):", accuracy_tree, "\n")


#################################XG Boost on PCA ###############################################
library('xgboost')

data_pca$Load_Type <- as.numeric(as.character(data_pca$Load_Type))

set.seed(123)  # Set seed for reproducibility
split_index <- sample(1:nrow(data_pca), 0.8 * nrow(data_pca))
train_data <- data_pca[split_index, ]
test_data <- data_pca[-split_index, ]

# Define X and Y variables
X <- subset(train_data, select = -Load_Type)
Y <- train_data$Load_Type

# Train the XGBoost model
xgb_model <- xgboost(data = as.matrix(X), label = as.numeric(Y), nrounds = 200, objective = "multi:softmax", num_class = 3)

# Make predictions on the test set
test_predictions <- predict(xgb_model, as.matrix(subset(test_data, select = -Load_Type)))

# Evaluate the model
conf_matrix <- table(test_predictions, as.numeric(test_data$Load_Type))
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Display the confusion matrix and accuracy
print(conf_matrix)
cat("Accuracy:", accuracy, "\n")


################################################################################################
#================================12-19-23========================================

library(xgboost)
library(caret)

# Assuming your data frame is named 'steel_df'
# Convert 'Load_Type' to numeric and subtract 1
#steel_df$Load_Type <- as.numeric(as.character(steel_df$Load_Type)) - 1

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
split_index <- sample(1:nrow(steel_df), 0.8 * nrow(steel_df))
train_data <- steel_df[split_index, ]
test_data <- steel_df[-split_index, ]

# Define X and Y variables
X <- subset(train_data, select = -Load_Type)
Y <- train_data$Load_Type

# Train the XGBoost model
xgb_model <- xgboost(data = as.matrix(X), label = as.numeric(Y), nrounds = 100, objective = "multi:softmax", num_class = 3)

# Make predictions on the test set
test_predictions <- predict(xgb_model, as.matrix(subset(test_data, select = -Load_Type)))

# Evaluate the model
conf_matrix <- table(test_predictions, as.numeric(test_data$Load_Type))
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Display the confusion matrix and accuracy
print(conf_matrix)
cat("Accuracy:", accuracy, "\n")

precision <- diag(conf_matrix) / rowSums(conf_matrix)
recall <- diag(conf_matrix) / colSums(conf_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Display precision, recall, and F1 score
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

overall_f1_score <- weighted.mean(f1_score, na.rm = TRUE)

# Display overall F1 score
cat("Overall F1 Score:", overall_f1_score, "\n")

#----------------------Works Perfect Till here--------------------------------------------


library(ranger)

# Assuming your data frame is named 'steel_df'
# Convert 'Load_Type' to a factor with appropriate levels
steel_df$Load_Type <- as.factor(steel_df$Load_Type)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
split_index <- sample(1:nrow(steel_df), 0.8 * nrow(steel_df))
train_data <- steel_df[split_index, ]
test_data <- steel_df[-split_index, ]

# Define the ranger model
tree_model <- ranger(Load_Type ~ ., data = train_data, num.trees = 500)

# Make predictions on the test set
test_predictions_tree <- predict(tree_model, data = test_data)$predictions

# Evaluate the ranger model
conf_matrix_tree <- table(test_predictions_tree, as.character(test_data$Load_Type))
accuracy_tree <- sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)

# Display the confusion matrix and accuracy for the ranger model
print(conf_matrix_tree)
cat("Accuracy (Decision Tree - Ranger):", accuracy_tree, "\n")


# ----------------------------------- Metrics---------------

precision_tree <- diag(conf_matrix_tree) / rowSums(conf_matrix_tree)
recall_tree <- diag(conf_matrix_tree) / colSums(conf_matrix_tree)
f1_score_tree <- 2 * (precision_tree * recall_tree) / (precision_tree + recall_tree)

# Display precision, recall, and F1 score for the ranger model
cat("Precision (Decision Tree - Ranger):", precision_tree, "\n")
cat("Recall (Decision Tree - Ranger):", recall_tree, "\n")
cat("F1 Score (Decision Tree - Ranger):", f1_score_tree, "\n")

# Calculate overall F1 score for the ranger model
overall_f1_score_tree <- weighted.mean(f1_score_tree, na.rm = TRUE)

# Display overall F1 score for the ranger model
cat("Overall F1 Score (Decision Tree - Ranger):", overall_f1_score_tree, "\n")

