library(readr)
library(ggplot2)


#### EDA ####
# read dataset
df <- read_csv("/Users/mr.youssef/Dropbox/data-analytics/assign5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

# Easier column names: replace spaces with underscores
names(df) <- gsub(" ", "_", names(df))

# Subset for QUEENS and the 4 variables we care about (Plus NEIGHBORHOOD for part 2)
df_queens = df[df$BOROUGH == "QUEENS", ]
cols_to_keep <- c("SALE_PRICE", "GROSS_SQUARE_FEET", "LAND_SQUARE_FEET", "YEAR_BUILT", "NEIGHBORHOOD")
df_queens <- df_queens[, cols_to_keep]

# Clean the dollar signs and commas
df_queens$SALE_PRICE <- parse_number(as.character(df_queens$SALE_PRICE))
df_queens$GROSS_SQUARE_FEET <- parse_number(as.character(df_queens$GROSS_SQUARE_FEET))
df_queens$LAND_SQUARE_FEET <- parse_number(as.character(df_queens$LAND_SQUARE_FEET))
df_queens$YEAR_BUILT <- as.numeric(df_queens$YEAR_BUILT)

# Drop NAs
rows_before <- nrow(df_queens)
df_queens <- df_queens[!is.na(df_queens$SALE_PRICE), ]
df_queens <- df_queens[!is.na(df_queens$GROSS_SQUARE_FEET), ]
df_queens <- df_queens[!is.na(df_queens$LAND_SQUARE_FEET), ]
df_queens <- df_queens[!is.na(df_queens$YEAR_BUILT), ]
rows_after <- nrow(df_queens)
rows_before
rows_after
rows_removed <- rows_before - rows_after
rows_removed
# Plot the distributions to find Outliers

ggplot(df_queens, aes(y = SALE_PRICE)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 1) +
  labs(title = "Boxplot of Sale Price in Queens", y = "Sale Price ($)") +
  theme_minimal()

ggplot(df_queens, aes(y = GROSS_SQUARE_FEET)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 1) +
  labs(title = "Boxplot of Gross Sq Ft in Queens", y = "Gross Square Feet") +
  theme_minimal()

ggplot(df_queens, aes(y = LAND_SQUARE_FEET)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 1) +
  labs(title = "Boxplot of Land Sq Ft in Queens", y = "Land Square Feet") +
  theme_minimal()


sum(df_queens$SALE_PRICE <= 1000)
# There are 15581 rows with Sale Price < 1000 that are clearly skewing the data
# So we remove them
df_queens <- df_queens[df_queens$SALE_PRICE > 1000, ]

# Calculate the 1st and 99th Percentiles for Sale Price and Filter them out
price_lower <- quantile(df_queens$SALE_PRICE, 0.01, na.rm = TRUE)
price_upper <- quantile(df_queens$SALE_PRICE, 0.99, na.rm = TRUE)
price_lower
price_upper
df_queens <- df_queens[df_queens$SALE_PRICE >= price_lower & df_queens$SALE_PRICE <= price_upper, ]


# Repeat for Gross Square Feet
# (Assuming any building under 100 sq ft is an error)
sum(df_queens$GROSS_SQUARE_FEET <= 100)
df_queens <- df_queens[df_queens$GROSS_SQUARE_FEET > 100, ]

gsf_lower <- quantile(df_queens$GROSS_SQUARE_FEET, 0.01, na.rm = TRUE)
gsf_upper <- quantile(df_queens$GROSS_SQUARE_FEET, 0.99, na.rm = TRUE)

gsf_lower
gsf_upper
df_queens <- df_queens[df_queens$GROSS_SQUARE_FEET >= gsf_lower & df_queens$GROSS_SQUARE_FEET <= gsf_upper, ]

# Filter Land Square Feet (Trimming the top 1%)
# We won't remove the lower percentile to keep Condos safe
lsf_upper <- quantile(df_queens$LAND_SQUARE_FEET, 0.99, na.rm = TRUE)
lsf_upper
df_queens <- df_queens[df_queens$LAND_SQUARE_FEET <= lsf_upper, ]

rows_after_v2 = nrow(df_queens)
rows_removed <- rows_after - rows_after_v2
rows_after_v2
rows_removed

# Plot the cleaned distributions

ggplot(df_queens, aes(y = SALE_PRICE)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 1) +
  labs(title = "Cleaned Boxplot of Sale Price in Queens", y = "Sale Price ($)") +
  theme_minimal()

ggplot(df_queens, aes(y = GROSS_SQUARE_FEET)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 1) +
  labs(title = "Cleaned Boxplot of Gross Sq Ft in Queens", y = "Gross Square Feet") +
  theme_minimal()

ggplot(df_queens, aes(y = LAND_SQUARE_FEET)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 1) +
  labs(title = "Cleaned Boxplot of Land Sq Ft in Queens", y = "Land Square Feet") +
  theme_minimal()

# Check all the years built
table(df_queens$YEAR_BUILT)


#### Regression Analysis ####

# 80/20 Train/Test Split
set.seed(42) 
train_indices <- sample(1:nrow(df_queens), size = floor(0.8 * nrow(df_queens)))
train_data <- df_queens[train_indices, ]
test_data <- df_queens[-train_indices, ]

# Model 1: Gross Square Feet
model1 <- lm(SALE_PRICE ~ GROSS_SQUARE_FEET, data = train_data)

# Model 2: Gross Square Feet + Land Square Feet
model2 <- lm(SALE_PRICE ~ GROSS_SQUARE_FEET + LAND_SQUARE_FEET, data = train_data)

# Model 3: Gross Square Feet + Land Square Feet + Year Built
model3 <- lm(SALE_PRICE ~ GROSS_SQUARE_FEET + LAND_SQUARE_FEET + YEAR_BUILT, data = train_data)

# Evaluate the Models
pred1 <- predict(model1, test_data)
pred2 <- predict(model2, test_data)
pred3 <- predict(model3, test_data)

# Calculate RMSE for each model. The lower the better.
rmse1 <- sqrt(mean((test_data$SALE_PRICE - pred1)^2))
rmse2 <- sqrt(mean((test_data$SALE_PRICE - pred2)^2))
rmse3 <- sqrt(mean((test_data$SALE_PRICE - pred3)^2))
rmse1
rmse2
rmse3
# Also check overall summary, the higher the Adjusted R-squared, the better.
summary(model1)
summary(model2)
summary(model3)


#### Classification ####

library(class)        # For kNN
library(e1071)        # For SVM and tune()
library(randomForest) # For Random Forest

# Calculate Precision, Recall, and F1
get_metrics <- function(cm) {
  classes <- rownames(cm)
  results <- data.frame(Class = classes, Precision = NA, Recall = NA, F1_Score = NA)
  
  for (i in 1:length(classes)) {
    tp <- cm[i, i]
    fp <- sum(cm[, i]) - tp
    fn <- sum(cm[i, ]) - tp
    # Precision
    if ((tp + fp) == 0) {
      precision <- 0
    } else {
      precision <- tp / (tp + fp)
    }
    
    # Recall
    if ((tp + fn) == 0) {
      recall <- 0
    } else {
      recall <- tp / (tp + fn)
    }
    
    # F1 Score
    if ((precision + recall) == 0) {
      f1 <- 0
    } else {
      f1 <- 2 * (precision * recall) / (precision + recall)
    }
    
    results[i, c("Precision", "Recall", "F1_Score")] <- c(precision, recall, f1)
  }
  return(results)
}


# Subset to the 3 specific neighborhoods
neighborhoods_to_keep <- c("ASTORIA", "LONG ISLAND CITY", "FOREST HILLS")
df_class <- df_queens[df_queens$NEIGHBORHOOD %in% neighborhoods_to_keep, ]
# Convert Neighborhood to a factor for prediction
df_class$NEIGHBORHOOD <- factor(df_class$NEIGHBORHOOD)

# Select features and scale the quantitative variables
features <- c("SALE_PRICE", "GROSS_SQUARE_FEET", "LAND_SQUARE_FEET", "YEAR_BUILT")
df_class[, features] <- scale(df_class[, features])

# Train/test split
train_indices <- sample(1:nrow(df_class), size = floor(0.8 * nrow(df_class)))
train <- df_class[train_indices, ]
test <- df_class[-train_indices, ]

X_train <- train[, features]
X_test <- test[, features]
y_train <- train$NEIGHBORHOOD
y_test <- test$NEIGHBORHOOD

# Check class distribution
table(y_train)
table(y_test)

# Model 1: KNN

# Loop to find the best k value based on accuracy
best_k <- 1
best_acc <- 0
for (k_val in seq(1, 15, 2)) {
  temp_pred <- knn(X_train, X_test, y_train, k = k_val)
  temp_acc <- mean(temp_pred == y_test)
  if (temp_acc > best_acc) {
    best_acc <- temp_acc
    best_k <- k_val
  }
}
best_k

# Train kNN with the best k
knn_pred <- knn(X_train, X_test, y_train, k = best_k)
cm_knn <- table(y_test, knn_pred)

# Model 2: Radial SVM

# Tune cost and gamma
cost.range <- c(0.01, 0.1, 1, 10, 100)
gamma.range <- seq(0.1, 10, 0.1)
tuned.svm.radial <- tune.svm(NEIGHBORHOOD ~ SALE_PRICE + GROSS_SQUARE_FEET + LAND_SQUARE_FEET + YEAR_BUILT, 
                             data = train, kernel = 'radial', cost = cost.range, gamma = gamma.range)

best.cost.rad <- tuned.svm.radial$best.parameters$cost
best.gamma.rad <- tuned.svm.radial$best.parameters$gamma

# Train optimized radial model
svm.radial <- svm(NEIGHBORHOOD ~ SALE_PRICE + GROSS_SQUARE_FEET + LAND_SQUARE_FEET + YEAR_BUILT, 
                  data = train, kernel = 'radial', cost = best.cost.rad, gamma = best.gamma.rad)

# Evaluate SVM
svm_pred <- predict(svm.radial, test)
cm_svm <- table(y_test, svm_pred)

# Model 3: Random Forest

model.rf <- randomForest(NEIGHBORHOOD ~ SALE_PRICE + GROSS_SQUARE_FEET + LAND_SQUARE_FEET + YEAR_BUILT, 
                         data = train, ntree = 100)

# Evaluate RF
rf_pred <- predict(model.rf, test)
cm_rf <- table(y_test, rf_pred)

# Final Results
cm_knn
get_metrics(cm_knn)

cm_svm
get_metrics(cm_svm)

cm_rf
get_metrics(cm_rf)
