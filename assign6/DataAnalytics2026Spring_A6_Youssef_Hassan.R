library(corrplot)
library(randomForest)
library(e1071)
library(readr)
library(ggplot2)
library(tidyr)
library(ggfortify)
library(cluster)
library(factoextra)

# ==========================
# Exploratory Data Analysis
# ==========================

df <- read_csv("/Users/mr.youssef/Dropbox/data-analytics/assign6/sobar-72.csv")

# View structure and summary
str(df)
summary(df)

# Check for missing values 
sum(is.na(df))

# We convert the target variable to a factor so the models know this is a 
# classification problem and not a regression problem
df$ca_cervix <- factor(df$ca_cervix, levels = c(0, 1), labels = c("No", "Yes"))

# Target Variable Distribution
ggplot(df, aes(x = ca_cervix, fill = ca_cervix)) +
  geom_bar(color = "black", width = 0.6) +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  labs(title = "Distribution of Cervical Cancer Diagnosis", 
       x = "Cervical Cancer Diagnosis", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Since we have 19 features, we're going to reshape the data to have
# less columns and more rows for easier plotting (the data itself stays the same)
plot_df <- df %>%
  pivot_longer(
    cols = 1:19, 
    names_to = "Feature", 
    values_to = "Value"
  )

#  Histograms of all independent variables
ggplot(plot_df, aes(x = Value)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 10) +
  facet_wrap(~ Feature, scales = "free") +
  labs(title = "Histograms of Independent Variables", x = "Value", y = "Frequency") +
  theme_minimal()

# Boxplots to check for Outliers, split by diagnosis
ggplot(plot_df, aes(x = ca_cervix, y = Value, fill = ca_cervix)) +
  geom_boxplot(color = "black") +
  facet_wrap(~ Feature, scales = "free_y") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  labs(title = "Feature Distributions by Diagnosis", x = "Diagnosis", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

# Correlation Matrix
cor_matrix <- cor(df[, 1:19])
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, 
         title = "Feature Correlation Matrix", mar = c(0, 0, 1, 0))

# =========================================================================
# MODEL DEVELOPMENT, VALIDATION AND OPTIMIZATION
# =========================================================================

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


set.seed(42)
# Train/Test Split
n_rows <- nrow(df)
train_indices <- sample(n_rows, 0.75 * n_rows)

train_data <- df[train_indices, ]
test_data  <- df[-train_indices, ]

# =============================================
# MODEL 1: Logistic Regression (Classification)
# =============================================

# Train using all features
model_glm <- glm(ca_cervix ~ ., data = train_data, family = binomial)

# Predict probabilities on the test data
pred_glm_prob <- predict(model_glm, newdata = test_data, type = "response")

# Convert probabilities to Yes/No class labels based on a 0.5 threshold
pred_glm_class <- factor(ifelse(pred_glm_prob > 0.5, "Yes", "No"), levels = c("No", "Yes"))

cm_a <- table(Actual = test_data$ca_cervix, Predicted = pred_glm_class)
get_metrics(cm_a)

# ==========================================
# MODEL 2: Random Forest (Classification)
# ==========================================

# Train using all features
model_rf <- randomForest(ca_cervix ~ ., data = train_data, importance = TRUE)

pred_rf <- predict(model_rf, newdata = test_data)

cm_b <- table(Actual = test_data$ca_cervix, Predicted = pred_rf)
get_metrics(cm_b)

# Feature Importance Plot
imp_df <- data.frame(
  Feature = rownames(importance(model_rf)),
  Importance = importance(model_rf)[, "MeanDecreaseGini"]
)

ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "forestgreen", color = "black", width = 0.7) +
  coord_flip() +
  labs(title = "Random Forest Feature Importance", 
       x = "Features", 
       y = "Importance") +
  theme_minimal()

# =========================================
# MODEL 3: SVM with PCA Dimension Reduction
# =========================================

Xmat <- as.matrix(train_data[, 1:19])
Xc <- scale(Xmat, center = TRUE, scale = TRUE) 
principal_components <- princomp(Xc)

# Plot PC1 vs PC2
# We use 'ca_cervix' to color the points to see if the classes separate natively
autoplot(principal_components, data = train_data, colour = 'ca_cervix',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0) +
  ggtitle("PCA of Sobar-72 Dataset (PC1 vs PC2)") +
  scale_color_manual(values = c("skyblue", "salmon")) +
  theme_minimal()

# Get loadings for the 1st Principal Component
loadings_pc1 <- principal_components$loadings[, 1]

# Get The 5 Highest contributors to PC1
sorted_loadings_pc1 <- sort(abs(loadings_pc1), decreasing = TRUE)
print(head(sorted_loadings_pc1, 5))

# Prepare PCA data for the SVM Model
# Extract the first 5 principal components (scores) from the training set
train_pca_data <- data.frame(principal_components$scores[, 1:5], ca_cervix = train_data$ca_cervix)

# Train the SVM with the 5 principal components
model_svm <- svm(ca_cervix ~ ., data = train_pca_data, kernel = "radial")

# Project the Test Data into the PCA space
# We must scale the test data using the exact same center and scale from the training data
test_Xmat <- as.matrix(test_data[, 1:19])
test_Xc <- scale(test_Xmat, 
                 center = attr(Xc, "scaled:center"), 
                 scale = attr(Xc, "scaled:scale"))

# Predict PCs for the test set
test_pca <- predict(principal_components, newdata = test_Xc)
test_pca_data <- data.frame(test_pca[, 1:5])

# Predict actual outcomes with SVM and evaluate
pred_svm <- predict(model_svm, newdata = test_pca_data)


cm_c <- table(Actual = test_data$ca_cervix, Predicted = pred_svm)
get_metrics(cm_c)

# ============================
# MODEL 4: K-Means Clustering
# ============================

# We drop the target variable entirely
features_only <- df[, 1:19]
features_scaled <- scale(features_only)

# Group into 2 clusters (to mirror our 2 diagnostic classes)
model_kmeans <- kmeans(features_scaled, centers = 2, nstart = 25)

# Calculate the distance matrix for the scaled data
dist_matrix <- dist(features_scaled)

# Plot silhouette
sil_info <- silhouette(model_kmeans$cluster, dist_matrix)
fviz_silhouette(sil_info, 
                palette = c("skyblue", "salmon"), 
                ggtheme = theme_minimal()) +
  labs(title = "Silhouette Plot for K-Means Clustering (k=2)")
