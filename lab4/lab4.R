##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## read dataset
wine <- read_csv("/Users/mr.youssef/Dropbox/data-analytics/lab4/wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type

####### PCA #######
Xmat <- as.matrix(X)
Xc <- scale(Xmat, center = TRUE, scale = TRUE) 
principal_components <- princomp(Xc)

# Plot PC1 vs PC2
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0) +
  ggtitle("PCA of Wine Dataset (PC1 vs PC2)")

# Get loadings for the 1st Principal Component
loadings_pc1 <- principal_components$loadings[, 1]
# Get The 5 Highest contributors
sorted_loadings_pc1 <- sort(abs(loadings_pc1), decreasing = TRUE)
head(sorted_loadings_pc1, 5)

# 80/20 train/test split
train_index <- sample(1:nrow(wine), size = 0.8 * nrow(wine))
y.train <- Y[train_index]
y.test  <- Y[-train_index]

# Model A: Subset of 3 original variables
X.A <- Xc[, c("Alcohol", "Total phenols", "Color Intensity")]
X.A_train <- X.A[train_index, ]
X.A_test  <- X.A[-train_index, ]

# Model B: First 2 Principal Components
new_vectors <- principal_components$scores[, 1:2]
X.B_train <- new_vectors[train_index, ]
X.B_test  <- new_vectors[-train_index, ]

# Classifier: knn (k = 5)
knn_A <- knn(X.A_train, X.A_test, y.train, k = 5)
knn_B <- knn(X.B_train, X.B_test, y.train, k = 5)

# Compare both models with contingency tables and precision/recall/F1 metrics.

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

cm_a <- table(y.test, knn_A)
cm_b <- table(y.test, knn_B)

cat("\nResults Model A (original subset of 3 variables)\n")
cm_a
get_metrics(cm_a)
cat("\nResults Model B (with PCA)\n")
cm_b
get_metrics(cm_b)
