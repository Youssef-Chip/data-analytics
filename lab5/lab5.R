library(readr)
library(randomForest)
library(e1071)

## read dataset
wine <- read_csv("/Users/mr.youssef/Dropbox/data-analytics/lab5/wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total.phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color.Intensity","Hue","Od280/od315 of diluted wines","Proline")

# Convert target variable to Factor for classification
wine$Type <- as.factor(wine$Type)

set.seed(1)
n <- nrow(wine)
train.indexes <- sample(n, 0.8 * n)

train <- wine[train.indexes,]
test <- wine[-train.indexes,]


# Predict type of wine using the following variables:
# Alcohol, Total.phenols, Color.Intensity

# Model 1: SVM with linear kernel
# Tune cost parameter
cost.range <- c(0.01, 0.1, 1, 10, 100)
tuned.svm.linear <- tune.svm(Type ~ Alcohol + Total.phenols + Color.Intensity, data = train, 
                             kernel = 'linear', cost = cost.range)

best.cost.linear <- tuned.svm.linear$best.parameters$cost

# Train optimized linear model
svm.linear <- svm(Type ~ Alcohol + Total.phenols + Color.Intensity, data = train, 
                  kernel = 'linear', cost = best.cost.linear)
# Evaluate
test.pred.linear <- predict(svm.linear, test)
cm.svm.linear = table(test$Type, test.pred.linear)


# Model 2: SVM with radial kernel
# Tune cost and gamma
gamma.range <- seq(0.1, 10, 0.1)
tuned.svm.radial <- tune.svm(Type ~ Alcohol + Total.phenols + Color.Intensity, data = train, 
                             kernel = 'radial', cost = cost.range, gamma = gamma.range)

best.cost.rad <- tuned.svm.radial$best.parameters$cost
best.gamma.rad <- tuned.svm.radial$best.parameters$gamma

# Train optimized radial model
svm.radial <- svm(Type ~ Alcohol + Total.phenols + Color.Intensity, data = train, 
                  kernel = 'radial', cost = best.cost.rad, gamma = best.gamma.rad)

# Evaluate
test.pred.rad <- predict(svm.radial, test)
cm.svm.rad = table(test$Type, test.pred.rad)


# Model 3: Random forest classifier
model.rf <- randomForest(Type ~ Alcohol + Total.phenols + Color.Intensity, data = train, ntree = 100)
test.pred.rf <- predict(model.rf, test)
cm.rf = table(test$Type, test.pred.rf)


# Compare performances

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

cat("\nResults of SVM with Linear Kernel\n")
cm.svm.linear

get_metrics(cm.svm.linear)
cat("\nResults of SVM with Radial Kernel\n")
cm.svm.rad
get_metrics(cm.svm.rad)
cat("\nResults of Random Forest\n")
cm.rf
get_metrics(cm.rf)

