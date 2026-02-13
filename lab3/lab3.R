#################################### Exercise 1 #################################### 
library(class)
df <- read.csv("/Users/mr.youssef/Dropbox/data-analytics/lab3/abalone.csv", header = FALSE)
colnames(df) <- c("Sex", "Length", "Diameter", "Height", "WholeW", "ShuckedW", "VisceraW", "ShellW", "Rings")
# Add target
df$age.group <- cut(df$Rings, br=c(0,8,11,35), labels = c("young", "adult", "old"))

# Split Training/Test
s.train <- sample(nrow(df), 0.7 * nrow(df))
df.train <- df[s.train,]
df.test <- df[-s.train,]

# Model A: Physical dimensions: Length, Diameter, Height
knnA <- knn(train = df.train[, 2:4], test = df.test[, 2:4], cl = df.train$age.group, k = 5)

# Model B: Weight: Whole, Shucked, Viscera, Shell
knnB <- knn(train = df.train[, 5:8], test = df.test[, 5:8], cl = df.train$age.group, k = 5)

# Compare using Contingency Tables
table_A <- table(knnA, df.test$age.group, dnn=list('predicted','actual'))
table_B <- table(knnB, df.test$age.group, dnn=list('predicted','actual'))

# Diagonals are true positives and true negatives ==> Correct predictions
accuracy_A <- sum(diag(table_A)) / sum(table_A)
accuracy_B <- sum(diag(table_B)) / sum(table_B)

round(accuracy_A, 4)
round(accuracy_B, 4)

# The better performing model is B
# Find Optimal k for model B
krange <- 1:20
accuracies <- c()

for (k in krange) {
  pred <- knn(train = df.train[, 5:8], test = df.test[, 5:8], cl = df.train$age.group, k = k)
  accuracy <- sum(pred == df.test$age.group) / nrow(df.test)
  accuracies <- c(accuracies, accuracy)
}

plot(krange, accuracies, type="b", main="Accuracy vs k (Model B)")
best_k <- krange[which.max(accuracies)]
best_accuracy <- max(accuracies)
best_k
round(best_accuracy, 4)

#################################### Exercise 2 #################################### 
library(cluster)
library(factoextra)

cluster_data <- df[, 5:8] # Using features from Model B

# Finding Optimal k for K-Means and PAM
k_values <- 2:10 # 2 to 10 clusters
km.sil <- c()
pam.sil <- c()

d_matrix <- dist(cluster_data)
for (k in k_values) {
  # K-Means
  km <- kmeans(cluster_data, centers = k)
  km.sil <- c(km.sil, mean(silhouette(km$cluster, d_matrix)[, 3]))
  # PAM
  pam_res <- pam(cluster_data, k, diss = TRUE)
  pam.sil <- c(pam.sil, mean(silhouette(pam_res$cluster, d_matrix)[, 3]))
}

# Optimal k
optimal.k.km <- k_values[which.max(km.sil)]
optimal.k.pam <- k_values[which.max(pam.sil)]
optimal.k.km
optimal.k.pam

final.km <- kmeans(cluster_data, centers = optimal.k.km)
final.pam <- pam(cluster_data, optimal.k.pam)

# Silhouette Plots
fviz_silhouette(silhouette(final.km$cluster, dist(cluster_data))) + labs(title = "K-Means Silhouette")
fviz_silhouette(silhouette(final.pam$cluster, dist(cluster_data))) + labs(title = "PAM Silhouette")


