library(readr)
library(ggplot2)

epi.data <- read_csv("/Users/mr.youssef/Dropbox/data-analytics/assign2/epi_results_2024_pop_gdp.csv")

# Variable of choice = AIR.new

# 1.1 Histogram with Density
ggplot(epi.data, aes(x = AIR.new)) +
  geom_histogram(aes(y = after_stat(density)), fill = "lightblue", color = "white") +
  geom_density(color = "red") +
  labs(title = "Distribution of Air Quality", x = "Air Quality Score", y = "Density") +
  theme_minimal()

# 1.2 Box plots by Region
ggplot(epi.data, aes(x = region, y = AIR.new, fill = region)) +
  geom_boxplot() +
  labs(title = "Air Quality Scores by Region", x = "Region", y = "Air Quality Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotates labels so they don't overlap


subset1 <- epi.data[epi.data$region == "Southern Asia", ]
subset2 <- epi.data[epi.data$region == "Global West", ]
# combine them by stacking them vertically
new_data <- rbind(subset1, subset2)

# 2.1 Histograms for each region
ggplot(new_data, aes(x = AIR.new, fill = region)) +
  geom_histogram() +
  labs(title = "Air Quality Comparison: Southern Asian vs Global West", x = "Air Quality Score", y = "Count") +
  theme_minimal()

# 2.2 QQ Plot between the two subsets
qqplot(subset1$AIR.new,
       subset2$AIR.new, 
       main = "QQ Plot: Air Quality (Southern Asia vs Global West)",
       xlab = "Quantiles of Southern Asia", 
       ylab = "Quantiles of Global West")

###########################################
# 3.1

# Southern Asia

# Population vs AIR
ggplot(subset1, aes(x = log10(population), y = AIR.new)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Southern Asia: Air Quality vs Log10(Population)", 
       x = "Log10(Population)", y = "Air Quality Score") +
  theme_minimal()

# GDP vs AIR
ggplot(subset1, aes(x = log10(gdp), y = AIR.new)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Southern Asia: Air Quality vs Log10(GDP)", 
       x = "Log10(GDP)", y = "Air Quality Score") +
  theme_minimal()

# Global West

# Population vs AIR
ggplot(subset2, aes(x = log10(population), y = AIR.new)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Global West: Air Quality vs Log10(Population)", 
       x = "Log10(Population)", y = "Air Quality Score") +
  theme_minimal()

# GDP vs AIR
ggplot(subset2, aes(x = log10(gdp), y = AIR.new)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Global West: Air Quality vs Log10(GDP)", 
       x = "Log10(GDP)", y = "Air Quality Score") +
  theme_minimal()


# 3.2

# Southern Asia

# Fit Model 1: AIR Quality predicted by Population
lm1.SA <- lm(AIR.new ~ log10(population), data = subset1)
summary(lm1.SA)

# Fit Model 2: AIR Quality predicted by GDP
lm2.SA <- lm(AIR.new ~ log10(gdp), data = subset1)
summary(lm2.SA)

# Plot Residuals
plot(lm1.SA, which = 1, main = "Residuals: Southern Asia Population")
plot(lm2.SA, which = 1, main = "Residuals: Southern Asia GDP")


# Global West

# Fit Model 1: AIR Quality predicted by Population
lm1.GW <- lm(AIR.new ~ log10(population), data = subset2)
summary(lm1.GW)
# Fit Model 2: AIR Quality predicted by GDP
lm2.GW <- lm(AIR.new ~ log10(gdp), data = subset2)
summary(lm2.GW)

# Plot Residuals for Global West Models
plot(lm1.GW, which = 1, main = "Residuals: Global West Population")
plot(lm2.GW, which = 1, main = "Residuals: Global West GDP")

# For both regions, the model that uses log10(population) as the predictor
# is the better one because the p value was only statistically significant 
# using that predictor (p < 0.05 for population but >> 0.05 for GDP).
# Additionally, for both regions as well, the model that uses log10(population)
# as the predictor has a higher Multiple R-squared value than the GDP models,
# which means it explains more of the variance in Air quality.

library(class)
set.seed(42)
# 4.1

# 3 new regions
new_regions <- c("Sub-Saharan Africa", "Greater Middle East", "Former Soviet States")
knn.data <- epi.data[epi.data$region %in% new_regions, ]
knn.data$log10_pop <- log10(knn.data$population + 1) # + 1 to avoid log(0)
knn.data$log10_gdp <- log10(knn.data$gdp + 1)
# Keep necessary columns
knn.data_1 = knn.data[, c("log10_pop", "log10_gdp", "AIR.new", "region")]
knn.data_1 <- na.omit(knn.data_1)

X <- knn.data_1[, c("log10_pop", "log10_gdp", "AIR.new")]
y <- as.factor(knn.data_1$region)
# Train/test split (80% train, 20% test)
indices <- sample(1:nrow(X), 0.8 * nrow(X))
X.train <- X[indices, ]
X.test <- X[-indices, ]
y.train <- y[indices]
y.test <- y[-indices]

# Train KNN with different k
knn_3 <- knn(X.train, X.test, y.train, k = 3)
confusion_matrix_3 <- table(knn_3, y.test)
accuracy_3 <- sum(diag(confusion_matrix_3)) / sum(confusion_matrix_3)

knn_7 <- knn(X.train, X.test, y.train, k = 7)
confusion_matrix_7 <- table(knn_7, y.test)
accuracy_7 <- sum(diag(confusion_matrix_7)) / sum(confusion_matrix_7)

knn_10 <- knn(X.train, X.test, y.train, k = 10)
confusion_matrix_10 <- table(knn_10, y.test)
accuracy_10 <- sum(diag(confusion_matrix_10)) / sum(confusion_matrix_10)

# Compare accuracies
accuracy_3
accuracy_7
accuracy_10

# k = 3 has the best accuracy
accuracy_3
confusion_matrix_3

# 4.2, Train with population + gdp + H2O
knn.data_2 = knn.data[, c("log10_pop", "log10_gdp", "H2O.new", "region")]
knn.data_2 <- na.omit(knn.data_2)

X_2 <- knn.data_2[, c("log10_pop", "log10_gdp", "H2O.new")]
y_2 <- as.factor(knn.data_2$region)

indices <- sample(1:nrow(X_2), 0.8 * nrow(X_2))
X_2.train <- X_2[indices, ]
X_2.test <- X_2[-indices, ]
y_2.train <- y_2[indices]
y_2.test <- y_2[-indices]

knn_second <- knn(X_2.train, X_2.test, y_2.train, k = 3)
confusion_matrix <- table(knn_second, y_2.test)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

accuracy
confusion_matrix

# The second model has a higher accuracy and thus performs better. It seems that the 
# quality of drinking water and sanitation is a better predictor than the air quality
# for predicting the region.

