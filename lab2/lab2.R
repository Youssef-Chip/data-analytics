library("ggplot2")
library("readr")

# Read dataset
dataset <- read_csv("/Users/mr.youssef/Dropbox/data-analytics/lab2/NY-House-Dataset.csv")

# Data Cleaning
dataset <- dataset[dataset$PRICE < 195000000, ]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862, ]

# Model 1
model1 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), data = dataset)
summary(model1)

# Plot Significant Variable vs Price
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

# Residual Plot
ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue")

# Model 2
model2 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS, data = dataset)
summary(model2)

# PROPERTYSQFT remains the most significant variable, it's t value is the highest

# Residual Plot
ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue")

# Model 3
model3 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS + BATH, data = dataset)
summary(model3)

# PROPERTYSQFT remains the most significant variable, it's t value is the highest

# Residual Plot
ggplot(model3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue")

# Model 3 is the most useful because it has the highest Adjusted R-squared (0.6047) 
# and the lowest Residual Standard Error (0.2813)
