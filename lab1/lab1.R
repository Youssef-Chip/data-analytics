library(readr)
library(EnvStats)
library(nortest)

# set working directory (path)
setwd("/Users/mr.youssef/Dropbox/data-analytics/lab1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# Get AIR.new and handle NAs
AIR <- epi.data$AIR.new
NAs_AIR <- is.na(AIR)
AIR.complete <- AIR[!NAs_AIR]

# Get H2O.new and handle NAs
H2O <- epi.data$H2O.new
NAs_H2O <- is.na(H2O)
H2O.complete <- H2O[!NAs_H2O]


# 1. Variable Summaries
summary(AIR.complete)
summary(H2O.complete)


# 2. Variable Boxplots
boxplot(AIR.complete, H2O.complete, names = c("AIR.new", "H2O.new"))

# 3. Histograms with Overlayed Distributions
hist(AIR.complete, prob=TRUE, main="Histogram of AIR.new")
lines(density(AIR.complete, bw="SJ"))
rug(AIR.complete)

x_air <- seq(min(AIR.complete), max(AIR.complete), length=100)
d_air <- dnorm(x_air, mean=mean(AIR.complete), sd=sd(AIR.complete))
lines(x_air, d_air, col="blue")


hist(H2O.complete, prob=TRUE, main="Histogram of H2O.new")
lines(density(H2O.complete, bw="SJ"))
rug(H2O.complete)

x_h2o <- seq(min(H2O.complete), max(H2O.complete), length=100)
d_h2o <- dnorm(x_h2o, mean=mean(H2O.complete), sd=sd(H2O.complete))
lines(x_h2o, d_h2o, col="blue")


# 4. ECDF Plots
plot(ecdf(AIR.complete), do.points=FALSE, verticals=TRUE, main="ECDF for AIR.new")
plot(ecdf(H2O.complete), do.points=FALSE, verticals=TRUE, main="ECDF for H2O.new")


# 5. QQ Plots against the normal distribution
qqnorm(AIR.complete, main="Q-Q Plot: AIR.new"); qqline(AIR.complete)
qqnorm(H2O.complete, main="Q-Q Plot: H2O.new"); qqline(H2O.complete)


# 6. QQ Plot against each other
qqplot(AIR.complete, H2O.complete, xlab = "Q-Q plot for AIR.new & H2O.new")


# 7. Normality Statistical Tests
shapiro.test(AIR.complete)
ad.test(AIR.complete)

shapiro.test(H2O.complete)
ad.test(H2O.complete)

# 8. Statistical Test for Identical Distributions
ks.test(AIR.complete, H2O.complete)
wilcox.test(AIR.complete, H2O.complete)