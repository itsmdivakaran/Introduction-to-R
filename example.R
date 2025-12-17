# House Price Prediction - Load Training Data

# Load necessary library
library(readr)
library(ggplot2)
library(corrplot)
library(dplyr)
library(DataExplorer)


# Load the training data from CSV file
train_data <- read_csv("~/Downloads/train data.csv")
# Display the first few rows of the training data
head(train_data)
# Display the structure of the training data
str(train_data)
# Display summary statistics of the training data
summary(train_data)

# Check for missing values in the training data
missing_values <- sapply(train_data, function(x) sum(is.na(x)))
print(missing_values)

# Correlation Plot of the Variables
numeric_vars <- sapply(train_data, is.numeric)
corr_matrix <- cor(train_data[, numeric_vars], use = "pairwise.complete.obs")
corrplot(corr_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Apply variables as factors
train_data$MSZoning <- as.factor(train_data$MSZoning)
train_data$Street <- as.factor(train_data$Street)
train_data$Alley <- as.factor(train_data$Alley)
train_data$LotShape <- as.factor(train_data$LotShape)   
train_data$LandContour <- as.factor(train_data$LandContour)
train_data$Utilities <- as.factor(train_data$Utilities)
train_data$LotConfig <- as.factor(train_data$LotConfig)



ggplot(train_data, aes(SalePrice)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Sale Price")

ggplot(train_data, aes(log1p(SalePrice))) +
  geom_histogram(bins = 30, fill = "tomato", color = "white") +
  labs(title = "Log-Transformed SalePrice")

plot_missing(train_data)

sapply(train_data, function(x) sum(is.na(x))) %>% sort(decreasing = TRUE) %>% head(20)