# Load Libraries
install.packages(c("ggplot2", "dplyr", "corrplot", "caret", "ggpubr"))

library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(ggpubr)

# Load Built-in Dataset
data(mtcars)
df <- mtcars
df$cyl <- as.factor(df$cyl)  # Convert to categorical for better visualization

# Summary Statistics
summary(df)

# Visualization 1: Histogram of MPG
ggplot(df, aes(x = mpg, fill = cyl)) +
  geom_histogram(bins = 15, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Miles Per Gallon", x = "MPG", y = "Count")

# Visualization 2: Scatterplot of Horsepower vs MPG
ggplot(df, aes(x = hp, y = mpg, color = cyl)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Horsepower vs MPG", x = "Horsepower", y = "Miles Per Gallon")

# Visualization 3: Boxplot of MPG by Cylinders
ggplot(df, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "MPG by Cylinder Count", x = "Cylinders", y = "MPG")

# Visualization 4: Correlation Heatmap

# Machine Learning: Predict MPG using HP & WT
set.seed(123)
trainIndex <- createDataPartition(df$mpg, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

lm_model <- lm(mpg ~ hp + wt, data = train_data)
predictions <- predict(lm_model, test_data)
rmse <- sqrt(mean((test_data$mpg - predictions)^2))

print(paste("Model RMSE:", rmse))

# Regression Line Visualization
ggplot(df, aes(x = hp, y = mpg)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Regression Line: MPG vs HP", x = "Horsepower", y = "Miles Per Gallon")
