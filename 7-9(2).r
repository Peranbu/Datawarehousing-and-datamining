# Load the dataset
data(water)
# Plot the data
plot(water$hardness, water$mortality, main = "Mortality vs. Hardness", xlab = "Hardness", ylab = "Mortality")
# Fit a linear regression model
model <- lm(mortality ~ hardness, data = water)
# Add the regression line to the plot
abline(model, col = "red")
# Predict the mortality for a hardness of 88
predicted_mortality <- predict(model, newdata = data.frame(hardness = 88))
print(paste("Predicted mortality for hardness = 88: ", predicted_mortality))

# Load the dataset
data(mtcars)
# Create the boxplot
boxplot(mpg ~ cyl, data = mtcars, main = "Boxplot of MPG by Number of Cylinders", xlab = "Number of Cylinders", ylab = "Miles Per Gallon")


# Assume these are the points scored by each player in a series of matches
points_scored <- c(15, 20, 22, 18, 25, 30, 19, 21, 60, 23, 17, 16, 24)
# Create the boxplot
boxplot(points_scored, main = "Boxplot of Points Scored", ylab = "Points Scored")

