# Define the scores
classA <- c(76, 35, 47, 64, 95, 66, 89, 36, 84, 76, 35, 47, 64, 95, 66, 89, 36, 84)
classB <- c(51, 56, 84, 60, 59, 70, 63, 66, 50, 51, 56, 84, 60, 59, 70, 63, 66, 50)
# Calculate the mean
meanA <- mean(classA)
meanB <- mean(classB)
# Calculate the median
medianA <- median(classA)
medianB <- median(classB)
# Calculate the range
rangeA <- max(classA) - min(classA)
rangeB <- max(classB) - min(classB)
# Print the results
print(paste("Mean of Class A: ", meanA))
print(paste("Mean of Class B: ", meanB))
print(paste("Median of Class A: ", medianA))
print(paste("Median of Class B: ", medianB))
print(paste("Range of Class A: ", rangeA))
print(paste("Range of Class B: ", rangeB))
# Plot the boxplot
boxplot(classA, classB, names=c("Class A", "Class B"), main="Boxplot of Scores")


# Define the data
data <- c(200, 300, 400, 600, 1000)
# Min-max normalization
min_max_normalized <- (data - min(data)) / (max(data) - min(data))
print(paste("Min-max normalized data: ", toString(min_max_normalized)))
# Z-score normalization
z_score_normalized <- (data - mean(data)) / sd(data)
print(paste("Z-score normalized data: ", toString(z_score_normalized)))


data(AirPassengers)
start_value <- 100
bin_width <- 150
bins <- seq(start_value, max(AirPassengers) + bin_width, bin_width)
hist(AirPassengers, breaks = bins, col = "skyblue", main = "Histogram of AirPassengers",
     xlab = "Number of Passengers", ylab = "Frequency")
grid()


# Load the dataset
data(mtcars)
# Create a blank plot
plot(1, 1, xlim = c(1, nrow(mtcars)), ylim = c(min(mtcars$mpg, mtcars$qsec), max(mtcars$mpg, mtcars$qsec)), type = "n", xlab = "Index", ylab = "Value", main = "Line Chart of mpg and qsec")
# Add the lines
lines(mtcars$mpg, type = "l", col = "blue")
lines(mtcars$qsec, type = "l", col = "red")
# Add a legend
legend("topright", legend = c("mpg", "qsec"), col = c("blue", "red"), lty = 1)


