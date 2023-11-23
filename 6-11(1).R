age <- c(23,23,27,27,39,41,47,49,52,54,54,56,57,58,58,61)
fat <- c(9.5,26.5,7.8,17.8,31.4,25.9,27.4,27.2,34.6,42.5,28.8,33.4,30.2,34.1,32.9,35.7)
mean_age <- mean(age)
median_age <- median(age)
sd_age <- sd(age)
mean_fat <- mean(fat)
median_fat <- median(fat)
sd_fat <- sd(fat)
print(paste("Mean of age: ",mean_age))
print(paste("Median of age: ",median_age))
print(paste("Standard deviation of age: ",sd_age))
print(paste("Mean of %fat: ",mean_fat))
print(paste("Median of %fat: ",median_fat))
print(paste("Standard deviation of %fat: ",sd_fat))
boxplot(age,main="Boxplot of Age")
boxplot(fat,main="Boxplot of %Fat")
plot(age,fat,main="Scatter Plot of Age vs %Fat",xlab="Age",ylab="%Fat")
qqplot(age,fat,main="Q-Q Plot of Age vs %Fat", xlab="Age",ylab="%Fat")

x <- c(4,1,5,7,10,2,50,25,90,36)
y <- c(12,5,13,19,31,7,153,72,275,110)
plot(x,y,main="Scatter Plot",xlab="Number of Mobile Phones Sold",ylab="Money",pch=19)


marks <- c(55,60,71,63,55,65,50,55,58,59,61,63,65,67,71,72,75)
equal_freq_bins <- cut(marks,breaks=quantile(marks,probs=seq(0,1,by=1/3)),include.lowest=TRUE)
freq_equal_freq <- table(equal_freq_bins)
barplot(freq_equal_freq,main="Equal-frequency(equi-depth)partitioning",xlab="Marks",ylab="Frequency")
equal_width_bins <- cut(marks,breaks=seq(min(marks),max(marks),length.out=4),include.lowest=TRUE)
freq_equal_width <- table(equal_width_bins)
barplot(freq_equal_width,main="Equal-width partitioning",xlab="Marks",ylab="Frequency")


speed <- c(78.3,81.8,82,74.2,83.4,84.5,82.9,77.5,80.9,70.6)
Q1 <- quantile(speed, 0.25)
Q3 <- quantile(speed, 0.75)
IQR <- Q3 - Q1
sd <- sd(speed)
print(paste("Interquartile Range (IQR): ", IQR))
print(paste("Standard Deviation: ", sd))


speed <- c(78.3,81.8,82,74.2,83.4,84.5,82.9,77.5,80.9,70.6)
Q1 <- quantile(speed, 0.25)
Q3 <- quantile(speed, 0.75)
IQR <- Q3 - Q1
sd <- sd(speed)
print(paste("Interquartile Range (IQR): ", IQR))
print(paste("Standard Deviation: ", sd))

age <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,30,33,33,35,35,35,35,36,40,45,46,52,70)
Q1 <- quantile(age,0.25)
Q3 <- quantile(age,0.75)
print(paste("First Quartile (Q1): ",Q1))
print(paste("Third Quartile (Q3): ",Q3))


age <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,30,33,33,35,35,35,35,36,40,45,46,52,70)
Q1 <- quantile(age,0.25)
Q3 <- quantile(age,0.75)
print(paste("First Quartile (Q1): ",Q1))
print(paste("Third Quartile (Q3): ",Q3))
age <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,30,33,33,35,35,35,35,36,40,45,46,52,70)
Q1 <- quantile(age,0.25)
Q3 <- quantile(age,0.75)
print(paste("First Quartile (Q1): ",Q1))
print(paste("Third Quartile (Q3): ",Q3))


pencils <- c(9, 25, 23, 12, 11, 6, 7, 8, 9, 10)
mean_value <- mean(pencils)
median_value <- median(pencils)
get_mode <- function(x){
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
mode_value <- get_mode(pencils)
cat("Mean: ",mean_value,"\n")
cat("Median: ",median_value,"\n")
cat("Mode: ",mode_value,"\n")

data <- c(11,13,13,15,15,16,19,20,20,20,21,21,22,23,24,30,40,45,45,45,71,72,73,75)
bin_size <- 3
bin_smooth <- function(x,bin_size,FUN){
  n <- length(x)
  smoothed_data <- numeric(0)
  for(i in seq(1,n,bin_size)){
    bin <- x[i:min(i+bin_size-1,n)]
    value <- FUN(bin)
    smoothed_data <- c(smoothed_data,rep(value,length(bin)))
  }
  return(smoothed_data)
}
smoothed_mean <- bin_smooth(data,bin_size,mean)
smoothed_median <- bin_smooth(data,bin_size,median)
smoothed_boundaries <- bin_smooth(data, bin_size, function(x) c(min(x), max(x)))
cat("Original Data:", data, "\n\n")
cat("Smoothed by Bin Mean:", smoothed_mean, "\n")
cat("Smoothed by Bin Median:", smoothed_median, "\n")
cat("Smoothed by Bin Boundaries:", smoothed_boundaries,"\n")

                                  
