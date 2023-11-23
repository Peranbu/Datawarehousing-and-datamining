age_group <- c(5.5,7.5,9.5)
preference_B <- c(18,22,20)
preference_C <- c(2,28,40)
cov_B_C <- cov(preference_B, preference_C)
print(paste("Covariance between B and C: ", cov_B_C))
cov_matrix <- cov(cbind(preference_B, preference_C))
print("Covariance matrix for the preferences:")
print(cov_matrix)
cor_B_C <- cor(preference_B, preference_C)
print(paste("Correlation between B and C: ", cor_B_C))
cor_matrix <- cor(cbind(preference_B, preference_C))
print("Correlation matrix for the preferences:")
print(cor_matrix)


intervals <- c("1-5","5-15","15-20","20-50","50-80","80-110")
frequencies <- c(200,450,300,1500,700,44)
cumulative_freq <- cumsum(frequencies)
total_freq <- sum(frequencies)
median_interval_index <- which(cumulative_freq>= total_freq/2)[1]
median_interval <- strsplit(intervals[median_interval_index],"-")[[1]]
lower_bound <- as.numeric(median_interval[1])
upper_bound <- as.numeric(median_interval[2])
median_value <- lower_bound + (total_freq /2 -cumulative_freq[median_interval_index -1]) / frequencies[median_interval_index]*(upper_bound - lower_bound)
median_value

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


age_value <- 35
std_dev_age <- 12.94
min_age <- 15
max_age <- 38
mean_age <- 8
m1 <- (age_value - min_age) / (max_age - min_age)
m1
m2 <- (age_value - mean_age) / std_dev_age
m2
m3 <- age_value / (10 ^ (ceiling(log10(age_value))))
m3

 data <- c(200, 300, 400, 600, 1000)
min_max_normalize <- function(x){(x-min(x))/(max(x)-min(x))
}
min_max_normalized_data <- min_max_normalize(data)
z_score_normalize <- function(x){(x-mean(x))/sd(x)
}
z_score_normalized_data <- z_score_normalize(data)
cat("(a) Min-Max Normalized Data:",min_max_normalized_data,"\n")
cat("(b) Z-Score Normalized Data:",z_score_normalized_data,"\n")

age_data <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,30,33,33,35,35,35,35,36,40,45,46,52,70)
mean_age <- mean(age_data)
median_age <- median(age_data)
mode_result <- table(age_data)
modes <- as.numeric(names(mode_result[mode_result == max(mode_result)]))
modality <- length(modes)
midrange <- (max(age_data) + min(age_data)) /2
q1 <- quantile(age_data, 0.25)
q3 <- quantile(age_data, 0.75)
cat("mean:",mean_age,"\n")
cat("Median:",median_age,"\n")
cat("Mode(s):",modes,"\n")
cat("modality:",modality,"\n")
cat("midrange:",midrange,"\n")
cat("first Quartile(Q1):",q1,"\n")
cat("Third Quartile(Q3):",q3,"\n")

data <- c(1,1,5,18,20,20,20,20,20,20,20,21,21,21,21,25,25,25,25,25,28,28,30,10,10,10,10,12,14,14,14,15,15,15,15,15,15,18,18,18,18,18,5,5,5,5,8,8)
data <- sort(data)
bins <- 3
bin_indices <- cut(data,breaks=bins,labels=FALSE,include.lowest=TRUE)
bin_means <- tapply(data, bin_indices, mean)
smoothed_data_means <- bin_means[bin_indices]
bin_mins <- tapply(data, bin_indices, min)
bin_maxs <- tapply(data,bin_indices,max)
smoothed_data_boundaries <- ifelse(data-bin_mins[bin_indices]<bin_maxs[bin_indices]-data,bin_mins[bin_indices],bin_maxs[bin_indices])
hist(data,breaks=bins,main="Histogram for Frequency Division",xlab="Data",ylab ="Frequency")

