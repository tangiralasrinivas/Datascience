# Name: Krishna Prasad
# Date: 04/06/2019

# For this assignment use a pre-loaded dataset in R named attitude.

# This is from a survey of the clerical employees of a large financial organization, 
# the data are aggregated from the questionnaires of the approximately 35 employees for each 
# of 30 (randomly selected) departments. The numbers give the percent proportion of favorable 
# responses to seven questions in each department. attitude is already pre-loaded in R. To view it, type

# View(attitude)

# Create an R script that computes the measures of central tendency and measures of variability 
# and the relationships for each of the seven variables in the attitude dataset. Use the functions below:
#   
#   mean, median, mode, max, min, range, quantile, IQR, var( ), sd( ), and cor( )
# 

library(psych)

print("List of columns in attitude dataset are:")
names(attitude)


# Using apply commands to calculate their Measure of Central Tendancy and Measure of Variablility


print("", quote = FALSE)
print(paste("----------","Measure of Central Tendency","----------"), quote = FALSE)
print("", quote = FALSE)


# Calculate & Print Mean for each variable
apply(attitude, 2, mean)

# Calculate & Print Median for each variable
apply(attitude, 2, median)

# Calculate & Print Mode for each variable
modefunc <- function(att_col){
  attitude_col_df <- attitude[, att_col]
  modeforattitude <- table(as.vector(attitude_col_df))
  mode <- names(modeforattitude)[modeforattitude == max(modeforattitude)]
  print(paste("Mode for", att_col, "="), quote = FALSE)
  print(mode, quote = FALSE)
}

invisible(lapply(colnames(attitude), modefunc)) 

# Print headers
print("", quote = FALSE)
print(paste("----------","Measure of Variability","---------------"), quote = FALSE)
print("", quote = FALSE)

# Calculate & Print Max Value for each variable
apply(attitude,2,max)

# Calculate & Print Min Value for each variable
apply(attitude,2,min)

# Calculate & Print Range for each variable
apply(attitude,2,range)

# Calculate & Print Quantile for each variable
apply(attitude,2,quantile)

# Calculate & Print IQR for each variable
apply(attitude,2,IQR)

# Calculate & Print Standard Deviation for each variable
apply(attitude,2,sd)

# Calculate & Print Variance for each variable
apply(attitude,2,var)


# Calculate & Print Correlation for each variable
cor(attitude)

cor(attitude, method="spearman")


# Check your work by using the summary and/or describe functions.
summary(attitude)

describe(attitude)