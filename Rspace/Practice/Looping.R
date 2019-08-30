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

# Get all column names in to a variable
att_colnames <- names(attitude)


# Iterate through all the columns to calculate their Measure of Central Tendancy and Measure of Variablility

for (att_col in att_colnames) {
  
  attitude_col_df <- attitude[,att_col]
  
  # Print headers
  print(paste("================================","Calculations for",toupper(att_col),"================================"), quote = FALSE)
  print("", quote = FALSE)
  print(paste("----------","Measure of Central Tendency","----------"), quote = FALSE)
  print("", quote = FALSE)
  
  # Calculate & Print Mean for each variable
  print(paste("Mean = ", mean(attitude_col_df,na.rm = TRUE)), quote = FALSE)
  
  # Calculate & Print Median for each variable
  print(paste("Median = ", median(attitude_col_df,na.rm = TRUE)), quote = FALSE)
  
  # Calculate & Print Mode for each variable
  modeforattitude <- table(as.vector(attitude_col_df))
  mode <- names(modeforattitude)[modeforattitude == max(modeforattitude)]
  print(paste("Mode = ", mode), quote = FALSE)
  
  # Print headers
  print("", quote = FALSE)
  print(paste("----------","Measure of Variability","---------------"), quote = FALSE)
  print("", quote = FALSE)
  
  # Calculate & Print Max Value for each variable
  print(paste("Max Value = ", max(attitude_col_df)), quote = FALSE)
  
  # Calculate & Print Min Value for each variable
  print(paste("Min Value = ", min(attitude_col_df)), quote = FALSE)
  
  # Calculate & Print Range for each variable
  print(paste("Range = ", range(attitude_col_df)), quote = FALSE)
  
  # Calculate & Print Quantile for each variable
  print(paste("quantile = ", quantile(attitude_col_df)), quote = FALSE)
  
  # Calculate & Print IQR for each variable
  print(paste("IQR = ", IQR(attitude_col_df)), quote = FALSE)
  
  # Calculate & Print Standard Deviation for each variable
  print(paste("Standard Deviation = ", sd(attitude_col_df)), quote = FALSE)
  
  # Calculate & Print Variance for each variable
  print(paste("Variance = ", var(attitude_col_df)), quote = FALSE)
  
  # Print headers
  print("", quote = FALSE)
  print(paste("============================================================================================="), quote = FALSE)
  print("", quote = FALSE)
}

# Calculate & Print Correlation for each variable
cor(attitude)

cor(attitude, method="spearman")

# Check your work by using the summary and/or describe functions.
summary(attitude)

describe(attitude)
