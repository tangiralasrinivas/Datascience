# create a vector called unemploy_rate with 12 values, one for each month in 2013. 
# The values for each month are listed below (beginning with January’s rate of 7.9)
# 7.9	7.7	7.5	7.5	7.5	7.5	7.3	7.2	7.2	7.2	7.0	6.7

unemploy_rate <- c(7.9,	7.7,	7.5,	7.5,	7.5,	7.5,	7.3,	7.2,	7.2,	7.2,	7.0,	6.7)

# create a vector called month and add 12 values, one for the name of each month in a year.
# Jan	Feb	Mar	Apr	May	Jun	July	Aug	Sep	Oct	Nov	Dec

month <- c("Jan",	"Feb",	"Mar",	"Apr",	"May",	"Jun",	"July",	"Aug",	"Sep",	"Oct",	"Nov",	"Dec")

# convert month to a factor variable

month <- as.factor(month)


# create a data frame called monthly_rate that is comprised of unemploy_rate and month.

monthly_rate <- data.frame(month, unemploy_rate)

# How would you extract the unemployment rate for March?

monthly_rate$unemploy_rate[monthly_rate$month == "Mar"]
# Answer : [1] 7.5

# Extract only those months where unemployment was below 7.5%.

monthly_rate$month[monthly_rate$unemploy_rate < 7.5]

# Answer: [1] July Aug  Sep  Oct  Nov  Dec 

# What is a factor variable? When would you want to use a factor variable?

# Answer: Factor variables are nominal variables which can be either numerics or character variables. 
# Factors can be created when the obervations are repetitive with fewer variations and there is no 
# relationships between the observations. Storing character variables as factors when required 
# enables a more efficient use of memory. 

# What is unique about a numeric variable?

# Answer: By default numerics are of type decimal/double, in most of other programming languages numerics equate to 
# whole numbers as they have a different data type like float/decimal to handle decimal points.
# R has a different way of handling Integers vs doubles. 

a <- 1
is.integer(a)
is.numeric(a)

# > a <- 1
# > is.integer(a) # Internally 1 is considered as 1.0 and by default R considers it as a nummeric and NOT an integer
# [1] FALSE
# > is.numeric(a)
# [1] TRUE

a <- 1L
is.integer(a)
is.numeric(a)

# > a <- 1L
# > is.integer(a)
# [1] TRUE
# > is.numeric(a)
# [1] TRUE


# Why would you use a data frame over a vector to store your data?

# Answer: Vector can store data of a specific type like numeric or characters or factors. They are atomic in nature. 
# where as dataframes can store different types of data in different columns but each column must have the same type of data.
# Data frame are a list of different types of vectors.
