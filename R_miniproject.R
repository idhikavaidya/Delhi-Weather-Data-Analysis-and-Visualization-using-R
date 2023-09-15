#A.Data Preprocessing

library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For time manipulation


weather <- read_csv("C:\\Users\\HOME\\Desktop\\Delhi_Weather_data.csv")  # Replace with the actual path of the data file

#To display the weather table
View(weather) 

# Summary statistics to get an idea of the data.
summary(weather)


# data types of each columns
str(weather)


#Checking if there are any missing values or not
sum(is.na(weather))


#2.Data Analysis



# Calculate the correlation between x and y
correlation <- cor(weather$temp, weather$pressure)

# Print the correlation
print(correlation)

#Correlation matrix
num.cols <- sapply(weather,is.numeric) 
cor.data <- cor(weather[,num.cols]) 
print(cor.data)

corrplot::corrplot(cor.data)


#Scatter Plot of Air pressure and Sea Level
# Create sample data
y <- weather$sea_level
x <- weather$pressure
df <- data.frame(x, y)

# Plot the scatterplot
library(ggplot2)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(title = "Scatterplot Example", x = "Air-Pressure", y = "Sea-Level")


#To create a barplot for monthly mean temperature
# Aggregate the data by month
monthly_mean <- aggregate(weather$temp, by = list(month(weather$date)), FUN = mean)



# Create the barplot
label <-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
barplot(monthly_mean$x, names.arg = label, xlab = "Month", ylab = "Mean Temperature (C)", main = "Monthly Mean Temperature",col='red')

#Plot-1
b <- weather$sea_level
a <- weather$temp
plot(a,b,xlab="Temperature",ylab = "Sea Level",col="green")

#Plot-2
c <- weather$grnd_level
d <- weather$temp
plot(d,c,xlab="Temperature",ylab = "Ground Level",col="orange")


# Create a factor variable
category <- factor(weather$main)

# Extract the levels of the factor
lvl=levels(category)

barplot(table(category), main = "Overview of Weather", xlab = "Sky", ylab = "Frequency")


# Create a factor variable
category2 <- factor(weather$description)

# Extract the levels of the factor
lvl2=levels(category2)

barplot(table(category2), main = "Detailed Overview of Weather", xlab = "Sky", ylab = "Frequency")


# Create a box plot
boxplot(weather$clouds ~ category2, main = "Statistical Summary", xlab = "Weather Overview", ylab = "Frequency",col="red")
