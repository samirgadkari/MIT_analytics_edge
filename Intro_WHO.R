# Intro to R
getwd()              # Find the working directory
setwd('~/datasets')  # Change to given working directory
c(1, 2, 200)    # Creates a vector with same-typed-values. c() is for concatenate.
seq(1, 100, 2)  # Creates a vector with values from 1 to 100 in steps of 2
which.min(c(1, 90, -1))  # Index of element that has lowest value
ls()                 # List the variables in memory
rm(V)                # Remove variable V from memory

Countries = c('Brazil', 'India')
Populations = c(109, 130)
FirstDataframe = data.frame(Countries, Populations)  # Create dataframe with given columns.
                                                     # Column names are the names of the
                                                     # given variables.
FirstDataframe$LifeExpectancy = c(80, 65)  # Add another column named LifeExpectancy to df
FirstDataframe[1]  # Select first column of df

# Create another dataframe
Countries = c('USA', 'Switzerland')
Populations = c(400, 50)
LifeExpectancy = c(70, 90)
SecondDataframe = data.frame(Countries, Populations, LifeExpectancy)

# Concatenate rows from two dataframes into one dataframe
FinalDataframe = rbind(FirstDataframe, SecondDataframe)  # rbind for concatenating rows

WHO = read.csv('datasets/WHO.csv')  # Create dataframe from CSV file
str(WHO)      # Shows structure of df
# Results in:
# 'data.frame':	194 obs. of  13 variables:
# $ Country                      : chr  "Afghanistan" "Albania" "Algeria" "Andorra" ...
# $ Region                       : chr  "Eastern Mediterranean" "Europe" "Africa" "Europe" ...
# $ Population                   : int  29825 3162 38482 78 20821 89 41087 2969 23050 8464 ...
# $ Under15                      : num  47.4 21.3 27.4 15.2 47.6 ...

summary(WHO)  # Shows summary of df
# Results in:
# Country             Region            Population         Under15          Over60     
# Length:194         Length:194         Min.   :      1   Min.   :13.12   Min.   : 0.81  
# Class :character   Class :character   1st Qu.:   1696   1st Qu.:18.72   1st Qu.: 5.20  
# Mode  :character   Mode  :character   Median :   7790   Median :28.65   Median : 8.53  
# Mean   :  36360   Mean   :28.73   Mean   :11.16  
# 3rd Qu.:  24535   3rd Qu.:37.75   3rd Qu.:16.69  
# Max.   :1390000   Max.   :49.99   Max.   :31.92

WHO_Europe = subset(WHO, Region == 'Europe')  # Select part of df
str(WHO_Europe)
summary(WHO_Europe)

write.csv(WHO_Europe, 'datasets/WHO_Europe.csv')  # Write df to csv file
rm(WHO_Europe)  # Remove given variable from memory

mean(WHO$Under15)  # Mean of the Under15 column of WHO df
sd(WHO$Under15)    # Standard Deviation of the Under15 column of WHO df
which.min(WHO$Under15)  # Index of value that is the minumum of the Under15 column of WHO df
which.max(WHO$Under15)  # Index of value that is the maximum of the Under15 column of WHO df
WHO$Country[86]    # Shows the 86th value of the Country column of WHO df

plot(WHO$GNI, WHO$FertilityRate)  # Scatter plot given x and y variables
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)  # Select part of df
nrow(Outliers)  # Number of rows in df
Outliers[c('Country', 'GNI', 'FertilityRate')]  # Select columns of df

hist(WHO$CellularSubscribers)  # Plot histogram of column of df
boxplot(WHO$LifeExpectancy ~ WHO$Region)  # Boxplot of LifeExpectancy as y against Region
boxplot(WHO$LifeExpectancy ~ WHO$Region, 
        xlab='', ylab='Life Expectancy',  # x and y labels
        main='Life Expectancy of Countries by Region')  # Title of plot

table(WHO$Region)  # Count number of values for each given label
# Results in:
# Africa              Americas Eastern Mediterranean                Europe 
# 46                    35                    22                    53 
# South-East Asia       Western Pacific 
# 11                    27

tapply(WHO$Over60, WHO$Region, mean)  # Group by Region, and apply mean on the Over60 col
tapply(WHO$Over60, WHO$Region, min, na.rm=TRUE)  # Group by Region, and apply min on the
                                                 # Over60 col. If any value is missing,
                                                 # result will be all NA. So use
                                                 # na.rm=TRUE to remove missing values
                                                 # from column.
