USDA = read.csv('datasets/USDA.csv')

str(USDA)
# Results in:
# 'data.frame':	7058 obs. of  16 variables:
#   $ ID          : int  1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 ...
# $ Description : chr  "BUTTER,WITH SALT" "BUTTER,WHIPPED,WITH SALT" "BUTTER OIL,ANHYDROUS" "CHEESE,BLUE" ...
# $ Calories    : int  717 717 876 353 371 334 300 376 403 387 ...
# $ Protein     : num  0.85 0.85 0.28 21.4 23.24 ...
# etc

summary(USDA)
# Results in:
# ID        Description           Calories        Protein         TotalFat       Carbohydrate   
# Min.   : 1001   Length:7058        Min.   :  0.0   Min.   : 0.00   Min.   :  0.00   Min.   :  0.00  
# 1st Qu.: 8387   Class :character   1st Qu.: 85.0   1st Qu.: 2.29   1st Qu.:  0.72   1st Qu.:  0.00  
# Median :13294   Mode  :character   Median :181.0   Median : 8.20   Median :  4.37   Median :  7.13  
# Mean   :14260                      Mean   :219.7   Mean   :11.71   Mean   : 10.32   Mean   : 20.70  
# 3rd Qu.:18337                      3rd Qu.:331.0   3rd Qu.:20.43   3rd Qu.: 12.70   3rd Qu.: 28.17  
# Max.   :93600                      Max.   :902.0   Max.   :88.32   Max.   :100.00   Max.   :100.00  
# NA's   :1       NA's   :1       NA's   :1        NA's   :1       
# etc

# Column names can be seen with names()
names(USDA)

# Find the food with the largest sodium
USDA$Description[which.max(USDA$Sodium)]  # It is table salt

# Find foods with high sodium
HighSodium = subset(USDA, Sodium>10000)
nrow(HighSodium)  # Number of rows in dataframe
HighSodium$Description

# How much sodium does caviar have
USDA$Sodium[match('CAVIAR', USDA$Description)]  # Returns 1500mg sodium in caviar

# Is that a lot compared to other foods?
# Check the summary for Sodium to get the mean.
summary(USDA$Sodium)  # You can also do summary(USDA$Sodium[4]) to get the 4th column value
                      # This value is 322mg
sd(USDA$Sodium, na.rm=TRUE)  # sd is 1045mg
# So Caviar sodium (1500mg) is larger than mean + 1 * sd = 322 + 1045 = 1367mg

plot(USDA$Protein, USDA$TotalFat, 
     xlab = 'Protein', ylab = 'Total Fat', 
     main = 'Protein vs Total Fat',
     col = 'red')

hist(USDA$VitaminC, xlab = 'Vitamin C (mg)', main = 'Vitamin C Levels')
# Most of the data concentrated around 0mg. Zoom into that area.
hist(USDA$VitaminC, xlab = 'Vitamin C (mg)', main = 'Vitamin C Levels',
     xlim = c(0, 100),  # Zoom into this x-value region
     breaks = 2000)     # Number of breaks between bins
# This shows us that most foods have 0mg of Vitamin C

USDA$Sodium[1] > mean(USDA$Sodium)
# Returns:
# [1] NA
USDA$Sodium[1] > mean(USDA$Sodium, na.rm=TRUE)
# Returns:
# [1] TRUE
USDA$HighSodium = USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE)
str(USDA$HighSodium)
# Returns:
# logi [1:7058] TRUE TRUE FALSE TRUE TRUE TRUE ...

USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
str(USDA)
# Returns:
# 'data.frame':	7058 obs. of  20 variables:
# $ ID          : int  1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 ...
# $ Description : chr  "BUTTER,WITH SALT" "BUTTER,WHIPPED,WITH SALT" "BUTTER OIL,ANHYDROUS" "CHEESE,BLUE" ...
# $ Calories    : int  717 717 876 353 371 334 300 376 403 387 ...
# $ Protein     : num  0.85 0.85 0.28 21.4 23.24 ...
# $ TotalFat    : num  81.1 81.1 99.5 28.7 29.7 ...
# $ Carbohydrate: num  0.06 0.06 0 2.34 2.79 0.45 0.46 3.06 1.28 4.78 ...
# $ Sodium      : int  714 827 2 1395 560 629 842 690 621 700 ...
# $ SaturatedFat: num  51.4 50.5 61.9 18.7 18.8 ...
# $ Cholesterol : int  215 219 256 75 94 100 72 93 105 103 ...
# $ Sugar       : num  0.06 0.06 0 0.5 0.51 0.45 0.46 NA 0.52 NA ...
# $ Calcium     : int  24 24 4 528 674 184 388 673 721 643 ...
# $ Iron        : num  0.02 0.16 0 0.31 0.43 0.5 0.33 0.64 0.68 0.21 ...
# $ Potassium   : int  24 26 5 256 136 152 187 93 98 95 ...
# $ VitaminC    : num  0 0 0 0 0 0 0 0 0 0 ...
# $ VitaminE    : num  2.32 2.32 2.8 0.25 0.26 0.24 0.21 NA 0.29 NA ...
# $ VitaminD    : num  1.5 1.5 1.8 0.5 0.5 0.5 0.4 NA 0.6 NA ...
# $ HighSodium  : num  1 1 0 1 1 1 1 1 1 1 ...
# $ HighProtein : num  0 0 0 1 1 1 1 1 1 1 ...
# $ HighFat     : num  1 1 1 1 1 1 1 1 1 1 ...
# $ HighCarbs   : num  0 0 0 0 0 0 0 0 0 0 ...

# New columns are added at the end.
# When accessing columns by location, 
# you can use USDA[2] to access the Description column.

table(USDA$HighSodium)  # With a single column, table gives count of each factor
# Results in
# 0    1 
# 4884 2090 

table(USDA$HighSodium, USDA$HighFat)  # With multiple columns, table gives count of each
                                      # combination of factors
# Results in
# 0    1
# 0 3529 1355
# 1 1378  712

tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)  # Groups by HighProtein, then applies mean
                                                       # to Iron for each group
# Results in
# 0        1 
# 2.558945 3.197294

tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)  # Groups by HighCarbs, then applies max
                                                        # to VitaminC for each group
# Results in
# 0      1 
# 1677.6 2400.0
