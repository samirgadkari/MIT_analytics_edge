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
