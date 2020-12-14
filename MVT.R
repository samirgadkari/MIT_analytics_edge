# Motor Vehicle Theft dataset processing.
# This dataset contains:
# ID: a unique identifier for each observation.
# Date: the date the crime occurred.
# LocationDescription: the location where the crime occurred.
# Arrest: whether or not an arrest was made for the crime (TRUE if an arrest was made, 
#   and FALSE if an arrest was not made).
# Domestic: whether or not the crime was a domestic crime, meaning that it was committed 
#   against a family member (TRUE if it was domestic, and FALSE if it was not domestic).
# Beat: the area, or "beat" in which the crime occurred. This is the smallest 
#   regional division defined by the Chicago police department.
# District: the police district in which the crime occured. Each district is composed of 
#   many beats, and are defined by the Chicago Police Department.
# CommunityArea: the community area in which the crime occurred. Since the 1920s, 
#   Chicago has been divided into what are called "community areas", of which there are now 77. 
#   The community areas were devised in an attempt to create socially homogeneous regions.
# Year: the year in which the crime occurred.
# Latitude: the latitude of the location at which the crime occurred.
# Longitude: the longitude of the location at which the crime occurred.

mvt = read.csv('../large_datasets/mvtWeek1.csv')

# Getting information from the dataframe
str(mvt)
summary(mvt)

# Getting specific information
table(mvt$LocationDescription)['ALLEY']  # How many locations are 'ALLEY'
# Results in
# ALLEY 
# 2308 

# This converts the variable "Date" into a Date object in R.
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvt$Month = months(DateConvert)     # Get months from date and add it to dataframe
mvt$Weekday = weekdays(DateConvert) # Get weekdays from date and add it to dataframe
mvt$Date = DateConvert              # Replace Date in dataframe with the converted dates

# To find the month with the smallest number of thefts
> table(mvt$Month)
# Returns
# April    August  December  February   January      July      June     March       May  November 
# 15280     16572     16426     13511     16047     16801     16002     15758     16035     16063 
# October September 
# 17086     16060 
min(table(mvt$Month))
# Returns
# [1] 13511
which.min(table(mvt$Month))
# Returns February 
# 4 

which.max(table(mvt$Weekday))  # Finds the weekday with most vehicle thefts
# Returns 
# Friday 
# 1

# Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Arrest, mvt$Month)  # Shows the table of arrests in each month
# Results in
#       April August December February January  July  June March   May November October September
# FALSE 14028  15243    15029    12273   14612 15477 14772 14460 14848    14807   15744     14812
# TRUE   1252   1329     1397     1238    1435  1324  1230  1298  1187     1256    1342      1248
# From this we see that in January we get the largest number of arrests for
# vehicle theft.

# Methods to save plots to files (from https://www.stat.berkeley.edu/~s133/saving.html):
# 1.
#    jpeg('rplot.jpg')  # Start driver with output filename
#    plot(x,y)          # Create plot. You will not see plot - commands to create it will go to driver.
#    dev.off()          # This is required, since it is the function that writes to file.
# 2.
#    plot(x, y)                  # Create your plot, make sure it is the one you want.
#    dev.copy(png,'myplot.png')  # Copy plot to driver
#    dev.off()                   # Write plot to file
# 3. This method is for Windows OS, which I don't have, so I did not work with it.

# Split Dates of crime by arrest and plot a boxplot
boxplot(mvt$Date ~ mvt$Arrest, 
        xlab = 'Arrest', ylab = 'Date', 
        main = 'Boxplot of dates sorted by arrests', 
        col = 'green')  # Color of green is used to fill in the boxes in the boxplot.

# Find the proportion of arrests made to crimes in certain years
table(mvt$Arrest, mvt$Year)
# Returns
#        2001  2002  2003  2004  2005  2006  2007  2008  2009  2010  2011  2012
# FALSE 18517 16638 14859 15169 14956 14796 13068 13425 11327 14796 15012 13542
# TRUE   2152  2115  1798  1693  1528  1302  1212  1020   840   701   625   550
2152/(18517 + 2152)  # Proportion of arrests made in 2001
# Resutls in
# [1] 0.1041173
1212/(1212+13068)    # Proportion of arrests made in 2007
# Resutls in
# [1] 0.08487395
550/(550+13542)      # Proportion of arrests made in 2012
# Resutls in
# [1] 0.03902924

# To find the popular vehicle theft spots
sort(table(mvt$LocationDescription))

# Get the subset of mvt where LocationDescription are the top 5 values only
Top5 = subset(mvt, mvt$LocationDescription == 'STREET' | 
                mvt$LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)' | 
                mvt$LocationDescription == 'ALLEY' | 
                mvt$LocationDescription == 'GAS STATION' | 
                mvt$LocationDescription == 'DRIVEWAY - RESIDENTIAL')

# The assignment page says this will save all other LocationDescription values as well,
# so we have to use:
#   Top5$LocationDescription = factor(Top5$LocationDescription)
# but I see that it already only has those 5 values.
# Maybe the R libraries have changed from 2017 (the year this assignment was created).

# To find the most arrests within these 5 locations
table(Top5$Arrest, Top5$LocationDescription)
# Results in
#         ALLEY DRIVEWAY - RESIDENTIAL GAS STATION PARKING LOT/GARAGE(NON.RESID.) STREET
# FALSE   2059                   1543        1672                          13249 144969
# TRUE     249                    132         439                           1603  11595
# Now we can get the fractions by #true/(#true + #false) for each category.

# To find the Weekday where the most number of thefts at gas stations occur,
# and     the Weekday where the least number of thefts on the driveway occur:
table(Top5$LocationDescription, Top5$Weekday)

# You can remove variables you created to save memory using rm()
rm(mvt, Top5)
