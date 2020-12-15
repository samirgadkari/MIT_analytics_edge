# Process Demographic and Employment dataset called the
# CPS (Current Population Survey)

CPS = read.csv('../large_datasets/CPS_dataset/CPSData.csv')
str(CPS)             # Structure of the dataset
table(CPS$Industry)  # Shows the number of people in each Industry type
sort(table(CPS$State), decreasing=FALSE) # Sorted number of people in each State.
                                         # decreasing=FALSE is the default.

# Percentage of people that are citizens
citizenship = sort(table(CPS$Citizenship))  # This shows there are: 
                                            # "Citizen, Naturalized"
                                            # "Non-Citizen", and 
                                            # "Citizen, Native"
str(citizenship)
library('stringr')
names(citizenship) = str_replace_all(names(citizenship),  # Replace spaces, commas, and hyphens
                                     c(" " = "." , "," = "", "-" = "." ))
citizens = 1.0 - citizenship['Non.Citizen']/(citizenship['Citizen.Naturalized'] +
                                             citizenship['Non.Citizen'] +
                                             citizenship['Citizen.Native'])
# When the above line is executed, the citizens table has only one column.
# Unfortunately, the column is labeled 'Non.Citizen', probably because it is
# the first name in the code on the right of the equal sign.
# We must change the name to reflect the truth.
# We use names() for that.
names(citizens) = 'Fraction of citizens'
str(citizens)  # Check if the change occurred, and is correct

# To find the races for which there are at least 250 interviewees of Hispanic ethnicity
table(CPS$Hispanic, CPS$Race)  # From the output we see these races are:
                               # American Indian, Black, Multiracial, and White.

# for loops are to be avoided, since they're really slow.
# But since we don't know the details on how to do this using dplyr,
# let's do it this way for now.
# To find variables in CPS that have at least one missing (NA) value:
for (var.name in names(CPS)) {
  if (sum(is.na(CPS[[var.name]])) > 0)
    print(var.name)
}

is.na(CPS$Married)  # Returns a list of TRUE/FALSE based on NA values in that cell.
table(CPS$Region, is.na(CPS$Married))  # Compares the Region values to the missing values in Married.
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

# How many states had all interviewees living in a non-metropolitan area 
# (aka they have a missing MetroAreaCode value).
# How many states had all interviewees living in a metropolitan area
table(CPS$State, is.na(CPS$MetroAreaCode))  
# The number of values that are 0 in the FALSE value column is the answer for the first question.
# The number of values that are 0 in the TRUE value column is the answer for the second question.

# Which region of the United States has the largest proportion of interviewees 
# living in a non-metropolitan area
table(CPS$Region, is.na(CPS$MetroAreaCode))
# Now we find the proportions by TRUE_number/(TRUE_number + FALSE_number) for each region

# There is an easier way to find proportions with TRUE/FALSE,
# Use tapply and the mean function. Since TRUE = 1 and FALSE = 0,
# the mean will be the proportion of TRUE values.
# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)) # The is.na() function returns a list of 
                                                        # logical values. We group by the State, 
                                                        # and apply the mean to the
                                                        # group of each state's logical values.
                                                        # Then we sort the array output.

# MetroAreaCodes.csv stores the mapping between CSV$MetroAreaCode integer values and the
# actual code. CountryCodes.csv stores the mapping between CSV$CountryOfBirthCode and
# the actual country name string.
MetroAreaMap = read.csv('../large_datasets/CPS_dataset/MetroAreaCodes.csv')
CountryMap = read.csv('../large_datasets/CPS_dataset/CountryCodes.csv')
str(MetroAreaMap)
str(CountryMap)

# Merge the CPS and MetroAreaMap dataframes.
# Populates the merged dataset with the columns from MetroAreaMap
# that are not part of the matching column (MetroAreaMap$Code).
# Keeps the original CPS$MetroAreaCode intact.
CPS = merge(CPS, MetroAreaMap, 
            by.x='MetroAreaCode', # Use values from the CSV$MetroAreaCode column to match
            by.y='Code',          # values from the MetroAreaMap$Code column.
            all.x=TRUE)           # Keep all rows of CPS (left outer join).
str(CPS)

table(CPS$MetroArea)  # Will allow you to compare number of interviewees in each metro area.
                      # You can sort() around the table with decreasing=TRUE if needed.

# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean), decreasing=FALSE)
# Then just read off the last metro area.
# Or you can do this:
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean), decreasing=TRUE)[1]
# Results in
# Laredo, TX 
# 0.9662921 

# Determine the number of metropolitan areas in the United States from which
# at least 20% of interviewees are Asian.
sort(tapply(CPS$Race == 'Asian', CPS$MetroArea, mean), decreasing=FALSE)
# Then just read off the last few >=20% entries.
# You can also do this:
sum(sort(tapply(CPS$Race == 'Asian', CPS$MetroArea, mean), decreasing=FALSE) > 0.2)
# Returns
# [1] 4

# Determine which metropolitan area has the smallest proportion of interviewees who have 
# received no high school diploma.
sort(tapply(CPS$Education == 'No high school diploma', CPS$MetroArea, mean, 
            na.rm=TRUE))[1]  # na.rm is set to TRUE since interviewees below age 14
                             # don't have high school diploma, and should not be counted.

# Merge the Country of birth string from the CountryMap into the CPS dataframe
CPS = merge(CPS, CountryMap, by.x = 'CountryOfBirthCode', by.y = 'Code', all.x = TRUE)
str(CPS)
sum(is.na(CPS$Country))  # Number of missing Country values

# Among all interviewees born outside of North America, which country was the 
# most common place of birth
sort(table(CPS$Country), decreasing = FALSE)

# What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, 
# NY-NJ-PA" metropolitan area have a country of birth that is not the United States? 
# Don't include people from this metropolitan area who have a missing country of birth.
table(CPS$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA',
      CPS$Country != 'United States')
# Then you can find out from the table
1668/(3736 + 1668)

# Which metropolitan area has the largest number (note -- not proportion) of interviewees 
# with a country of birth in India
sort(tapply(CPS$Country == 'India', CPS$MetroArea, sum, na.rm=TRUE),
     decreasing=TRUE)[1]

# Which metropolitan area has the largest number (note -- not proportion) of interviewees 
# with a country of birth in Brazil
sort(tapply(CPS$Country == 'Brazil', CPS$MetroArea, sum, na.rm=TRUE),
     decreasing=TRUE)[1]

# Which metropolitan area has the largest number (note -- not proportion) of interviewees 
# with a country of birth in Somalia
sort(tapply(CPS$Country == 'Somalia', CPS$MetroArea, sum, na.rm=TRUE),
     decreasing=TRUE)[1]
