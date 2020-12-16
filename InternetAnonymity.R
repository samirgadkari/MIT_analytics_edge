# Processing of poll data related to internet anonymity.

poll = read.csv('datasets/AnonymityPoll.csv')
str(poll)

# How many interviewees:
#   1. Used a smartphone
#   2. Did not use a smartphone
#   3. Did not answer the question
table(poll$Smartphone)    # Table of counts of each value in the column.
summary(poll$Smartphone)  # Summary contains the number of NAs in addition to quartile info.

table(poll$State, poll$Region == 'Midwest')[, 2] > 0  # The 2 value selects the second column.
                                                      # Without the >0, we get a count of the
                                                      # number of interviewees in each State.
# You can read off each state where the value is TRUE.
# TODO: How can you go from this to a list of states?

# Which was the state in the South census region with the largest number of interviewees
table(poll$State, poll$Region == 'South')
# Then from the output find the state in the TRUE column that has the largest number
# of interviewees

# How many interviewees reported:
#   - not having used the Internet and not having used a smartphone
#   - having used the Internet and having used a smartphone
#   - having used the Internet but not having used a smartphone
#   - having used a smartphone but not having used the Internet
table(poll$Internet.Use, poll$Smartphone)

# How many interviewees have a missing value for their Internet use
summary(poll$Internet.Use)

# How many interviewees have a missing value for their smartphone use
summary(poll$Smartphone)

# Obtain a data frame called "limited", which is limited to 
# interviewees who reported Internet use or who reported smartphone use.
# How many interviewees are in the new dataframe
limited = subset(poll, poll$Internet.Use == 1 | poll$Smartphone == 1)
str(limited) # Shows the number of observations, and the number of variables

# Which variables have missing values in the limited data frame.
# What is the average number of pieces of personal information on the Internet, 
# according to the Info.On.Internet variable.
summary(limited)
# Then you can read the output to answer the questions.

# How many interviewees reported a value of 0 for Info.On.Internet.
# How many interviewees reported the maximum value of 11 for Info.On.Internet
table(limited$Info.On.Internet)

# What proportion of interviewees who answered the Worry.About.Info question 
# worry about how much information is available about them on the Internet
table(limited$Worry.About.Info)
# From the result, you can find the proportion

# What proportion of interviewees who answered the Anonymity.Possible question 
# think it is possible to be completely anonymous on the Internet
table(limited$Anonymity.Possible)
# From the result, you can find the proportion

# What proportion of interviewees who answered the Tried.Masking.Identity 
# question have tried masking their identity on the Internet
table(limited$Tried.Masking.Identity)
# From the result, you can find the proportion

# What proportion of interviewees who answered the Privacy.Laws.Effective 
# question find United States privacy laws effective
table(limited$Privacy.Laws.Effective)
