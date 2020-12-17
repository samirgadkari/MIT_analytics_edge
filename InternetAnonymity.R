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

# Histogram of age of interviewees.
# What is the most represented age group?
hist(limited$Age)

# What is the largest number of interviewees that have exactly the same value 
# in their Age variable AND the same value in their Info.On.Internet variable? i. e.
# What is the largest number of overlapping points?
plot(limited$Age, limited$Info.On.Internet)
# Since there are many points that can be plotted in the same location on this graph,
# we won't be able to look at it and answer the question. Instead, build a table
table(limited$Age, limited$Info.On.Internet)
# Then read off the highest value in the table

# The jitter() function can jitter points on a graph, so that all points are seen.
# With the jittered points on the graph how are Age and Info.on.Internet related.
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
# Looking at the graph, it seems there is a moderate negative relationship between age and
# Info.on.Internet

# Use tapply() to obtain the summary of Info.on.Internet, broken down by
# the Smartphone feature.
tapply(limited$Info.On.Internet, limited$Smartphone, summary)
# Result shows that the mean of Smartphone users for Info.on.Internet is larger
# than the mean for non-Smartphone users.

# Use tapply() to obtain the summary of the Tried.Masking.Identity,
# broken down by the Smartphone feature.
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
# The result shows that the mean of the Smartphone users for Tried.Masking.Identity
# is larger than the mean for non-Smartphone users.

# Removed created dataframes from the environment to decrease memory usage.
remove(limited, poll)
