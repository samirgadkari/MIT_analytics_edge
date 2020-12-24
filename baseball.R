# Analyze the baseball.csv file.
# Analytics in baseball. The Oakland A's team was told to strictly control
# spending. Still over the next 5 years they were still increasing their wins.
# They lost 3 key players in 2002. Since they're on a strict budget,
# how can they compete if they cannot spend on the best players?
#
# What we find is that the A's used analytics to create the best team.
# The players they picked were based on player statistics while other teams
# reported player skills as well as player build/looks. They got around
# scout-based prejudices of the other teams.
# Thus they could spend 3x less and get the same results.

baseball = read.csv('datasets/baseball.csv')
str(baseball)
# RS = Runs Scored      OBP = On-base percentage (time on base)
# RA = Runs Allowed     SLG = Slugging percentage (measures power of hitter)
# W  = Wins
# OOBP = Opponents on-base percentage (time on base). Goes towards
#        how many RA (Runs Allowed) by our team.
# OSLG = Opponents slugging percentage. Goes towards RA (Runs Allowed)
#        by opponents.

summary(baseball)

# How many games does a team need to win to get into the playoffs.
# Paul dePodesta, the analyst, predicted 95 wins are needed.
# Let's see if this makes sense using data that was available to him.
moneyball = subset(baseball, baseball$Year < 2002)
moneyball$RD = moneyball$RS - moneyball$RA  # RD = Run difference
str(moneyball)
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)
# We see that both the Intercept and the RD are very significant,
# and adjusted R^2 = 0.88. So we have a great model to predict
# wins from RS - RA.

# Now let's see if as predicted in moneyball, we need the team to score
# at least 135 more runs that they allow to win at least 95 games.
# Our Regression equation is:
#   W = 80.8814 + 0.1058 * RD
#   We want W >= 95, so
#   80.8814 + 0.1058 * RD >= 95
# so RD = (95 - 80.8814) / 0.1058 = 133.4
# This is very close to the claim made in moneyball.

# Let's see the number of runs a team will score using batting stats.
# The Oakland A's found that the OBP and SLG were more important than
# the BA (Batting Average). BA was considered overvalued.
# Let's see if they were right.
# OBP = On-base percentage (time on base)
# SLG = Slugging percentage (measures power of hitter)
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)
# Our output shows that the BA coefficient is negative, and not as high.
# That's because of multi-collinearity. Let's remove BA:
RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
# Now the R^2 is the same (0.92), so we get the same result
# without BA. This must be because BA was collinear to one of the other features.
# Also we see the OBP coefficient is 2737 compared to SLG coeff of 1584.
# This shows that OBP is more important than SLG.
# So they were right - BA was overvalued.
# We could have tried removing SLG or OBP and seen that our R^2 decreased.

# To predict RA (Runs Allowed)
RunsAllowedReg = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsAllowedReg)
# We get a good R^2 of 0.91, with both variables being significant.
# The effect size of OOBP is twice as much as that of OSLG.

# How many runs and wins will the 2002 Oakland A's will get?
# Let's assume past performance correlates with future performance.
# Also assume few injuries.
# Let's predict 2002 team statistics using 2001 team statistics.
# Using the regular season statistics for the 2001 players,
#   Team OBP is 0.339
#   Team SLG is 0.430
# We know these because 2001 is already done, so we already have these statistics.
# Putting these in our RunsReg equation gives us: RS = 805
# Similarly, using 2001 statistics and RunsAllowedReg gives us: RA = 622
# Using WinsReg and these values, we can predict Wins = 100.
# Our predictions and Paul dePodesta's predictions match the result.

# Suppose you are the General Manager of a baseball team, 
# and you are selecting two players for your team. 
# You have a budget of $1,500,000, and you have the choice between:
#   Player Name
#                   OBP 	  SLG 	Salary
# Eric Chavez 	  0.338 	0.540 	$1,400,000
# Jeremy Giambi 	0.391 	0.450 	$1,065,000
# Frank Menechino	0.369 	0.374 	$295,000
# Greg Myers 	    0.313 	0.447 	$800,000
# Carlos Pena 	  0.361 	0.500 	$300,000
#
# Given your budget and the player statistics, 
# which two players would you select?
# Let's create a dataframe from this table first.
testDF = data.frame(Name=c('Eric Chavez', 'Jeremy Giambi', 'Frank Menechino', 'Greg Myers', 'Carlos Pena'), 
                    OBP=c(0.338, 0.391, 0.369, 0.313, 0.361),
                    SLG=c(0.540, 0.450, 0.374, 0.447, 0.5),
                    Salary=c(1400000, 1065000, 295000, 800000, 300000))
str(testDF)
summary(testDF)

# Using RunsReg, Runs Scored by each player is:
summary(RunsReg)
testDF$RS = -804.63 + 2737.77 * testDF$OBP + 1584.91 * testDF$SLG
summary(RunsAllowedReg)
# We cannot use RunsAllowedReg since it works with opponents OBP and SLG.
# Let's plot the Salary of the player against their RS to make a decision.
plot(RS~Salary, data=testDF)
with(testDF, text(RS~Salary, labels=testDF$Name, pos=1))
# The plot shows that the lowest-salary players with the most TS are:
#   Carlos Pena and Jeremy Giambi. Their salary put together comes to
#   $ 1,365,000, which is less than the 1,500,000 budget.

# But why was the focus on getting into the playoffs. It so happens that
# with the large number of games, luck is not as important as skill.
# Over all the games, skill shines through.
# In the playoffs, a series of 3 out of 5 or even 4 out of 7,
# anything can happen.
# Let's see if that is true.
# Let's use years 1994 - 2011. This is needed because the number of teams
# has changed over time, but from 1994 - 2011, there were a fixed 8 teams.
# Correlation between winning the world series and regular season wins is 0.03.
# In the playoffs, there are too few games for luck to even out.