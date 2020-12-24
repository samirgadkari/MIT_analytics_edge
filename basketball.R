# Apply moneyball statistics to NBA dataset
NBA = read.csv('datasets/NBA_train.csv')
str(NBA)
# W = Wins during regular season
# PTS = Points scored during regular season
# oppPTS = Opponent points scored during regular season
# 
# Variable name ending with A means attempted:
# FG, FGA = Field Goals successful, Field Goals attempted
# X2P, X2PA = # 2-pointers successful, attempted. R put the X in front since 
#               it needs variables beginning with a letter.
# X3P, X3PA = # 3-pointers successful, attempted. R put the X in front since 
#               it needs variables beginning with a letter.
# FT, FTA = # free throws successful, attempted
# ORB, DRB = Offensive and defensive rebounds
# AST = Assists
# STL = Steals
# BLK = Blocks
# TOV = Turnovers

# Let's see how many wins are required before a team gets into the playoffs.
table(NBA$W, NBA$Playoffs)
# It looks like if a team wins around 42 games, it's chance of getting into
# the playoffs is high.

NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
# Looks like a strong correlation. So linear regression looks like a good
# method for predicting wins based on PTSdiff. Let's verify this:
WinsReg = lm(W ~ PTSdiff, data=NBA)
summary(WinsReg)
# Great adjusted R^2 value of 0.9423.
# W = 41 + 0.0326 * PTSdiff
# We said earlier that we want W >= 42,
# so 41 + 0.0326 * PTSdiff >= 42
# And this gives PTSdiff = 30.67.
# So we need to score >= 31 points than we allow to win at least 42 games.

# Let's build a regression of PTS vs all other so we can see what gives us
# more points
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK,
               data=NBA)
summary(PointsReg)
# Adjusted R^2 is good at 0.8981. Some variables are very significant,
# others (STL) are significant, while the rest are not significant at all.
# The R^2 being high shows that there is a linear relationship between
# these variables.
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE
# A large SSE value (28394314). Let's calculate RMSE:
RMSE = sqrt(SSE/nrow(NBA))
RMSE
# So this is the average error per season, and it's value is 184.4049.
mean(NBA$PTS)
# So this is 8370 average points. So 184 out of 8370 points is not bad.
# But we can do better. Let's remove some insignificant variables from the model.
# Let's remove TOV, since it's p-value is high.

PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK,
                data=NBA)
summary(PointsReg2)
# R^2 is almost the same. So removing TOV was the right approach.

PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK,
                data=NBA)
summary(PointsReg3)
# R^2 is almost the same. So removing DRB was the right approach.

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL,
                data=NBA)
summary(PointsReg4)
# R^2 is almost the same. So removing DRB was the right approach.

# Let's make sure we have similar SSE and RMSE to see that our
# model is still as good.
SSE_4 = sum(PointsReg4$residuals^2)
RMSE_4 = sqrt(SSE_4/nrow(NBA))
SSE_4
RMSE_4
# This is 184.5 which is similar to the 184.4 value earlier.
# So removing those variables did not cause deteriorate our model.

# Now let's make perdictions for 2012-1013 season.
# Our training set only included 1980-2011 seasons,
# so load the test set.
NBA_test = read.csv('datasets/NBA_test.csv')
PointsPredictions = predict(PointsReg4, newdata=NBA_test)
# Compute the Out-of-sample R^2 (the R^2 on the test data).

SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE/SST
R2
# R^2 = 0.8139
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE
# RMSE = 195.7653 which is a little higher than before, but still reasonable.

rm(NBA, NBA_test, PointsReg, PointsReg2, PointsReg3, PointsReg4, WinsReg,
   PointsPredictions, R2, RMSE, RMSE_4, SSE, SSE_4, SST)
