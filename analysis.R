################################################################################
# Data analysis for rationalizaion Experiment
# Fridolin Linder
#
# Input: rat_prerun.csv (cleaned qualtrix data output)
################################################################################
setwd("~/Dropbox/rationalization/rationalization")
library(randomForest)

# Load data
dat <- read.csv("Data/prerun/rat_prerun.csv", sep = ",")

# Drop other subjcts with other party preference
dat <- dat[dat$pref != 3, ]

# REcode climate change question
dat$gwhow[is.na(dat$gwhow)] <- dat$gwhow2[!is.na(dat$gwhow2)]
dat$gwhow2 <- NULL 

# Balance Statistics
tapply(dat$age, dat$group, mean)
tapply(dat$edu, dat$group, mean)
tapply(dat$sex, dat$group, mean)

# Train predictive model
mod <- self_eg1 ~ hltref + gaymarry + gayadopt + abrtch + abrthlth + abrtinc + 
  abrtbd + abrtrpe + abrtdth + abrtfin + fedenv + fedwlf + fedpoor + fedschool + 
  drill + gwhap + gwhow + aauni + aawork + gun + comm
fit <- randomForest(mod, data = dat[dat$group == 1, ],  importance = T)

## Naive experiment: mean squared distance in treatment and control
## for self position

# Group one: Naive control group
mean((dat$self_eg1 - dat$can_eg1_1)^2, na.rm = T)
mean((dat$self_eg2 - dat$can_eg2_1)^2, na.rm = T)

## Allow for bias in both directions

# Bias in self
# Out of bag prediction for group 1:
pred1 <- predict(fit)
# plot(pred1, dat$self_eg1[dat$group == 1])

# Prediction for group two
pred2 <- predict(fit, dat[dat$group == 2, ])
#plot(pred, dat$self_eg2[dat$group == 2])

mean((dat[dat$group == 1, "self_eg1"] - pred1)^2)
mean((dat[dat$group == 2, "self_eg2"] - pred2)^2)

# Bias in candidate
mean((dat$self_eg1 - dat$can_eg1_1)^2, na.rm = T)
mean((pred2 - dat$can_eg2_1[dat$group == 2])^2)