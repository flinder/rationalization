################################################################################
# Data analysis for rationalizaion Experiment
# Fridolin Linder
#
# Input: rat_prerun.csv (cleaned qualtrix data output)
################################################################################
#setwd("~/Dropbox/rationalization/rationalization")
setwd("C:/Users/flinder/Dropbox/rationalization/analysis")
library(randomForest)
library(xtable)

# Load data
dat <- read.csv("Data/prerun/rat_prerun.csv", sep = ",")

# Recode climate change question
dat$gwhow[is.na(dat$gwhow)] <- dat$gwhow2[!is.na(dat$gwhow2)]
dat$gwhow2 <- NULL 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Description

# Balance Statistics
bal = rbind(tapply(dat$age, dat$group, mean),
            tapply(dat$edu, dat$group, mean),
            tapply(dat$sex, dat$group, mean)
)
colnames(bal) <- c("Group 1", "Group 2")
rownames(bal) <- c("Age", "Education", "Gender")

xtable(bal)

# Time to complete
png(filename = "figures/time.png", , height = 4, width = 5, units = "in",
    res = 400)
hist(dat$tcompl, breaks = 20, col = "grey", main = "Survey completion times",
     xlab = "Minutes")
dev.off()

# Drop other subjcts with other party preference
dat <- dat[dat$pref != 3, ]

# Train predictive model
mod <- self_eg1 ~ hltref + gaymarry + gayadopt + abrtch + abrthlth + abrtinc + 
  abrtbd + abrtrpe + abrtdth + abrtfin + fedenv + fedwlf + fedpoor + fedschool + 
  drill + gwhap + gwhow + aauni + aawork + gun + comm
fit <- randomForest(mod, data = dat[dat$group == 1, ],  importance = T)

# Plot variable importance
imp <- sort(fit$importance[, 1], decreasing = T)
pdat <- data.frame(imp = imp, var = names(imp))
rownames(pdat) <- NULL
pdat$var <- factor(pdat$var, levels = pdat$var[order(pdat$imp)])

p <- ggplot(pdat, aes(var, imp))
p <- p + geom_bar(stat = "identity")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + labs(y = "Mean Increase in MSE after Permutation")
p <- p + theme_bw()
p <- p + theme(plot.margin = unit(rep(.15, 4), "in"), axis.title.y = element_blank())
p <- p + coord_flip()
ggsave(plot = p, filenam = "figures/varimp_prerun.png")

## Naive experiment: mean squared distance in treatment and control
## for self position

# Group one: Naive control group
mean((dat$self_eg1 - dat$can_eg1_1)^2, na.rm = T)
mean((dat$self_eg2 - dat$can_eg2_1)^2, na.rm = T)

## Allow for bias in both directions

# Bias in self
# Out of bag prediction for group 1:
pred1 <- predict(fit)

# Prediction for group two
pred2 <- predict(fit, dat[dat$group == 2, ])

png(filename = "figures/prediction.png", height = 5, width = 10, units = "in",
    res = 400)
par(mfrow = c(1, 2))
plot(dat$self_eg1[dat$group == 1], pred1, xlab = "Self placement", 
     ylab = "Prediction", pch = 16)
abline(0, 1)
plot(dat$self_eg2[dat$group == 2], pred2, xlab = "Self placement", 
     ylab = "Prediction", pch = 16)
abline(0, 1)
par(mfrow = c(1, 1))
dev.off()

mean((dat[dat$group == 1, "self_eg1"] - pred1)^2)
mean((dat[dat$group == 2, "self_eg2"] - pred2)^2)

# Bias in candidate
mean((dat$self_eg1 - dat$can_eg1_1)^2, na.rm = T)
mean((pred2 - dat$can_eg2_1[dat$group == 2])^2)
