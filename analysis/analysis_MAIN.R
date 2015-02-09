rm(list = ls())
################################################################################
# Data analysis for rationalizaion Experiment
# Fridolin Linder
#
# Input: main_study_clean.csv (cleaned qualtrix data output)
################################################################################
#setwd("~/Dropbox/rationalization/rationalization")
library(randomForest)
library(xtable)
library(ggplot2)
library(scales)
library(grid)
library(dplyr)

# Load data
dat <- read.csv("../data/main_study/main_study_clean.csv", sep = ",")

# Recode climate change question
dat$gwhow[is.na(dat$gwhow)] <- dat$gwhow2[!is.na(dat$gwhow2)]
dat$gwhow2 <- NULL 

#===============================================================================
# Some descriptive plots for the research plan
#===============================================================================

# Balance Statistics
stats <- c(tapply(dat$age, dat$group, mean),
           tapply(dat$edu, dat$group, mean),
           tapply(dat$sex, dat$group, mean),
           tapply(dat$tcompl, dat$group, mean),
           tapply(dat$gaymarry, dat$group, mean),
           tapply(dat$aauni, dat$group, mean),
           tapply(dat$drill, dat$group, mean),
           tapply(dat$hltref, dat$group, mean))
pdat <- data.frame(val = stats, grp = rep(c("Group 1 (Self first)", "Group 2"), 8), 
                   var = rep(c("Age", "Education", "Gender", "Survey Time",
                               "gaymarry", "aauni", "drill", "obamacare"), 
                             each = 2)
)
p <- ggplot(pdat, aes(var, val, fill = grp))
p <- p + geom_bar(position = "dodge", stat = "identity")
p <- p + theme_bw() + scale_fill_brewer(palette="Set2")
p <- p + labs(y = "Unit of Variable", x = "")
ggsave(plot = p, filenam = "../figures/bal_main.png")

# Time to complete survey
p <- ggplot(dat, aes(tcompl))
p <- p + geom_density() + theme_bw()
p <- p + geom_histogram(aes(y = ..density..), colour = "#FC8D62", fill = "#66C2A5",
                        alpha = 0.8, binwidth = 1) 
p <- p + labs(x = "Time in Minutes")
ggsave(plot = p, filenam = "../figures/main/time.png")

# Deal with other pref = other party
dat$pref2[dat$pref2 == ""] <- NA
dat$pref2_fac <- NA
dat$pref2_fac[grep("^[I,i]ndepend[e,a]nt$", dat$pref2)] <- 3
dat$pref2_fac[grep("^[G,g]reen$", dat$pref2)] <- 4
dat$pref2_fac[grep("^[L,l]ibertarian$", dat$pref2)] <- 5
dat$pref[dat$pref == 3]  <- NA
dat$pref <- apply(cbind(dat$pref2_fac, dat$pref), 1, sum, na.rm = TRUE)
dat$pref <- factor(dat$pref, labels = c("democrat", "republican", 
                                        "independent", "green", "libertarian"),
                   levels = c(1:5))
dat$pref2_fac <- NULL

# Plot preferences
p <- ggplot(dat, aes(dat$pref)) + geom_bar()
p <- p + labs(x = "Prefered Party") + theme_bw()
ggsave(plot = p, filenam = "../figures/main/preferences.png")

### Train predictive model

# Recode opinion variables to factors
vars_to_recode <- c("hltref", "gaymarry", "gayadopt", "abrtch", "abrthlth", "abrtinc", 
                      "abrtbd", "abrtrpe", "abrtdth", "abrtfin", "fedenv", "fedwlf", 
                      "fedpoor", "fedschool", "drill", "gwhap", "gwhow", "aauni", 
                      "aawork", "gun")
for(var in vars_to_recode)
  dat[, var] <- as.factor(dat[, var])


mod <- self_placement ~ hltref + gaymarry + gayadopt + abrtch + abrthlth + abrtinc + 
  abrtbd + abrtrpe + abrtdth + abrtfin + fedenv + fedwlf + fedpoor + fedschool + 
  drill + gwhap + gwhow + aauni + aawork + gun + comm + edu
fit <- randomForest(mod, data = dat[dat$group == 1, ],  importance = T)

imp <- sort(fit$importance[, 1], decreasing = T)
pdat <- data.frame(imp = imp, var = names(imp))
rownames(pdat) <- NULL
pdat$var <- factor(pdat$var, levels = pdat$var[order(pdat$imp)])


# Plot importance for prerun data
p <- ggplot(pdat, aes(var, imp))
p <- p + geom_bar(stat = "identity")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + labs(y = "Mean Increase in MSE after Permutation")
p <- p + theme_bw()
p <- p + theme(plot.margin = unit(rep(.15, 4), "in"), axis.title.y = element_blank())
p <- p + coord_flip()
ggsave(plot = p, filenam = "../figures/main/varimp.png")


# Prediction for both groups
pred1 <- predict(fit)
pred2 <- predict(fit, dat[dat$group == 2, ])
pdat <- data.frame(observed = c(dat$self_placement[dat$group == 1],
                                dat$self_placement[dat$group == 2]),
                   predicted = c(pred1, pred2),
                   group = c(rep("Group 1 (Self first)", length(pred1)),
                             rep("Group 2", length(pred2)))
                   )
p <- ggplot(pdat, aes(observed, predicted, color = group, shape = group))
p <- p + geom_point(size = 3) + geom_abline(intercept = 0, slope = 1)
p <- p + theme_bw() + ylim(0, 100) + xlim(0, 100)
ggsave(plot = p, filenam = "../figures/main/prediction.png")


#===============================================================================
# Test the hypotheses
#===============================================================================

## Allow for bias in both directions

# Bias in self
X <- (dat[dat$group == 1, "self_placement"] - pred1)^2
Y <- (dat[dat$group == 2, "self_placement"] - pred2)^2

# Bias in candidate
Z <- (dat$self_placement[dat$group == 1] - dat$candidate_placement[dat$group == 1])^2
W <- (pred2 - dat$candidate_placement[dat$group == 2])^2
