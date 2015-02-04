################################################################################
# Data analysis for rationalizaion Experiment
# Fridolin Linder
#
# Input: rat_prerun.csv (cleaned qualtrix data output)
################################################################################
#setwd("~/Dropbox/rationalization/rationalization")
library(randomForest)
library(xtable)
library(ggplot2)
library(scales)
library(grid)

# Load data
dat <- read.csv("../data/pre_run/pre_run_clean.csv", sep = ",")

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
ggsave(plot = p, filenam = "../figures/bal.png")

# Time to complete survey
p <- ggplot(dat, aes(tcompl))
p <- p + geom_density() + theme_bw()
p <- p + geom_histogram(aes(y = ..density..), colour = "#FC8D62", fill = "#66C2A5",
                        alpha = 0.9) 
p <- p + labs(x = "Time in Minutes")
ggsave(plot = p, filenam = "../figures/time.png")

# Variable importance in prerun compared to pred model from anes

imp <- sort(fit$importance[, 1], decreasing = T)
pdat <- data.frame(imp = imp, var = names(imp))
rownames(pdat) <- NULL
pdat$var <- factor(pdat$var, levels = pdat$var[order(pdat$imp)])

# Get original anes labels for comparison of varimp
load("fitted_forest.RData")
labs <- data.frame(anes = as.character(c("aa_uni_x", "aa_work_x", "abort_bd_x",
                                         "abort_choice_x", "abort_fatal_x",
                                         "abort_fin_x", "abort_health_x", 
                                         "abort_incest_x", "abort_rape_x", 
                                         "guarpr_self", "envir_drill",  
                                         "dem_edugroup_x", "fedspend_enviro",
                                         "fedspend_poor", "fedspend_schools",
                                         "fedspend_welfare", "gayrt_adopt", 
                                         "gayrt_marry","gun_control","envir_gwarm", 
                                         "envir_gwhow", "health_2010hcr_x")),
                   mturk = as.character(sort(as.character(pdat$var))),
                   stringsAsFactors = F
                   )
# rename anes results
anes_imp <- grow$importance[names(grow$importance) %in% labs[, 1]]
for(i in 1:length(anes_imp)) {
  names(anes_imp)[i] <- labs[labs[, 1] == names(anes_imp)[i], 2]
}

# Standardize and sort
range01 <- function(x) (x - min(x)) / (max(x) - min(x))

anes_imp_p <- range01(anes_imp[order(names(anes_imp), decreasing = T)])
imp_p <- range01(imp[order(names(imp), decreasing = T)])

pdat2 <- data.frame(imp = c(imp_p, anes_imp_p),
                    var = rep(names(anes_imp), 2),
                    dset= rep(c("pre_run", "anes"), each = length(imp)))

# Plot importance for prerun data
p <- ggplot(pdat2, aes(var, imp))
p <- p + geom_bar(stat = "identity")
p <- p + facet_wrap( ~ dset, scales = "fixed") 
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + labs(y = "Standardized (0, 1) Mean Increase in MSE after Permutation")
p <- p + theme_bw()
p <- p + theme(plot.margin = unit(rep(.15, 4), "in"), axis.title.y = element_blank())
p <- p + coord_flip()
ggsave(plot = p, filenam = "../figures/varimp.png")

# Train predictive model
mod <- self_eg1 ~ hltref + gaymarry + gayadopt + abrtch + abrthlth + abrtinc + 
  abrtbd + abrtrpe + abrtdth + abrtfin + fedenv + fedwlf + fedpoor + fedschool + 
  drill + gwhap + gwhow + aauni + aawork + gun + comm + edu
fit <- randomForest(mod, data = dat[dat$group == 1, ],  importance = T)

# Prediction for group two
pred1 <- predict(fit)
pred2 <- predict(fit, dat[dat$group == 2, ])
pdat <- data.frame(observed = c(dat$self_eg1[dat$group == 1],
                                dat$self_eg2[dat$group == 2]),
                   predicted = c(pred1, pred2),
                   group = c(rep("Group 1 (Self first)", length(pred1)),
                             rep("Group 2", length(pred2)))
                   )
p <- ggplot(pdat, aes(observed, predicted, color = group, shape = group))
p <- p + geom_point(size = 3) + geom_abline(intercept = 0, slope = 1)
p <- p + theme_bw() + ylim(0, 100) + xlim(0, 100)
ggsave(plot = p, filenam = "../figures/prediction.png")

#===============================================================================
# How the data will be analyzed
#===============================================================================

## Naive experiment: mean squared distance in treatment and control
## for self position

# Group one: Naive control group
mean((dat$self_eg1 - dat$can_eg1_1)^2, na.rm = T)
mean((dat$self_eg2 - dat$can_eg2_1)^2, na.rm = T)


## Allow for bias in both directions

# Bias in self
mean((dat[dat$group == 1, "self_eg1"] - pred1)^2)
mean((dat[dat$group == 2, "self_eg2"] - pred2)^2)

# Bias in candidate
mean((dat$self_eg1 - dat$can_eg1_1)^2, na.rm = T)
mean((pred2 - dat$can_eg2_1[dat$group == 2])^2)
