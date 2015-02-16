rm(list = ls())
################################################################################
# Data analysis for rationalizaion Experiment
# Fridolin Linder
#
# Input: main_study_clean.csv (cleaned qualtrix data output)
################################################################################
#setwd("~/Dropbox/rationalization/rationalization")
library(randomForest)
library(ggplot2)
library(scales)
library(grid)
library(dplyr)
library(rstan)
library(reshape2)

# Load data
dat <- read.csv("../data/main_study/main_study_clean.csv", sep = ",")

# Recode climate change question
dat$gwhow[is.na(dat$gwhow)] <- dat$gwhow2[!is.na(dat$gwhow2)]
dat$gwhow2 <- NULL 

# Group labels
dat$group_name <- NA
dat$group_name[dat$group == 1] <- "Group 1 (S asked first)"
dat$group_name[dat$group == 2] <- "Group 2 (P asked first)"

#===============================================================================
# Some descriptive plots
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
pdat <- data.frame(val = stats, grp = rep(c("Group 1 (S asked first)", 
                                            "Group 2 (P asked first)"), 8), 
                   var = rep(c("Age", "Education", "Gender", "Survey Time",
                               "gaymarry", "aauni", "drill", "obamacare"), 
                             each = 2)
)
p <- ggplot(pdat, aes(var, val, fill = grp))
p <- p + geom_bar(position = "dodge", stat = "identity")
p <- p + scale_fill_manual(values = c("cornflowerblue", "yellowgreen"), 
                                        name = "Experimental Group")
p <- p + labs(y = "Unit of Variable", x = "")
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
ggsave(plot = p, filename = "../figures/main/bal_main.png")

# Time to complete survey
p <- ggplot(dat, aes(tcompl))
p <- p + geom_density()
p <- p + geom_histogram(aes(y = ..density..), colour = "yellowgreen", fill = "cornflowerblue",
                        alpha = 0.8, binwidth = 1) 
p <- p + labs(x = "Time in Minutes")
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
ggsave(plot = p, filename = "../figures/main/time.png")

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
p <- ggplot(dat, aes(x = pref, fill = group_name)) + geom_bar(position = "dodge")
p <- p + labs(x = "Preferred Party")
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
p <- p + scale_fill_manual(values = c("cornflowerblue", "yellowgreen"), 
                                      name = "Experimental Group")
ggsave(plot = p, filename = "../figures/main/preferences.png")

# Exclude observations without party preference
dat <- dat[!is.na(dat$pref), ]

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

# put predictions into data.frame
dat$pred[dat$group == 1] <- predict(fit)
dat$pred[dat$group == 2] <- predict(fit, dat[dat$group == 2, ])

# Plot importance
p <- ggplot(pdat, aes(var, imp))
p <- p + geom_bar(stat = "identity", fill = "cornflowerblue")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + labs(y = "Mean Increase in MSE after Permutation")
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
p <- p + theme(plot.margin = unit(rep(.15, 4), "in"), axis.title.y = element_blank())
p <- p + coord_flip()
ggsave(plot = p, filename = "../figures/main/varimp.png")


# Prediction for both groups
pdat <- data.frame(observed = dat$self_placement,
                   predicted = dat$pred,
                   candidat = dat$party_placement,
                   group = dat$group_name
                   )
p <- ggplot(pdat, aes(observed, predicted, color = group))
p <- p + geom_point() + geom_abline(intercept = 0, slope = 1)
p <- p + theme_bw() + ylim(0, 100) + xlim(0, 100)
p <- p + scale_color_manual(values = c("cornflowerblue", "yellowgreen"), 
                            name = "Experimental Group")
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
ggsave(plot = p, filename = "../figures/main/prediction.png")



# Look at distribution of self and party placements
p <- ggplot(dat, aes(x = self_placement, color = group_name))
p <- p + scale_color_manual(values = c("cornflowerblue", "yellowgreen"), name = "Experimental Group")
p <- p + geom_density(alpha = .3, size = 1)
p <- p + facet_wrap( ~ pref)
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
p <- p + labs(x = "Self Placement", y = "Density")
ggsave(plot = p, filename = "../figures/main/dist_self.png")

p <- ggplot(dat, aes(x = party_placement, color = group_name))
p <- p + scale_color_manual(values = c("cornflowerblue", "yellowgreen"), name = "Experimental Group")
p <- p + facet_wrap( ~ pref)
p <- p + geom_density(alpha = .3, size = 1)
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
p <- p + labs(x = "Party Placement", y = "Density")
ggsave(plot = p, filename = "../figures/main/dist_party.png")


#===============================================================================
# Main Analysis
#===============================================================================

# Bias in self
X <- (dat[dat$group == 1, "self_placement"] - dat$pred[dat$group == 1])^2
Y <- (dat[dat$group == 2, "self_placement"] - dat$pred[dat$group == 2])^2
#t.test(log(X), log(Y))

# Bias in party
Z <- (dat$self_placement[dat$group == 1] - dat$party_placement[dat$group == 1])^2
W <- (dat$pred[dat$group == 2] - dat$party_placement[dat$group == 2])^2
#epsilon <- 1e-2
#t.test(log(Z + epsilon), log(W + epsilon))

# Visualize distribution of differences
df <- data.frame(distance = c(X, Y, Z, W),
                 group = rep(c(rep("Group 1 (S asked first)", length(X)), 
                               rep("Group 2 (P asked first)", length(Y))), 2),
                 experiment = c(rep("Experiment: Bias in Self", length(c(X, Y))), 
                                rep("Experiment: Bias in Party", length(c(Z, W))))
                 )

p <- ggplot(df, aes(distance, color = group))
p <- p + geom_density(size = 1)
p <- p + facet_wrap( ~ experiment, scales = "fixed")
p <- p + scale_color_manual(values = c("cornflowerblue", "yellowgreen"), name = "Experimental Group")
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
p <- p + labs(x = "Distance", y = "Density")
ggsave(plot = p, filename = "../figures/main/dist_dista_dens.png")

p <- ggplot(df, aes(x = group, y = distance, color = group))
p <- p + geom_boxplot(color = "grey10", outlier.size = 0.1)
p <- p + geom_point(alpha = 0.5, position = "jitter")
p <- p + facet_wrap( ~ experiment, scales = "fixed")
p <- p + scale_color_manual(values = c("cornflowerblue", "yellowgreen"), guide = F)
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
p <- p + labs(x = "Experimental Group", y = "Distance")
ggsave(plot = p, filename = "../figures/main/dist_dista_box.png")


#===============================================================================
# Bayesian analysis of results
#===============================================================================

# First: just estimate the parameters
model <- "
data {
  int<lower=1> N;
  vector[N] y;
  int groupID[N];
}

parameters {
  vector<lower=0>[2] alpha;
  vector<lower=0>[2] beta;
}

model {
  // priors
  alpha ~ cauchy(0, 5);
  beta ~ cauchy(0, 5);

  // likelihood
  for (n in 1:N){
    y[n] ~ gamma(alpha[groupID[n]], beta[groupID[n]]);
  }
}

generated quantities {
  vector[2] mu;
  real mu_diff;
  real mu_ratio;
  //matrix[N, 50] y_rep;
  vector[N] y_rep;
  mu[1] <- alpha[1] / beta[1];
  mu[2] <- alpha[2] / beta[2];
  mu_diff <- mu[1] - mu[2];
  mu_ratio <- mu[1] / mu[2];

  //for (n in 1:N){
  //  for(j in 1:50) {
  //    y_rep[n, j] <- gamma_rng(alpha[groupID[n]], beta[groupID[n]]);
  //  }
  //}
  for (n in 1:N)
    y_rep[n] <- gamma_rng(alpha[groupID[n]], beta[groupID[n]]);
}
"

# Compile model
c_mod <- stan_model(model_code = model)

# ---------------------------------------------------
# Compare Bias in self (x and Y)

y = c(X, Y)
groupID = c(rep(1, length(X)), rep(2,  length(Y)))
standata_1 <- list(N = length(y), groupID = groupID, y = y)

# Sample from it
stanfit_1 <- sampling(object = c_mod, data = standata_1, iter = 5000, warmup = 1000, chains = 2)
post_1 <- as.data.frame(do.call(cbind, extract(stanfit_1, pars = c("alpha", "beta", "mu", "mu_diff", "mu_ratio"))))
colnames(post_1) <- c("alpha_1", "alpha_2", "beta_1", "beta_2", "mu_1", "mu_2",
                      "mu_diff", "mu_ratio")

# ---------------------------------------------------
# Compare Bias in party

y = c(Z, W)
groupID = c(rep(1, length(X)), rep(2,  length(Y)))
standata_2 <- list(N = length(y), groupID = groupID, y = y)

# Sample from it
stanfit_2 <- sampling(object = c_mod, data = standata_2, iter = 5000, warmup = 1000, chains = 2)
post_2 <- as.data.frame(do.call(cbind, extract(stanfit_2, pars = c("alpha", "beta", "mu", "mu_diff", "mu_ratio"))))
colnames(post_2) <- c("alpha_1", "alpha_2", "beta_1", "beta_2", "mu_1", "mu_2",
                      "mu_diff", "mu_ratio")

# ------------------------------------------------
# Check Model fit
B <- 100 # number of replications
y_rep_full_1 <- extract(stanfit_1, "y_rep")$y_rep
y_rep_1 <- y_rep_full_1[sample(c(1:nrow(y_rep_full_1)), B), ]
y_rep_full_2 <- extract(stanfit_2, "y_rep")$y_rep
y_rep_2 <- y_rep_full_2[sample(c(1:nrow(y_rep_full_2)), B), ]

# Prepare replications from experiment 1
groupID_name <- NA
groupID_name[groupID == 1] <- "Group 1 (S asked first)"
groupID_name[groupID == 2] <- "Group 2 (P asked first)"
gdat_1 <- melt(y_rep_1)
colnames(gdat_1) <- c('iteration', 'observation', 'value')
gdat_1$experiment <- "Experiment 1: Bias in S"
gdat_1$groupID <- factor(rep(groupID_name, e = B))

# Prepare replications from experiment 2
gdat_2 <- melt(y_rep_2)
colnames(gdat_2) <- c('iteration', 'observation', 'value')
gdat_2$experiment <- "Experiment 2: Bias in P"
gdat_2$groupID <- factor(rep(groupID_name, e = B))

gdat <- rbind(gdat_1, gdat_2)
gdat$type <- "replication"


observed <- data.frame(iteration = 0,
                       observation = NA,
                       value = c(X, Y, Z, W),
                       experiment = c(rep("Experiment 1: Bias in S", length(y)),
                                      rep("Experiment 2: Bias in P", length(y))),
                       groupID = rep(c("Group 1 (S asked first)", 
                                       "Group 2 (P asked first)"), 
                                     c(length(X), length(Y)), 2),
                       type = "observed"
                       )

pdat <- as.data.frame(rbind(gdat, observed))
pdat$iteration <- as.factor(pdat$iteration)

# Zoom in
pdat <- filter(pdat, value < 2000)

p <- ggplot(pdat, aes(x = value, alpha = iteration, color = type, size = type)) + 
        geom_line(stat = "density") + 
        scale_alpha_manual(values = c(1, rep(0.15, B)), guide = F) +
        scale_color_manual(values = c("yellowgreen", rep("cornflowerblue", B))) +
        scale_size_manual(values = c(1.5, rep(0.5, B))) +
        facet_wrap( ~ experiment + groupID) + 
        theme(panel.background = element_rect(fill = "white", colour = "black"),
              panel.grid.major = element_line(colour = "gray80"))
p
ggsave(plot = p, filename = "../figures/main/post_pred.png")


# ------------------------------------------------
# Visualize results

# POsterior distribution of ratio and difference in means
pdat <- data.frame(difference = c(post_1$mu_diff, post_1$mu_ratio, post_2$mu_diff, post_2$mu_ratio),
                   experiment = rep(c("Experiment 1: Bias in S", "Experiment 2: Bias in C"), each = 2 * nrow(post_1)),
                   type = rep(rep(c("Difference", "Ratio"), each = nrow(post_1)), 2)
                   )
pdat$above <- 0
pdat$above[pdat$type == "Difference" & pdat$difference > 0] <- 1
pdat$above[pdat$type == "Ratio" & pdat$difference > 1] <- 1
pdat$above <- as.factor(pdat$above)

p <- ggplot(pdat, aes(x = difference, fill = above))
p <- p + facet_wrap( ~ type + experiment, scales = "free")
p <- p + geom_histogram(color = "yellowgreen")
p <- p + theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"))
p <- p + scale_fill_manual(values = c("cornflowerblue", "red"))
p <- p + labs(x = "Mean Difference/Ratio", y = "")
ggsave(plot = p, filename = "../figures/main/mean_diff_ratio.png")