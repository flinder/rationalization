rm(list = ls())
################################################################################
# Data analysis for rationalizaion Experiment
# Fridolin Linder
#
# Input: main_study_clean.csv (cleaned qualtrix data output)
################################################################################
library(randomForest)
library(ggplot2)
library(scales)
library(grid)
library(dplyr)
library(rstan)
library(reshape2)
library(xtable)
#devtools::install_github("swager/randomForestCI")
#devtools::install_github("swager/randomForest")
#devtools::install_github("zmjones/edarf")
library(edarf)
library(gbm)
library(dismo)

# Load data
dat <- read.csv("../data/main_study/main_study_clean.csv", sep = ",")
  
#===============================================================================
# Some more data preparation
#===============================================================================

# Recode climate change question
dat$gwhow[is.na(dat$gwhow)] <- dat$gwhow2[!is.na(dat$gwhow2)]
dat$gwhow2 <- NULL 

# Group labels
dat$group_name <- NA
dat$group_name[dat$group == 1] <- "Group 1 (S asked first)"
dat$group_name[dat$group == 2] <- "Group 2 (P asked first)"

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
dat$pref2 <- NULL

#===============================================================================
# Descriptive plots
#===============================================================================

# Common plotting elements
THEME <- theme(panel.background = element_rect(fill = "white", colour = "black"),
               panel.grid.major = element_line(colour = "gray80"),
               axis.text=element_text(size=12),
               axis.title=element_text(size=14),
               legend.text=element_text(size=12),
               legend.title=element_text(size=14))
FILL  <- scale_fill_manual(values = c("cornflowerblue", "yellowgreen"), 
                           name = "Experimental Group")
COLOR <- scale_color_manual(values = c("cornflowerblue", "yellowgreen"), 
                            name = "Experimental Group")
WIDTH <- 6
HEIGHT <- 5

## Balance statistics 


# Choice questions
colnames(dat)[9] <- "req"
df <- dplyr::select(dat, -(startDate:req), -contains("attchk"), -contains("t_"),
             -contains("placement"), -group, -comment, -age, -tcompl, -comm)
df$pref <- as.integer(df$pref)


df1 <- melt(df)
df1$value <- factor(df1$value)
df1$group_name <- factor(df1$group_name)
df1$variable <- factor(df1$variable, levels = sort(levels(df1$variable)))
df1 <- filter(df1, value != -99)
df1$group_name <- as.factor(gsub(" \\(.+\\)", "", as.character(df1$group_name)))

p <- ggplot(df1) + 
      geom_bar(aes(x = group_name, fill = value)) + 
      facet_wrap( ~ variable) + 
      scale_fill_manual(values = c("cornflowerblue", "yellowgreen", "orange", "skyblue", "grey30")) +
      THEME + theme(axis.text.x = element_text(angle = 20, hjust = 1))
#ggsave(plot = p, filename = "../figures/main/bal.png")

# Time to complete survey
p <- ggplot(dat, aes(tcompl)) + THEME + geom_density() + 
      geom_histogram(aes(y = ..density..), colour = "yellowgreen", 
                     fill = "cornflowerblue", alpha = 0.8, binwidth = 1) + 
      labs(x = "Time in Minutes", y = "Density")
#ggsave(plot = p, filename = "../figures/main/time.png", width = 1.5 * WIDTH, 
#       height = HEIGHT)

# Plot preferences
p <- ggplot(dat, aes(x = pref, fill = group_name)) + THEME + FILL + 
      geom_bar(position = "dodge") + labs(x = "Preferred Party", y = "Count")
#ggsave(plot = p, filename = "../figures/main/preferences.png", width = 2 * WIDTH, 
#       height = HEIGHT)

# Exclude observations without party preference
dat <- dat[!is.na(dat$pref), ]


#===============================================================================
# Standard experimental test
#===============================================================================

## Completely naive: just average positions

# Experiment 1 (bias in S)
a <- dat$self_placement[dat$group == 1] # control group
b <- dat$self_placement[dat$group == 2]
t.test(a, b)
wilcox.test(a, b)

# Experiment 2 (bias in P)
c <- dat$party_placement[dat$group == 1]
d <- dat$party_placement[dat$group == 2] # control group
t.test(c, d)

## Split by party

# Experiment 1
# Democrats
e <- dat$self_placement[which(dat$group == 1 & dat$pref == "democrat")]
f <- dat$self_placement[which(dat$group == 2 & dat$pref == "democrat")]
t.test(e, f)

# Republicans
g <- dat$self_placement[which(dat$group == 1 & dat$pref == "republican")]
h <- dat$self_placement[which(dat$group == 2 & dat$pref == "republican")]
t.test(g, h)

# Experiment 2
# Democrats
i <- dat$party_placement[which(dat$group == 1 & dat$pref == "democrat")]
j <- dat$party_placement[which(dat$group == 2 & dat$pref == "democrat")]
t.test(i, j)

# Republicans
k <- dat$party_placement[which(dat$group == 1 & dat$pref == "republican")]
l <- dat$party_placement[which(dat$group == 2 & dat$pref == "republican")]
t.test(i, j)

## Means table

# self
self <- rbind(c(mean(e), mean(f), mean(e) - mean(f)),
              c(mean(g), mean(h), mean(g) - mean(h)))
self <- cbind(self, c(t.test(e, f)$p.value, t.test(g, h)$p.value))
colnames(self) <- c("Group 1", "Group 2", "Difference", "p-value")
rownames(self) <- c("Democrats.self", "Republicans.self")

# party
party <- rbind(c(mean(i), mean(j), mean(i) - mean(j)),
              c(mean(k), mean(l), mean(k) - mean(l)))
party <- cbind(party, c(t.test(i, j)$p.value, t.test(k, l)$p.value))
colnames(party) <- c("Group 1", "Group 2", "Difference", "p-value")
rownames(party) <- c("Democrats.party", "Republicans.party")

tot <- rbind(c(mean(a), mean(b), mean(a) - mean(b), t.test(a, b)$p.value),
             c(mean(c), mean(d), mean(c) - mean(d), t.test(c, d)$p.value))
colnames(tot) <- colnames(self)
rownames(tot) <- c("All.self", "All.party")

m_tab <- rbind(tot, self, party)
m_tab <- xtable(m_tab, digits = 3, caption = "Means for self and party placement in both groups for both experiments and split by preferred party")
#print(m_tab, type = "latex" , file = "../paper/means_table.tex" )

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
  drill + gwhap + gwhow + aauni + aawork + gun + comm + edu + sex + age
fit <- randomForest(mod, data = dat[dat$group == 1, ],  importance = T, keep.inbag = TRUE, mtry = 7, ntree = 1000)
fit

### Test other algos

## Boosted trees
brt <- gbm(mod, distribution = "gaussian", data = dat[dat$group == 1, ],
           n.trees = 10000)
perf <- gbm.perf(brt, method = "OOB")
brt <- gbm(mod, distribution = "gaussian", data = dat[dat$group == 1, ],
           n.trees = perf)
mean(brt$train.error)

# EDA for random forest
## pd <- partial_dependence(var = c("hltref", "gaymarry", "gayadopt", "abrtch",
##                            "abrthlth", "abrtinc", "abrtbd", "abrtrpe", "abrtdth",
##                            "abrtfin", "fedenv", "fedwlf", "fedpoor", "fedschool",
##                            "drill", "gwhap", "gwhow", "aauni", "aawork", "gun",
##                            "comm", "edu", "sex", "age"), fit = fit, df = dat)

## p <- plot_pd(pd)
#ggsave(plot = p, filename = "../figures/main/partial_dependence.png")

imp <- sort(fit$importance[, 1], decreasing = T)
pdat <- data.frame(imp = imp, var = names(imp))
rownames(pdat) <- NULL
pdat$var <- factor(pdat$var, levels = pdat$var[order(pdat$imp)])

# Plot importance
p <- ggplot(pdat, aes(var, imp)) + THEME + 
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(y = "Mean Increase in MSE after Permutation") +
  theme(plot.margin = unit(rep(.15, 4), "in"), axis.title.y = element_blank()) + 
  coord_flip()
#ggsave(plot = p, filename = "../figures/main/varimp.png", height = HEIGHT, 
#       width = 1.5 * WIDTH)


# Put predictions into data.frame
dat$pred[dat$group == 1] <- predict(fit)
dat$pred[dat$group == 2] <- predict(fit, dat[dat$group == 2, ])

# Check prediction bias on group 1
mean(dat$pred[dat$group == 1] - dat$self_placement[dat$group == 1])
mean(dat$pred[dat$group == 2] - dat$self_placement[dat$group == 2])
# Prediction for both groups
pdat <- data.frame(observed = dat$self_placement,
                   predicted = dat$pred,
                   candidat = dat$party_placement,
                   group = dat$group_name
                   )
p <- ggplot(pdat, aes(observed, predicted, color = group)) + THEME + COLOR + 
        geom_point() +# geom_abline(intercept = 0, slope = 1) + 
        ylim(0, 100) + xlim(0, 100) + 
        stat_smooth(size = 1, se = FALSE) + 
        labs(y = "Predicted S", x = "Observed S")
#ggsave(plot = p, filename = "../figures/main/prediction.png", width = 2.3 * WIDTH,
#       height = 2 * HEIGHT)

# Look at distribution of self and party placements
p <- ggplot(dat, aes(x = self_placement, color = group_name)) + THEME + 
      scale_color_manual(values = c("cornflowerblue", "yellowgreen"), 
                         name = "Experimental Group") + 
      geom_density(alpha = .3, size = 1) + 
      facet_wrap( ~ pref) +
      labs(x = "Self Placement", y = "Density")
#ggsave(plot = p, filename = "../figures/main/dist_self.png", width = 2 * WIDTH,
#       height = 1.5 * HEIGHT)

p <- ggplot(dat, aes(x = party_placement, color = group_name)) + THEME + COLOR + 
      facet_wrap( ~ pref) + 
      geom_density(alpha = .3, size = 1) + 
      labs(x = "Party Placement", y = "Density")
#ggsave(plot = p, filename = "../figures/main/dist_party.png", width = 2 * WIDTH,
#       height = 1.5 * HEIGHT)

#===============================================================================
# Main Analysis
#===============================================================================

## Bias in self
#X <- (dat[dat$group == 1, "self_placement"] - dat$pred[dat$group == 1])^2
#Y <- (dat[dat$group == 2, "self_placement"] - dat$pred[dat$group == 2])^2

# Bias in Self: Deviations towards preferred party
s_dist <- cbind(dat$party_placement, dat$self_placement)
# normalize to have S* = 0 for all i
s_dist_norm <- s_dist - dat$pred
dat$signed_s_dist <- s_dist_norm[, 1] * sign(s_dist_norm[, 2])

X <- dat$signed_s_dist[dat$group == 1]
Y <- dat$signed_s_dist[dat$group == 2]

# Bias in party
Z <- (dat$self_placement[dat$group == 1] - dat$party_placement[dat$group == 1])^2
W <- (dat$pred[dat$group == 2] - dat$party_placement[dat$group == 2])^2

# Visualize distribution of differences
df <- data.frame(distance = c(X, Y, Z, W),
                 group = rep(c(rep("Group 1 (S asked first)", length(X)), 
                               rep("Group 2 (P asked first)", length(Y))), 2),
                 experiment = c(rep("Experiment 1: Bias in Self", length(c(X, Y))), 
                                rep("Experiment 2: Bias in Party", length(c(Z, W))))
                 )

p <- ggplot(df, aes(distance, color = group)) + THEME + COLOR + 
      geom_density(size = 1) +
      facet_wrap( ~ experiment, scales = "free") +
      labs(x = "Distance", y = "Density")
#ggsave(plot = p, filename = "../figures/main/dist_dista_dens.png", width = 2 * WIDTH, 
#       height = HEIGHT)

p <- ggplot(df, aes(x = group, y = distance, color = group)) + THEME + 
      geom_boxplot(color = "grey10", outlier.size = 0.1) + 
      geom_point(alpha = 0.8, position = "jitter") + 
      facet_wrap( ~ experiment, scales = "free") + 
      scale_color_manual(values = c("cornflowerblue", "yellowgreen"), 
                          guide = F) + 
      labs(x = "Experimental Group", y = "Distance")
#ggsave(plot = p, filename = "../figures/main/dist_dista_box.png", width = 2 * WIDTH, 
#       height = HEIGHT)

# Logg dists in Exp 2 
df2 <- data.frame(distance = log(c(Z, W) + 1),
                  group = rep(c(rep("Group 1 (S asked first)", length(X)), 
                                rep("Group 2 (P asked first)", length(Y))), 2)
)
p <- ggplot(df2, aes(distance, color = group)) + THEME + COLOR + 
  geom_density(size = 1) +
  labs(x = "Distance", y = "Density")
#ggsave(plot = p, filename = "../figures/main/dist_dista_log.png", width = 1.5 * WIDTH,
#       height = HEIGHT)

# K-S tests
lZ <- log(Z + 1)
lW <- log(W + 1)

ksX <- ks.test(X, "pnorm")
ksY <- ks.test(Y, "pnorm")
ksZ <- ks.test(lZ, "pnorm")
ksW <- ks.test(lW, "pnorm")

ks_tab <- rbind(c(ksX$statistic, ksX$p.value),
                c(ksY$statistic, ksY$p.value),
                c(ksZ$statistic, ksZ$p.value),
                c(ksW$statistic, ksW$p.value))
colnames(ks_tab) <- c("Statistic", "p-value")
rownames(ks_tab) <- c("X", "Y", "log(Z + 1)", "log(W + 1)")
ks_tab <- xtable(ks_tab, digits = 3, caption = "Kolmogorov-Smirnoff test for normality
                 for the outcomes of Experiment 1 and the log-transformed outcomes of
                 Experiment 2. In all cases the Null-Hypothesis of a normal distribution
                 is rejected")
#print(ks_tab, type = "latex" , file = "../paper/ks_tests.tex" )
#===============================================================================
# Frequentist Tests
#===============================================================================

# Wilcoxon test
wc1 <- wilcox.test(X, Y, alternative = "less")
wc2 <- wilcox.test(Z, W, alternative = "less")

wc <- cbind(c(wc1$statistic, wc1$p.value),
            c(wc2$statistic, wc2$p.value))
colnames(wc) <- c("Experiment 1", "Experiment 2")
rownames(wc) <- c("Statistic", "p-value")
wc <- xtable(wc, digits = 3, caption = "Wilcoxon rank tests for Experiments 1
             and 2. P-values for one sided tests.")
#print(wc, type = "latex" , file = "../paper/np_res_table.tex" )

# T-test for experiment 1
ttest <- t.test(X, Y, alternative = "less")
tt <- cbind(mean(X), mean(Y), mean(X) - mean(Y), ttest$p.value)
colnames(tt) <- c("Group 1 (control)", "Group 2", "Difference in Means", "p-value")
tt <- xtable(tt, digits = 3, caption = "One sided T-test for Experiment 1")
#print(tt, type = "latex" , file = "../paper/t_res_table.tex" )

#===============================================================================
# Bayesian analysis of results
#===============================================================================

# First: just estimate the parameters
model_gamma <- "
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
  alpha ~ gamma(0.0001, 0.0001);
  beta ~ gamma(0.0001, 0.0001);
  
  // likelihood
  for (n in 1:N){
    y[n] ~ gamma(alpha[groupID[n]], beta[groupID[n]]);
  }
}

generated quantities {
  vector[2] mu;
  real mu_diff;
  real mu_ratio;
  vector[N] y_rep;
  mu[1] <- alpha[1] / beta[1];
  mu[2] <- alpha[2] / beta[2];
  mu_diff <- mu[1] - mu[2];
  mu_ratio <- mu[1] / mu[2];

  for (n in 1:N)
    y_rep[n] <- gamma_rng(alpha[groupID[n]], beta[groupID[n]]);
}
"

model_t = "
  data {
    int<lower=1> N;
    vector[N] y;
    int<lower=1> groupID[N];
  }
  parameters {
    vector[2] mu;                                
    vector<lower=0>[2] sigma;                    
    real<lower=0, upper=100> nu;                 
  }
  model {
    // priors
    mu ~ normal(0, 1000);                  
    sigma ~ inv_gamma(1, 1);
    nu ~ exponential(0.001);   
    // likelihood
    for (n in 1:N){
      y[n] ~ student_t(nu, mu[groupID[n]], sigma[groupID[n]]);
  
    }
  }
  generated quantities {
    vector[N] y_rep;
    real mu_diff;
    real mu_ratio;
    mu_diff <- mu[1] - mu[2];
    mu_ratio <- mu[1] / mu[2];
    for (n in 1:N){
      y_rep[n] <- student_t_rng(nu, mu[groupID[n]], sigma[groupID[n]]);
    }
    
    
  }
"

## Compile models
#c_mod_gamma <- stan_model(model_code = model_gamma)
#save(c_mod_gamma, file = "c_mod_gamma.RData")
#c_mod_t <- stan_model(model_code = model_t)
#save(c_mod_t, file = "c_mod_t.RData")
load("c_mod_gamma.RData")
load("c_mod_t.RData")


# ---------------------------------------------------
# Compare Bias in self (x and Y) (Experiment 1)

y = c(X, Y)
groupID = c(rep(1, length(X)), rep(2,  length(Y)))
standata_1 <- list(N = length(y), groupID = groupID, y = y)

# Sample from it
stanfit_1 <- sampling(object = c_mod_t, data = standata_1, iter = 30000, 
                      warmup = 5000, chains = 1)
save(stanfit_1, file = "stanfit_1.RData")
#load("stanfit_1.RData")
pex1 <- c("mu", "sigma", "nu", "mu_diff", "mu_ratio")
post_1 <- as.data.frame(do.call(cbind, rstan::extract(stanfit_1, pars = pex1)))
colnames(post_1) <- c("mu_1", "mu_2", "sigma_1", "sigma_2", "nu", "mu_diff", 
                      "mu_ratio")

# Probability for substantively significant effect
length(which(post_1$mu_ratio < 0.9))/nrow(post_1)


# ---------------------------------------------------
# Compare Bias in party

y = c(Z, W)
groupID = c(rep(1, length(Z)), rep(2,  length(W)))
standata_2 <- list(N = length(y), groupID = groupID, y = y)

# Sample from it
#stanfit_2 <- sampling(object = c_mod_gamma, data = standata_2, iter = 15000, 
#                      warmup = 5000, chains = 2)
#save(stanfit_2, file = "stanfit_2.RData")
load("stanfit_2.RData")
pex2 <- c("alpha", "beta", "mu", "mu_diff", "mu_ratio")
post_2 <- as.data.frame(do.call(cbind, rstan::extract(stanfit_2, pars = pex2)))
colnames(post_2) <- c("alpha_1", "alpha_2", "beta_1", "beta_2", "mu_1", "mu_2",
                      "mu_diff", "mu_ratio")

# Probability for substantively significant effect
length(which(post_2$mu_ratio < 0.9))/nrow(post_2)

# ------------------------------------------------
# Check Model fit

B <- 100 # number of replications
y_rep_full_1 <- rstan::extract(stanfit_1, "y_rep")$y_rep
y_rep_1 <- y_rep_full_1[sample(c(1:nrow(y_rep_full_1)), B), ]
y_rep_full_2 <- rstan::extract(stanfit_2, "y_rep")$y_rep
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
                       groupID = rep(rep(c("Group 1 (S asked first)", 
                                           "Group 2 (P asked first)"), 
                                         c(length(X), length(Y))), 2),
                       type = "observed"
                       )

pdat <- as.data.frame(rbind(gdat, observed))
lvl <- rev(as.character(sort(unique(pdat$iteration))))
pdat$iteration <- factor(pdat$iteration, levels = lvl)

# Zoom in
#pdat <- filter(pdat, value < 2500)

p <- ggplot(pdat, aes(x = value, alpha = iteration, color = type, size = type)) +
        THEME + 
        geom_line(stat = "density") + 
        scale_alpha_manual(values = c(rep(0.2, B), 1), guide = F) +
        scale_color_manual(values = c("orangered1", rep("cornflowerblue", B)), 
                           name = "") +
        scale_size_manual(values = c(1, rep(0.5, B)), name = "") +
        facet_wrap( ~ experiment + groupID, scales = "free")
## ggsave(plot = p, filename = "../figures/main/post_pred.png", width = 2.5 * WIDTH, 
##        height = 1.5 * HEIGHT)

# ------------------------------------------------
# Visualize results

## Tables of results

# Experiment 1
res_1 <- data.frame(Mean = apply(post_1, 2, mean),
                    "Quantile_0.025" = apply(post_1, 2, quantile, 0.025),
                    "Quantile_0.975" = apply(post_1, 2, quantile, 0.975),
                    "Std.Error" = apply(post_1, 2, function(x) sqrt(var(x))))
# Experiment 2
res_2 <- data.frame(Mean = apply(post_2, 2, mean),
                    "Quantile_0.025" = apply(post_2, 2, quantile, 0.025),
                    "Quantile_0.975" = apply(post_2, 2, quantile, 0.975),
                    "Std.Error" = apply(post_2, 2, function(x) sqrt(var(x))))
rownames(res_2) <- gsub("mu_", "theta_", rownames(res_2))

res_out_1 <- xtable(res_1, digits = 3, caption = "Parameter estimates for Experiment 1")
#print(res_out_1, type = "latex" , file = "../paper/res_table_1.tex" )
res_out_2 <- xtable(res_2, digits = 3, caption = "Parameter estimates for Experiment 2")
#print(res_out_2, type = "latex" , file = "../paper/res_table_2.tex")


# Posterior distribution of ratio and difference in means
pdat <- data.frame(difference = c(post_1$mu_diff, post_1$mu_ratio, post_2$mu_diff, post_2$mu_ratio),
                   experiment = rep(c("Experiment 1: Bias in S", "Experiment 2: Bias in P"), each = 2 * nrow(post_1)),
                   type = rep(rep(c("Difference", "Ratio"), each = nrow(post_1)), 2)
                   )
pdat$above <- "Probability Mass in Favor \nof the Hypothesis"
pdat$above[pdat$type == "Difference" & pdat$difference > 0] <- "Probability Mass Against \nthe Hypothesis"
pdat$above[pdat$type == "Ratio" & pdat$difference > 1] <- "Probability Mass Against \nthe Hypothesis"
pdat$above <- as.factor(pdat$above)
pdat$vline <- ifelse(pdat$type == "Ratio", 1, 0)

# Posterior probabilities for/against Hypotheses
for_H1 <- nrow(filter(pdat, type == "Difference" & difference < 0 & 
                        experiment == "Experiment 1: Bias in S")) 
n_H1 <- nrow(filter(pdat, type == "Difference" & experiment == "Experiment 1: Bias in S"))
p_for_H1 <- for_H1 / n_H1

for_H2 <- nrow(filter(pdat, type == "Difference" & difference < 0 & 
                        experiment == "Experiment 2: Bias in P")) 
n_H2 <- nrow(filter(pdat, type == "Difference" & experiment == "Experiment 2: Bias in P"))
p_for_H2 <- for_H2 / n_H2

pdat_text <- data.frame(experiment = rep(c("Experiment 1: Bias in S",
                                           "Experiment 2: Bias in P"),
                                         each = 2),
                        type = rep(c("Difference", "Ratio"), 2),
                        x = c(-3, 0.72, -150, 0.71),
                        y = rep(750, 4),
                        lab = as.character(rep(round(c(p_for_H1, p_for_H2), 3), 
                                               each = 2)),
                        above = NA
                        )

p <- ggplot(pdat, aes(x = difference, fill = above)) + THEME + 
      facet_wrap( ~ type + experiment, scales = "free") + 
      geom_histogram(color = "yellowgreen") + 
      theme(legend.key.height=unit(2,"line")) + 
      scale_fill_manual(values = c("red", "cornflowerblue"), name = "") +
      geom_vline(aes(xintercept=vline), data = pdat) +
      labs(x = "Mean Difference/Ratio", y = "") + 
      geom_text(data = pdat_text, aes(x = x, y = y, label = lab), size = 7, color = "white")
## ggsave(plot = p, filename = "../figures/main/mean_diff_ratio.png", width = 2.5 * WIDTH, 
##        height = 1.5 * HEIGHT)

## Plot only for difference in means
p <- ggplot(pdat[pdat$type == "Difference", ], aes(x = difference, fill = above)) +
      THEME + 
      facet_wrap( ~ experiment, scales = "free") + 
      geom_histogram(color = "yellowgreen") + 
      theme(legend.key.height=unit(2,"line")) + 
      scale_fill_manual(values = c("red", "cornflowerblue"), name = "") +
      geom_vline(aes(xintercept = vline), data = pdat[pdat$type == "Difference", ]) +
      labs(x = "Mean Difference/Ratio", y = "") + 
      geom_text(data = pdat_text[pdat_text$type == "Difference", ],
                aes(x = x, y = y, label = lab), size = 7, color = "white")
ggsave(plot = p, filename = "../figures/main/mean_diff.png", width =  2.5 * WIDTH, 
       height = HEIGHT)

# -------------------------------------------
# Trace plots
pex1 <- c("mu", "sigma", "nu")
samp_1 <- extract(stanfit_1, pars = pex1, permute = F)
pdat1 <- melt(samp_1)
p <- ggplot(pdat1, aes(x = iterations, y = value, color = chains)) + THEME +
      geom_line() + facet_wrap( ~ parameters, scales = "free", ncol = 1) + 
      scale_color_manual(values = c("cornflowerblue", "yellowgreen"))
ggsave(plot = p, filename = "../figures/main/trace_1.png", height = 2 * WIDTH, 
       width = 2 * HEIGHT)

pex2 <- c("alpha", "beta")
samp_2 <- extract(stanfit_2, pars = pex2, permute = F)
pdat2 <- melt(samp_2)
p <- ggplot(pdat2, aes(x = iterations, y = value, color = chains)) + THEME + 
      geom_line() + facet_wrap( ~ parameters, scales = "free", ncol = 1) + 
      scale_color_manual(values = c("cornflowerblue", "yellowgreen"))
ggsave(plot = p, filename = "../figures/main/trace_2.png", height = 2 * WIDTH, 
       width = 2 * HEIGHT)

# ------------------------------------------------------------------------
# Robustness check
# ------------------------------------------------------------------------

# Different priors for gamma model

# First: just estimate the parameters
model_gamma_r <- "
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
      vector[N] y_rep;
      mu[1] <- alpha[1] / beta[1];
      mu[2] <- alpha[2] / beta[2];
      mu_diff <- mu[1] - mu[2];
      mu_ratio <- mu[1] / mu[2];
      
      for (n in 1:N)
        y_rep[n] <- gamma_rng(alpha[groupID[n]], beta[groupID[n]]);
    }
"

## Compile models
#c_mod_gamma_r <- stan_model(model_code = model_gamma_r)
#save(c_mod_gamma_r, file = "c_mod_gamma_r.RData")
load("c_mod_gamma_r.RData")

y = c(Z, W)
groupID = c(rep(1, length(X)), rep(2,  length(Y)))
standata_2_r <- list(N = length(y), groupID = groupID, y = y)

# Sample from it
stanfit_2_r <- sampling(object = c_mod_gamma_r, data = standata_2_r, iter = 5000, 
                      warmup = 1000, chains = 2)
post_2_r <- as.data.frame(do.call(cbind, extract(stanfit_2_r, pars = c("alpha",
                                                                       "beta", "mu", 
                                                                       "mu_diff", 
                                                                       "mu_ratio"))))
colnames(post_2_r) <- c("alpha_1", "alpha_2", "beta_1", "beta_2", "mu_1", "mu_2",
                        "mu_diff", "mu_ratio")

## Tables of results

# Experiment 2 robustness
res_2_r <- data.frame(Mean = apply(post_2_r, 2, mean),
                     "Quantile_0.025" = apply(post_2_r, 2, quantile, 0.025),
                     "Quantile_0.975" = apply(post_2_r, 2, quantile, 0.975),
                     "Std.Error" = apply(post_2_r, 2, function(x) sqrt(var(x))))

res_out_r <- xtable(res_2_r, digits = 3, caption = "Parameter estimates for 
                    robustness check for Experiment 2")
print(res_out_r, type = "latex" , file = "../paper/res_r_table.tex" )
