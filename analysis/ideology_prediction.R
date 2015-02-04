# Find best predictors of self reported ideology from anes data using random 
# forests

rm(list = ls())

library(ggplot2)
library(scales)
library(randomForestSRC)
library(party)
library(edarf)

df <- read.table('../data/preparation/anes_clean.csv', sep = ",", header = T)
#df$libcpre_self <- as.factor(df$libcpre_self)

#=================
# Predictive model
#=================

mod <- formula(paste0('libcpre_self~', 
                     paste(names(df)[!names(df) == 'libcpre_self'], 
                           collapse = '+')
                     )
              )
#grow <- rfsrc(mod, data = df, importance = 'permute',na.action = 'na.impute')

#save(grow,file='fitted_forest.RData')
load('fitted_forest.RData')

# Plot variable importance
imp <- sort(grow$importance, decreasing = T)
pdat <- data.frame(imp = imp, var = names(imp))
rownames(pdat) <- NULL
pdat$var <- factor(pdat$var, levels = pdat$var[order(pdat$imp)])

# plot importance of all predictors
p <- ggplot(pdat, aes(var, imp))
p <- p + geom_bar(stat = "identity")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + labs(y = "Mean Increase in MSE after Permutation")
p <- p + theme_bw()
p <- p + theme(plot.margin = unit(rep(.15, 4), "in"), axis.title.y = element_blank())
p <- p + coord_flip()
ggsave(plot = p, filenam = "figures/varimp_anes.png")

# Get category with highest predicted prob in out of bag prediction
pred <- apply(grow$predicted.oob, 1, which.max)
clss <- table(pred, df$libcpre_self)

# perc correctly classified
sum(diag(clss))/sum(clss) * 100

# random classification
r <- sample(c(1:7), nrow(df), replace = T)
clss1 <- table(r, df$libcpre_self)

# perc correctly classified
sum(diag(clss1))/sum(clss1) *100

## Visualize Classifier performance

# Scatterplot
d <- data.frame(pred = grow$predicted, obs = df$libcpre_self, 
                class = round(grow$predicted, 0), 
                cor = ifelse(round(grow$predicted, 0) == df$libcpre_self, 1, 0)
                )
p <- ggplot(d, aes(obs, pred))
p <- p + geom_jitter(alpha = .3)
p <- p + labs(y = "OOB Predicted", x = "Observed")
p

## 20 Best predictors and redced sample
df.red <- df[sample(c(1:nrow(df)), 500), ]
pred <- names(imp)[1:20]
mod.red <- formula(paste0('libcpre_self~', paste(pred, collapse = '+')))
#grow.red <- rfsrc(mod.red, data = df.red, importance = 'permute', 
#                  na.action = 'na.impute'
#                  )
#save(grow.red, file = "reduced_fitted_forest.RData")
load(reduced_fitted_forest.RData)

pred.red <- apply(grow.red$predicted.oob, 1, which.max)
clss.red <- table(pred.red, df.red$libcpre_self)
sum(diag(clss.red))/sum(clss.red) *100
