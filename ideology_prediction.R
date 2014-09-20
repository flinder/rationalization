# Find best predictors of self reported ideology from anes data using random forests
rm(list=ls())
library(ggplot2)
library(randomForestSRC)

setwd("C:/Users/samsung/Dropbox/rationalization/rationalization")
df = read.table('data/anes.csv')

#=================
# Predictive model
#=================

mod = formula(paste0('libcpre_self~',paste(names(df)[!names(df)=='libcpre_self'],collapse='+')))
grow = rfsrc(mod,data=df,importance='permute',na.action='na.impute')


#save(grow,file='grow_pred.RData')
#load('grow_pred.RData')

# Plot variable importance
imp = sort(grow$importance,decreasing=T)
plot(imp,seq(1:length(imp)),pch=18,cex=.8)
imp[1:20]

clss = table(round(grow$predicted,0),df$libcpre_self)

# perc correctly classified
sum(diag(clss))/sum(clss) *100

# random classification
r = sample(c(1:7),nrow(df),replace=T)
clss1 = table(r,df$libcpre_self)

# perc correctly classified
sum(diag(clss1))/sum(clss1) *100

 
d = data.frame(pred=grow$predicted,obs=df$libcpre_self,class=round(grow$predicted,0),
               cor=ifelse(round(grow$predicted,0)==df$libcpre_self,1,0))
p <- ggplot(d, aes(obs, pred))
p <- p + geom_jitter(alpha=.3)
p



## 20 Best predictors
pred= names(imp)[1:20]
mod.red = formula(paste0('libcpre_self~',paste(pred,collapse='+')))
grow.red = rfsrc(mod.red,data=df,importance='permute',na.action='na.impute')
clss.red = table(round(grow.red$predicted,0),df$libcpre_self)
sum(diag(clss.red))/sum(clss) *100




## Visualize classification

# Confusion PLot
plot <- ggplot(as.data.frame(clss))
plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq))

# Jittered scatterplot
n = nrow(df.nna)
a = as.numeric(df.nna$K1) + rnorm(n,0,0.1)
b = as.numeric(forest$predicted) + rnorm(n,0,0.1)
dta = data.frame(a,b)
plot(a,b)

ggplot(dta, aes(x=a, y=b)) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4)     # 1/4 opacity


stop()
############ Old stuff

incl = '^health_|^libcpre_self|^divgov_|^ineq_|^usworld_|^war_|^spsrvpr_^inspre_|^gun_|^guarpr_self|^immig_|^aidbpr_|^envjobs_|^aa_|^fedspend_|^trust_|^respons_|^envir_|^gayrt_|^penalty_|^relig_|^dem_age_r_x|^dem_edu|^wealth_|^inc_|^owngun_|^orientn_|^happ|^abort_'


#abort_health|abort_health_x|abort_fatal|abort_fatal_x|abort_incest|
#abort_incest_x|abort_rape|abort_rape_x|abort_bd|abort_bd_x|abort_fin|
#  abort_fin_x|abort_sex|abort_sex_x|abort_choice|abort_choice_x

df = anes[,grep(incl,names(anes))]


# Drop vars
drops = which(names(df) %in% c('gayrt_discrev','gayrt_discstrev','gayrt_milrev'
                               ,'gayrt_milstrev','gayrt_discstd','gayrt_discststd'
                               ,'gayrt_milstd','gayrt_milststd','dem_edu','penalty_favdpen'
                               ,'penalty_dpenstr','aa_work','aa_uni','ineq_incgap'
                               ,'health_2010hcr','finance_finnext','abort_rep4','abort_dem4'
                               ,'abort_rpc4','abort_dpc4','abort_incest','abort_bd'
                               ,'abort_fatal','abort_health','abort_choice','abort_rape'
                               ,'abort_sex','abort_fin','aa_unifav','aa_uniopp','aa_workfav'
                               ,'aa_workopp','abort_bdfav','abort_bdln','abort_bdopp','abort_choicefav'
                               ,'abort_choiceln','abort_choiceopp','abort_dem4_oth'
                               ,'abort_dpc4_oth',
))
df = df[,-drops]

