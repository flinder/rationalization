### Prep ANES data for use in rationalizationproject
# Author: Fridolin Linder

rm(list=ls())
library(foreign)

#=================
# Data preparation
#=================

setwd("C:/Users/flinder/Dropbox/rationalization/analysis/")
anes <- read.dta('data/anes/anes_timeseries_2012_stata12.dta')

incl <- c('aa_uni_x','aa_work_x','abort_bd_x','abort_choice_x','abort_fatal_x'
         ,'abort_fin_x','abort_health_x','abort_incest_x','abort_rape_x'
         ,'abort_sex_x','dem_edugroup_x','envir_drill','envir_gwarm','envir_gwgood'
         ,'envir_gwhow','envir_nuke','fedspend_child','fedspend_crime','fedspend_enviro'
         ,'fedspend_poor','fedspend_schools','fedspend_scitech','fedspend_ss'
         ,'fedspend_welfare','gayrt_adopt','gayrt_disc','gayrt_marry','gayrt_mil'
         ,'guarpr_self','gun_control','gun_importance','health_2010hcr_x','happ_lifesatisf'
         ,'libcpre_self','gayrt_discrev','gayrt_discstd','gayrt_milrev','gayrt_milstd'
)

df <- anes[,colnames(anes) %in% incl]
rm(anes)

## Merge std and rev q-format on gayrights
names(df)[grep('std',names(df))]

# Conform levels
levels(df$gayrt_milrev_x) <- levels(df$gayrt_milstd_x)

df$gayrt_discstd_x[df$gayrt_discstd_x == '-1. inapplicable'] <- df$gayrt_discrev_x[df$gayrt_discstd_x == '-1. inapplicable']
colnames(df)[colnames(df) == 'gayrt_discstd_x'] <- 'gayrt_disc'
df <- df[,colnames(df)!='gayrt_discrev_x']

df$gayrt_milstd_x[df$gayrt_milstd_x == '-1. inapplicable'] <- df$gayrt_milrev_x[df$gayrt_milstd_x == '-1. inapplicable']
colnames(df)[colnames(df) == 'gayrt_milstd_x'] <- 'gayrt_mil'
df <- df[,colnames(df)!='gayrt_milrev_x']


# recode NAs
options(warn=1)
pat <- '[-][1-9]\\.'
for(i in 1:ncol(df)){
  if(is.factor(df[,i])){
    df[grep(pat,df[,i]),i] <- NA
    df[,i] <- factor(df[,i])
  } 
}

## drop vars with more than 11% NAs (lib-cons self has 10%)
#propNA <- function(vector) length(which(is.na(vector)))/nrow(df)
#miss <- data.frame(sort(apply(df,2,propNA)))
#drop <- which(apply(df,2,propNA)>0.11)
#df <- df[,-drop]

## Recode covars to numeric (where sensible)
lapply(df,class)

# What should stay factor:
stay <- c('orientn_rgay','envir_drill','envir_gwgood','envir_gwhow','envir_nuke'
         ,'immig_checks','immig_citizen','gun_control','war_terror'
         ,'divgov_splitgov','abort_bd_x','abort_choice_x','abort_fatal_x'
         ,'abort_fin_x','abort_health_x','abort_incest_x','abort_rape_x'
         ,'abort_sex_x'
         #,'libcpre_self'
)


for(i in 1:ncol(df)){
  if(is.factor(df[,i]) & !colnames(df)[i] %in% stay){
    df[,i] <- as.numeric(df[,i])
  }
}


# Drop vars with missingness on dependent var
df <- df[-which(is.na(df$libcpre_self)),]

write.table(df,'data/anes/anes.csv')
