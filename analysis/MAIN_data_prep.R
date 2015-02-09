rm(list=ls())
#===============================================================================
# This script takes the outputs from MTurk and Qualtrics MAIN study and prepares 
# the data set used for analysis
#===============================================================================

library(dplyr)

qualt <- read.csv("../data/main_study/rationalization_main.csv", 
                      header = T)
turk <- read.csv("../data/main_study/Batch_1811015_batch_results.csv", 
                 header = T)
code <- read.csv("../data/main_study/qualt_codebook.csv", 
                 header = T, sep = ",")

# Variable names
colnames(qualt) <- code$var_name

# Merge Mturk and qualtrics data
dat <- merge(qualt, turk, by.x = "mturkCode", all = T)

# Drop non validated
dat$mturkCode[dat$mturkCode == ""] <- NA
dat$mturkCode[dat$mturkCode == "no survey code given!  Worker ID: A248LF7KKHXZ14"] <- NA
dat$mturkCode[grep("code in email", dat$mturkCode)] <- NA # Put worker ID into code field
dat <- dat[!is.na(dat$mturkCode), ]
dat$mturkCode <- as.numeric(dat$mturkCode)
dat$comment[grep("@", dat$comment)]  <- NA # Delete personal email from comments

# Get rid of special chars in comments
dat$comment <- gsub("[[:punct:]]", " ", dat$comment)
dat$pref2 <- gsub("[[:punct:]]", " ", dat$pref2)

# Completion time
dat$startDate <- strptime(dat$startDate, format = "%m/%d/%Y %H:%M")
dat$endDate <- strptime(dat$endDate, format = "%m/%d/%Y %H:%M")
dat$tcompl <- as.numeric(dat$endDate - dat$startDate)

# Experimental group indicator
dat$group <- NA
dat$group[!is.na(dat$self_eg1)] <- 1
dat$group[!is.na(dat$self_eg2)] <- 2

# Process Ideological placement (make numeric and put each in one var)
num_ideo <- apply(as.matrix(dat[, grep("eg", colnames(dat))]), 2, as.numeric)
mergeNA <- function(x, y) {
  x[is.na(x)] <- y[is.na(x)]
  return(x)
} 
dat$self_placement <- mergeNA(dat$self_eg1, dat$self_eg2)
dat$candidate_placement <- mergeNA(mergeNA(dat$can_eg1_1, dat$can_eg1_2),
                                  mergeNA(dat$can_eg2_1, dat$can_eg2_2))
dat <- dat[, -grep("eg", colnames(dat))]

# select columns for anonymized publishable data set
out <- select(dat, startDate:comment, tcompl:candidate_placement)
write.table(out, file = "../data/main_study/main_study_clean.csv", sep = ",",
            row.names = F)

# geo locations for map, shuffle randomly for anonymization
geo_loc <- select(dat, locLa:locLo)
geo_loc$rand <- runif(nrow(geo_loc))
geo_loc <- geo_loc[order(geo_loc$rand), -3]
write.table(geo_loc, file = "../data/main_study/main_study_geo_loc.csv", sep = ",",
            row.names = F)
