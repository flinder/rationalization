# This script takes the outputs from MTurk and Qualtrics pre_run and prepares 
# the data set used for analysis
library(dplyr)

rm(list=ls())

qualt.raw <- read.csv("../data/pre_run/qualtrics/rationalization_prerun_raw.csv", 
                      header = T)
turk <- read.csv("../data/pre_run/mturk/Batch_1739345_batch_results.csv", 
                 header = T)
code <- read.csv("../data/pre_run/qualtrics/prerun_codebook.csv", 
                 header = T, sep = "\t")

### Prep qualtrics data
# Variable names
qualt <- qualt.raw[-1, ]
colnames(qualt) <- code$var_name

# Completion time
qualt$startDate <- strptime(qualt$startDate, format = "%Y-%m-%d %H:%M:%S")
qualt$endDate <- strptime(qualt$endDate, format = "%Y-%m-%d %H:%M:%S")
qualt$tcompl <- as.numeric(qualt$endDate - qualt$startDate)/60

# Compare validation codes
out.code <- na.omit(as.numeric(as.character(qualt$mturkCode)))
in.code <- turk$Answer.surveycode

# Dummy for validated mturkers
qualt$validated <- qualt$mturkCode %in% in.code

# Save non validated
qualt.nv <- qualt[!qualt$validated, ]

# Drop non validated
qualt <- qualt[qualt$validated, ]

# Experimental group indicator
qualt$group <- NA
qualt$group[qualt$self_eg1 != ""] <- 1
qualt$group[qualt$self_eg2 != ""] <- 2

# level ideo
num_ideo <- apply(as.matrix(qualt[, grep("eg", colnames(qualt))]), 2, as.numeric)
qualt[, grep("eg", colnames(qualt))] <- num_ideo

# change typo varname
colnames(qualt)[38] <- "can_eg1_1"

# Anonymize data set
head(qualt)
out <- select(qualt, startDate:finished, hltref:comment, tcompl:group)

# Output data file
write.table(out, file = "../data/pre_run/pre_run_clean.csv", sep = ",", 
            row.names = F)
