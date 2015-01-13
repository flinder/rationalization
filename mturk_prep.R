rm(list=ls())
setwd("C:/Users/Fridolin Linder/Dropbox/rationalization/rationalization")

qualt.raw <- read.csv("Data/prerun/qualtrix/rationalization_prerun_raw.csv", 
                      header = T)
turk <- read.csv("Data/prerun/mturk/Batch_1739345_batch_results.csv", 
                 header = T)
code <- read.csv("Data/prerun/qualtrix/prerun_codebook.csv", 
                 header = T, sep = "\t")

### Prep qualtrix data
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

# Output data file
write.table(qualt, file = "Data/prerun/rat_prerun.csv", sep = ",")
