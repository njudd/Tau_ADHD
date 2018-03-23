#merging SPSS
library(foreign)
library(tidyverse)
library(data.table)
setwd("~/Documents/doug_sav/")
fullspss <- read.spss("DICOM_T3.sav", to.data.frame=TRUE)
fullspss <- as_tibble(fullspss)
# damn dat ugly
fullspss$uuid <- gsub(" ", "", fullspss$Account_Training, fixed = TRUE)

x <- read_csv("Tau_wmgrid_alltrials.csv")
names(x)[1] <- "uuid"
names(x)[4] <- "tau"
names(x)[8] <- "mean"
x <- dcast(setDT(x), uuid ~ Problem.Level, value.var = c("tau", "mu", "ratio", "mean", "n_trials"), fill = NA) 
x <- as_tibble(x)

#solving the fucking comma issue, seriously a pain in the ass because of REGEX
keep2 <- x[2:16]
keep2 <- as.data.frame(lapply(keep2, function(y) gsub("\\.", ",", y)))
x[2:16] <- keep2

# joining the existing SPSS file to the new vars (i.e. x)
fullspss <- fullspss %>% 
  left_join(x, by = c("uuid"))

#spss is a crap program that uses blank spaces as NA's
fullspss<- sapply(fullspss, as.character)
fullspss[is.na(fullspss)] <- "        "
fullspss <- as_tibble(fullspss)

#this library should be called heven if it actually works
library(haven)
write_sav(fullspss, "MASTER_UBER_willwork.sav")

#write_csv(fullspss, "fullspssfile.csv")
# fuck this shit didn't work
write_csv(x, "I_am_a_failure.csv")


