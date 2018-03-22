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

fullspss <- fullspss %>% 
  left_join(x, by = c("uuid"))
write_csv(fullspss, "fullspssfile.csv")

