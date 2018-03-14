#tau for WM_grid

#cut outliers <100000 (must be a glitch)
## be carefull, the distributions aren't ex-gaussian** 
# check other tasks, make a lm controlling for lv? or somehow get rid of lv


# have reasonable cutoff's 
# take the 2 most frequent lv's of a sub
# get tau for each lv
# make a weighted average, might as well also output the amount of trials (as a confidence measure)
# if more than 100 trials, take it, also output # of levels

library('tidyverse')
library('retimes')

setwd("~/Dropbox/KI/Diff_project/Grid_diff")
x <- read_csv("WMgrid_training_full_pure.csv")
keep <-x



#x <- x %>% 
#  filter(Correct==1) #%>% 


setwd("~/Projects/R_projects/Tau_ADHD/")
source("onetask_tau_func.R")
tau <- onetask_tau(x)









# some descriptive histograms
# shitty tau
ct <- x[x$Account=="batasage" & x$Problem.Level==4,]$Response.Time # lv = 5
ct <- ct[ct>5000]
hist(ct, breaks = 45)
mexgauss(ct)

ct <- x[x$Account=="behaputsu" & x$Problem.Level==4,]$Response.Time # lv = 5
ct <- ct[ct>5000]
hist(ct, breaks = 45)
mexgauss(ct)

ctl3 <- x[x$Account=="kigetzutsu" & x$Problem.Level==3,]$Response.Time # lv = 3-6
ctl4 <- x[x$Account=="kigetzutsu" & x$Problem.Level==4,]$Response.Time # lv = 3-6
ctl5 <- x[x$Account=="kigetzutsu" & x$Problem.Level==5,]$Response.Time # lv = 3-6
ctl6 <- x[x$Account=="kigetzutsu" & x$Problem.Level==6,]$Response.Time # lv = 3-6
hist(ctl3); hist(ctl4); hist(ctl5); hist(ctl6)

###### lets see how to change it

q1 <- rnorm(10000, 100, 1)
q2 <- rnorm(10000, 100, 5)
q3 <- rnorm(10000, 100, 10)

t1 <- rnorm(10000, 0, 3)
t2 <- rnorm(10000, 0, 5)
t3 <- rnorm(10000, 0, 15)

post1 <- t1[t1<0]
post2 <- t2[t2<0]
post3 <- t3[t3<0]
neg1 <- t1[t1>0]
neg2 <- t2[t2>0]
neg3 <- t3[t3>0]

q2_exponent_sameSD <- c(q2, post2+100) #shit simulation
hist(q2_exponent_sameSD)

mexgauss(q3)

dexgauss() #density function


g <- rexgauss(10000, 10)

####### loading dougs sav
library(foreign)
setwd("~/Documents/doug_sav/")
doug <- read.spss("DICOM_T3.sav")
doug <- as_tibble(doug)
doug$uuid <- gsub(" ", "", doug$Account_Training, fixed = TRUE)


doug <- select(doug, one_of(c("uuid", "Inatt_DSM", "SDQ_Inatt_T3", "SDQ_Inatt_T2")))

sum(!is.na(doug$Inatt_DSM))
####### 

#### temp work area for preliminary cor

lets_see <- tau %>% 
  left_join(doug, by = "uuid")

sum(!is.na(lets_see$Inatt_DSM))

lets_see$tau_weighted <- as.numeric(lets_see$tau_weighted)
cor.test(lets_see$tau_weighted, lets_see$Inatt_DSM, use = "pairwise.complete.obs")




##### lets add a test of fit ##### 


# Cumulative relative frequency 

# points are the 2nd, 4th, 6th, 92nd, 94th and 96th percentiles, AND every fifth percentile from 10th -> 90th

?ecdf

ct <- rexgauss(100)

ecdf(rt)[30]





















