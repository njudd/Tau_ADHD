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

timefit(ct)

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
cor.test(lets_see$tau_weighted, lets_see$SDQ_Inatt_T1, use = "pairwise.complete.obs")


cor.test(lets_see$SDQ_Inatt_T2, lets_see$SDQ_Inatt_T1, use = "pairwise.complete.obs")
cor.test(lets_see$SDQ_Inatt_T3, lets_see$SDQ_Inatt_T1, use = "pairwise.complete.obs")
cor.test(lets_see$SDQ_Inatt_T2, lets_see$SDQ_Inatt_T3, use = "pairwise.complete.obs")

##### lets add a test of fit ##### 


# Cumulative relative frequency 

# points are the 2nd, 4th, 6th, 92nd, 94th and 96th percentiles, AND every fifth percentile from 10th -> 90th

?ecdf

ct <- rexgauss(100)

ecdf(rt)[30]












### try tau on the most popular lv for the 42 subs
table(x$Problem.Level) # 4 is the most frequent
#lets only use the 42 we need
limited_subs <- lets_see[complete.cases(lets_see),]
limited_subs <- limited_subs$uuid

wmgrid_limited <- x %>% 
  filter(Account %in% limited_subs & Problem.Level==4) %>% 
  group_by(Account) %>% 
  mutate(n = row_number(), min_sub = n()) %>% # 56 is the lowest # of trials
  filter(n <= 56)   %>% 
  summarise(tau_m = mexgauss(Response.Time)[3] ,tau = slot(timefit(Response.Time), "par")[3], mu = slot(timefit(Response.Time), "par")[1], logLik = slot(timefit(Response.Time), "logLik") ,ratio = unique(sum(Correct, na.rm = TRUE)/min_sub), m = mean(Response.Time, na.rm = TRUE)) %>% 
  left_join(doug, by = c("Account" = "uuid"))

cor.test(wmgrid_limited$tau_m, wmgrid_limited$Inatt_DSM)
cor.test(wmgrid_limited$tau, wmgrid_limited$mu)
summary(lm(tau ~ Inatt_DSM + logLik + mu, wmgrid_limited))


# do lv 3 & 5 and see if there's and effect of lv
wmgrid_limited <- x %>% 
  filter(Account %in% limited_subs) %>% 
  filter(Problem.Level==3 | Problem.Level==4 | Problem.Level==5) %>% 
  group_by(Account, Problem.Level) %>% 
  summarise(tau = mexgauss(Response.Time)[3]) %>% 
  left_join(doug, by = c("Account" = "uuid"))
summary(lm(tau ~ Problem.Level, wmgrid_limited))


wmgrid_limited <- x %>% 
  filter(Account %in% limited_subs) %>% 
  filter(Problem.Level==3 | Problem.Level==4 | Problem.Level==5) %>% 
  group_by(Account, Problem.Level) %>% 
  mutate(n = row_number(), min_sub = n()) %>% # 56 is the lowest # of trials
  summarise(tau_m = mexgauss(Response.Time)[3] ,tau = slot(timefit(Response.Time), "par")[3], mu = slot(timefit(Response.Time), "par")[1], logLik = slot(timefit(Response.Time), "logLik") ,ratio = unique(sum(Correct, na.rm = TRUE)/min_sub), m = mean(Response.Time, na.rm = TRUE)) %>% 
  mutate(tau_l = log(tau)) %>% 
  left_join(doug, by = c("Account" = "uuid")) 

#cor.test(wmgrid_limited$tau, wmgrid_limited$Problem.Level)

hist(wmgrid_limited$tau[wmgrid_limited$Problem.Level==3], breaks = 40)
hist(wmgrid_limited$tau[wmgrid_limited$Problem.Level==4], breaks = 40)
hist(wmgrid_limited$tau[wmgrid_limited$Problem.Level==5], breaks = 40)


hist(log(wmgrid_limited$tau[wmgrid_limited$Problem.Level==3]), breaks = 40)
hist(log(wmgrid_limited$tau[wmgrid_limited$Problem.Level==4]), breaks = 40)
hist(log(wmgrid_limited$tau[wmgrid_limited$Problem.Level==5]), breaks = 40)



tauresidi <- lm(tau ~ Problem.Level, wmgrid_limited)

##### RESIDUALS SHOULD BE NORMALLY DISTRIBUTED
tauresidi <- tau_residi$residuals
hist(tauresidi, breaks = 30)
# min(tauresidi)
tauresidi <- tauresidi + 6130
hist(tauresidi, breaks = 30)


#tauresidi <- 1/tauresidi
#hist(tauresidi, breaks = 30)


tauresidi <- log(tauresidi)
hist(tauresidi, breaks = 30)
tauresidi <- tauresidi[tauresidi>6]
hist(tauresidi, breaks = 30)

wmgrid_limited$tau_res <- tauresidi

summary(lm(tau_res ~ Inatt_DSM, wmgrid_limited))


summary(lm(tau ~ Problem.Level, wmgrid_limited))




