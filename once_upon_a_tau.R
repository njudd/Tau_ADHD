#tau for WM_grid

#cut outliers <100000 (must be a glitch)
## be carefull, the distributions aren't ex-gaussian** 
# check other tasks, make a lm controlling for lv? or somehow get rid of lv


# have reasonable cutoff's 
# take the 2 most frequent lv's of a sub
# get tau for each lv
# make a weighted average, might as well also output the amount of trials (as a confidence measure)
# if more than 100 trials, take it, also output # of levels


table(x$Phase.Type) # whats the difference, If i remember correctly this df is clean


setwd("~/Dropbox/KI/Diff_project/Grid_diff")
x <- read.csv("WMgrid_training_full_pure.csv")
keep <-x

library('tidyverse')
library('retimes')


####### loading dougs sav
library(foreign)
setwd("~/Documents/doug_sav/")
doug <- read.spss("DICOM_T3_180312.sav")
doug <- as_tibble(doug)
doug$uuid <- gsub(" ", "", doug$Account_training, fixed = TRUE)
doug <- select(doug, one_of(c("uuid", "Inatt_DSM", "SDQ_Inatt_T3", "SDQ_Inatt_T2")))

####### 

#### temp work area for preliminary cor

lets_see <- doug_leavn %>% 
  left_join(doug, by = "uuid")

sum(is.na(lets_see$Inatt_DSM))


x <- x %>% 
  filter(Correct==1) #%>% 
  


group_by(Account,Problem.Level) %>% 
  summarise(mean = mean(Response.Time), median = median(Response.Time), sd = sd(Response.Time), count = n())

write_csv(x, "rt_values.csv")

# shitty tau
ct <- x[x$Account=="batasage" & x$Problem.Level==4,]$Response.Time # lv = 5
ct <- ct[ct>5000]
hist(ct, breaks = 45)
mexgauss(ct)
skew(ct)
# good tau
# ct <- x[x$Account=="behaputsu" & x$Problem.Level==4,]$Response.Time # lv = 5
# hist(ct, breaks = 45)
# mexgauss(ct)

ct <- x[x$Account=="behaputsu" & x$Problem.Level==4,]$Response.Time # lv = 5
ct <- ct[ct>5000]
hist(ct, breaks = 45)
mexgauss(ct)
skew(ct)

ctl3 <- x[x$Account=="kigetzutsu" & x$Problem.Level==3,]$Response.Time # lv = 3-6
ctl4 <- x[x$Account=="kigetzutsu" & x$Problem.Level==4,]$Response.Time # lv = 3-6
ctl5 <- x[x$Account=="kigetzutsu" & x$Problem.Level==5,]$Response.Time # lv = 3-6
ctl6 <- x[x$Account=="kigetzutsu" & x$Problem.Level==6,]$Response.Time # lv = 3-6
hist(ctl6)
mexgauss(ctl4)


set.seed(123)
ct <- rnorm(100000, 10)
hist(ct)
mexgauss(ct)
skew(ct)
ct2 <- rnorm(100000, 10)
hist(ct2)
mexgauss(ct2)
skew(ct2)
ct3 <- rnorm(100000, 10)
hist(ct3)
mexgauss(ct3)
skew(ct3)
ct4 <- rnorm(100000, 10)
hist(ct4)
mexgauss(ct4)
skew(ct4)

##### making a right tail

expont <- rnorm(100000, 0, 3)
expont <- expont[expont>0.00001]+10

#norm distro with tail
ct5 <- c(ct,expont)
hist(ct5)
mexgauss(ct5)
skew(ct5)
# lessening the normal distribution on the left
ct6 <- ct5[ct5>8]
hist(ct6)
mexgauss(ct6)



