# feed only correct data with outliers excluded
# quite rigid to our data


#### NOTES ####
#cut outliers <100000 (must be a glitch)
## be carefull, the distributions aren't ex-gaussian** 
# check other tasks, make a lm controlling for lv? or somehow get rid of lv
# have reasonable cutoff's 
# take the 2 most frequent lv's of a sub
# get tau for each lv
# make a weighted average, might as well also output the amount of trials (as a confidence measure)
# if more than 100 trials, take it, also output # of levels
# if the last (2nd most freq has the same # trials as 3rd most freq take both)

ct <- table(x$Account, x$Problem.Level)
ct$Var2 <- paste0("l", ct$Var2) # add a prefix to all rows in R stackoverflowww

spread(as_tibble(ct), Var2, n)

weighted.mean() # good example section




### staging area

onetask_tau <- function(df){
  
  
  return()
}