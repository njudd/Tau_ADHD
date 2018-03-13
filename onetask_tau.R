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

finding_lvs <- as_tibble(table(x$Account, x$Problem.Level))
#ct$Var2 <- paste0("l_", ct$Var2) # add a prefix to all rows in R stackoverflowww
finding_lvs <- finding_lvs %>% 
  rename(uuid = Var1) %>% 
  group_by(uuid) %>% 
  arrange(desc(n)) %>% 
  arrange(desc(uuid,n))

#need a lil loop which decides how many lvs to take from each account and rep's that in another col

finding_lvs$uuid <- as_factor(finding_lvs$uuid)

take_me_rep <- c()
num_of_trials <- c()

for(i in levels(finding_lvs$uuid)){
  g <-  finding_lvs[finding_lvs$uuid==i,]
  counter <- 2
  repn <- c()
  rept <- c()
    for(z in 2:max(dim(g)[1])){ #weak loop as there's no fail safe against subs with less than 3 rows (aka lvs), not true because with the spread everyone is forced to have multiple rows
      if(g[z,3] == g[z+1,3] | g[z+1,3]>=100){
        counter <- counter + 1
        }
      else if(z==2){
        repn <- rep(counter, dim(g)[1])
        rept <- rep(sum(g[1:counter,3]), dim(g)[1])  # make sure to rep it
        break
        }
      else{
        repn <- rep(counter, dim(g)[1])
        rept <- rep(sum(g[1:counter,3]), dim(g)[1])
        break
      }
    #take_me_rep <- c(take_me_rep, repn)
    #num_of_trials <- c(num_of_trials, rept) # need to have this out of a few loops
    }
  take_me_rep <- c(take_me_rep, repn)
  num_of_trials <- c(num_of_trials, rept)
}




ct <- spread(as_tibble(ct), Var2, n) # if you dont spread them you can use dyplr
# order



weighted.mean() # good example section




### staging area

onetask_tau <- function(df){
  
  
  return()
}