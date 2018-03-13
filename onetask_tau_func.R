# feed only correct data (try with all data as well) with outliers excluded
# quite rigid to our data
#cut outliers <100000 (must be a glitch)
# have reasonable cutoff's 

#### what is does:
# takes the 2 most frequent lv's of an account (or >2 if they have multiple lvs with over 100 observations)
# get tau for each lv
# make a weighted average
# outputs a tibble with: uuid, weighted_tau, sum of trials and number of lvs used!


onetask_tau <- function(df){


finding_lvs <- as_tibble(table(df$Account, df$Problem.Level))
finding_lvs <- finding_lvs %>% 
  rename(uuid = Var1, lv = Var2) %>% 
  group_by(uuid) %>% 
  arrange(desc(n)) %>% 
  arrange(desc(uuid,n))


#need a loop which decides how many lvs to take from each account and rep's that in another col
finding_lvs$uuid <- as_factor(finding_lvs$uuid) # making accounts as a factor to use in loop
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
    }
  take_me_rep <- c(take_me_rep, repn)
  num_of_trials <- c(num_of_trials, rept)
}
finding_lvs$take_me_rep <- as.integer(take_me_rep)
finding_lvs$lv <- as.integer(finding_lvs$lv)
finding_lvs$num_of_trials <- num_of_trials


# first trim down the dataframe to the needed rows
# might not be that easy


### now for the real deal


uuid <- c()
tau_weighted <- c()
n_trials <- c()
n_lvs <- c()

for(i in levels(finding_lvs$uuid)){
  g <-  df[df$Account==i,]
  # holding vecs
  tau_vals <- c()
  l <- c()
  for(p in 1:unique(finding_lvs$take_me_rep[finding_lvs$uuid==i])){ 
    #looping for as many times as determined earlier (aka # of lvs with 100+ trials)
    tau_vals <- c(tau_vals, mexgauss(g[g$Problem.Level==as.integer(finding_lvs[finding_lvs$uuid==i,][p,2]),]$Response.Time)[3])
    #this nasty line of code gets tau by subseting the df g$Response.Time (which is already subset per account) and extracts the problem lv from the ordered finding_lvs df based on how far the loop is
    # [3] gets only tau from mexgauss function from the package library('retimes')
    l <- c(l, length(g[g$Problem.Level==as.integer(finding_lvs[finding_lvs$uuid==i,][p,2]),]$Response.Time)/unique(finding_lvs[finding_lvs$uuid==i,]$num_of_trials))
    # this line of code, is the make a weighted vector to average later (divides by total num of trials, determined earlier by finding_lvs df)
  }
  uuid <- c(uuid, i)
  tau_weighted <- c(tau_weighted, weighted.mean(tau_vals,l)) # making a vector (per account) of weighted.means
  n_trials <- c(n_trials, unique(finding_lvs[finding_lvs$uuid==i,]$num_of_trials)) # making a vector of number of trials used per account
  n_lvs <- c(n_lvs, length(tau_vals)) # making a vector of number of lvs used per account
}
out <- as_tibble(cbind(uuid, tau_weighted, n_trials, n_lvs))
return(out)
}