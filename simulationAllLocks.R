# Fastest way to open all locks

library(ggplot2)
library(magrittr)
library(dplyr)

# try all keys on one lock at a time
stratA = function(keys = 50, locks = 7){
  keys = 1:keys
  locks = sample(keys, locks, replace = FALSE)
  trials = 0
  for(i in locks){
    # how many keys does it take to open the lock?
    trials = trials + sum(keys <= i)
    # remove the key that opened the lock
    keys = keys[-i]
  }
  return(trials)
}

# try each key on all locks
stratB = function(keys = 50, locks = 7){
  keys = 1:keys
  locks = sample(keys, locks, replace = FALSE)
  trials = 0
  for(i in keys){
    # if no more locks then no need for more trials
    if(length(locks) == 0) return(trials)
    # does the key open a lock?
    if(i %in% locks) {
      # if a key opens a lock, do not try on next ones
      trials = trials + which(locks == i)
      #  remove opened lock from tested ones
      locks = locks[-which(locks == i)]
    } else {
      # otherwise the key is tested on all unopened locks
      trials = trials + length(locks)
    }
  }
  return(trials)
}

# simulation

keys = 50
locks = 7
n.sims = 10000
trialsA = rep(NA,n.sims)
trialsB = rep(NA,n.sims)

for(t in 1:n.sims){
  trialsA[t] = stratA(keys, locks)
  trialsB[t] = stratB(keys, locks)
}

df = data.frame(
  "trials" = c(trialsA, trialsB),
  "strategy" = factor(c(rep("A",n.sims),rep("B",n.sims)))
)


ggplot(df, aes(x = strategy, y = trials, col = strategy)) + geom_boxplot()

df %>% filter(strategy == "A") %>% summary
df %>% filter(strategy == "B") %>% summary
