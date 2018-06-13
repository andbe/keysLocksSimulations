# Fastest way to open one lock

library(ggplot2)
library(magrittr)
library(dplyr)

stratA = function(keys = 50, locks = 7){
  keys = 1:keys
  locks = sample(keys, locks, replace = FALSE)
  lock = sample(locks, 1)
  trials = 1
  for(i in keys){
    if(i == lock) return(trials)
    trials = trials + 1
  }
}

stratB = function(keys = 50, locks = 7){
  keys = 1:keys
  locks = sample(keys, locks, replace = FALSE)
  trials = 1
  for(i in keys){
    for(j in locks){
      if(i == j) return(trials)
      trials = trials + 1
    }
  }
}

trialsA = c()
trialsB = c()
keys = 50
locks = 7
n.sims = 10000

for(t in 1:n.sims){
  trialsA = c(trialsA, stratA(keys, locks))
  trialsB = c(trialsB, stratB(keys, locks))
}

df = data.frame(
  "trials" = c(trialsA, trialsB),
  "strategy" = factor(c(rep("A",n.sims),rep("B",n.sims)))
  )


ggplot(df, aes(x = strategy, y = trials, col = strategy)) + geom_boxplot()

df %>% filter(strategy == "A") %>% summary
df %>% filter(strategy == "B") %>% summary
