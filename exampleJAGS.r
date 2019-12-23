library("rjags")
library("tidyverse")


## Build the fake data

## Number of trials
numTrial <- floor(rbeta(200, 2,2) * 20)
Ntotal  <- 200

y <- rbinom(n = Ntotal,size = numTrial, p = 0.25)

listdata <-
    list(
        y = y,
        numTrial = numTrial,
        Ntotal = Ntotal)

mod <- "model {
    for (i in 1:Ntotal){
        y[i] ~ dbinom(p, numTrial[i])
    }
    p ~ dbeta(2, 5)
}"

writeLines
