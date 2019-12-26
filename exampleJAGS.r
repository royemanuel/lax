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
        y[i] ~ dbinom(p[i], numTrial[i])
        p[i] ~ dbeta(2, 5)
    }
}"

write(mod, "mod.txt")
model <- jags.model("mod.txt",
                    data=listdata,
                    n.chains = 2)
update(model, n.iter=1000)

samples <- coda.samples(model,
                        variable.names = c("p"),
                        n.iter=1000)
