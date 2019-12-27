library("rjags")
library("tidyverse")


## Build the fake data

## Number of trials
numTrial <- floor(rbeta(400, 2,2) * 20)
Ntotal  <- 400

y <- c(rbinom(n = Ntotal/2,size = numTrial[1:200], p = 0.35),
       rbinom(n = Ntotal/2,size = numTrial[201:400], p = 0.15))

t <- c(rep(2, 200), rep(1, 200))

Nlvl <- length(unique(t))

listdata <-
    list(
        y = y,
        numTrial = numTrial,
        t = t,
        Ntotal = Ntotal,
        Nlvl = Nlvl)

mod <- "model {
    for (i in 1:Ntotal){
        y[i] ~ dbinom(p[i], numTrial[i])
        p[i] ~ dbeta(omega[t[i]] * (kappa - 2) + 1, (1 - omega[t[i]]) * (kappa-2) + 1)
    }
    for (j in 1:Nlvl){
        omega[j] <- ilogit(a0 + a[j])
        a[j] ~ dnorm(0, 1/aSigma^2)
    }
    a0 ~ dnorm(0, 1/4)
    aSigma ~ dgamma(1.64, .32)
    kappa <- kappaMinusTwo + 2
    kappaMinusTwo ~ dgamma( 0.01 , 0.01 )  # mean=1 , sd=10 (generic vague)
    # Convert a0,a[] to sum-to-zero b0,b[] :
    for ( j in 1:Nlvl ) { m[j] <- a0 + a[j] } # cell means 
    b0 <- mean( m[1:Nlvl] )
    for ( j in 1:Nlvl ) { b[j] <- m[j] - b0 }
}"

writeLines(mod, "mod.txt")
model <- jags.model("mod.txt",
                    data=listdata,
                    n.chains = 4)
update(model, n.iter=1000)

samples <- coda.samples(model,
                        variable.names = c("a0", "a", "b0", "b"),
                        n.iter=5000)
