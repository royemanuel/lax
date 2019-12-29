library("rjags")
library("tidyverse")
library("boot")


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
        p[i] ~ dbeta(omega[t[i]] * (kappa[t[i]] - 2) + 1, (1 - omega[t[i]]) * (kappa[t[i]]-2) + 1)
    }
    for (j in 1:Nlvl){
        omega[j] <- ilogit(a0 + a[j])
        a[j] ~ dnorm(0, 1/aSigma^2)
        kappa[j] <- kappaMinusTwo[j] + 2
        kappaMinusTwo[j] ~ dgamma( 0.01 , 0.01 )  # mean=1 , sd=10 (generic vague)
    }
    a0 ~ dnorm(0, 1/4)
    aSigma ~ dgamma(1.64, .32)
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
                        variable.names = c("b0", "b", "omega", "kappa"),
                        n.iter=5000)



calcP <- list()
for(i in 1:length(samples)){
    calcP[[i]] <- as_tibble(samples[[i]]) %>%
        mutate(chain = i)
}


calcP <- bind_rows(calcP)

t1 <- c(rep(0,200), rep(1,200))
t2 <- c(rep(1,200), rep(0,200))

allT <- bind_cols(t1 = t1, t2 = t2)

allData <- bind_cols(y = y, numTrial = numTrial, allT)

sumData <- summary(samples)

## sumData <-
##     sumData %>%
##     mutate(P1

## allData  <-
##     allData %>%
##     mutate(p50 = 


allData <-
    mutate(Palpha = inv.logit(






mod2 <- "model {
    for (i in 1:Ntotal){
        y[i] ~ dbinom(p[t[i]], numTrial[i])
    }
    for (j in 1:Nlvl){
        p[j] <- ilogit(a0 + a[j])
        a[j] ~ dnorm(0, 1/aSigma^2)
        kappa[j] <- kappaMinusTwo[j] + 2
        kappaMinusTwo[j] ~ dgamma( 0.01 , 0.01 )  # mean=1 , sd=10 (generic vague)
    }
    a0 ~ dnorm(0, 1/4)
    aSigma ~ dgamma(1.64, .32)
    # Convert a0,a[] to sum-to-zero b0,b[] :
    for ( j in 1:Nlvl ) { m[j] <- a0 + a[j] } # cell means 
    b0 <- mean( m[1:Nlvl] )
    for ( j in 1:Nlvl ) { b[j] <- m[j] - b0 }
}"



writeLines(mod2, "mod2.txt")
model2 <- jags.model("mod2.txt",
                    data=listdata,
                    n.chains = 4)
update(model2, n.iter=1000)

samples2 <- coda.samples(model2,
                        variable.names = c("b0", "b", "p", "kappa"),
                        n.iter=5000)





mod3 <- "model {
    for (i in 1:Ntotal){
        y[i] ~ dbinom(p[i], numTrial[i])
        p[i] <- ilogit(a0 + a1 * t[i])    }
    a1 ~ dnorm(0, 1/aSigma^2)
    a0 ~ dnorm(0, 1/4)
    aSigma ~ dgamma(1.64, .32)
    # Convert a0,a[] to sum-to-zero b0,b[] :
    ## for ( j in 1:Nlvl ) { m[j] <- a0 + a[j] } # cell means 
    ## b0 <- mean( m[1:Nlvl] )
    ## for ( j in 1:Nlvl ) { b[j] <- m[j] - b0 }
}"



writeLines(mod3, "mod3.txt")
model3 <- jags.model("mod3.txt",
                    data=listdata,
                    n.chains = 4)
update(model3, n.iter=1000)

samples3 <- coda.samples(model3,
                        variable.names = c("a0", "a1"),
                        n.iter=5000)






