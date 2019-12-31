library("rjags")
library("tidyverse")
library("boot")
library("loo")
library("parallel")

options(mc.cores = detectCores())


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
        loglik[i] <- logdensity.bin(y[i], p[i], numTrial[i])
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
                        variable.names = c("b0", "b", "omega", "kappa", "loglik"),
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


## allData <-
##     mutate(Palpha = inv.logit(

## Working a WAIC calculation using look

s1_all <- rbind(samples[[1]],
                samples[[2]],
                samples[[3]],
                samples[[4]])

s1_ll <- s1_all[,paste0("loglik[", 1:400, "]")]
w_s1 <- waic(s1_ll)
loo(s1_ll)




mod2 <- "model {
    for (i in 1:Ntotal){
        y[i] ~ dbinom(p[t[i]], numTrial[i])
        loglik2[i] <- logdensity.bin(y[i], p[t[i]], numTrial[i])
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
                        variable.names = c("b0", "b", "p", "kappa", "loglik2"),
                        n.iter=5000)

## Working a WAIC calculation using look

s2_all <- rbind(samples2[[1]],
                samples2[[2]],
                samples2[[3]],
                samples2[[4]])

s2_ll <- s2_all[,paste0("loglik2[", 1:400, "]")]
w_s2 <- waic(s2_ll)
loo(s2_ll)


t3 <- t - 1

datalist3 <- list(
    y =y,
    numTrial = numTrial,
    Ntotal = Ntotal,
    t = t3)


mod3 <- "model {
    for (i in 1:Ntotal){
        y[i] ~ dbinom(p[i], numTrial[i])
        p[i] <- ilogit(a0 + a1 * t[i])    
        loglik3[i] <- logdensity.bin(y[i], p[i], numTrial[i])
    }
    a1 ~ dnorm(0, 1/100)
    a0 ~ dnorm(0, 1/100)
    # Convert a0,a[] to sum-to-zero b0,b[] :
    ## for ( j in 1:Nlvl ) { m[j] <- a0 + a[j] } # cell means 
    ## b0 <- mean( m[1:Nlvl] )
    ## for ( j in 1:Nlvl ) { b[j] <- m[j] - b0 }
}"



writeLines(mod3, "mod3.txt")
model3 <- jags.model("mod3.txt",
                    data=datalist3,
                    n.chains = 4)
update(model3, n.iter=1000)

samples3 <- coda.samples(model3,
                        variable.names = c("a0", "a1", "loglik3"),
                        n.iter=5000)

sum3 <- summary(samples3)
a0_3 <- sum3[[1]]["a0",1]
a1_3 <- sum3[[1]]["a1",1]
allData <-
    allData %>%
    mutate(medP3 = inv.logit(a0_3 + a1_3 * t2))

s3_all <- rbind(samples3[[1]],
                samples3[[2]],
                samples3[[3]],
                samples3[[4]])

s3_ll <- s3_all[,paste0("loglik3[", 1:400, "]")]
w_s3 <- waic(s3_ll)
loo(s3_ll)




mod4 <- "model {
    for (i in 1:Ntotal){
        y[i] ~ dbinom(p[i], numTrial[i])
        p[i] <- ilogit(a0)    
        loglik4[i] <- logdensity.bin(y[i], p[i], numTrial[i])
    }
    a0 ~ dnorm(0, 1/100)
}"



writeLines(mod4, "mod4.txt")
model4 <- jags.model("mod4.txt",
                    data=datalist3,
                    n.chains = 4)
update(model4, n.iter=1000)

samples4 <- coda.samples(model4,
                        variable.names = c("a0", "loglik4"),
                        n.iter=5000)


s4_all <- rbind(samples4[[1]],
                samples4[[2]],
                samples4[[3]],
                samples4[[4]])

s4_ll <- s4_all[,paste0("loglik4[", 1:400, "]")]
w_s4 <- waic(s4_ll)
loo(s4_ll)

loo_model_weights(list(s2_ll, s3_ll, s4_ll))
