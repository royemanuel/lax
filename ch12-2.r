## This will only work with ch12-1.r loaded

## source("ch12-1.r")
library(rethinking)
data(reedfrogs)
d <- reedfrogs
str(d)

## simulating ponds that we get multiple
## measurements from instead of tanks
a <- 1.4
sigma <- 1.5
nponds <- 60
ni  <- as.integer(rep(c(5, 10, 25, 35), each=15))

a_pond <- rnorm(nponds, mean=a, sd=sigma)

dsim <- data.frame(pond = 1:nponds, ni = ni, true_a = a_pond)
dsim$si <- rbinom(nponds, prob=logistic(dsim$true_a), size=dsim$ni)

dsim$p_nopool <- dsim$si / dsim$ni

m12.3 <- map2stan(
    alist(
        si ~ dbinom(ni, p),
        logit(p)  <- a_pond[pond],
        a_pond[pond] ~ dnorm( a, sigma),
        a ~ dnorm(0, 1),
        sigma ~ dcauchy(0,1)),
    data = dsim, iter = 1e4, warmup=1000)

precis(m12.3, depth =2 )

estimated.a_pond <- as.numeric(coef(m12.3)[1:60])
dsim$p_partpool <- logistic(estimated.a_pond)

dsim$p_true <- logistic(dsim$true_a)

nopool_error <- abs(dsim$p_nopool - dsim$p_true)
partpool_error <- abs(dsim$p_partpool - dsim$p_true)

plot(1:60, nopool_error, xlab = "pond", ylab="absolute error", col = rangi2, pch = 16)
points(1:60, partpool_error)

a <- 1.4
sigma <- 1.5
nponds <- 60
ni <- as.integer(rep(c(5, 10, 25, 35), each = 15))
a_pond <- rnorm(nponds, mean = a, sd = sigma)
dsim <- data.frame(pond = 1:nponds, ni = ni, true_a = a_pond)
dsim$si <- rbinom(nponds, prob = logistic(dsim$true_a), size = dsim$ni)
dsim$p_nopool <- dsim$si / dsim$ni
newdat <- list(si = dsim$si, ni = dsim$ni, pond = 1:nponds)
m12.3new <- map2stan(m12.3, data = newdat, iter = 1e4, warmup = 1000)
