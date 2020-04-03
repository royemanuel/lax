## prep data

library(rethinking)
data(Kline)
d <- Kline
d$logpop <- log(d$population)
d$society <- 1:10

## fit model
m12.6 <-
    map2stan(
        alist(
            total_tools ~ dpois(mu),
            log(mu) <- a + a_society[society] + bP * logpop,
            a ~ dnorm(0, 10),
            bP ~ dnorm(0, 1),
            a_society[society] ~ dnorm(0, sigma_society),
            sigma_society ~ dcauchy(0,1)),
        data = d,
        iter = 4000,
        chains = 3)

## GOAL - Rewrite this model in JAGS.

post <- extract.samples(m12.6)
d.pred <- list(
    logpop = seq(from = 6, to = 14, length.out = 30),
    society = rep(1, 30)
)
a_society_sims <- rnorm(20000, 0, post$sigma_society)
a_society_sims <- matrix(a_society_sims, 2000, 10)
link.m12.6 <- link(m12.6,
                   n = 2000,
                   data = d.pred,
                   replace = list(a_society = a_society_sims))

## plot raw data
plot(d$logpop, d$total_tools, col = rangi2, pch = 16,
     xlab = "log population", ylab = "total tools")

## plot posterior median
mu.median <- apply (link.m12.6, 2, median)
lines(d.pred$logpop, mu.median)

## plot 97%, 89%, and 67% intervals (all prime numbers)
mu.PI <- apply(link.m12.6, 2, PI, prob = 0.97)
shade(mu.PI, d.pred$logpop)
mu.PI <- apply(link.m12.6, 2, PI, prob = 0.89)
shade(mu.PI, d.pred$logpop)
mu.PI <- apply(link.m12.6, 2, PI, prob = 0.67)
shade(mu.PI, d.pred$logpop)
