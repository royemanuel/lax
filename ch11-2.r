library(tidyverse)
library(rethinking)

## define parameters
prob_drink <- 0.2 # 20% of days
rate_work <- 1 # average 1 manuscript ber day

## sample one year of production
N <- 365

## simulate days monks drink
drink <- rbinom(N, 1, prob_drink)

## simulate manuscripts completed
y <- (1- drink) * rpois( N, rate_work)

simplehist(y, xlab="manuscripts completed", lwd=4)
zeros_drink <- sum(drink)
zeros_work <- sum(y==0 & drink==0)
zeros_total <- sum(y==0)
lines(c(0,0), c(zeros_work, zeros_total), lwd=4, col=rangi2)

m11.4 <-
    map(
        alist(
            y ~ dzipois(p, lambda),
            logit(p) <- ap,
            log(lambda) <- al,
            ap ~ dnorm(0,1),
            al ~ dnorm(0,10)),
        data=list(y=y))
precis(m11.4)

logistic(-1.7) ## porbability drink
exp(0.05) ## rate finish manuscripts, when not drinking
