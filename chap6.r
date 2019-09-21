library(tidyverse)
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

## standardize predictor
d <-
    d %>%
    mutate(MedianAgeMarriage.s = (MedianAgeMarriage - mean(MedianAgeMarriage))/
               sd(MedianAgeMarriage))

## Fit model
m5.1 <-
    map(
        alist(
            Divorce ~ dnorm(mu , sigma),
            mu  <- a  + bA * MedianAgeMarriage.s,
            a ~ dnorm(10, 10),
            bA ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)),
        data = d)


## Compute percentile interval of mean
MAM.seq <- seq(from = -3, to= 3.5, length.out=30)
mu <- link(m5.1, data=data.frame(MedianAgeMarriage.s=MAM.seq))
mu.PI <- apply(mu, 2, PI)

## Plot it all
plot( Divorce ~ MedianAgeMarriage.s, data=d, col=rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

d <-
    d %>%
    mutate(Marriage.s = (Marriage - mean(Marriage))/sd(Marriage))

m5.2 <-
    map(
        alist(
            Divorce ~ dnorm(mu, sigma),
            mu <- a + bR * Marriage.s,
            a ~ dnorm(10, 10),
            bR ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)),
        data = d)
MAM.seq <- seq(from=-3, to = 3.5, length.out=30)
mu <- link(m5.2, data=data.frame(Marriage.s=MAM.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ Marriage.s, data=d, col=rangi2)
abline(m5.2)
shade(mu.PI, MAM.seq)

m5.3 <-
    map(
        alist(
            Divorce ~ dnorm(mu, sigma),
            mu <-  a  + bR*Marriage.s + bA*MedianAgeMarriage.s,
            a ~ dnorm(10, 10),
            bR ~ dnorm( 0, 1),
            bA ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)),
        data = d)
precis(m5.3)
plot(precis(m5.3))


## Find the marriage rate predictor using age at marriage predictor to
## build predictor residual plots
m5.4 <-
    map(
        alist(
            Marriage.s ~ dnorm(mu, sigma),
            mu <- a  + b * MedianAgeMarriage.s,
            a ~ dnorm(0, 10),
            b ~ dnorm( 0, 1),
            sigma ~ dunif(0, 10)),
        data = d)

## compute expected value at MAP, for each State
mu <- coef(m5.4)['a'] + coef(m5.4)['b'] * d$MedianAgeMarriage.s
## compute the residual for each state
m.resid <- d$Marriage.s - mu


plot( Marriage.s ~ MedianAgeMarriage.s, d, col=rangi2)
abline(m5.4)
for (i in 1:length(m.resid)){
    x <- d$MedianAgeMarriage.s[i] ## x location of line segments
    y <- d$Marriage.s[i] ## observed endpoint of line segments
    ## Draw it
    lines(c(x,x), c(mu[i],y), lwd = 0.5, col = col.alpha("black", 0.7))
}
