library(tidyverse)
library(rethinking)
## 5E2 Write down a multiple regression
## Da ~ dnorm(mu, sigma)
## mu <- a + bP * plant.diversity + bL * latitude

## 5E3
## tPHD ~ dnorm(mu, sigma)
## mu <- a + bL * size.lab + bF * funding
## both bL and bF > 0

N <- 100
x1 <- rnorm(100,1,3)
y <- rnorm(100, 1, 1) * x1
x2 <- x1
d <- data.frame(y, x1, x2)
mh1<-
    map(
        alist(
            y ~ dnorm(mu, sigma),
            mu <- a + b1 * x1,
            a ~ dnorm(1,10),
            b1 ~ dnorm(1,10),
            sigma ~ dunif(0,50)),
        data = d)
mh2<-
    map(
        alist(
            y ~ dnorm(mu, sigma),
            mu <- a + b2 * x2,
            a ~ dnorm(1,10),
            b2 ~ dnorm(1,10),
            sigma ~ dunif(0,50)),
        data = d)
mh3 <-
    map(
        alist(
            y ~ dnorm(mu, sigma),
            mu <- a + b1 * x1 + b2 * x2,
            a ~ dnorm(1,10),
            b1 ~ dnorm(1,10),
            b2 ~ dnorm(1,10),
            sigma ~ dunif(0,50)),
        data = d)
