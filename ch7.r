library(tidyverse)
library(rethinking)
data(rugged)
d <- rugged

## make log version of the outcome
d$log_gdp <- log(d$rgdppc_2000)

## exctract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000), ]

## split countries into Africa and not-Africa
d.A1 <- dd[dd$cont_africa==1, ]
d.A0 <- dd[dd$cont_africa == 0, ]

## African nations model
m7.1 <-
    map(
        alist(
            log_gdp ~ dnorm(mu, sigma),
            mu <- a + bR * rugged,
            a ~ dnorm(8, 100),
            bR ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)),
        data=d.A1) 
