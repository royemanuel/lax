library("tidyverse")
library("rethinking")

data(milk)
d <- milk

m5.10 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, sigma),
            mu <- a + bf * perc.fat,
            a ~ dnorm(10, 3),
            bf ~ dnorm(0, 1),
            sigma ~ dunif(0, 50)),
        data = d)
m5.11 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, sigma),
            mu <- a + bl * perc.lactose,
            a ~ dnorm(10, 3),
            bl ~ dnorm(0, 1),
            sigma ~ dunif(0, 50)),
        data = d)
precis(m5.10, digits=3)
precis(m5.11, digits=3)

m5.12 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, sigma),
            mu <- a + bf * perc.fat + bl * perc.lactose,
            a ~ dnorm(10, 3),
            bf ~ dnorm(0, 1),
            bl ~ dnorm(0,1),
            sigma ~ dunif(0, 50)),
        data = d)
precis(m5.12, digits=3)

pairs( ~ kcal.per.g + perc.fat + perc.lactose, data = d, col=rangi2)
