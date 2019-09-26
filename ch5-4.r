library(tidyverse)
library(rethinking)

data(Howell1)
d <- Howell1
str(d)

m5.15 <-
    map(
        alist(
            height ~ dnorm(mu, sigma),
            mu  <- a + bM * male,
            a ~ dnorm(178, 100),
            bM ~ dnorm(0, 10),
            sigma ~ dunif(0,50)),
        data=d)
precis(m5.15)

post <- extract.samples(m5.15)
mu.male <- post$a + post$bM
PI(mu.male)

## Reparamaterize the model to include both male and female

m5.15b <-
    map(
        alist(
            height ~ dnorm(mu, sigma),
            mu <- af * (1-male) + am * male,
            af ~ dnorm(178, 100),
            am ~ dnorm(178, 100),
            sigma ~ dnorm(0,10)),
        data = d)
precis(m5.15b)

data(milk)
d <- milk
unique(d$clade)

(d$clade.NWM <- ifelse(d$clade=="New World Monkey", 1, 0))
(d$clade.OWM <- ifelse(d$clade=="Old World Monkey", 1, 0))
(d$clade.S <- ifelse(d$clade=="Strepsirrhine", 1, 0))

m5.16 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, sigma),
            mu <- a + bNWM * clade.NWM + bOWM * clade.OWM + bS * clade.S,
            a ~ dnorm(0.6, 10),
            bNWM ~ dnorm(0,1),
            bOWM ~ dnorm(0,1),
            bS ~ dnorm(0,1),
            sigma ~ dunif(0, 10)),
        data = d)

precis(m5.16)

## sample the posterior
post <- extract.samples(m5.16)

## compute averages for every category
mu.ape <- post$a
mu.NWM  <- post$a + post$bNWM
mu.OWM <- post$a + post$bOWM
mu.S <- post$a + post$bS

precis(data.frame(mu.ape, mu.NWM, mu.OWM, mu.S))

diff.NWM.OWM <- mu.NWM - mu.OWM
quantile(diff.NWM.OWM, probs=c(0.025,0.5,0.975))

m5.17 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, sigma),
            mu <- a + bNWM * clade.NWM + bOWM * clade.OWM + bS * clade.S + bF * perc.fat,
            a ~ dnorm(0.6, 10),
            bF ~ dnorm(0,1),
            bNWM ~ dnorm(0,1),
            bOWM ~ dnorm(0,1),
            bS ~ dnorm(0,1),
            sigma ~ dunif(0, 10)),
        data = d)

(d$clade_id <- coerce_index(d$clade))

m5.16alt <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, sigma),
            mu <- a[clade_id],
            a[clade_id] ~ dnorm(0.6, 10),
            sigma ~ dunif(0, 10)),
        data = d)
