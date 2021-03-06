library("tidyverse")
library("rethinking")

sppnames <- c("afarensis", "africanus", "habilis", "boisei",
              "rudolfensis", "ergaster", "sapiens")
brainvolcc <- c( 438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.3, 34.5, 41.5, 55.5, 61.0, 53.5)
d <- data.frame(species = sppnames, brain=brainvolcc, mass=masskg)

m6.1 <- lm( brain ~ mass, data = d)

1 - var(resid(m6.1))/var(d$brain)
m6.2 <- lm( brain ~ mass + I(mass^2), data=d)
m6.3 <- lm( brain ~ mass + I(mass^2) + I(mass^3), data=d)
m6.4 <- lm( brain ~ mass + I(mass^2) + I(mass^3) +
         I(mass^4), data=d) 
m6.5 <- lm( brain ~ mass + I(mass^2) + I(mass^3) +
         I(mass^4) + I(mass^5), data=d)
m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) +
         I(mass^4) + I(mass^5) + I(mass^6), data=d)


d$mass.s <- (d$mass - mean(d$mass)) / sd(d$mass)
m.6.8 <- map(
    alist(
        brain ~ dnorm(mu, sigma),
        mu <- a + b * mass.s),
    data=d,
    start=list(a=mean(d$brain),b=0, sigma=sd(d$brain)),
    method = "Nelder-Mead")

theta <- coef(m.6.8)
dev <- (-2) * sum(dnorm(d$brain, mean=theta[1] + theta[2] * d$mass.s,
                        sd = theta[3],
                        log=TRUE))



data(milk)
d <- milk[complete.cases(milk),]
d$neocortex <- d$neocortex.perc / 100
dim(d)

a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))
m6.11 <-
    map(
        alist(
            kcal.per.g ~ dnorm(a, exp(log.sigma))),
        data = d,
        start = list(a=a.start, log.sigma=sigma.start))

m6.12 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, exp(log.sigma)),
            mu <- a + bn * neocortex),
        data = d,
        start = list(a=a.start, bn=0, log.sigma=sigma.start))

m6.13 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, exp(log.sigma)),
            mu <- a + bm * log(mass)),
        data = d,
        start = list(a=a.start, bm = 0, log.sigma=sigma.start))

m6.14 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, exp(log.sigma)),
            mu <- a + bm * log(mass) + bn * neocortex),
        data = d,
        start = list(a=a.start, bm = 0, bn = 0, log.sigma=sigma.start))
            
            
                  
