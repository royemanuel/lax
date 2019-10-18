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
<<<<<<< HEAD
=======
<<<<<<< HEAD
cor(d$perc.fat, d$perc.lactose)

sim.coll <- function(r=0.9){
    rnorm(nrow(d), mean=r * d$perc.fat, sd = sqrt(1-r^2)*var(d$perc.fat))
    m <- lm(kcal.per.g ~ perc.fat + x, data = d)
    sqrt(diag(vcov(m)))[2]
}

rep.sim.coll <- function(r = 0.9, n = 100) {
    stddev <- replicate(n, sim.coll(r))
    mean(stddev)
}

r.seq <- seq(from=0, to= 0.99, by=0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r=z, n=100))
plot(stddev ~ r.seq, type="l", col=rangi2, lwd=2, xlab = "correlation")
=======
>>>>>>> 6c7342129c8c92411bdce4806135a8f997ac1773

## Post-treatment bias work

## number of plants
N <- 100

## simulate initial hieghts
h0 <- rnorm(N,10,2)

## assign treatments and simulte fungus and growth
treatment <- rep(0:1, each = N/2)
fungus  <- rbinom(N, size=1, prob=0.5 - treatment * 0.4)
h1 <- h0 + rnorm(N, 5 - 3 * fungus)

## data frame
d <- data.frame(h0=h0, h1=h1, treatment=treatment, fungus=fungus)

m5.13 <-
    map(
        alist(
            h1 ~ dnorm(mu, sigma),
            mu  <- a + bH * h0 + bT * treatment + bF * fungus,
            a ~ dnorm(0, 100),
            bT ~ dnorm(0, 10),
            bF ~ dnorm(0, 10),
            bH ~ dnorm(0, 10),
            sigma ~ dunif(0, 10)),
        data=d)

m5.14 <-
    map(
        alist(
            h1 ~ dnorm(mu, sigma),
            mu  <- a + bH * h0 + bT * treatment,
            a ~ dnorm(0, 100),
            bT ~ dnorm(0, 10),
            bH ~ dnorm(0, 10),
            sigma ~ dunif(0, 10)),
        data=d)
<<<<<<< HEAD
=======
>>>>>>> 8d93df4439321326e02e45a727f00d38407bc253
>>>>>>> 6c7342129c8c92411bdce4806135a8f997ac1773
