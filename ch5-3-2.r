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
