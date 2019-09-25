library(tidyverse)
library(rethinking)

data(milk)
d <- milk
str(d)

## This is an invalid model because the NA's have not been removed
## m5.5 <-
##     map(
##         alist(
##             kcal.per.g ~ dnorm(mu, sigma),
##             mu <- a + bn * neocortex.perc,
##             a ~ dnorm(0, 100),
##             bn ~ dnorm(0, 1),
##             sigma ~ dunif(0, 1)),
##         data = d)

dcc <-
    d %>%
    filter(!is.na(neocortex.perc))
            
m5.5 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, sigma),
            mu <- a + bn * neocortex.perc,
            a ~ dnorm(0, 100),
            bn ~ dnorm(0, 1),
            sigma ~ dunif(0, 1)),
        data = dcc)
precis(m5.5, digits = 3)

coef(m5.5)["bn"] * (76-55)

np.seq <- 0:100
pred.data <- data.frame(neocortex.perc=np.seq)

mu <- link(m5.5, data = pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data=dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

dcc <-
    dcc %>%
    mutate(log.mass = log(mass))

m5.6 <- map(
    alist(
        kcal.per.g ~ dnorm(mu, sigma),
        mu <- a + bm * log.mass,
        a ~ dnorm(0, 100),
        bm ~ dnorm(0, 1),
        sigma ~ dunif(0, 1)),
    data = dcc)
precis(m5.6)

np.seq <- -3:5
pred.data <- data.frame(log.mass=np.seq)
mu <- link(m5.6, data = pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
## silly change to commit
plot(kcal.per.g ~ log.mass, data=dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

## Writing a multiple factor regression
m5.7 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, sigma),
            mu  <-  a + bm * log.mass + bn * neocortex.perc,
            a ~ dnorm(0, 100),
            bm ~ dnorm(0, 1),
            bn ~ dnorm(0, 1),
            sigma ~ dunif(0, 50)),
        data = dcc)

mean.log.mass <- mean(log(dcc$mass))
np.seq <- 0:100
pred.data <-
    data.frame(
        neocortex.perc=np.seq,
        log.mass=mean.log.mass)

mu <- link(m5.7, data = pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data=dcc, type="n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

mean.neocortex <- mean(dcc$neocortex.perc)
np.seq <- seq(from = -5, to = 5, length.out = 100)
pred.data <-
    data.frame(
        log.mass = np.seq,
        neocortex.perc = mean.neocortex)

mu <- link(m5.7, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data=dcc, type="n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

## Generating a model with a masking relationship

N <- 100
rho <- 0.7
x_pos <- rnorm(N)
x_neg <- rnorm(N, rho*x_pos, sqrt(1-rho^2))
y <- rnorm(N, x_pos - x_neg)
d <- data.frame(y, x_pos, x_neg)


m5.8 <-
    map(
        alist(
            y ~ dnorm(mu, sigma),
            mu  <- a  + bp * x_pos,
            a ~ dnorm(0, 3), 
            bp ~ dnorm(0, 1),
            sigma ~ dunif(0, 50)),
        data = d)
m5.9 <-
    map(
        alist(
            y ~ dnorm(mu, sigma),
            mu  <- a  + bn * x_neg,
            a ~ dnorm(0, 3), 
            bn ~ dnorm(0, 1),
            sigma ~ dunif(0, 50)),
        data = d)
m5.10 <-
    map(
        alist(
            y ~ dnorm(mu, sigma),
            mu  <- a  + bp * x_pos + bn * x_neg,
            a ~ dnorm(0, 3), 
            bp ~ dnorm(0, 1),
            bn ~ dnorm(0, 1),
            sigma ~ dunif(0, 50)),
        data = d)
precis(m5.8)
precis(m5.9)
precis(m5.10)












