library(tidyverse)
library(rethinking)

data(milk)
d <- milk
str(d)

m5.5 <-
    map(
        alist(
            kcal.per.g ~ dnorm(mu, sigma),
            mu <- a + bn * neocortex.perc,
            a ~ dnorm(0, 100),
            bn ~ dnorm(0, 1),
            sigma ~ dunif(0, 1)),
        data = d)

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
plot(kcal.per.g ~ log.mass, data=dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)



















