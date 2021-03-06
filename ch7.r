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

rugged.seq <- seq(0, 10, by = .5)
pred.data <- data.frame(rugged = rugged.seq)

mu <- link(m7.1, data = pred.data, n = 1e4)
mu.mean <- (apply(mu, 2, mean))
mu.PI <- apply(mu, 2, PI)
sim.gdp <- sim(m7.1, data=pred.data)
gdp.PI <- apply(sim.gdp, 2, PI, prob = 0.89)
plot(log_gdp ~ rugged, d, col=col.alpha(rangi2,0.5))
lines(rugged.seq, mu.mean)
shade(mu.PI, rugged.seq)
shade(gdp.PI, rugged.seq)

## non-African nations model
m7.2 <-
    map(
        alist(
            log_gdp ~ dnorm(mu, sigma),
            mu  <- a  + bR * rugged,
            a ~ dnorm(8, 100),
            bR ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)),
        data=d.A0)
mu <- link(m7.2, data = pred.data, n = 1e4)
mu.mean <- (apply(mu, 2, mean))
mu.PI <- apply(mu, 2, PI)
sim.gdp <- sim(m7.2, data=pred.data)
gdp.PI <- apply(sim.gdp, 2, PI, prob = 0.89)
plot(log_gdp ~ rugged, d, col=col.alpha(rangi2,0.5))
lines(rugged.seq, mu.mean)
shade(mu.PI, rugged.seq)
shade(gdp.PI, rugged.seq)


m7.3 <-
    map(
        alist(
            log_gdp ~ dnorm( mu, sigma),
            mu  <- a + bR * rugged,
            a ~ dnorm(8, 100),
            bR ~ dnorm(0, 1),
            sigma ~ dunif(0,10)),
        data = dd)
precis(m7.3)

m7.4 <-
    map(
        alist(
            log_gdp ~ dnorm(mu, sigma),
            mu <- a + bR * rugged + bA * cont_africa,
            a ~ dnorm(0, 1),
            bR ~ dnorm( 0, 1),
            bA ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)),
        data=dd)
precis(m7.4)

compare(m7.3, m7.4)

rugged.seq  <- seq(from=-1, to=8, by=0.25)

## compute mu over samples, fixing cont_africa = 0
mu.NotAfrica <- link(m7.4, data=data.frame(cont_africa=0, rugged=rugged.seq))
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob = 0.97)
mu.Africa <- link(m7.4, data=data.frame(cont_africa=1, rugged=rugged.seq))
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)

plot(log_gdp ~ rugged, d.A0)
points(log_gdp ~ rugged, d.A1, col=col.alpha(rangi2, 0.5))
lines(rugged.seq, mu.NotAfrica.mean)
lines(rugged.seq, mu.Africa.mean, col = col.alpha(rangi2, 0.5))
shade(mu.NotAfrica.PI, rugged.seq)
shade(mu.Africa.PI, rugged.seq, col = col.alpha(rangi2, 0.5))

m7.5 <-
    map(
        alist(
            log_gdp ~ dnorm(mu, sigma),
            mu  <- a + gamma * rugged + bA * cont_africa,
            gamma <- bR + bAR *cont_africa,
            a ~ dnorm(8, 100),
            bA ~ dnorm(0, 1),
            bR ~ dnorm(0, 1),
            bAR ~ dnorm(0, 1),
            sigma ~ dunif(0,10)),
        data = dd)
precis(m7.5)
compare(m7.3, m7.4, m7.5)

mu.NotAfrica <- link(m7.5, data=data.frame(cont_africa=0, rugged=rugged.seq))
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob = 0.97)
mu.Africa <- link(m7.5, data=data.frame(cont_africa=1, rugged=rugged.seq))
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)
plot(log_gdp ~ rugged, d.A0)
points(log_gdp ~ rugged, d.A1, col=col.alpha(rangi2, 0.5))
lines(rugged.seq, mu.NotAfrica.mean)
lines(rugged.seq, mu.Africa.mean, col = col.alpha(rangi2, 0.5))
shade(mu.NotAfrica.PI, rugged.seq)
shade(mu.Africa.PI, rugged.seq, col = col.alpha(rangi2, 0.5))


m7.5b <-
    map(
        alist(
            log_gdp ~ dnorm(mu, sigma),
            mu <- a + bR * rugged + bAR * rugged * cont_africa + bA * cont_africa,
            a ~ dnorm(8, 100),
            bA ~ dnorm( 0, 1),
            bR ~ dnorm(0,1),
            bAR ~ dnorm(0, 1),
            sigma ~ dunif(0, 10)),
        data = dd)
precis(m7.5b)

post <- extract.samples(m7.5)
gamma.Africa <- post$bR + post$bAR * 1
gamma.notAfrica <- post$bR + post$bAR * 0

dens(gamma.Africa, xlim=c(-0.5, 0.6), ylim = c(0, 5.5), xlab="gamma", col=rangi2)
dens(gamma.notAfrica, add = TRUE)

diff <- gamma.Africa - gamma.notAfrica
sum(diff < 0) / length(diff)


## get minimum and maximum rugged values
q.rugged <- range(dd$rugged)

## compute lines and confidence intervals
mu.ruggedlo <- link(m7.5,
                    data=data.frame(rugged=q.rugged[1], cont_africa=0:1))
mu.ruggedlo.mean <- apply(mu.ruggedlo, 2, mean)
mu.ruggedlo.PI <- apply(mu.ruggedlo, 2, PI)
mu.ruggedhi <- link(m7.5,
                    data=data.frame(rugged=q.rugged[2], cont_africa=0:1))
mu.ruggedhi.mean <- apply(mu.ruggedhi, 2, mean)
mu.ruggedhi.PI <- apply(mu.ruggedhi, 2, PI)

## plot it all, splitting points at median
med.r <- median(dd$rugged)
ox <- ifelse(dd$rugged > med.r, 0.05, -0.05)
plot(dd$cont_africa +ox, log(dd$rgdppc_2000),
     col=ifelse(dd$rugged>med.r, rangi2, "black"),
     xlim=c(-0.25, 1.25) ,
     xaxt="n",
     ylab="log GDP year 2000",
     xlab="Continent")
axis(1, at=c(0,1), labels=c("other", "Africa"))
lines(0:1, mu.ruggedlo.mean, lty=2)
shade(mu.ruggedlo.PI, 0:1)
lines(0:1, mu.ruggedhi.mean, lty=2, col=rangi2)
shade(mu.ruggedhi.PI, 0:1, col=col.alpha(rangi2, 0.25))
