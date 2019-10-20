library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000), ]

m8.1 <-
    map(
        alist(
            log_gdp ~ dnorm(mu, sigma),
            mu <- a + bR * rugged + bA * cont_africa + bAR * rugged * cont_africa,
            a ~ dnorm(0, 100),
            bR ~ dnorm(0, 10),
            bA ~ dnorm(0, 10),
            bAR ~ dnorm(0, 10),
            sigma ~ dunif(0, 10)),
        data = dd)
precis(m8.1)

dd.trim <- dd[, c("log_gdp", "rugged", "cont_africa")]
str(dd.trim)

m8.1stan <-
    map2stan(
        alist(
            log_gdp ~ a + bR * rugged + bA * cont_africa + bAR * rugged * cont_africa,
            a ~ dnorm(0,100),
            bR ~ dnorm(0,10),
            bA ~ dnorm(0, 10),
            bAR ~ dnorm(0, 10),
            sigma ~ dcauchy(0,2)),
        data = dd.trim)

precis(m8.1stan)
