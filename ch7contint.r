library(rethinking)
data(tulips)
d <- tulips
str(d) 

## model with no interactions
m7.6 <-
    map(
        alist(
            blooms ~ dnorm(mu, sigma),
            mu <- a + bW * water + bS * shade,
            a ~ dnorm(0,100),
            bW ~ dnorm( 0, 100),
            bS ~ dnorm(0, 100),
            sigma ~ dunif(0, 100)),
        data = d,
        method="Nelder-Mead",
        control=list(maxit=1e4))
precis(m7.6)
m7.7 <-
    map(
        alist(
            blooms ~ dnorm(mu, sigma),
            mu <- a + bW * water + bS * shade + bWS * shade * water,
            a ~ dnorm(0,100),
            bW ~ dnorm( 0, 100),
            bS ~ dnorm(0, 100),
            bWS ~ dnorm(0,100),
            sigma ~ dunif(0, 100)),
        data = d,
        method="Nelder-Mead",
        control=list(maxit=1e4))
precis(m7.7)
coeftab(m7.6, m7.7)
compare(m7.6, m7.7)
