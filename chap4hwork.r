## Problem 4M1

mu <- rnorm(n=1000,mean = 0, sd = 10 )
sigma <- runif(n=1000,min = 0, max = 10 )

p4m1 <-
    map(
        alist(
            y ~ dnorm(mu, sigma)
            mu ~ dnorm(0, 10)
            sigma ~ dunif(0, 10)),
        data = XXX)

