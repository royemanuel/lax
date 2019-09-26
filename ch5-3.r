library(tidyverse)
library(rethinking)

N <- 100
height <- rnorm(N, 10, 2)
leg_prop  <- runif(N, 0.4, 0.5)
leg_left <- leg_prop * height + rnorm(N, 0, 0.02)
leg_right <- leg_prop * height + rnorm(N, 0, 0.02)
d <- data.frame(height, leg_left, leg_right)


m5.8 <-
    map(
        alist(
            height ~ dnorm(mu, sigma),
            mu <- a + bl * leg_left + br * leg_right,
            a ~ dnorm(10, 100),
            bl ~ dnorm(2, 10),
            br ~ dnorm(2, 10),
            sigma ~ dunif(0, 10)),
        data = d)
precis(m5.8)
            
post <- extract.samples(m5.8)
plot(bl ~ br, post, col=col.alpha(rangi2, 0.1), pch=16)

## plot the posterior distribution of the sum of bl and br
sum_blbr <- post$bl + post$br
dens(sum_blbr, col=rangi2, lwd=2, xlab="sum of bl and br")


m5.9 <-
    map(
        alist(
            height ~ dnorm(mu, sigma),
            mu <- a + bl * leg_left,
            a ~ dnorm(10, 100),
            bl ~ dnorm(2, 10),
            sigma ~ dunif(0, 10)),
        data = d)
precis(m5.9)
            
post <- extract.samples(m5.8)
plot(bl ~ br, post, col=col.alpha(rangi2, 0.1), pch=16)


