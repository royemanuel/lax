library(rethinking)

data(Howell1)
d <- Howell1
d2 <- filter(d, age >= 18)

dens(d2$height)

## PLotting the priors

curve(dnorm(x, 178, 20), from=100, to=250)

curve(dunif(x, 0, 50), from = -10, to = 60)

## Sampling from the priors for the hieght model

sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu , sample_sigma)
dens(prior_h)

## Grid approximation

mu.list <- seq(from = 140, to = 160, length.out = 200)
sigma.list <- seq(from = 4, to = 9, length.out = 200)
post <- expand.grid(mu = mu.list, sigma=sigma.list)
post$LL <- sapply(1:nrow(post), function(i) sum(dnorm(
                                                d2$height,
                                                mean=post$mu[i],
                                                sd=post$sigma[i],
                                                log=TRUE)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) +
    dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))

contour_xyz( post$mu, post$sigma, post$prob)

image_xyz(post$mu, post$sigma, post$prob)

sample.rows <- sample(1:nrow(post), size=1e4, replace=TRUE, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col= col.alpha(rangi2, 0.1))

dens(sample.mu)
dens(sample.sigma)

## Focus around 20 samples from the data
d3 <- sample(d2$height, size=20)

mu.list <- seq(from = 150, to = 170, length.out = 200)
sigma.list <- seq(from = 4, to = 20, length.out = 200)
post2 <- expand.grid(mu = mu.list, sigma=sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i) sum(dnorm(
                                                d2$height,
                                                mean=post2$mu[i],
                                                sd=post2$sigma[i],
                                                log=TRUE)))
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) +
    dunif(post2$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod - max(post2$prod))

contour_xyz( post2$mu, post2$sigma, post2$prob)

image_xyz(post2$mu, post2$sigma, post2$prob)

sample2.rows <- sample(1:nrow(post2), size=1e4, replace=TRUE, prob = post2$prob)
sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]
plot(sample2.mu, sample2.sigma, cex=0.5, col=col.alpha(rangi2,0.1),
     xlab="mu", ylab="sigma", pch=16)

dens(sample2.sigma, norm.comp = TRUE)

## Building a quadratic fit using map
flist <- alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 20),
    sigma ~ dunif(0, 50))

m4.1 <- map(flist, data = d2)
precis(m4.1)

m4.2 <- map(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ dnorm( 178, 0.1),
        sigma ~ dunif(0, 50)),
    data=d2)

precis(m4.2)

## Sample from the posterior built from the quadratic
post <- extract.samples(m4.1, n=1e4)

head(post)
precis(post)

## Switching to a logarithmic Sigma
m4.1_logsima <- map(
    alist(
        height ~ dnorm(mu, exp(log_sigma)),
        mu ~ dnorm( 178, 20),
        log_sigma ~ dnorm(2, 10)),
    data = d2)

postlog <- extract.samples(m4.1_logsima)
sigma <- exp(post$log_sigma)

ggplot(d2, aes(x=height, y = weight))  + geom_point()

## Linear model

m4.3 <-
    map(
        alist(
            height ~ dnorm(mu, sigma),
            mu <- a + b * weight,
            a ~ dnorm( 178, 100),
            b ~ dnorm(0, 10),
            sigma ~ dunif( 0, 50)),
        data = d2)

precis(m4.3)
precis(m4.3, corr=TRUE)

## Centering the weight data
d2 <-
    d2 %>%
    mutate(weight.c = weight - mean(weight))

m4.4 <-
    map(
        alist(
            height ~ dnorm(mu, sigma),
            mu <- a + b * weight.c,
            a ~ dnorm( 178, 100),
            b ~ dnorm(0, 10),
            sigma ~ dunif( 0, 50)),
        data = d2)

precis(m4.4)
precis(m4.4, corr=TRUE)

plot(height ~ weight, data = d2)
abline(a=coef(m4.3)["a"], b = coef(m4.3)["b"])

ggplot(d2, aes(x=weight, y=height)) +
    geom_point() +
    geom_abline(intercept = coef(m4.3)["a"], slope = coef(m4.3)["b"])

post <- extract.samples(m4.3)

## Plot multiple regression lines

N <- 352
dN <- d2[1:N,]
mN <- map(
    alist(
        height ~ dnorm(mu , sigma),
        mu <- a  + b*weight,
        a ~ dnorm(178, 100),
        b ~ dnorm(0, 10),
        sigma ~ dunif(0, 50)),
    data = dN)

## plot 20 of the lines

post3 <- extract.samples(mN, n=20)

plot(dN$weight, dN$height,
     xlim=range(d2$weight), ylim=range(d2$height),
     col=rangi2, xlab="weight", ylab="height")
mtext(concat("N = ",N))

for (i in 1:20)
    abline(a=post3$a[i], b=post3$b[i], col=col.alpha("black",0.3))

mu_at_50 <- post$a  + post$b * 50

dens(mu_at_50, col=rangi2, lwd = 2, xlab="mu|weight = 50")

HPDI(mu_at_50, prob = 0.89)

## building a shaded region instead of multiple lines using the link
## function that is part of the rethinking package
mu <- link(m4.3)

weight.seq <- seq(from=25, to=70, by=1)

mu <- link(m4.3, data=data.frame(weight=weight.seq))

plot(height ~ weight, d2, type="n")
for(i in 1:100)
    points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2, 0.1))

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)

                                        # Plot the HPDI for each weight
# Raw data
plot(height ~ weight, d2, col=col.alpha(rangi2,0.5))
## MAP linear
lines( weight.seq, mu.mean)
## PLot a shaded region for the 89% HPDI
shade(mu.HPDI, weight.seq)

## Prediction intervals for actual heights, rather than the mean.
## UNDERSTAND THIS!!
sim.height <- sim(m4.3, data=list(weight=weight.seq, n=1e7))
str(sim.height)
height.PI <- apply(sim.height, 2, PI, prob=0.89)
# Raw data
plot(height ~ weight, d2, col=col.alpha(rangi2,0.5))
## MAP linear
lines( weight.seq, mu.mean)
## PLot a shaded region for the 89% HPDI
shade(mu.HPDI, weight.seq)
## draw PI region for simulated heights
shade(height.PI, weight.seq)

## Standardize weight

d <-
    d %>%
    mutate(weight.s = (weight - mean(weight) )/sd(weight),
           weight.s2 = weight.s^2,
           weight.s3 = weight.s^3)


m4.5 <-
    map(
        alist(
            height ~ dnorm(mu, sigma),
            mu <- a + b1 * weight.s + b2 * weight.s2  + b3 * weight.s3,
            a ~ dnorm( 178, 100),
            b1 ~ dnorm(0, 10),
            b2 ~ dnorm(0, 10),
            b3 ~ dnorm(0, 10),
            sigma ~ dunif( 0, 50)),
        data = d)

weight.seq <- seq(from=-2.2, to=2, length.out=30)
pred_dat <- list(weight.s=weight.seq, weight.s2 = weight.seq^2, weight.s3 = weight.seq^3)
mu <- link(m4.5, data=pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(m4.5, data=pred_dat)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

# Raw data
plot(height ~ weight.s, d, col=col.alpha(rangi2,0.5))
## MAP linear
lines( weight.seq, mu.mean)
## PLot a shaded region for the 89% HPDI
shade(mu.HPDI, weight.seq)
## draw PI region for simulated heights
shade(height.PI, weight.seq)

## Problem 4H1

weight.seq <- c(46.95, 43.72, 64.78, 32.59, 54.63)
pred_dat <- list(weight = weight.seq)
mu <- link(m4.3, data=pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)



## problem 4H2
d4 <-
    d %>%
    filter(age < 18) %>%
    mutate(weight.s = (weight - mean(weight))/sd(weight))

m4.6 <-
    map(
        alist(
            height ~ dnorm(mu, sigma),
            mu <- a  + b * weight.s,
            a ~ dnorm(108, 30),
            b ~ dnorm(0, 10),
            sigma ~ dunif(0, 50)),
        data = d4)

## Raw data plot

ggplot(d4, aes(weight.s, height)) + geom_point()
plot(height ~ weight.s, d4, col = col.alpha(rangi2, 0.5))

weight.seq <- seq(from=-2, to=3, length.out=30)
pred_dat <- list(weight.s = weight.seq)
mu <- link(m4.6, data = pred_dat)
mu.mean <- apply(mu, 2, mean)

lines(weight.seq, mu.mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m4.6, data=pred_dat)
height.PI <- apply(sim.height, 2, PI, prob=0.89)
shade(mu.HPDI, weight.seq)
shade(height.PI, weight.seq)

## Problem 4H3

d5 <-
    d %>%
    ## filter(age < 18) %>%
    mutate(weightlog = log(weight),
           weight.s = (weightlog - mean(weightlog))/sd(weightlog))

m4.7 <-
    map(
        alist(
            height ~ dnorm(mu, sigma),
            mu <- a  + b * weightlog,
            a ~ dnorm(178, 100),
            b ~ dnorm(0, 100),
            sigma ~ dunif(0, 50)),
        data = d5)

## Raw data plot

ggplot(d5, aes(weight.s, height)) + geom_point()
plot(height ~ weight, d5, col = col.alpha(rangi2, 0.5))

weight.seq <- seq(from=0, to=70, length.out=30)
pred_dat <- list(weightlog = log(weight.seq))
mu <- link(m4.7, data = pred_dat)
mu.mean <- apply(mu, 2, mean)

lines(weight.seq, mu.mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.97)
mu.PI <- apply(mu, 2, PI, prob = 0.97)
sim.height <- sim(m4.7, data=pred_dat)
height.PI <- apply(sim.height, 2, PI, prob=0.97)
shade(mu.HPDI, weight.seq)
shade(height.PI, weight.seq)
