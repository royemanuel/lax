library(rethinking)
data(reedfrogs)
d <- reedfrogs
str(d)

d$tank <- 1:nrow(d)


m12.1 <- map2stan(
    alist(
        surv ~ dbinom(density, p),
        logit(p)  <- a_tank[tank],
        a_tank[tank] ~ dnorm(0, 5)),
    data = d)


m12.2 <- map2stan(
    alist(
        surv ~ dbinom(density, p),
        logit(p)  <- a_tank[tank],
        a_tank[tank] ~ dnorm(a, sigma),
        a ~ dnorm(0,1),
        sigma ~ dcauchy(0,1)),
    data = d , iter = 4000, chains = 4, cores = 4)


## Plot and compare the posterior medians from the two models

## extract Stan samples

post <- extract.samples(m12.2)

## compute median intercept for each tank and transform to probability with logistic

d$propsurv.est <- logistic(apply(post$a_tank, 2, median))

## display raw proportions surviving in each casefold
plot(d$propsurv, ylim=c(0,1), pch = 16, xaxt="n",
     xlab = "tank", ylab = "proportion survival", col = rangi2)
axis(1, at=c(1,16, 32, 48), labels = c(1, 16, 32, 48))
## Overlay the posterior medians

points(d$propsurv.est)

## mark the posteriour probability across tanks
abline(h = logistic(median(post$a)), lty=2)

## draw vertical dividors between tank densities
oabline(v=16.5, lwd=0.5)
abline(v=32.5, lwd=0.5)
text(8, 0, "small tanks")
text(16+8, 0, "medium tanks")
text(32+8, 0, "large tanks")


plot(NULL,xlim=c(-3,4), ylim=c(0, 0.35),
     xlab="log-odds survive", ylab="Density")
for (i in 1:100)
    curve( dnorm(x, post$a[i], post$sigma[i]), add=TRUE,
          col = col.alpha("black", 0.2))

## sample 8000 imaginary tanks from the posterior distribution
sim_tanks <- rnorm(8000, post$a, post$sigma)
## Transform to probability and visualize using logit
dens(logistic(sim_tanks), xlab = "probability survive")
