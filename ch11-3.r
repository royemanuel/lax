library(tidyverse)
library(rethinking)

## Over-dispersed outcomes

pbar <- .8
theta <- 2
curve(dbeta2(x,pbar,theta) ,from=0, to=1, xlab="probability", ylab="Density")
curve(dbeta(x,pbar,theta) ,from=0, to=1, xlab="probability", ylab="Density")

data(UCBadmit)
d <- UCBadmit

m11.5 <- map2stan(
    alist(
        admit ~ dbetabinom(applications, pbar, theta),
        logit(pbar)  <- a,
        a ~ dnorm(0,2),
        theta ~ dexp(1)),
    data = d,
    constraints = list(theta="lower=0"),
    start=list(theta=3),
    iter = 4000, warmup = 1000, chains = 2, cores = 2 )

precis(m11.5)

post <- extract.samples(m11.5)
quantile(logistic(post$a), c(0.025, 0.5, 0.975))


## draw posterior mean beta distribution
curve(dbeta2(x, mean(logistic(post$a)), mean(post$theta)), from = 0, to = 1,
      ylab="Density", xlab = "probability admit", ylim=c(0,3), lwd=2)

## draw 100 beta distributions sampled from the posterior

for( i in 1:100){
    p <- logistic(post$a[i])
    theta <- post$theta[i]
    curve(dbeta2(x, p, theta), add=TRUE, col=col.alpha("black", 0.2))
}

postcheck(m11.5)

mu <- 5
theta <- 20
curve(dgamma2(x, mu, theta), from=0, to=10)
