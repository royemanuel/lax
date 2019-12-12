library("rethinking")
data(Trolley)
d <- Trolley

simplehist(d$response, xlim=c(1,7), xlab="response")
## discrete proportion of each response value
pr_k <- table(d$response) / nrow(d)
##cumsum converts to cumulative proportions
cum_pr_k <- cumsum(pr_k)


## plot
plot(1:7, cum_pr_k, type="b", xlab="response", ylab="cumulative proportion", ylim=c(0,1))

logit <- function(x) log(x / (1-x))
(lco <- logit(cum_pr_k))
plot(1:7, lco, type="b", xlab="response", ylab="log-cumulative-odds", ylim=c(-2,2))

m11.1 <- map(
    alist(
        response ~ dordlogit( phi, c(a1, a2, a3, a4,  a5, a6)),
        phi <- 0,
        c(a1, a2, a3, a4, a5, a6) ~ dnorm(0,10)),
    data = d,
    start=list(a1=-2, a2=-1, a3=0, a4=1, a5=2, a6=2.5))

precis(m11.1)
logistic(coef(m11.1))

m11.1stan <-
    map2stan(
        alist(
            response ~ dordlogit(phi, cutpoints),
            phi  <- 0,
            cutpoints ~ dnorm(0,10)),
        data = list(response = d$response),
        start= list(cutpoints=c(-2,-1,0,1,2,2.5)),
              chains = 2, cores = 2)


(pk <- dordlogit(1:7, 0, coef(m11.1)))

sum(pk * (1:7))

(pk <- dordlogit(1:7, 0, coef(m11.1)-0.5))

sum(pk * (1:7))

m11.2 <-
    map(
        alist(
            response ~ dordlogit( phi, c(a1, a2, a3, a4, a5, a6)),
            phi <- bA * action + bI * intention + bC * contact,
            c(bA, bI, bC) ~ dnorm(0,10),
            c(a1, a2, a3, a4, a5, a6) ~ dnorm(0, 10)),
        data = d,
        start = list(a1=-1.9, a2 = -1.2, a3 = -0.7, a4 = 0.2, a5 = 0.9, a6 = 1.8))

m11.3 <-
    map(
        alist(
            response ~ dordlogit( phi, c(a1, a2, a3, a4, a5, a6)),
            phi <- bA * action + bI * intention + bC * contact +
           bAI * action * intention + bCI * contact * intention,
            c(bA, bI, bC, bAI, bCI) ~ dnorm(0,10),
            c(a1, a2, a3, a4, a5, a6) ~ dnorm(0, 10)),
        data = d,
        start = list(a1=-1.9, a2 = -1.2, a3 = -0.7, a4 = 0.2, a5 = 0.9, a6 = 1.8))

coeftab(m11.1, m11.2, m11.3)

compare(m11.1, m11.2, m11.3, refresh = 0.1)
