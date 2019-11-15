y <- rbinom(1e5, 1000, 1/1000)
c(mean(y), var(y))

library(rethinking)
data(Kline)
d <- Kline
d

d$log_pop <- log(d$population)
d$contact_high <- ifelse(d$contact == "high", 1, 0)


m10.10 <-
    map(
        alist(
            total_tools ~ dpois(lambda),
            lambda  <- a + bp * log_pop + bc * contact_high  +
                bpc * contact_high * log_pop,
            a ~ dnorm(0, 100),
            bp ~ dnorm(0, 1),
            bc ~ dnorm(0,1),
            bpc ~ dnorm(0,1)),
        data = d)
precis(m10.10,corr=TRUE)
plot(precis(m10.10))

post <- extract.samples(m10.10)     
lambda_high <- exp(post$a + post$bc + (post$bp + post$bpc) * 8)
lambda_low <- exp(post$a + post$bp * 8)

diff  <- lambda_high - lambda_low
sum(diff > 0)/length(diff)


                                        # no interaction
m10.11 <-
    map(
        alist(
            total_tools ~ dpois(lambda),
            log(lambda) <- a + bp * log_pop + bc * contact_high,
            a ~ dnorm(0, 100),
            c(bp,bc) ~dnorm(0, 1)),
        data = d)
            
## no contact rate
m10.12 <-
    map(alist(
        total_tools ~ dpois(lambda),
        log(lambda) <- a +bp * log_pop,
        a ~ dnorm(0, 100),
        bp ~ dnorm(0,1)),
        data  = d)

## no log-pop
m10.13 <-
    map(
        alist(
            total_tools ~ dpois(lambda),
            log(lambda) <- a + bc * contact_high,
            a ~ dnorm(0, 100),
            bc ~ dnorm(0, 10)),
        data = d)

## intercept only
m10.14 <-
    map(
        alist(
            total_tools ~ dpois(lambda),
            log(lambda) <- a,
            a ~ dnorm(0, 100)),
        data = d)

(islands.compare <- compare(
     m10.10,
     m10.11,
     m10.12,
     m10.13,
     m10.14,
     n=1e4))
            
plot(islands.compare)


## make plot of raw data to begin
## point character (pch) indicates contact rate
pch <- ifelse(d$contact_high==1, 16, 1)
plot(d$log_pop, d$total_tools, col=rangi2, pch=pch,
     xlab = "log-population", ylab="total tools")
log_pop.seq <- seq(from=6, to=13, length.out = 30)

## compute trend for high contact islands
d.pred <- data.frame(
    log_pop = log_pop.seq,
    contact_high = 1)

lambda.pred.h <- ensemble(m10.10, m10.11, m10.12, data=d.pred)
lambda.med <- apply(lambda.pred.h$link, 2, median)
lambda.PI <- apply(lambda.pred.h$link, 2, PI)

## plot predicted trend for high contact islands
lines(log_pop.seq, lambda.med, col=rangi2)
shade(lambda.PI, log_pop.seq, col=col.alpha(rangi2, 0.2))








