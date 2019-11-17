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
            log(lambda)  <- a + bp * log_pop + bc * contact_high  +
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

##  compute trend for low contact islands
d.pred <- data.frame(
    log_pop = log_pop.seq,
    contact_high = 0)
lambda.pred.l <- ensemble(m10.10, m10.11, m10.12, data = d.pred)
lambda.med <- apply(lambda.pred.l$link, 2, median)
lambda.PI <- apply(lambda.pred.l$link, 2, PI)

## plot again
lines(log_pop.seq, lambda.med, lty=2)
shade(lambda.PI, log_pop.seq, col=col.alpha("black", 0.1))

m10.10 <- map2stan(m10.10, iter=3000, warmup=1000, chains=4)
precis(m10.10)

## center the log_pop predictor

d$log_pop.c <- d$log_pop - mean(d$log_pop)

m10.10stan.c <- map2stan(
    alist(
        total_tools ~ dpois(lambda),
        log(lambda) <- a + bp * log_pop.c +
            bc * contact_high + bpc * log_pop.c * contact_high,
        a ~ dnorm(0,100),
        bp ~ dnorm(0,1),
        bc ~ dnorm(0,1),
        bpc ~ dnorm(0,1)),
    data = d, iter=3000, warmup=1000, chains=4)
precis(m10.10stan.c)

pairs(m10.10stan.c)
 
## Multinomial problems
## Individuals choosing career and its dependence upon income expectation

N <- 500
income = c(1:3)
score = .5 * income
p <- softmax(score[1], score[2], score[3])

career <- rep(NA, N)
for (i in 1:N) career[i] <- sample(1:3, size=1, prob=p)

## Now fit a model to the generated data
m10.16 <- map(
    alist(
        career ~ dcategorical(softmax(0, s2, s3)),
        s2 <- b*2,
        s3 <- b*3,
        b ~ dnorm(0,5)),
    data = list(career=career))

precis(m10.16)


## Impact of family income on career choice

N <- 100
family_income <- runif(N)
b <- (-1:1)
career <- rep(NA,N)
for(i in 1:N){
    score <- 0.5 * (1:3) + b * family_income[i]
    p <- softmax(score[1], score[2], score[3])
    career[i] <- sample(1:3, size=1, prob=p)
}

m10.17 <-
    map(
        alist(
            career ~ dcategorical(softmax(0, s2, s3)),
            s2 <-  a2 + b2 * family_income,
            s3 <-  a3 + b3 * family_income,
            c(a2, a3, b2, b3) ~ dnorm(0, 0.5)),
        data = list(career=career, family_income=family_income))

precis(m10.17)
