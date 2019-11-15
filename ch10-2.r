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

     
