library(rethinking)
data(chimpanzees)
d <- chimpanzees

m10.1 <-
    map(
        alist(
            pulled_left ~ dbinom(1,p),
            logit(p)  <- a,
            a ~ dnorm(0, 10)),
        data = d)
precis(m10.1)

m10.2 <-
    map(
        alist(
            pulled_left ~ dbinom(1,p),
            logit(p)  <- a  + bP * prosoc_left,
            a ~ dnorm(0, 10),
            bP ~ dnorm(0, 10)),
        data = d)
precis(m10.2) 


m10.3 <-
    map(
        alist(
            pulled_left ~ dbin(1, p),
            logit(p) <- a + (bP + bPC * condition) * prosoc_left,
            a ~ dnorm(0, 10),
            bP ~ dnorm(0, 10),
            bPC ~ dnorm(0, 10)),
        data=d)

