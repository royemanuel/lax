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
            pulled_left ~ dbinom(1, p),
            logit(p) <- a + (bP + bPC * condition) * prosoc_left,
            a ~ dnorm(0, 10),
            bP ~ dnorm(0, 10),
            bPC ~ dnorm(0, 10)),
        data=d)
precis(m10.3)
chimp.models  <- compare(m10.1, m10.2, m10.3)
plot(chimp.models, SE=TRUE, dSE = TRUE)

exp(0.61)
logistic(4)
logistic(4 + .61)

## dummy data for predictions across treatments
d.pred <- data.frame(
    prosoc_left = c(0,1,0,1),
    condition = c(0, 0, 1, 1))

## build prediction ensemble
chimp.ensemble <- ensemble(m10.1, m10.2, m10.3, data=d.pred)

## summarize
pred.p <- apply(chimp.ensemble$link, 2, mean)
pred.p.PI <- apply(chimp.ensemble$link, 2, PI)

## empty plot frame with good axes
plot(0, 0, type="n", xlab="prosoc_left/condition",
     ylab = "proportion pulled left", ylim = c(0,1), xaxt = "n",
     xlim=c(1,4))
axis(1, at=1:4, labels=c("0/0", "1/0", "0/1", "1/1"))

## plot raw data, one trend for each of 7 individual chimpanzees will
## use by() here; see Overthinking box for explanation
p <-  by(d$pulled_left, list(d$prosoc_left, d$condition, d$actor), mean)

for (chimp in 1:7)
    list(d$prosoc_left, d$condition,










