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
    lines(1:4, as.vector(p[,,chimp]), col=rangi2, lwd=1.5)

## now superimpose posterior predictions
lines(1:4, pred.p)
shade(pred.p.PI, 1:4)

## clean NAs from the data
d2 <- d
d2$recipient <- NULL

## re-use map fit to get the formula

m10.3stan <- map2stan(m10.3, data=d2, iter = 1e4, warmup = 1000)
precis(m10.3stan)
pairs(m10.3stan)

m10.4 <- map2stan(
    alist(
        pulled_left ~ dbinom(1, p),
        logit(p) <- a[actor] + (bp + bpC * condition) * prosoc_left,
        a[actor] ~ dnorm(0,10),
        bp ~ dnorm(0,10),
        bpC ~ dnorm(0, 10)),
    data = d2, chains = 2, iter = 2500, warmup = 500)

unique(d$actor) 

precis(m10.4, depth = 2)

post <- extract.samples(m10.4)
str(post)

dens(post$a[,2])

chimp <- 3
d.pred <- list(
    pulled_left = rep(0,4), ## empty outcome
    prosoc_left = c(0,1,0,1), ## right/left/right/left
    condition = c(0,0,1,1), ## control/control/partner/partner
    actor = rep(chimp, 4)
)

link.m10.4 <- link(m10.4, data=d.pred)
pred.p <- apply(link.m10.4, 2, mean)
pred.p.PI <- apply(link.m10.4, 2, PI )

plot(0, 0, type="n", xlab="prosoc_left/condition",
     ylab="proportion pulled left", ylim = c(0,1), xaxt="n",
     xlim=c(1,4), yaxp=c(0,1,2))

axis(1, at=1:4, labels=c("0/0", "1/0", "0/1", "1/1"))
mtext(paste("actor", chimp))

p <- by(d$pulled_left,
        list(d$prosoc_left, d$condition, d$actor), mean)
lines(1:4, as.vector(p[,,chimp]), col=rangi2, lwd=2)

lines(1:4, pred.p)
shade(pred.p.PI, 1:4)
