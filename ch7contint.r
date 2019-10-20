library(rethinking)
data(tulips)
d <- tulips
str(d) 

## model with no interactions
m7.6 <-
    map(
        alist(
            blooms ~ dnorm(mu, sigma),
            mu <- a + bW * water + bS * shade,
            a ~ dnorm(0,100),
            bW ~ dnorm( 0, 100),
            bS ~ dnorm(0, 100),
            sigma ~ dunif(0, 100)),
        data = d,
        method="Nelder-Mead",
        control=list(maxit=1e4))
precis(m7.6)
m7.7 <-
    map(
        alist(
            blooms ~ dnorm(mu, sigma),
            mu <- a + bW * water + bS * shade + bWS * shade * water,
            a ~ dnorm(0,100),
            bW ~ dnorm( 0, 100),
            bS ~ dnorm(0, 100),
            bWS ~ dnorm(0,100),
            sigma ~ dunif(0, 100)),
        data = d,
        method="Nelder-Mead",
        control=list(maxit=1e4))
precis(m7.7)
coeftab(m7.6, m7.7)
compare(m7.6, m7.7)

## centering the predictor variables
d$shade.c <- d$shade - mean(d$shade)
d$water.c <- d$water - mean(d$water)

m7.8 <-
    map(
        alist(
            blooms ~ dnorm(mu, sigma),
            mu <- a + bW * water.c + bS * shade.c,
            a ~ dnorm(130 ,100),
            bW ~ dnorm( 0, 100),
            bS ~ dnorm(0, 100),
            sigma ~ dunif(0, 100)),
        data = d,
        start=list(a=mean(d$blooms), bW=0, bW=0, sigma=sd(d$blooms)))
precis(m7.8)

m7.9 <-
    map(
        alist(
            blooms ~ dnorm(mu, sigma),
            mu <- a + bW * water.c + bS * shade.c + bWS * shade.c * water.c,
            a ~ dnorm(130,100),
            bW ~ dnorm( 0, 100),
            bS ~ dnorm(0, 100),
            bWS ~ dnorm(0,100),
            sigma ~ dunif(0, 100)),
        data = d,
        start=list(a=mean(d$blooms), bW=0, bW=0, sigma=sd(d$blooms)))

precis(m7.9)
coeftab(m7.8, m7.9)
compare(m7.8, m7.9)


## using the base R graphics and the stuff from rethinking package
par(mfrow=c(1,3))

## loop over values of water.c and plot predictions
shade.seq <- -1:1
for(w in -1:1){
    dt <- d[d$water.c==w,]
    plot(blooms ~ shade.c, data=dt, col=rangi2,
         main=paste("water.c =", w), xaxp=c(-1, 1, 2),
         ylim= c(0,362),
         xlab = "shade (centered)")
    mu <- link(m7.9, data=data.frame(water.c=w, shade.c=shade.seq))
    mu.mean <- apply(mu, 2, mean)
    mu.PI <- apply(mu, 2, PI)
    lines(shade.seq, mu.mean)
    lines(shade.seq, mu.PI[1,], lty=2)
    lines(shade.seq, mu.PI[2,], lty=2)
}
 
