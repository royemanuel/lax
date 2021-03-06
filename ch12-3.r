## Back to the chimpanzees!

library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$recipient <- NULL # get rid of NAs

m12.4 <-
    map2stan(
        alist(
            pulled_left ~ dbinom(1, p),
            logit(p) <- a + a_actor[actor] + (bp + bpC * condition) * prosoc_left,
            a_actor[actor] <- dnorm(0, sigma_actor),
            a ~ dnorm(0,10),
            bp ~ dnorm(0,10),
            bpC ~ dnorm(0,10),
            sigma_actor ~ dcauchy(0,1)
        ),
        data = d,
        warmup = 1000,
        iter = 10000,
        chains = 4
    )

post <- extract.samples(m12.4)

## This is the magic function that builds the real means that are useful
total_a_actor <- sapply(1:7, function(actor) post$a + post$a_actor[,actor])
round(apply(total_a_actor, 2, mean), 2)

## Two types of clusters

d$block_id  <- d$block

m12.5 <-
    map2stan(
        alist(
            pulled_left ~ dbinom(1, p),
            logit(p) <- a + a_actor[actor] +
                a_block[block_id] +
                (bp + bpc * condition) * prosoc_left,
            a_actor[actor] ~ dnorm(0, sigma_actor),
            a_block[block_id] ~ dnorm(0, sigma_block),
            c(a, bp, bpc) ~ dnorm(0,10),
            sigma_actor ~ dcauchy(0,1),
            sigma_block ~ dcauchy(0,1)),
        data = d, warmup = 1000, iter = 6000, chains = 4, cores = 3
    )

precis(m12.5, depth = 2)
plot(precis(m12.5, depth =2))
compare(m12.4, m12.5)

post <- extract.samples(m12.5)
dens(post$sigma_block, xlab = "sigma", xlim = c(0,4))
dens(post$sigma_actor, col = rangi2, lwd = 2, add=TRUE)
text(2, 0.85, "actor", col=rangi2)
text(0.75, 2, "block")

chimp <- 2
d.pred <-
    list(
        prosoc_left = c(0,1,0,1),
        condition = c(0, 0, 1, 1),
        actor = rep(chimp, 4)
    )

link.m12.4 <- link(m12.4, data = d.pred)
pred.p <- apply(link.m12.4, 2, mean)
pred.p.PI <- apply(link.m12.4, 2, PI)

plot(0, 0, type = "n", xlab = "prosoc_left/condition",
     ylab = "proportion pulled left", ylim = c(0,1), xaxt="n",
     xlim = c(1, 4))
axis(1, at=1:4, labels = c("0/0", "1/0", "0/1", "1/1"))
p <- by(d$pulled_left,
        list(d$prosoc_left, d$condition, d$actor), mean)
for (chimp in 1:7){
    lines(1:4, as.vector(p[,,chimp]),
          col = rangi2, lwd = 1.5)
}

lines(1:4, pred.p)
shade(pred.p.PI, 1:4)


## Method for building the posterior predictions

post <- extract.samples(m12.4)
str(post)
dens(post$a_actor[,5])

p.link <- function(prosoc_left, condition, actor){
    logodds <- with(post,
                    a + a_actor[,actor] +
                    (bp + bpC + condition) * prosoc_left)
    return(logistic(logodds))
}

prosoc_left  <- c(0, 1, 0, 1)
condition <- c(0, 0, 1, 1)
pred.raw <- sapply(1:4, function(i) p.link(prosoc_left[i], condition[i], 2))
pred.p <- apply(pred.raw, 2, mean)
pred.p.PI <- apply(pred.raw, 2, PI)

d.pred <- list(
    prosoc_left = c(0, 1, 0, 1),
    condition = c(0, 0, 1, 1),
    actor = rep(2, 4)
)

## replace varying intercept samples iwth zeros
## 1000 samples by 7 actors
a_actor_zeros <- matrix(0, 1000, 7)

## fire up link using replace list
link.m12.4 <- link(m12.4, n=1000, data = d.pred,
                   replace=list(a_actor = a_actor_zeros))

## summarize and plot
pred.p.mean <- apply(link.m12.4, 2, mean)
pred.p.PI <- apply(link.m12.4, 2, PI, prob = 0.8)

plot(0, 0, type = "n", xlab = "prosoc_left/condition",
     ylab = "proportion pulled left", ylim = c(0,1), xaxt="n",
     xlim = c(1, 4))
axis(1, at=1:4, labels = c("0/0", "1/0", "0/1", "1/1"))
lines(1:4, pred.p.mean)
shade(pred.p.PI, 1:4)

prosoc_left  <- c(0,1,0,1)
condition <- c(0, 0, 1, 1)
pred.raw <- sapply(1:4, function(i) p.link(prosoc_left[i], condition[i], 2))
pred.p  <- apply(pred.raw, 2, mean)
pred.p.PI <- apply(pred.raw, 2, PI)

d.pred <- list(
    prosoc_left = c(0, 1, 0, 1),
    condition = c(0, 0, 1, 1),
    actor = rep(2, 4))

## replace vaying intercept samples with zeros
## 1000 samples by 7 actors
a_actor_zeros <- matrix(0, 1000, 7)

link.m12.4 <- link(m12.4, n=1000, data = d.pred,
                   replace=list(a_actor = a_actor_zeros))

## summarize and plot
pred.p.mean <- apply(link.m12.4, 2, mean)
pred.p.PI <- apply(link.m12.4, 2, PI, prob = 0.8)
plot(0, 0, type = "n", xlab = "prosoc_left/condition",
     ylab = "proportion pulled left", ylim = c(0, 1),
     xaxt = "n", xlim = c(1,4))
axis(1, at = 1:4, labels= c("0/0", "1/0", "0/1", "1/1"))
lines(1:4, pred.p.mean)
shade(pred.p.PI, 1:4)



## replace varying intercept samples with simulations
post <- extract.samples(m12.4)
a_actor_sims <- rnorm(7000, 0, post$sigma_actor)
a_actor_sims <- matrix(a_actor_sims, 1000, 7)

link.m12.4 <- link(m12.4,
                   n = 1000,
                   data = d.pred,
                   replace = list(a_actor = a_actor_sims))


post <- extract.samples(m12.4)
sim.actor <- function(i) {
    sim_a_actor <- rnorm(1, 0, post$sigma_actor[i])
    P <- c(0, 1, 0, 1)
    C <- c(0, 0, 1, 1)
    p <- logistic(post$a[i] +
                  sim_a_actor +
                  (post$bp[i] + post$bpC[i] * C) * P
                  )
    return(p)
}

## empty plot
plot(0, 0, type = "n", xlab = "prosoc_left/condition",
     ylab = "proportion pulled left",
     ylim = c(0, 1),
     xaxt = "n",
     xlim = c(1, 4))
axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))

## plot 50 simulated actors

for(i in 1:50) lines(1:4, sim.actor(i), col = col.alpha("black", 0.5))

