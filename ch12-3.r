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










