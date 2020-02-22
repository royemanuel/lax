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
total_a_actor <- sapply(1:7, function(actor) post$a + post$a_actor[,actor])
round(apply(total_a_actor, 2, mean), 2)

