library(rethinking)

data(chimpanzees)
d <- chimpanzees
d$recipient <- NULL
d$block_id <- d$block

m13.6 <- map2stan(
    alist(
        ## likelihood
        pulled_left ~ dbinom(1, p),
        ## Linear model
        logit(p)  <-  A + (BP + BPC * condition) * prosoc_left,
        A <- a + a_actor[actor] + a_block[block_id],
        BP <- b + b_actor[actor] + b_block[block_id],
        BPC <- bpc + bpc_actor[actor] + bpc_block[block_id],
        ## adaptive priors
        c(a_actor, b_actor, bpc_actor)[actor] ~ dmvnorm2(0, sigma_actor, Rho_actor),
        c(a_block, b_block, bpc_block)[block_id] ~ dmvnorm2(0, sigma_block, Rho_block),
        ## fixed priors
        c(a, b, bpc) ~ dnorm(0, 1),
        sigma_actor ~ dcauchy(0, 2),
        sigma_block ~ dcauchy(0, 2),
        Rho_actor ~ dlkjcorr(4),
        Rho_block ~ dlkjcorr(4)),
    data = d, iter = 5000, warmup = 1000, chains = 3, cores = 3)

m13.6NC <- map2stan(
    alist(
        ## likelihood
        pulled_left ~ dbinom(1, p),
        ## Linear model
        logit(p)  <-  A + (BP + BPC * condition) * prosoc_left,
        A <- a + a_actor[actor] + a_block[block_id],
        BP <- b + b_actor[actor] + b_block[block_id],
        BPC <- bpc + bpc_actor[actor] + bpc_block[block_id],
        ## adaptive NON-CENTERED priors
        c(a_actor, b_actor, bpc_actor)[actor] ~ dmvnormNC(sigma_actor, Rho_actor),
        c(a_block, b_block, bpc_block)[block_id] ~ dmvnormNC(sigma_block, Rho_block),
        ## fixed priors
        c(a, b, bpc) ~ dnorm(0, 1),
        sigma_actor ~ dcauchy(0, 2),
        sigma_block ~ dcauchy(0, 2),
        Rho_actor ~ dlkjcorr(4),
        Rho_block ~ dlkjcorr(4)),
    data = d, iter = 5000, warmup = 1000, chains = 3, cores = 3)

## extract n__eff values for each model
neff_c <- precis(m13.6, 2)@.Data[[5]]
neff_nc <- precis(m13.6NC,depth = 2)@.Data[[5]]
## plot distributions
boxplot( list( 'm13.6' = neff_c, 'm13.6NC' = neff_nc),
        xgylab = "effectiveness samples", xlab = "model")
