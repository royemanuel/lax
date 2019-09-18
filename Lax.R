library("tidyverse")
library("rjags")
library("readxl")

laxData <- read_excel("2015-2019 Combined Lacrosse Statistics.xlsx",
                      sheet = "Results",
                      n_max = 353)

winView <- ggplot(laxData, aes(`GA/Game`, `G/Game`))  +
    geom_point(size=10*laxData$`Win %` + 0.5)

winView2 <- ggplot(laxData, aes(`GA/Game`,
                                `G/Game`,
                                colour = `Win %`))  +
    geom_point()  +
    scale_colour_gradient(low = "yellow", high = "blue")


GoalsFor <- scale(laxData$`G/Game`)
GoalsAgainst <- scale(laxData$`GA/Game`)

GF_noAttr <- GoalsFor
attr(GF_noAttr, 'scaled:scale') <- NULL
attr(GF_noAttr, 'scaled:center') <- NULL
GA_noAttr <- GoalsAgainst
attr(GA_noAttr, 'scaled:scale') <- NULL
attr(GA_noAttr, 'scaled:center') <- NULL

laxData$GoalsFor <- as.vector(GF_noAttr)
laxData$GoalsAgainst <- as.vector(GA_noAttr)

mod_string <- "model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = a0 + b[1] * GoalsFor[i]  + b[2] * GoalsAgainst[i] 
    }

    a0 ~ dnorm(0.0, 1.0/1e6)
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    
    for (j in 1:2) {
        b[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    sig = sqrt( 1.0 / prec)
}"

mod_string2 <- "model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = a0 + b[1] * GoalsFor[i]  + b[2] * GoalsAgainst[i] + b[3] * GoalsAgainst[i] * GoalsFor[i]
    }

    a0 ~ dnorm(0.0, 1.0/1e6)
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    
    for (j in 1:3) {
        b[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    sig = sqrt( 1.0 / prec)
}"

mod_string_h <- "model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = a0 + b[i,1] * GoalsFor[i]  + b[i,2] * GoalsAgainst[i] 
    }

    a0 ~ dnorm(0.0, 1.0/1e6)
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    
    for (j in 1:length(y)) {
        b[j,1] ~ dnorm(nu[j], prec2)
        b[j,2] ~ dnorm(omicron[j], prec3)
        nu[j] = n0 + c[1] * Shooting[j]
        omicron[j] = o0 + c[2]  * Clearing[j]
    }

    n0 ~ dbeta(1.0, 1.0)
    o0 ~ dbeta(1.0, 1.0)
    prec2 ~ dgamma(5/2.0, 5*10.0/2.0)
    prec3 ~ dgamma(5/2.0, 5*10.0/2.0)
    for (k in 1:2) {
        c[k] ~ dbeta(1, 1)
    }
    sig = sqrt( 1.0 / prec)
    sig2 = sqrt(1.0/prec2)
    sig3 = sqrt(1.0/prec3)
}"


set.seed(117)

data_jags <- list(y=laxData$`Win %`,
                  GoalsFor=laxData$`G/Game`,
                  GoalsAgainst=laxData$`GA/Game`)

data_jags_h <- list(y=laxData$`Win %`,
                  GoalsFor=laxData$`G/Game`,
                  GoalsAgainst=laxData$`GA/Game`,
                  Shooting=laxData$`Shooting %`,
                  Clearing=laxData$`Clearing %`)



data_jags2 <- list(y=laxData$`Win %`,
                  GoalsFor=laxData$GoalsFor,
                  GoalsAgainst=laxData$GoalsAgainst)

params <- c("a0", "b", "sig")

params_h <- c("a0", "n0", "o0", "c", "sig", "sig2", "sig3")


mod <- jags.model(textConnection(mod_string2), data=data_jags, n.chains=4)

update(mod, 1e3)

mod_sim <- coda.samples(model=mod,
                        variable.names=params,
                        n.iter = 5e4)

mod_csim <- as.mcmc(do.call(rbind, mod_sim))

mod_scaled <- jags.model(textConnection(mod_string), data=data_jags2, n.chains=4)

update(mod_scaled, 1e3)

mod_scaled_sim <- coda.samples(model=mod_scaled,
                        variable.names=params,
                        n.iter = 5e4)

mod_scaled_csim <- as.mcmc(do.call(rbind, mod_scaled_sim))


## mod_sim$GF <- mod_sim$`b[1]` * attr(GoalsFor, 'scaled:scale') +
##     attr(GoalsFor, 'scaled:center')

plot(mod_sim, ask=TRUE)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

dic.samples(mod, n.iter=1e3)

summary(mod_sim)

X <- cbind(rep(.48554, dim(laxData)[1]), data_jags$GoalsFor, data_jags$GoalsAgainst) 
pm_params <- colMeans(mod_csim)

yhat <- drop(X %*% pm_params[1:3])
resid <- data_jags$y - yhat
plot(yhat, resid)
sd(resid)
qqnorm(resid)

plot(mod_scaled_sim, ask=TRUE)
gelman.diag(mod_scaled_sim)
autocorr.diag(mod_scaled_sim)
autocorr.ploto(mod_scaled_sim)
effectiveSize(mod_scaled_sim)

dic.samples(mod_scaled, n.iter=1e3)

summary(mod_scaled_sim)

X_scaled <- cbind(rep(.48554, dim(laxData)[1]), data_jags2$GoalsFor, data_jags2$GoalsAgainst) 
pm_params_scaled <- colMeans(mod_scaled_csim)

yhat_scaled <- drop(X_scaled %*% pm_params_scaled[1:3])
resid_scaled <- data_jags2$y - yhat
plot(yhat_scaled, resid_scaled)
sd(resid_scaled)
qqnorm(resid_scaled)

plot(laxData$`Win %` ~ laxData$GoalsFor)

gf <- laxData$GoalsFor * sd(laxData$`G/Game`) + mean(laxData$`GA/Game`)
