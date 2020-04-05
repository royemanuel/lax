library("rethinking")
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

data(UCBadmit)
d <- UCBadmit
d$male <- ifelse(d$applicant.gender == "male", 1, 0)
d$dept_id <- coerce_index(d$dept)

m13.2 <-
    map2stan(
        alist(
            admit ~ dbinom(applications, p),
            logit(p)  <-  a_dept[dept_id] + bm * male,
            a_dept[dept_id] ~ dnorm(a, sigma_dept),
            a ~ dnorm(0, 10),
            bm ~ dnorm(0, 1),
            sigma_dept ~ dcauchy(0, 2)),
        data = d, warmup = 500, iter = 4500, chains = 3)
precis(m13.2, depth = 2)

m13.3 <-
    map2stan(
        alist(
            admit ~ dbinom(applications, p),
            logit(p) <- a_dept[dept_id] + bm_dept[dept_id] * male,
            c(a_dept, bm_dept)[dept_id] ~ dmvnorm2(c(a, bm), sigma_dept, Rho),
            a ~ dnorm(0, 10),
            bm ~ dnorm(0,1),
            sigma_dept ~ dcauchy(0, 2),
            Rho ~ dlkjcorr(2)),
        data = d, warmup = 1000, iter = 5000, chains = 4, cores = 3)



## Tried to redo the plot from 13.1, but this isn't working so well.

## p_int <-
##     d %>%
##     group_by(dept) %>%
##     summarise(app = sum(applications),
##               acc = sum(admit)) %>%
##     mutate(dept_int = acc / app,
##            logOdds = log(dept_int / (1 - dept_int)))

## p_maleint <-
##     d %>%
##     mutate(acc_rate = admit / applications) %>%
##     filter(male == 1) %>%
##     mutate(logOddsB = log(acc_rate / (1 - acc_rate)) - p_int$logOdds)

## a1 <- p_int$logOdds
## b1 <- p_maleint$logOddsB
## ## extract sample means from partially pooled estimates
## post <- extract.samples(m13.3)
## a2 <- apply(post$a_dept, 2, mean)
## pa2 <- exp(a2) / (1 + exp(a2))
## b2 <- apply(post$bm_dept, 2, mean)
## pb2 <- exp(a2 + b2) / (1 + exp(a2 + b2)) - pa2
## ## Plot each and connect hem
## plot(a1, b1, xlab = "intercept", ylab = "slope",
##      pch = 16, col = rangi2 , ylim= c(min(b1)-0.1, max(b1) + 0.1),
##      xlim = c(min(a1) - 0.1, max(a1) + 0.1))

## Mu_est <- c(mean(post$a_dept), mean(post$bm_dept))
## rho_est <- mean(post$Rho[,1,2])
## sa_est <- mean(post$sigma_dept

## ppoints(a2, b2, pch = 1)
## for( i in 1:6) lines(c(a1[i], a2[i]), c(b1[i], b2[i]))


## for(library(ellipse)

m13.4 <-
    map2stan(
        alist(
            admit ~ dbinom( applications, p),
            logit(p) <- a_dept[dept_id],
            a_dept[dept_id] ~ dnorm( a, sigma_dept),
            a ~ dnorm(0, 10),
            sigma_dept ~ dcauchy(0,2)
        ),
        data = d, warmup = 500, iter = 4500, chains = 3)

compare(m13.2, m13.3, m13.4)
