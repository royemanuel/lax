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
