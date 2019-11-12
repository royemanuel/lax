library(rethinking)
data(UCBadmit)
d <- UCBadmit

d$male <- ifelse(d$applicant.gender == "male", 1, 0)

m10.6 <-
    map(
        alist(
            admit ~ dbinom(applications, p),
            logit(p)  <-  a + bm * male,
            a ~ dnorm(0, 10),
            bm ~ dnorm(0, 10)),
        data=d)

precis(m10.6)

m10.7 <-
    map(
        alist(
            admit ~ dbinom( applications, p),
            logit(p)  <- a,
            a ~ dnorm(0,10)),
        data = d)
precis(m10.7)

compare(m10.7, m10.6)

post  <- extract.samples(m10.6)
p.admit.male <- logistic(post$a + post$bm)
p.admit.female <- logistic(post$a)
diff.admit <- p.admit.male - p.admit.female
quantile(diff.admit, c(0.025, 0.5, 0.975))

postcheck(m10.6, n=1e4)

## draw lines connecting points from the same depth
for(i in 1:6){
    x <- 1 + 2 * (i - 1)
    y1 <- d$admit[x]/d$applications[x]
    y2 <- d$admit[x+1]/d$applications[x+1]
    lines(c(x, x+1), c(y1, y2 ), col=rangi2, lwd=2)
    text(x + 0.5, (y1 +y2)/2 + 0.05, d$dept[x], cex=0.9, col = rangi2)
}


## now let's do this across departments

## No gender consideration

d$dept_id <- coerce_index(d$dept)

m10.8 <-
    map(
        alist(
            admit ~ dbinom(applications, p),
            logit(p) <- a[dept_id],
            a[dept_id] ~ dnorm(0,10)),
        data=d)

precis(m10.8)

m10.9 <- map(
    alist(
        admit ~ dbinom(applications, p),
        logit(p) <- a[dept_id] + bm*male,
        a[dept_id] ~ dnorm(0,10),
        bm ~ dnorm(0,10)),
    data=d)

compare(m10.6, m10.7, m10.8, m10.9)

precis(m10.9, depth=2)

m10.9stan <- map2stan(m10.9, chains=2, iter=2500, warmup=500)
precis(m10.9stan, depth=2)
