library("rethinking")
data(Trolley)
d <- Trolley

simplehist(d$response, xlim=c(1,7), xlab="response")
## discrete proportion of each response value
pr_k <- table(d$response) / nrow(d)
##cumsum converts to cumulative proportions
cum_pr_k <- cumsum(pr_k)


## plot
plot(1:7, cum_pr_k, type="b", xlab="response", ylab="cumulative proportion", ylim=c(0,1))

logit <- function(x) log(x / (1-x))
(lco <- logit(cum_pr_k))
plot(1:7, lco, type="b", xlab="response", ylab="log-cumulative-odds", ylim=c(-2,2))
