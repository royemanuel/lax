library(rethinking)
data(islandsDistMatrix)

## display short column names, so fits on screen
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml", "Ti", "SC", "Ya", "Fi", "Tr", "Ch", "Mn", "To", "Ha")
round(Dmat, 1)


## Linear reduction with distance
curve( exp(-1 * x), from = 0, to = 4, lty = 2,
      xlab = "distance", ylab = "correlation")

## Square reduction with distance
curve(exp(-1 * x^2), add = TRUE)

data(Kline2)
d <- Kline2
d$society <- 1:10

m13.7 <-
    map2stan(
        alist(
            total_tools ~ dpois(lambda),
            log(lambda) <- a + g[society] + bp * logpop,
            g[society] ~ GPL2(Dmat, etasq, rhosq, 0.01),
            a ~ dnorm(0,10),
            bp ~ dnorm(0, 1),
            etasq ~ dcauchy(0, 1),
            rhosq ~ dcauchy(0, 1)),
        data = list(
            total_tools = d$total_tools,
            logpop = d$logpop,
            society = d$society,
            Dmat = islandsDistMatrix),
        warmup = 2000, iter = 1e4, chains = 4)
            
precis(m13.7, depth = 2)

post <- extract.samples(m13.7)

## plot the posterior median covariance function
curve(median(post$etasq) * exp(-median(post$rhosq) * x^2),
      from = 0, to = 10,
      xlab = "distance (thousand km)", ylab = "covariance",
      ylim = c(0, 1), yaxp = c(0, 1, 4), lwd = 2)

## plot 100 functions sampled from the posterior
for(i in 1:100) {
    curve(post$etasq[i] * exp(-post$rhosq[i] * x^2), add = TRUE,
          col = col.alpha("black", 0.2))
}


## push the parameters back through the function for K, the covariance matrix:
K <- matrix(0, nrow = 10, ncol = 10)
for (i in 1:10) {
    for (j in 1:10) {
        K[i, j] <- median(post$etasq) *
                          exp(-median(post$rhosq) * islandsDistMatrix[i, j]^2)
    }
}

diag(K) <- median(post$etasq) + 0.01

## convert K to a correlation matrix
Rho <- round(cov2cor(K), 2)

## add row/col names for convenience
colnames(Rho) <- c("Ml", "Ti", "SC", "Ya", "Fi", "Tr", "Ch", "Mn", "To", "Ha")
rownames(Rho) <- colnames(Rho)
Rho

## scale point size to logpop
psize <- d$logpop/ max(d$logpop)
psize <- exp(psize * 1.5) - 2

## plot raw data and labels
plot(d$lon2, d$lat, xlab = "longitude", ylab = "latitude",
     col = rangi2, cex = psize, pch = 16, xlim = c(-50, 30))
labels <- as.character(d$culture)
text(d$lon2, d$lat, labels = labels, cex = 0.7, pos = c(2, 4, 3, 3, 4, 1, 3, 2, 4, 2))

## overlay lines shaded by Rho
for(i in 1:10) {
    for (j in 1:10) {
        if (i < j) {
            lines(c(d$lon2[i], d$lon2[j]),
                  c(d$lat[i], d$lat[j]),
                  lwd =2, col = col.alpha("black", Rho[i,j]^2))
        }
    }
}

## compute posterior median relationship, ignoring distance
logpop.seq <- seq(from = 6, to = 14, length.out = 30)
lambda <- sapply(logpop.seq, function(lp) exp(post$a  + post$bp * lp))
lambda.median <- apply(lambda, 2, median)
lambda.PI80 <- apply(lambda, 2, PI, prob = 0.8)

## plot raw data and labels
plot(d$logpop, d$total_tools, col = rangi2, cex = psize, pch = 16,
     xlab = "log population", ylab = "total tools")
text(d$logpop, d$total_tools, labels = labels, cex = 0.7,
     pos = c(4, 3, 4, 2, 2, 1, 4, 4, 4, 2))

## display posterior predictions

lines(logpop.seq, lambda.median, lty = 2)
lines(logpop.seq, lambda.PI80[1,], lty = 2)
lines(logpop.seq, lambda.PI80[2,], lty = 2)

## overlay correlations
for(i in 1:10) {
    for (j in 1:10) {
        if (i < j) {
            lines(c(d$logpop[i], d$logpop[j]),
                  c(d$total_tools[i], d$total_tools[j]),
                  lwd =2, col = col.alpha("black", Rho[i,j]^2))
        }
    }
}
