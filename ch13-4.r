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
 
