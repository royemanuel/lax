library(rethinking)
library(tidyverse)
library(readxl)
laxData <- read_excel("2015-2019 Combined Lacrosse Statistics.xlsx",
                      sheet = "Results",
                      n_max = 353)

winView <- ggplot(laxData, aes(`GA/Game`, `G/Game`))  +
    geom_point(size=10*laxData$`Win %` + 0.5)

stdz <- function(x){
    y <- (x - mean(x))/sd(x)
}

laxData  <-
    laxData %>%
    mutate(
           G.s   = stdz(`G/Game`),
           GA.s  = stdz(`GA/Game`),
           SP.s  = stdz(`Shooting %`),
           FO.s  = stdz(`Faceoff %`),
           EO.s  = stdz(`EMO %`),
           ED.s  = stdz(`EMD %`),
           CP.s  = stdz(`Clearing %`),
           TO.s  = stdz(`TO/Game`),
           CTO.s = stdz(`CTO/Game`),
           WinPer = stdz(`Win %`)) 
    

mLax <-
    map(
        alist(
            WinPer ~ dnorm(mu, sigma),
            mu  <- a +
                bG  * G.s  +
                bGA * GA.s +
                bSP * SP.s +
                bFO * FO.s +
                bEO * EO.s + 
                bED * ED.s + 
                bCP * CP.s + 
                bTO * TO.s + 
                bCTO*CTO.s,
            a ~ dnorm(0, 10) ,
            bG   ~ dnorm(0, 10),
            bGA  ~ dnorm(0, 10),
            bSP  ~ dnorm(0, 10),
            bFO  ~ dnorm(0, 10),
            bEO  ~ dnorm(0, 10),
            bED  ~ dnorm(0, 10),
            bCP  ~ dnorm(0, 10),
            bTO  ~ dnorm(0, 10),
            bCTO ~ dnorm(0, 10),
            sigma ~ dunif(0, 50)),
        data = data.frame(laxData))

plot(precis(mLax))
mtext(concat("Win Percentage and all parameters"))

mLaxW <- 
    map(
        alist(
            WinPer ~ dnorm(mu, sigma),
            mu  <- a +
                bG  * G.s  +
                bGA * GA.s,
            a ~ dnorm(0, 10) ,
            bG   ~ dnorm(0, 10),
            bGA  ~ dnorm(0, 10),
            sigma ~ dunif(0, 50)),
        data = data.frame(laxData))

plot(precis(mLaxW))
mtext(concat("Win Percentage with Goals and Goals Against"))

mu <- link(mLaxW)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
laxW.sim <- sim(mLaxW, n = 1e4)
laxW.PI <- apply(laxW.sim, 2, PI)

plot(mu.mean ~ laxData$WinPer, col=rangi2, ylim=range(mu.PI),
     xaxt="n", yaxt = "n", xlab = "Actual Win Percentage", ylab = "Predicted Win Percentage")
abline(a=0, b=1, lty=2)
at <- c(-5,-3, -2, -1, 0, 1, 2, 3, 5)
xlabels <- at*sd(laxData$`Win %`) + mean(laxData$`Win %`)
ylabels <- at*sd(laxData$`Win %`) + mean(laxData$`Win %`)
axis(side=1, at=at, labels = round(xlabels,1))
axis(side=2, at=at, labels = round(ylabels,1))
for(i in 1:nrow(laxData))
    lines(rep(laxData$WinPer[i],2), c(mu.PI[1,i], mu.PI[2,i]),
          col=rangi2)


mLaxG <-
    map(
        alist(
            G.s ~ dnorm(mu,sigma),
            mu <- a + 
                bSP * SP.s +
                bFO * FO.s +
                bEO * EO.s + 
                bED * ED.s + 
                bCP * CP.s + 
                bTO * TO.s + 
                bCTO*CTO.s,
            a ~ dnorm(0,10),
            bSP  ~ dnorm(0, 10),
            bFO  ~ dnorm(0, 10),
            bEO  ~ dnorm(0, 10),
            bED  ~ dnorm(0, 10),
            bCP  ~ dnorm(0, 10),
            bTO  ~ dnorm(0, 10),
            bCTO ~ dnorm(0, 10),
            sigma ~ dunif(0, 50)),
        data = data.frame(laxData))

plot(precis(mLaxG))
mtext(concat("Goals/Game and all parameters"))

mu <- link(mLaxG)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
laxG.sim <- sim(mLaxG, n = 1e4)
laxG.PI <- apply(laxG.sim, 2, PI)

plot(mu.mean ~ laxData$G.s, col=rangi2, ylim=range(mu.PI),
     xaxt="n", yaxt = "n", xlab = "Actual Goals/Game", ylab = "Predicted Goals/Game")
abline(a=0, b=1, lty=2)
at <- c(-5,-3, -2, -1, 0, 1, 2, 3, 5)
xlabels <- at*sd(laxData$`G/Game`) + mean(laxData$`G/Game`)
ylabels <- at*sd(laxData$`G/Game`) + mean(laxData$`G/Game`)
axis(side=1, at=at, labels = round(xlabels,1))
axis(side=2, at=at, labels = round(ylabels,1))
for(i in 1:nrow(laxData))
    lines(rep(laxData$G.s[i],2), c(mu.PI[1,i], mu.PI[2,i]),
          col=rangi2)

mLaxGA <-
    map(
        alist(
            GA.s ~ dnorm(mu,sigma),
            mu <- a + 
                bSP * SP.s +
                bFO * FO.s +
                bEO * EO.s + 
                bED * ED.s + 
                bCP * CP.s + 
                bTO * TO.s + 
                bCTO*CTO.s,
            a ~ dnorm(0,10),
            bSP  ~ dnorm(0, 10),
            bFO  ~ dnorm(0, 10),
            bEO  ~ dnorm(0, 10),
            bED  ~ dnorm(0, 10),
            bCP  ~ dnorm(0, 10),
            bTO  ~ dnorm(0, 10),
            bCTO ~ dnorm(0, 10),
            sigma ~ dunif(0, 50)),
        data = data.frame(laxData))

plot(precis(mLaxGA))
mtext(concat("Goals Against and all parameters"))

mu <- link(mLaxGA)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
laxGA.sim <- sim(mLaxGA, n = 1e4)
laxGA.PI <- apply(laxGA.sim, 2, PI)

plot(mu.mean ~ laxData$GA.s, col=rangi2, ylim=range(mu.PI),
     xaxt="n", yaxt = "n", xlab = "Actual Goals Against/Game", ylab = "Predicted Goals Against/Game")
abline(a=0, b=1, lty=2)
at <- c(-5,-3, -2, -1, 0, 1, 2, 3, 5)
xlabels <- at*sd(laxData$`GA/Game`) + mean(laxData$`GA/Game`)
ylabels <- at*sd(laxData$`GA/Game`) + mean(laxData$`GA/Game`)
axis(side=1, at=at, labels = round(xlabels,1))
axis(side=2, at=at, labels = round(ylabels,1))
for(i in 1:nrow(laxData))
    lines(rep(laxData$GA.s[i],2), c(mu.PI[1,i], mu.PI[2,i]),
          col=rangi2)
