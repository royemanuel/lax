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
    mutate(`Win %` = stdz(`Win %`),
           G.s   = stdz(`G/Game`),
           GA.s  = stdz(`GA/Game`),
           SP.s  = stdz(`Shooting %`),
           FO.s  = stdz(`Faceoff %`),
           EO.s  = stdz(`EMO %`),
           ED.s  = stdz(`EMD %`),
           CP.s  = stdz(`Clearing %`),
           TO.s  = stdz(`TO/Game`),
           CTO.s = stdz(`CTO/Game`),
           WinPer = `Win %`) 
    

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
