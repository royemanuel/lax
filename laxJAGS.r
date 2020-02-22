library("rjags")
library("tidyverse")
library("boot")
library("loo")
library("parallel")
library("readxl")
options(mc.cores = detectCores())


laxData <- read_excel("2015-2019 Combined Lacrosse Statistics.xlsx",
                      sheet = "Results",
                      n_max = 353)


## Make the Year and the Team factors
years <- unique(laxData$Year)
teams <- unique(laxData$Team)

tms <- laxData$Team
yrs <- laxData$Year

replaceWithNum <- function(tgtVec, refVec){
    for(i in 1:length(refVec)){
        for(j in 1:length(tgtVec)){
            if(tgtVec[j] == refVec[i]){
                tgtVec[j] <- i
            }
        }
    }
    tgtVec
}

yrs <- replaceWithNum(yrs, years)
tms <- replaceWithNum(tms, teams) 


laxData <- bind_cols(laxData, Year.f = yrs, Team.f = tms) %>%
    mutate(WP = `Win %` / (1 - `Win %`))

y <- laxData$`Win %`
goals <- laxData$`G/Game`
goalsAgainst <- laxData$`GA/Game`
year <- laxData$Year.f
numY <- length(year)
team <- laxData$Team.f
numT <- length(team)
WP <- laxData$WP


laxlist <-
    list(WP = WP,
         goals = goals,
         goalsAgainst = goalsAgainst,
         year = year,
         team = team,
         numY = numY,
         numT = numT,
         n = dim(laxData)[1])
         
         
         

lax <- "model {
    for(i in 1:n){
        OWP[i] ~ dbeta((pBar[i] * theta) , ((1 - pBar[i]) * theta))
        pBar[i] = ilogit(a0 + aG[i] * goals[i])    
        aG[i] ~ dnorm(aG0, 1/100)
    }
    aG0 ~ dnorm(0, 1/100)
    a0 ~ dnorm(0, 1/100)
    theta ~ dexp(1)
}"

writeLines(lax, "lax.txt")

laxmod <- jags.model("lax.txt",
                     data = laxlist,
                     n.chains = 4)
update(laxmod, n.iter = 1000)
samplesLax <- coda.samples(laxmod,
                           variable.names = c("a0",  "theta", "aG0"),
                           n.iter = 5000)



    
            

    

