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
    mutate(OWP = `Win %` / (1 - `Win %`))

y <- laxData$`Win %`
goals <- laxData$`G/Game`
goalsAgainst <- laxData$`GA/Game`
year <- laxData$Year.f
numY <- length(year)
team <- laxData$Team.f
numT <- length(team)
OWP <- laxData(OWP)


laxlist <-
    list(OWP = OWP,
         goals = goals,
         goalsAgainst = goalsAgainst,
         year = year,
         team = team,
         numY = numY,
         numT = numT,
         n = dim(laxData)[1])
         
         
         

laxmod <- model {
    for(i in 1:n){
        OWP[i] ~ dnorm(mu[year[i], team[i]], sigma)
    }
    for(j in 1:numY){
        for(k in 1:numT){
            mu[j,k] = a0 + aG * goals
    

