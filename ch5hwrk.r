library(tidyverse)
library(rethinking)
## 5E2 Write down a multiple regression
## Da ~ dnorm(mu, sigma)
## mu <- a + bP * plant.diversity + bL * latitude

## 5E3
## tPHD ~ dnorm(mu, sigma)
## mu <- a + bL * size.lab + bF * funding
## both bL and bF > 0

N <- 100
x1 <- rnorm(100,1,3)
y <- rnorm(100, 1, 1) * x1
x2 <- x1
d <- data.frame(y, x1, x2)
mh1<-
    map(
        alist(
            y ~ dnorm(mu, sigma),
            mu <- a + b1 * x1,
            a ~ dnorm(1,10),
            b1 ~ dnorm(1,10),
            sigma ~ dunif(0,50)),
        data = d)
mh2<-
    map(
        alist(
            y ~ dnorm(mu, sigma),
            mu <- a + b2 * x2,
            a ~ dnorm(1,10),
            b2 ~ dnorm(1,10),
            sigma ~ dunif(0,50)),
        data = d)
mh3 <-
    map(
        alist(
            y ~ dnorm(mu, sigma),
            mu <- a + b1 * x1 + b2 * x2,
            a ~ dnorm(1,10),
            b1 ~ dnorm(1,10),
            b2 ~ dnorm(1,10),
            sigma ~ dunif(0,50)),
        data = d)

## Divorce rate may cause a higher marriage rate because more couples
## are available if they are divorcing. Individuals can have more than
## wone marriage which is more

## Marriage rate ~ norm(mu, sigma)
## mu <- a + bDR * divorce.rate + bMA * median.age

library(XML)
mormon <- "https://www.worldatlas.com/articles/mormon-population-by-state.html"

mormon.tb <- readHTMLTable(mormon, header=T, which=1, stringsAsFactors=FALSE)
library(rvest)

mm <-read_html(mormon)  %>%
    html_node("#artReg-table :nth-child(1)") %>%
    html_table(.) %>%
    mutate(EMP = as.numeric(gsub(",", "", `Estimated Mormon Population`)),
           Location = `<U+FEFF>State`)


data(WaffleDivorce)
d <- WaffleDivorce

## standardize predictor
d <-
    d %>%
    mutate(MedianAgeMarriage.s = (MedianAgeMarriage - mean(MedianAgeMarriage))/
               sd(MedianAgeMarriage))

head(d)

d <-
    left_join(x = d, y = mm, by = "Location") %>%
    mutate(mormon.std = (EMP - mean(EMP)) / sd(EMP))
