library(rethinking)
library(MASS)
set.seed(5)
a <- 3.5        ## average morning wait time             
b <- (-1)       ## average difference afternoon wait time
sigma_a <- 1    ## std dev in intercepts                 
sigma_b <- 0.5  ## std dev in slopes                     
rho <- (-0.7)   ## correlation between intercepts and slopes

Mu  <- c(a, b)

## Build the covariance matrix
sigmas <- c(sigma_a, sigma_b)
Rho <- matrix(c(1, rho, rho, 1), nrow = 2)

Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

N_cafes <- 20
vary_effects <- mvrnorm(N_cafes, Mu, Sigma)

a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

plot(a_cafe, b_cafe, col = rangi2,
     xlab = "intercepts (a_cafe)", ylab = "slopes (b_cafe)")
## overlay population distribution
library(ellipse)
for (l in c(0.1, 0.3, 0.5, 0.7, 0.8, 0.99)){
    lines(ellipse(Sigma, centre = Mu, level = l),
          col = col.alpha("black", 0.2))
}

N_visits <- 10
afternoon <- rep(0:1, N_visits * N_cafes / 2)
cafe_id <- rep(1:N_cafes, each = N_visits)
mu <- a_cafe[cafe_id] + b_cafe[cafe_id] * afternoon
sigma <- 0.5 ## standard deviation within cafes
wait <- rnorm(N_visits * N_cafes, mu, sigma)
d <- data.frame(cafe = cafe_id, afternoon = afternoon, wait = wait)

m13.1 <-
    map2stan(
        alist(
            wait ~ dnorm(mu, sigma),
            mu <- a_cafe[cafe] + b_cafe[cafe] * afternoon,
            c(a_cafe, b_cafe)[cafe] ~ dmvnorm2(c(a, b), sigma_cafe, R),
            a ~ dnorm(0, 10),
            b ~ dnorm(0, 10),
            sigma ~ dcauchy(0, 2),
            sigma_cafe ~ dcauchy(0, 2),
            R ~ dlkjcorr(2)),
        data = d,
        iter = 5000, warmup = 2000, chains = 2)

post <- extract.samples(m13.1)
dens(post$R[,1,2])
