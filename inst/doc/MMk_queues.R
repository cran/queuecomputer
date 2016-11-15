## ------------------------------------------------------------------------
library(queuecomputer)

## ------------------------------------------------------------------------

P_0_func <- function(rho, k){
  sum_i <- rep(NA, k)
  
  for(i in 0:I(k-1))
  {
    sum_i[i+1] <- rho^i / factorial(i)
  }
  
  p_0 <- 1/(sum(sum_i) + rho^k/(factorial(k - 1) * (k - rho)))
  return(p_0)
}

P_n <- function(rho,n,k){
  
  p_0 <- P_0_func(rho, k)
  if(n <= k){
    output <- rho^n / factorial(n) * p_0
  } else {
    output <- rho^n / (factorial(k) * k^(n-k)) * p_0
  }
  return(output)
}

Lq <- function(rho, k){
  p_0 <- P_0_func(rho, k)
  
  output <- p_0 * rho^{k+1} / ( factorial(k-1) * (k - rho)^2)
  return(output)
}


## ------------------------------------------------------------------------

set.seed(1)

n_customers <- 250000

lambda_a <- 1/1
lambda_s <- 1/0.8

interarrivals <- rexp(n_customers, lambda_a)

arrivals <- cumsum(interarrivals)
arrival_df <- data.frame(ID = c(1:n_customers), times = arrivals)

service <- rexp(n_customers, lambda_s)

rho <- (1/lambda_s) / (1/lambda_a)

## ------------------------------------------------------------------------

k = 1

p_0 <- P_n(rho, n=0, k)

### System lengths -----------------------
Vectorize(P_n, "n")(rho=rho, n=c(0:30), k = k)

### Estimated queue length -----------------
LQ <- Lq(rho, k)

### Estimated units in system -----------
Lq(rho, k) + rho

Ws = 1/lambda_s
Wq = LQ / lambda_a
W = Ws + Wq

Wq # Mean waiting time (time in queue)
W # Mean response time (time in system)


## ------------------------------------------------------------------------

MM1 <- queue_step(arrival_df = arrival_df, service = service, servers = k)

MM1_summary <- summary(MM1)

signif(MM1_summary$system_lengths, 4)

MM1_summary$queue_lengths %*% c(0:I(length(MM1_summary$queue_lengths) - 1)) # Mean queue length
MM1_summary$system_lengths %*% c(0:I(length(MM1_summary$system_lengths) - 1)) # Mean system length (number of customers in system)

MM1_summary$mwt # Mean waiting time
MM1_summary$mrt # Mean response time

## ------------------------------------------------------------------------

k = 3

p_0 <- P_n(rho, 0, k)

### System lengths -----------------------
Vectorize(P_n, "n")(rho=rho, n=c(0:30), k = k)

### Estimated queue length -----------------
LQ <- Lq(rho, k)

### Estimated units in system -----------
Lq(rho, k) + rho

Ws = 1/lambda_s
Wq = LQ / lambda_a
W = Ws + Wq

Wq # Mean waiting time (time in queue)
W # Mean response time (time in system)


## ------------------------------------------------------------------------


MM3 <- queue_step(arrival_df = arrival_df, service = service, servers = k)

MM3_summary <- summary(MM3)

signif(MM3_summary$system_lengths, 4)

MM3_summary$queue_lengths %*% c(0:I(length(MM3_summary$queue_lengths) - 1)) # Mean queue length
MM3_summary$system_lengths %*% c(0:I(length(MM3_summary$system_lengths) - 1)) # Mean system length (number of customers in system)

MM3_summary$mwt # Mean waiting time
MM3_summary$mrt # Mean response time


## ------------------------------------------------------------------------

# Setup ----------

set.seed(2)

n_customers <- 250000

lambda_a <- 1/1
lambda_s <- 1/2.5

interarrivals <- rexp(n_customers, lambda_a)

arrivals <- cumsum(interarrivals)
arrival_df <- data.frame(ID = c(1:n_customers), times = arrivals)

service <- rexp(n_customers, lambda_s)


## ------------------------------------------------------------------------

rho <- (1/lambda_s) / (1/lambda_a)

# MM3 queue ------------------------------

k = 3

## Theoretical -------------------

p_0 <- P_n(rho, 0, k)

### System lengths -----------------------
Vectorize(P_n, "n")(rho=rho, n=c(0:30), k = k)

### Estimated queue length -----------------
LQ <- Lq(rho, k)

### Estimated units in system -----------
Lq(rho, k) + rho

### Waiting times -----------
Ws = 1/lambda_s
Wq = LQ / lambda_a
W = Ws + Wq

Wq # Mean waiting time (time in queue)
W # Mean response time (time in system)


## ------------------------------------------------------------------------


MM3_2 <- queue_step(arrival_df = arrival_df, service = service, servers = k)

MM3_2_summary <- summary(MM3_2)

signif(MM3_2_summary$system_lengths, 4)

MM3_2_summary$queue_lengths %*% c(0:I(length(MM3_2_summary$queue_lengths) - 1)) # Mean queue length
MM3_2_summary$system_lengths %*% c(0:I(length(MM3_2_summary$system_lengths) - 1)) # Mean system length (number of customers in system)

MM3_2_summary$mwt # Mean waiting time
MM3_2_summary$mrt # Mean response time

