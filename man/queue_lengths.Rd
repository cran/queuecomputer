% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr_plugins.R
\name{queue_lengths}
\alias{queue_lengths}
\title{Compute queue lengths from arrival, service and departure data}
\usage{
queue_lengths(arrivals, service = 0, departures, epsilon = 1e-10, ...)
}
\arguments{
\item{arrivals}{vector of arrival times}

\item{service}{vector of service times. Leave as zero if you want to compute the number of customers in the system rather than queue length.}

\item{departures}{vector of departure times}

\item{epsilon}{numeric small number added to departures to prevent negative queue lengths}

\item{...}{additional arguments - does nothing, for compatibility}
}
\description{
Compute queue lengths from arrival, service and departure data
}
\examples{
library(dplyr)
library(queuecomputer)

set.seed(1L)
n_customers <- 100

queueoutput_df <- data.frame(
  arrivals = runif(n_customers, 0, 300),
  service = rexp(n_customers)
)

queueoutput_df <- queueoutput_df \%>\% mutate(
  departures = queue(arrivals, service, servers = 2)
)

queue_lengths(
  queueoutput_df$arrivals,
  queueoutput_df$service,
  queueoutput_df$departures
)

# The dplyr way
queueoutput_df \%>\% do(
  queue_lengths(.$arrivals, .$service, .$departures))

n_customers <- 1000

queueoutput_df <- data.frame(
  arrivals = runif(n_customers, 0, 300),
  service = rexp(n_customers),
  route = sample(c("a", "b"), n_customers, TRUE)
)

server_df <- data.frame(
  route = c("a", "b"),
  servers = c(2, 3)
)

output <- queueoutput_df \%>\%
  left_join(server_df) \%>\%
  group_by(route) \%>\%
  mutate(
    departures = queue(arrivals, service, servers = servers[1])
  ) \%>\%
  do(queue_lengths(.$arrivals, .$service, .$departures))


if(require(ggplot2, quietly = TRUE)){
    ggplot(output) +
      aes(x = times, y = queuelength) + geom_step() +
      facet_grid(~route)
}
}
