% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr_plugins.R
\name{average_queue}
\alias{average_queue}
\title{Compute time average queue length}
\usage{
average_queue(times, queuelength)
}
\arguments{
\item{times}{numeric vector of times}

\item{queuelength}{numeric vector of queue lengths}
}
\description{
Compute time average queue length
}
\examples{
n <- 1e3
arrivals <- cumsum(rexp(n))
service <- rexp(n)
departures <- queue(arrivals, service, 1)

queuedata <- queue_lengths(arrivals, service, departures)
average_queue(queuedata$times, queuedata$queuelength)
}
