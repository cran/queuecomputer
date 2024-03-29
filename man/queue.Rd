% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_queuefunctions.R
\name{queue}
\alias{queue}
\title{Compute the departure times for a set of customers in a queue from their arrival and service times.}
\usage{
queue(arrivals, service, servers = 1, serveroutput = FALSE)
}
\arguments{
\item{arrivals}{numeric vector of non-negative arrival times}

\item{service}{numeric vector of non-negative service times}

\item{servers}{a non-zero natural number, an object of class \code{server.stepfun}
or an object of class \code{server.list}.}

\item{serveroutput}{boolean whether the server used by each customer should be returned.}
}
\description{
\code{queue} is a faster version of \code{queue_step} but the input returned is much simpler. It is not compatible with the \code{summary.queue_list} method or the \code{plot.queue_list} method.
}
\details{
If the arrival vector is out of order the function will reorder it. The same reordering will be applied to the service vector, this is so each customer keeps their service time. Once the queue is computed the original order is put back.
}
\examples{
n <- 1e2
arrivals <- cumsum(rexp(n, 1.8))
service <- rexp(n)

departures <- queue(
    arrivals, service, servers = 2)

head(departures)
curve(ecdf(departures)(x) * n,
    from = 0, to = max(departures),
    xlab = "Time", ylab = "Number of customers")
curve(ecdf(arrivals)(x) * n,
    from = 0, to = max(departures),
    col = "red", add = TRUE)

}
\seealso{
\code{\link{queue_step}}
}
