

## Testing Functions

reduce_bags <- function(bagdataset, number_of_passengers){
  ID = NULL
  times = NULL

  zerobags <- data.frame(BagID = NA, ID = c(1:number_of_passengers), times = 0)
  reduced_df <- as.data.frame(dplyr::summarise(dplyr::group_by(rbind(bagdataset, zerobags), ID), n = max(times, 0)))
  ord <- order(reduced_df$ID)
  reduced_df <- reduced_df[order(ord),]
  names(reduced_df) <- c("ID", "times")
  return(reduced_df)
}


next_function <- function(sf,time){
  output <- switch(sf(time) + 1, c(stats::knots(sf),Inf)[findInterval(time,stats::knots(sf)) + 1] , time)
  return(output)
}

#' Create a \code{server.stepfun} object with a roster of times and number of available servers.
#'
#' @param x numeric vector giving the times of changes in number of servers.
#' @param y numeric vector one longer than \code{x} giving the number of servers
#' available between x values.
#' @return A \code{list} and \code{server.stepfun} object with x and y as elements.
#' @details This function uses the analogy of a step function to specify the number of
#' available servers throughout the day. It is used as input to the \code{\link{queue_step}}
#' function. Alternatively one may use \code{as.server.list} to specify available servers as
#' a list, however \code{queue_step} is much faster when \code{as.server.stepfun} is used
#' as input rather than \code{as.server.list}.
#' @details If any of the service times are large compared to any element of \code{diff(x)} then the
#' \code{\link{as.server.list}} function should be used.
#' @seealso \code{\link{as.server.list}}, \code{\link{queue_step}}, \code{\link{stepfun}}.
#' @examples
#'
#' servers <- as.server.stepfun(c(15,30,50), c(0, 1, 3, 2))
#' servers
#'
#' @export
as.server.stepfun <- function(x, y){

  stopifnot(all(y%%1 == 0) & all(y >= 0))     # y's are integers
  stopifnot(!is.unsorted(x, strictly = TRUE)) # x's are ordered
  stopifnot(length(x) >= 1)                   # x is not of length zero
  stopifnot(length(y) == length(x) + 1)       # stepfun condition
  stopifnot(sum(y) != 0)                      # There is at least one server
  stopifnot(all(x >= 0))                      # All x's are positive
  stopifnot(all(is.finite(c(x,y))))           # All x's and y's are finite
  stopifnot(is.numeric(c(x,y)))               # All x's and y's are numeric

  output <- list()
  class(output) <- c("server.stepfun", "list")


  output$x <- x
  output$y <- y

  return(output)
}

server_make <- function(x, y){

  stopifnot(all(y%%1 == 0))
  stopifnot(!is.unsorted(x))


  plateaus <- y
  max_plat <- max(plateaus)
  num_plat <- length(plateaus)

  output <- list()

  intermediate_plateaus <- plateaus
  newplat <- matrix(NA,nrow=max_plat, ncol = num_plat)
  for(i in 1:max_plat){
    for(j in 1:num_plat){
      if(intermediate_plateaus[j] == 0){
        newplat[i,j] <- 0
      }else{
        newplat[i,j] <- 1
        intermediate_plateaus[j] <- intermediate_plateaus[j] - 1
      }
    }

    # Fix for issue 1
    newrow <- rep(TRUE, length(newplat[i,]))
    for(k in 2:length(newplat[i,])){
       if(newplat[i,][k-1] == 0 && newplat[i,][k] == 0){
         newrow[k] <- FALSE
       }
    }

    output[[i]] <- stats::stepfun(x[newrow[-1]],newplat[i,][newrow])
  }
  class(output) <- c("server.list", "list")

  return(output)
}

#' Creates a \code{"server.list"} object from a list of times and starting availability.
#'
#' @param times list of numeric vectors giving change times for each server.
#' @param init vector of 1s and 0s with equal length to \code{times}.
#' It represents whether the server starts in an available \code{(1)} or unavailable \code{(0)} state.
#' @return an object of class \code{"server.list"}, which is a list of step functions of range \{0, 1\}.
#' @seealso \code{\link{as.server.stepfun}}, \code{\link{queue_step}}
#' @examples
#' # Create a server.list object with the first server available anytime before time 10,
#' # and the second server available between time 15 and time 30.
#' as.server.list(list(10, c(15,30)), c(1,0))
#' @export
as.server.list <- function(times, init){

  stopifnot("list" %in% class(times))
  stopifnot(all(init %in% c(1,0)))
  stopifnot(length(times) == length(init))

  n <- length(times)

  output <- list()

  for(i in 1:n){
    switch(init[i] + 1,
      y <- rep(c(0,1), length.out = length(times[[i]]) + 1),
      y <- rep(c(1,0), length.out = length(times[[i]]) + 1)
      )
    output[[i]] <- stats::stepfun(times[[i]], y)
  }

  class(output) <- c("list", "server.list")
  return(output)

}





do_func_ignore_things <- function(data, what){
  acceptable_args <- data[names(data) %in% (formals(what) %>% names)]

  do.call(what, acceptable_args %>% as.list)
}



check_queueinput <- function(arrivals, service, departures = NULL){
  stopifnot(all(service >= 0))
  stopifnot(all(arrivals >= 0))
  stopifnot(length(arrivals) == length(service))
  #stopifnot(anyNA(c(arrivals, service)) == FALSE )
  stopifnot(is.numeric(arrivals))
  stopifnot(is.numeric(service))

  if(!is.null(departures)){
    stopifnot(all(departures >= 0))
    stopifnot(length(departures) == length(service))
    #stopifnot(anyNA(departures) == FALSE )
    stopifnot(is.numeric(departures))
  }
}



generate_input <- function(mag = 3, full = FALSE){

  n <- 10^mag
  arrivals <- cumsum(rexp(n, 1.9))
  service <- stats::rexp(n)

  out <- list(arrivals = arrivals, service = service)

  if(full){
    out$departures <- queue(arrivals, service, 2)
  }

  return(out)
}

integrate_stepfun <- function(x, y, last = 1000){
  less_than_last <- which(x <= last)
  stopifnot(c(1:length(less_than_last)) == less_than_last)

  x <- x[less_than_last]
  y <- y[c(less_than_last, utils::tail(less_than_last, 1) + 1)]

  x <- c(0,x,last)
  return((y %*% diff(x)) %>% as.numeric)
}

#' Integrate step function over interval
#' @param x numeric vector giving the times of changes in number of servers.
#' @param y numeric vector one longer than \code{x} giving the number of servers
#' available between x values.
#' @param from start of integration
#' @param to end of intergration
#' @examples
#' x_knots <- c(7.5, 15, 25, 40)
#' y <- c(5, 4, 2, 6, 4)
#'
#' curve(stepfun(x_knots, y)(x), from = 0, to = 100)
#' abline(v = 10, col = "red")
#' abline(v = 30, col = "red")
#'
#' integrate_stepfun_interval(x_knots, y, from = 10, to = 30)
#'
#' # Time-average number of servers available in interval
#' integrate_stepfun_interval(x_knots, y, from = 10, to = 30) / (30 - 10)
#' @noRd
integrate_stepfun_interval <- function(x, y, from = 0, to = 1000){
  stopifnot(from <= to)
  stopifnot(length(x) == (length(y) - 1))

  x_keep <- x
  y_keep <- y

  discard_x_small <- which(x < from)
  if(!is.null(discard_x_small) & length(discard_x_small) != 0){
    x_keep <- x[-discard_x_small]
    y_keep <- y[-discard_x_small]
  }

  discard_x_large <- which(x_keep > to)
  if(!is.null(discard_x_large) & length(discard_x_large) != 0){
    x_keep <- x_keep[-discard_x_large]
    y_keep <- y_keep[-(discard_x_large+1)]
  }

  x <- c(from,x_keep,to)
  x_diff <- diff(x)
  return((y_keep %*% x_diff) %>% as.numeric)
}
