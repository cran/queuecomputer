## ------------------------------------------------------------------------
library(queuecomputer)

arrivals <- data.frame(ID = c(1:100), times = cumsum(rexp(100)))

head(arrivals)

service <- rexp(100)

departures <- queue_step(arrival_df = arrivals, service = service)

head(departures)

## ------------------------------------------------------------------------
# Zero servers available before time 10
# One server available between time 10 and time 50
# Three servers available between time 50 and time 100
# One server available from time 100 onwards
resource_schedule <- as.server.stepfun(c(10,50,100), c(0, 1, 3, 1))

resource_schedule

departures <- queue_step(arrival_df = arrivals, service = service, servers = resource_schedule)

head(departures)

## ------------------------------------------------------------------------
# Server 1 is available before time 10.
# Server 2 is available between time 15 and time 30.
# Server 3 is available after time 10. 
as.server.list(list(10, c(15,30), 10), c(1,0,0))

## ------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

set.seed(500)

arrivals <- data.frame(ID = c(1:100), times = cumsum(rexp(100)))
service_l <- rexp(100, 0.8)
service_q <- rexp(100, 0.5)
arrivals_b <- data.frame(ID = c(1:100), times = cumsum(rexp(100, 0.8)))

# The queue elements can be computed one by one. 

departures_1 <- lag_step(arrivals, service_l)
departures_2 <- queue_step(departures_1, service = service_q, servers = 2)
departures_3 <- wait_step(departures_2, arrivals_b$times)

departure_df <- data.frame(arrival_times = arrivals$times, departures_1 = departures_1$times, departures_2 = departures_2$times, departures_3 = departures_3$times) %>% reshape2::melt()

qplot(value, data = departure_df, colour = variable, geom = "density") + xlab("time")

# The queue elements can be chained together with the %>% operator. 

departures <- lag_step(arrivals, service_l) %>% queue_step(service = service_q, servers = 2) %>% wait_step(arrivals_b$times)

all(departures$times == departures_3$times)


