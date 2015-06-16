
# Run Single Queue Simulation ----------------------------------------------------------


# devtools::install_github("Bart6114/simmer")
library(simmer)
set.seed(103)
rm(list=ls())

path <- create_trajectory("Path") %>%
  add_seize_event("server",1.0) %>%
  add_timeout_event("rexp(1,3)") %>%
  add_release_event("server",1.0)

arrivals <- list(Hour=1:168,Load = runif(168,min=3,max=6))

loop_simulations <- function(Hour,Load) {

  sim <- create_simulator(sim_name = "Single Queue",n=5,until = 60) %>%
    add_resource(name="server",1)

  poisson_rate <- Load

  for (n in rpois(sim@n,poisson_rate)) {
    arrival_time <- rexp(1,1/poisson_rate)
    for (i in 1:(n+1)) {
      add_entity(sim,path,name=paste0("Customer_",i),activation_time = arrival_time)
      arrival_time <- arrival_time + rexp(1,1/poisson_rate)
    }

  }
  #print(row.names(poisson_rate))
  simmer(sim)
  data.frame(get_entity_monitor_values(sim, aggregated = TRUE),Hour)
}


results <- mapply(loop_simulations,arrivals$Hour,arrivals$Load,SIMPLIFY = FALSE) %>% rbind_all()

sdresults %>% group_by(Hour) %>% summarize(mean(waiting_time))

mean(results$waiting_time)+sd(results$waiting_time)*1.96/sqrt(length(results$waiting_time))

group_by(

# View(results %>% group_by(Hour,replication) %>% summarize(mean(waiting_time)))
# View(results %>% mutate(start_time+end_time,flow_time-activity_time))
# get_resource_serve_mon_values(sim,"server")
# get_resource_system_mon_values(sim,"server")
# get_entity_monitor_values(sim,"server")


# test --------------------------------------------------------------------

# create_poisson_arrivals <- function(poisson_parameter=1,reps=1,period=60) {
#   entity_per_rep <- unlist(lapply(1:reps,function(x) rep(x,rpois(1,poisson_parameter))))
#   data.frame(Replication=entity_per_rep,Interarrival=rexp(length(entity_per_rep),(1/poisson_parameter))) %>%
#     group_by(Replication) %>%
#     mutate(Arrival_Time=cumsum(Interarrival)) %>%
#     filter(Arrival_Time < period) %>%
#     ungroup()
# }

arrivals <- create_poisson_arrivals(poisson_parameter=10,reps=10,period=60)

for (i in

# for (arrival in arrivals) {
#
#   rm(sim)
#   sim <- create_simulator(sim_name = "Single Queue",n=1000,until = 60) %>%
#     add_resource(name="server",1)
#
#   rep <- 1
#   poisson_rate <- arrival
#
#   for (n in rpois(sim@n,poisson_rate)) {
#     arrival_time <- rexp(1,1/poisson_rate)
#     for (i in 1:(n+1)) {
#       add_entity(sim,path,name=paste0("Customer_",i,rep),activation_time = arrival_time)
#       arrival_time <- arrival_time + rexp(1,1/poisson_rate)
#     }
#     rep <- rep+1
#   }
#   simmer(sim)
#
#   results <- data.frame(get_entity_monitor_values(sim, aggregated = TRUE),rep)
#   results_df <- rbind
#
# }