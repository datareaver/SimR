
# Run Single Queue Simulation ----------------------------------------------------------


# devtools::install_github("Bart6114/simmer")
library(simmer)
set.seed(103)
rm(list=ls())

u <- paste0("rexp(1,",server_rate,")")

path <- create_trajectory("Path") %>%
  add_seize_event("server",1.0) %>%
  add_timeout_event(u) %>%
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

results %>% group_by(Hour) %>% summarize(Mean = mean(waiting_time))

mean(results$waiting_time)+(-1:1)*sd(results$waiting_time)*qt(.975,length(results$waiting_time))/sqrt(length(results$waiting_time))

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
#     filter(Arrival_Time < period)
# }

create_poisson_arrivals <- function(poisson_parameter=1,period=60) {
  Entity <- 0:rpois(1,poisson_parameter)
  data.frame(Entity,Interarrival=rexp(length(Entity),1/poisson_parameter)) %>%
    mutate(Interval_Arrival_Time=cumsum(Interarrival)) %>%
    filter(Interval_Arrival_Time < period,Entity > 0)
}

create_poisson_arrivals(1)
create_poisson_arrivals(load)

load <- read.table('C:/Users/m097845/Google Drive/Scripts/Staff.Scheduling.Tool/load.csv',sep=",",
                   header=FALSE,row.names=paste0('t',0:167),col.names="Value") %>%
  apply(2,as.list)


arrivals <- lapply(load[['Value']],create_poisson_arrivals) %>%
  do.call(what="rbind")

Time_ID <- as.numeric(gsub("t","",sapply(strsplit(row.names(arrivals),"\\."),getElement,1)))*60

arrivals <- data.frame(Time_ID,arrivals) %>%
  mutate(Arrival_Time = Time_ID+Interval_Arrival_Time)

