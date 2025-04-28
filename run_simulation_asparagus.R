source("asparagus/simulation_asparagus.R")

####Input data####
scenario_today<-read.csv("asparagus/asparagus_today.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_245<-read.csv("asparagus/asparagus_245.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_370<-read.csv("asparagus/asparagus_370.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_585<-read.csv("asparagus/asparagus_585.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")



####Simulation run: today####
sim_today<-mcSimulation(estimate = as.estimate(scenario_today),
                        model_function = asparagus_sim,
                        numberOfModelRuns = 10000,
                        functionSyntax = "plainNames")

sim_today$x$quality_loss<-sim_today$y$quality_loss.quality_loss
sim_today$x$growth_potential<-sim_today$y$growth_pot

saveRDS(sim_today, "asparagus/MC_results/MC_results_today.RDS")
write.csv(sim_today, "asparagus/MC_results/MC_results_today.csv")

####Simulation run: ssp245####
sim_245<-mcSimulation(estimate = as.estimate(scenario_245),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames")

sim_245$x$quality_loss<-sim_245$y$quality_loss.quality_loss
sim_245$x$growth_potential<-sim_245$y$growth_pot

saveRDS(sim_245, "asparagus/MC_results/MC_results_245.RDS")
write.csv(sim_245, "asparagus/MC_results/MC_results_245.csv")

####Simulation run: ssp370####
sim_370<-mcSimulation(estimate = as.estimate(scenario_370),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames")

sim_370$x$quality_loss<-sim_370$y$quality_loss.quality_loss
sim_370$x$growth_potential<-sim_370$y$growth_pot

saveRDS(sim_370, "asparagus/MC_results/MC_results_370.RDS")
write.csv(sim_370, "asparagus/MC_results/MC_results_370.csv")

####Simulation run: ssp585####
sim_585<-mcSimulation(estimate = as.estimate(scenario_585),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames")

sim_585$x$quality_loss<-sim_585$y$quality_loss.quality_loss
sim_585$x$growth_potential<-sim_585$y$growth_pot

saveRDS(sim_585, "asparagus/MC_results/MC_results_585.RDS")
write.csv(sim_585, "asparagus/MC_results/MC_results_585.csv")
