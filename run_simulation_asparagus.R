source("asparagus/simulation_asparagus.R")
source("functions/various_functions.R")

library(decisionSupport)

####Input data####
scenario_today<-read.csv("asparagus/asparagus_today.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_245<-read.csv("asparagus/asparagus_245.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_370<-read.csv("asparagus/asparagus_370.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_585<-read.csv("asparagus/asparagus_585.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
risk_df<-read.csv("weathergenerator/risk_df.csv")

outputs<-c("water_stress_risk",
           "insect_risk",
           "disease_risk",
           "pad",
           "weather_damage_risk",
           "growth_start_day",
           "days_till_harvest",
           "actual_chill",
           "late_frost_risk",
           "temp_fluctuation_risk",
           "extreme_rainfall_risk",
           "extreme_heat_risk")


####Simulation run: today####
sim_today<-mcSimulation(estimate = as.estimate(scenario_today),
                        model_function = asparagus_sim,
                        numberOfModelRuns = 10000,
                        functionSyntax = "plainNames",
                        risk_df)


sim_today_output<-youtputs_to_xinputs(sim_today, outputs)

saveRDS(sim_today_output, "asparagus/MC_results/MC_results_today.RDS")
write.csv(sim_today_output, "asparagus/MC_results/MC_results_today.csv")

####Simulation run: ssp245####
sim_245<-mcSimulation(estimate = as.estimate(scenario_245),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames",
                      risk_df)

sim_245_output<-youtputs_to_xinputs(sim_245, outputs)

saveRDS(sim_245_output, "asparagus/MC_results/MC_results_245.RDS")
write.csv(sim_245_output, "asparagus/MC_results/MC_results_245.csv")

####Simulation run: ssp370####
sim_370<-mcSimulation(estimate = as.estimate(scenario_370),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames",
                      risk_df)

sim_370_output<-youtputs_to_xinputs(sim_370, outputs)

saveRDS(sim_370_output, "asparagus/MC_results/MC_results_370.RDS")
write.csv(sim_370_output, "asparagus/MC_results/MC_results_370.csv")

####Simulation run: ssp585####
sim_585<-mcSimulation(estimate = as.estimate(scenario_585),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames",
                      risk_df)

sim_585_output<-youtputs_to_xinputs(sim_585, outputs)

saveRDS(sim_585_output, "asparagus/MC_results/MC_results_585.RDS")
write.csv(sim_585_output, "asparagus/MC_results/MC_results_585.csv")
