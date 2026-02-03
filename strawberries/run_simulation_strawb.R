source("strawberries/R/strawb_sim_scen.R")
source("strawberries/R/youtputs_to_xinputs_scenarios.R")

library(decisionSupport)

set.seed(1847)

#Simulation run with scenarios####
#one run for all scenarios
input<-read.csv("strawberries/data/strawb_input.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
risk_df<-read.csv("weathergenerator/risk_df_strawb.csv")
scenarios<-read.csv("strawberries/data/scenarios.csv")

sim_scenarios<-mcSimulation(estimate = as.estimate(input),
                      model_function = strawb_sim_scen,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames",
                      risk_df,
                      scenarios)

outputs<-c("summer_drought_stress_risk",
           "summer_insect_risk",
           "summer_disease_risk",
           "summer_mean_temp",
           "pad",
           "actual_chill",
           "winter_mean_temp",
           #"spring_start",
           "spring_pollinator",
           "spring_disease_risk",
           "spring_frost_risk",
           "spring_mean_temp",
           #"fruit_start",
           "fruit_frost_risk",
           "fruit_drought_stress_risk",
           "fruit_hail_risk",
           "fruit_sunburn_risk",
           "fruit_mean_temp",
           "fruit_insect_risk",
           "fruit_disease_risk",
           "fruit_snail_risk",
           "T_opt_fruit",
           #"harvest_start",
           "harvest_frost_risk",
           "harvest_heat_risk",
           "harvest_hail_risk",
           "harvest_rain_risk",
           "harvest_fly_risk",
           "harvest_mean_temp")

sim_scenarios_output<-youtputs_to_xinputs_scenarios(sim_scenarios, outputs)
saveRDS(sim_scenarios_output, "strawberries/MC_results/MC_results_scenarios.RDS")
write.csv(sim_scenarios_output, "strawberries/MC_results/MC_results_scenarios.csv")

source("strawberries/R/plot_yield_asparagus.R")
source("strawberries/R/VIP_plot.R")
plot_yield_asparagus(sim_scenarios_output)
VIP_plot(sim_scenarios_output)



