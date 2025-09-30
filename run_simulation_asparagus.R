source("asparagus/simulation_asparagus.R")
source("functions/various_functions.R")

library(decisionSupport)

set.seed(1847)

####Input data####
scenario_today<-read.csv("asparagus/asparagus_today.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_126<-read.csv("asparagus/asparagus_126.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_245<-read.csv("asparagus/asparagus_245.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_370<-read.csv("asparagus/asparagus_370.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenario_585<-read.csv("asparagus/asparagus_585.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")

input<-read.csv("asparagus/asparagus_input.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")

risk_df<-read.csv("weathergenerator/risk_df.csv")

scenarios<-read.csv("asparagus/scenarios.csv")


outputs<-c("water_stress_risk",
           "insect_risk",
           "disease_risk",
           "photosynthetic_active_days",
           "weather_damage_risk",
           "growth_start_doy",
           "speargrowth",
           "chill_portions",
           "late_frost_risk",
           "temp_fluctuation_risk",
           "extreme_rainfall_risk",
           "extreme_heat_risk",
           "Tsoil_mean")

id<-as.numeric(id_df$today)
####Simulation run: today####
sim_today<-mcSimulation(estimate = as.estimate(input),
                        model_function = asparagus_sim,
                        numberOfModelRuns = 10000,
                        functionSyntax = "plainNames",
                        risk_df,
                        id)


sim_today_output<-youtputs_to_xinputs(sim_today, outputs)
#sim_today_output<-sim_today


saveRDS(sim_today_output, "asparagus/MC_results/MC_results_today.RDS")
write.csv(sim_today_output, "asparagus/MC_results/MC_results_today.csv")

id<-as.numeric(id_df$ssp1)
####Simulation run: ssp126####
sim_126<-mcSimulation(estimate = as.estimate(input),
                        model_function = asparagus_sim,
                        numberOfModelRuns = 10000,
                        functionSyntax = "plainNames",
                        risk_df,
                        id)


sim_126_output<-youtputs_to_xinputs(sim_126, outputs)
#sim_126_output<-sim_126


saveRDS(sim_126_output, "asparagus/MC_results/MC_results_126.RDS")
write.csv(sim_126_output, "asparagus/MC_results/MC_results_126.csv")

id<-as.numeric(id_df$ssp2)
####Simulation run: ssp245####
sim_245<-mcSimulation(estimate = as.estimate(input),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames",
                      risk_df,
                      id)

sim_245_output<-youtputs_to_xinputs(sim_245, outputs)
#sim_245_output<-sim_245

saveRDS(sim_245_output, "asparagus/MC_results/MC_results_245.RDS")
write.csv(sim_245_output, "asparagus/MC_results/MC_results_245.csv")

id<-as.numeric(id_df$ssp3)
####Simulation run: ssp370####
sim_370<-mcSimulation(estimate = as.estimate(input),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames",
                      risk_df,
                      id)

sim_370_output<-youtputs_to_xinputs(sim_370, outputs)
#sim_370_output<-sim_370


saveRDS(sim_370_output, "asparagus/MC_results/MC_results_370.RDS")
write.csv(sim_370_output, "asparagus/MC_results/MC_results_370.csv")

id<-as.numeric(id_df$ssp5)
####Simulation run: ssp585####
sim_585<-mcSimulation(estimate = as.estimate(input),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames",
                      risk_df,
                      id)


sim_585_output<-youtputs_to_xinputs(sim_585, outputs)
#sim_585_output<-sim_585


saveRDS(sim_585_output, "asparagus/MC_results/MC_results_585.RDS")
write.csv(sim_585_output, "asparagus/MC_results/MC_results_585.csv")

#Simulation run with scenarios####
#one run for all scenarios
input<-read.csv("asparagus/asparagus_input.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
risk_df<-read.csv("weathergenerator/risk_df.csv")
scenarios<-read.csv("asparagus/scenarios.csv")



sim_scenarios<-mcSimulation(estimate = as.estimate(input),
                      model_function = asparagus_sim_scen,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames",
                      risk_df,
                      scenarios)

sim_scenarios_output<-youtputs_to_xinputs_scenarios(sim_scenarios, outputs)
saveRDS(sim_scenarios_output, "asparagus/MC_results/MC_results_scenarios.RDS")
write.csv(sim_scenarios_output, "asparagus/MC_results/MC_results_scenarios.csv")


# #####plot trys####
# plot_distributions(mcSimulation_object = sim_scenarios_output,
#                                     vars = c("marketable_yield_today", "marketable_yield_ssp1", "marketable_yield_ssp2", "marketable_yield_ssp3", "marketable_yield_ssp5"),
#                                     method = "boxplot",
#                                     #method = "smooth_simple_overlay",
#                                     #old_names = c("NPV_Treeless_System", "NPV_Agroforestry_System"),
#                                     #new_names = c("Monoculture (baseline)", "Agroforestry with current funding"),
#                                     x_axis_name = "Scenario",
#                                     y_axis_name = "Yield")
# sim_scenarios_output$y$marketable_yield_ssp1