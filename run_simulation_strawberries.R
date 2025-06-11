source("strawberries/simulation_strawb.R")
source("functions/various_functions.R")

library(decisionSupport)

####Input data####
scenario<-read.csv("scenarios.csv",fileEncoding = "UTF-8-BOM")
strawb_data<-read.csv("strawberries/strawberrie_data.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")

risk_df<-read.csv("weathergenerator/risk_df.csv")

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


####Simulation run: today####
sim_strawberries<-scenario_mc(base_estimate = strawb_data,
                              scenarios = scenario,
                              model_function = strawb_sim,
                              numberOfModelRuns = 10000,
                              functionSyntax = "plainNames",
                              risk_df)

strawb_output<-youtputs_to_xinputs(sim_strawberries, outputs)

saveRDS(strawb_output, "strawberries/MC_results/MC_results_strawb.RDS")
write.csv(strawb_output, "strawberries/MC_results/MC_results_strawb.csv")

