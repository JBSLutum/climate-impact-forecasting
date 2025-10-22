source("asparagus/R/asparagus_sim_scen.R")
source("asparagus/R/youtputs_to_xinputs_scenarios.R")

library(decisionSupport)

set.seed(1847)

#Simulation run with scenarios####
#one run for all scenarios
input<-read.csv("asparagus/data/asparagus_input.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
risk_df<-read.csv("weathergenerator/risk_df.csv")
scenarios<-read.csv("asparagus/data/scenarios.csv")

sim_scenarios<-mcSimulation(estimate = as.estimate(input),
                      model_function = asparagus_sim_scen,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames",
                      risk_df,
                      scenarios)

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

sim_scenarios_output<-youtputs_to_xinputs_scenarios(sim_scenarios, outputs)
saveRDS(sim_scenarios_output, "asparagus/MC_results/MC_results_scenarios.RDS")
write.csv(sim_scenarios_output, "asparagus/MC_results/MC_results_scenarios.csv")

input_all<-c("water_stress_risk",
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

pls_result_today_yield <- plsr.mcSimulation(object = sim_scenarios_output,
                                            resultName = names(sim_scenarios_output$y)[2], ncomp = 1)
pls_result_today_yield_table<- VIP_table(pls_result_today_yield, threshold = 0.8)
library(ggplot2)
library(chillR)
library(tidyverse)
plot <- ggplot(pls_result_today_yield_table, aes(x = reorder(Variable, VIP), y = VIP)) +
  geom_col(fill = "firebrick")+
  coord_flip()
plot



sim_scenarios_output$y$npv<-sim_scenarios_output$y$marketable_yield_ssp1-sim_scenarios_output$y$marketable_yield_today
sim_scenarios_output_voi <- data.frame(sim_scenarios_output$x %>% select(-matches("_ssp[235]")), 
                                       sim_scenarios_output$y[16])
evpi_today <- multi_EVPI(mc = sim_scenarios_output_voi, 
                        first_out_var = "npv")
plot_evpi(evpi_today, decision_vars = "npv")
