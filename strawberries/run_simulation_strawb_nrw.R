source("strawberries/R/asparagus_sim_scen.R")
source("strawberries/R/youtputs_to_xinputs_scenarios.R")

library(decisionSupport)

set.seed(1847)

#Simulation run with scenarios####
#one run for all scenarios
input<-read.csv("strawberries/data/strawb_input.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenarios<-read.csv("strawberries/data/scenarios.csv")

f_list_fullname <- list.files('weathergenerator/indices_strawb/', full.names = TRUE)

dir.create('strawberries/MC_results_NRW/')
dir_folder <- 'strawberries/MC_results_NRW/'
f_result<- 'MC_results_scenarios.RDS'

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

for(i in 1:length(f_list_fullname)){
  
  #get the pixel id
  p <- strsplit(f_list_fullname[i], '/') %>% 
    purrr::map_chr(3) %>% 
    strsplit(split = '_') %>% 
    purrr::map_chr(1)
  
  #generate file name
  fname_res <- paste0(dir_folder, p, '_', f_result)
  
  #check if file already exists
  if(file.exists(fname_res)){
    cat('File:', fname_res, 'already exists. Skip\n')
    next
  } 
  
  risk_df <- read.csv(f_list_fullname[i])
  
  sim_scenarios<-mcSimulation(estimate = as.estimate(input),
                              model_function = strawb_sim_scen,
                              numberOfModelRuns = 10000,
                              functionSyntax = "plainNames",
                              risk_df,
                              scenarios)
  
  sim_scenarios_output<-youtputs_to_xinputs_scenarios(sim_scenarios, outputs)
  saveRDS(sim_scenarios_output, fname_res)
  #write.csv(sim_scenarios_output, "asparagus/MC_results/MC_results_scenarios.csv")
  
  
}

risk_df<-read.csv("weathergenerator/risk_df_nrw/risk_df.csv")


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

