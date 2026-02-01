source("asparagus/R/asparagus_sim_scen.R")
source("asparagus/R/youtputs_to_xinputs_scenarios.R")

library(decisionSupport)

set.seed(1847)

#Simulation run with scenarios####
#one run for all scenarios
input<-read.csv("asparagus/data/asparagus_input.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
scenarios<-read.csv("asparagus/data/scenarios.csv")

f_list_fullname <- list.files('weathergenerator/indices/', full.names = TRUE)

dir.create('asparagus/MC_results_NRW/')
dir_folder <- 'asparagus/MC_results_NRW/'
f_result<- 'MC_results_scenarios.RDS'

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
                              model_function = asparagus_sim_scen,
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

