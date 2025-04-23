library(decisionSupport)


#Growth Potential####
growth_potential <- function(pad, 
                             water_stress_occ, 
                             disease_occ, 
                             insect_damage_occ, 
                             weather_damage_occ) {
  
  # Base growth potential from PAD
  if (pad >= 100) {
    potential <- 1  # 100%
  } else {
    potential <- pad / 100  # Scale proportionally
  }
  
  # Apply damage reductions if they occurred
  
    potential <- potential *(1-water_stress_occ)
  
    potential <- potential  *(1-disease_occ)
 
    potential <- potential  *(1-insect_damage_occ)
 
    potential <- potential  *(1-weather_damage_occ)

  
  # Ensure potential stays within [0, 1]
  potential <- max(min(potential, 1), 0)
  
  return(potential)
}

#Chill Requirment####
chill_requirement <- function(required_chill, actual_chill) {
  chillratio<-actual_chill/required_chill
  chillratio <- max(min(chillratio, 1), 0)
  return(chillratio)
}
#Season Length####
season_length <- function(growth_start_day,days_till_harvest,season_end_day) {
  days <- season_end_day - (growth_start_day + days_till_harvest)  # Inclusive the start days for first spear growth
  days <- max(days, 0)  # Prevent negative values if start is after end
  return(days)
}
#Harvest Season####
yield_estimate <- function(
    standard_yield,
    growth_potential,       # 0-1
    chill_ratio,            # 0-1
    season_length,          # actual season length
    standard_season_length, # reference full-length season
    
    # Quality risks
    late_frost_occ,
    temp_fluctuation_occ,
    extreme_rainfall_occ,
    extreme_heat_occ
) {
  # 1. Base yield under perfect conditions scaled by growth factors
  season_factor <- season_length / standard_season_length
  actual_yield <- standard_yield * growth_potential * chill_ratio * season_factor
  
  # 2. Quality risk evaluation
  total_quality_loss <- 0
  
  
    total_quality_loss <- total_quality_loss + late_frost_occ
  
    total_quality_loss <- total_quality_loss + temp_fluctuation_occ
  
    total_quality_loss <- total_quality_loss + extreme_rainfall_occ
  
    total_quality_loss <- total_quality_loss + extreme_heat_occ
  
  
  # Cap loss to max 100%
  total_quality_loss <- min(total_quality_loss, 1)
  
  # 3. Marketable yield after quality loss
  marketable_yield <- actual_yield * (1 - total_quality_loss)
  
  # 4. Return output
  return(list(
    actual_yield = actual_yield,
    marketable_yield = marketable_yield,
    quality_loss = total_quality_loss
  ))
}



asparagus_sim<-function(){
  
#Simulation run####
#variable initiation
pad <- photosynthetic_active_days
water_stress_occ <- chance_event(water_stress_risk,
                                      value_if = water_stress_damage,
                                      value_if_not = 0)
disease_occ <- chance_event(disease_risk,
                                 value_if = disease_damage,
                                 value_if_not = 0)

insect_damage_occ <- chance_event(insect_risk,
                                       value_if = insect_damage,
                                       value_if_not = 0)

weather_damage_occ <- chance_event(weather_damage_risk,
                                        value_if = weather_damage,
                                        value_if_not = 0)
required_chill <- chill_need
actual_chill <- chill_portions
growth_start_day <- growth_start_doy
season_end_day <- season_end_doy
standard_yield <- expected_yield
days_till_harvest <- speargrowth
standard_season_length <- expected_season_length
late_frost_occ <- chance_event(late_frost_risk,
                               value_if = late_frost_damage,
                               value_if_not = 0)
temp_fluctuation_occ <- chance_event(temp_fluctuation_risk,
                                  value_if = temp_fluctuation_damage,
                                  value_if_not = 0)
extreme_rainfall_occ <- chance_event(extreme_rainfall_risk,
                                  value_if = extreme_rainfall_damage,
                                  value_if_not = 0)
extreme_heat_occ <- chance_event(extreme_heat_risk,
                                 value_if = extreme_heat_damage,
                                 value_if_not = 0)

# part 1: Calculate growth potential
gp <- growth_potential(
  pad,
  water_stress_occ,
  disease_occ,
  insect_damage_occ,
  weather_damage_occ
)

# part 2: Calculate chill ratio
chill <- chill_requirement(
  required_chill,
  actual_chill
)

# part 3: Determine season length
season <- season_length(
  growth_start_day,
  days_till_harvest,
  season_end_day
)

# part 4: Estimate yield
yield <- yield_estimate(
  standard_yield,
  growth_potential = gp,
  chill_ratio = chill,
  season_length = season,
  standard_season_length,
  
  late_frost_occ,
  temp_fluctuation_occ,
  extreme_rainfall_occ,
  extreme_heat_occ
)
return(list(    actual_yield = yield[1],
                marketable_yield = yield[2],
                quality_loss = yield[3],
                growth_pot=gp))
}
####Input data####
scenario_today<-read.csv("asparagus/asparagus_today.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
####Simulation run: today####
sim_today<-mcSimulation(estimate = as.estimate(scenario_today),
             model_function = asparagus_sim,
             numberOfModelRuns = 10000,
             functionSyntax = "plainNames")
saveRDS(sim_today, "asparagus/MC_results/MC_results_today.RDS")
write.csv(sim_today, "asparagus/MC_results/MC_results_today.csv")

scenario_245<-read.csv("asparagus/asparagus_245.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
####Simulation run: ssp245####
sim_245<-mcSimulation(estimate = as.estimate(scenario_245),
                        model_function = asparagus_sim,
                        numberOfModelRuns = 10000,
                        functionSyntax = "plainNames")
saveRDS(sim_245, "asparagus/MC_results/MC_results_245.RDS")
write.csv(sim_245, "asparagus/MC_results/MC_results_245.csv")

scenario_370<-read.csv("asparagus/asparagus_370.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
####Simulation run: ssp370####
sim_370<-mcSimulation(estimate = as.estimate(scenario_370),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames")
saveRDS(sim_370, "asparagus/MC_results/MC_results_370.RDS")
write.csv(sim_370, "asparagus/MC_results/MC_results_370.csv")

scenario_585<-read.csv("asparagus/asparagus_585.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
####Simulation run: ssp585####
sim_585<-mcSimulation(estimate = as.estimate(scenario_585),
                      model_function = asparagus_sim,
                      numberOfModelRuns = 10000,
                      functionSyntax = "plainNames")
saveRDS(sim_585, "asparagus/MC_results/MC_results_585.RDS")
write.csv(sim_585, "asparagus/MC_results/MC_results_585.csv")

'
decisionSupport("asparagus/asparagus_today.csv",
                outputPath='asparagus/results',
                welfareFunction = asparagus_sim,
                numberOfModelRuns = 10000,
                functionSyntax = "plainNames")
'


####make_variables####
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("asparagus/asparagus_today.csv"))

