library(decisionSupport)

#Growth Potential####
#This function calculates the growth potential the asparagus plant accumulates
#during their vegetative crowd phase over the summer
#time point 24th of June till November

growth_potential <- function(pad,
                             pad_need,
                             water_stress_occ, 
                             disease_occ, 
                             insect_damage_occ, 
                             weather_damage_occ) {
  
  # Base growth potential from PAD
  if (pad[1] >= pad_need) {
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
  potential <- max(min(potential, 1), 0.1)
  
  return(potential)
}

#Chill Requirement####
#This function checks if the chill requirement of the plant is met
#Since the plant would grow even with no chill accumulated the output is a ratio
#time point November till Spring
chill_requirement <- function(required_chill, actual_chill) {
  if (required_chill > 0){
    chillratio<-actual_chill/required_chill
    chillratio <- max(min(chillratio, 1), 0.1)}
  else {chillratio <- 1}
  return(chillratio)
}
#Season Length####
#This function calculates the length of a season
#as soon as growth conditions (soil temp > 14°) are met the spear starts growing
#the season end in Germany is additionally the 24th of June
#time point spring
season_length <- function(growth_start_day,days_till_harvest,season_end_day) {
  days <- season_end_day - (growth_start_day + days_till_harvest)  # Inclusive the start days for first spear growth
  days <- max(days, 0)  # Prevent negative values if start is after end
  return(days)
}
#Harvest Season####
#This function estimates the yield of asparagus over the season
#the total yield that comes from the field is determined by:
#accumulated growth potential, if the chill requirement is met, and the season length
#the marketable yield is determined by the quality of the spears
#weather influences the quality directly via frost, heat and rapid temperature fluctuations
#and indirectly by prohibiting harvesting on time because of rain or heat
#the fieldworker cant get on the field
yield_estimate <- function(
    standard_yield,
    gp,       #growth potential 0-1
    chill,            # chill ratio 0-1
    season_days,          # actual season length
    standard_season_length, # reference full-length season
    
    # Quality risks
    late_frost_occ,
    temp_fluctuation_occ,
    extreme_rainfall_occ,
    extreme_heat_occ
) {
  # 1. Base yield under perfect conditions scaled by growth factors
  season_factor <- season_days / standard_season_length
  #season_factor <- max(min(season_factor, 1), 0)
  actual_yield <- standard_yield * gp * chill * season_factor
  
  # 2. Quality risk evaluation
  total_quality_loss <- 0
  
  
    total_quality_loss <- total_quality_loss + late_frost_occ
  
    total_quality_loss <- total_quality_loss + temp_fluctuation_occ
  
    total_quality_loss <- total_quality_loss + extreme_rainfall_occ
  
    total_quality_loss <- total_quality_loss + extreme_heat_occ
  
  
  # Cap loss to max 100%
  total_quality_loss <- min(total_quality_loss, 0.9)
  
  # 3. Marketable yield after quality loss
  marketable_yield <- actual_yield * (1 - total_quality_loss)
  
  # 4. Return output
  return(list(
    actual_yield = actual_yield/100,
    marketable_yield = marketable_yield/100,
    quality_loss = total_quality_loss
  ))
}




####make_variables####
# make_variables<-function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
# 
# make_variables(estimate_read_csv("asparagus/asparagus_today.csv"))

