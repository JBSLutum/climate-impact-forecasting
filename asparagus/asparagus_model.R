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
    Tsoil_mean, #average soil temp over harvest season
    
    # Quality risks
    late_frost_occ,
    temp_fluctuation_occ,
    extreme_rainfall_occ,
    extreme_heat_occ
) {
  #  Base yield under perfect conditions scaled by growth factors
  season_factor <- season_days / standard_season_length
  season_factor <- max(min(season_factor, 1), 0.1)
 
  #Soil temperature influence, ideal is 20 degree, temperature over or under are
  #give reduction via Gaussian response function
  T_opt<-20
  sigma<-5
  #Tsoil_mean<-15
  temp_factor<-exp(-((Tsoil_mean - T_opt)^2) / (2 * sigma^2))
  
  actual_yield <- standard_yield * gp * chill * season_factor *temp_factor
  

  
  
  # Quality risk evaluation
  total_quality_loss <- 1
  
    total_quality_loss <- total_quality_loss * chill
  
    total_quality_loss <- total_quality_loss -late_frost_occ
  
    total_quality_loss <- total_quality_loss -temp_fluctuation_occ
  
    total_quality_loss <- total_quality_loss -extreme_rainfall_occ
  
    total_quality_loss <- total_quality_loss -extreme_heat_occ
  
  
  # Cap loss to max 100%
  total_quality_loss <- max(min(total_quality_loss, 1), 0.1)
  
  # Marketable yield after quality loss
  marketable_yield <- actual_yield * total_quality_loss
  
  # Return output
  return(list(
    actual_yield = actual_yield/1000,
    marketable_yield = marketable_yield/1000,
    quality_loss = total_quality_loss
  ))
}




####make_variables####
# make_variables<-function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
# 
# make_variables(estimate_read_csv("asparagus/asparagus_today.csv"))

