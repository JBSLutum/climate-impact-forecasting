source("asparagus/asparagus_model.R")

#Simulation run####
asparagus_sim<-function(risk_df=NULL){
  
  
  #variable initiation----
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
  #call growth function----
  # part 1: Calculate growth potential
  gp <- growth_potential(
    pad,
    water_stress_occ,
    disease_occ,
    insect_damage_occ,
    weather_damage_occ
  )
  #call chill function----
  # part 2: Calculate chill ratio
  chill <- chill_requirement(
    required_chill,
    actual_chill
  )
  #call season length function----
  # part 3: Determine season length
  season_days <- season_length(
    growth_start_day,
    days_till_harvest,
    season_end_day
  )
  #call yield estimation function----
  # part 4: Estimate yield
  yield <- yield_estimate(
    standard_yield,
    gp,
    chill,
    season_days,
    standard_season_length,
    
    late_frost_occ,
    temp_fluctuation_occ,
    extreme_rainfall_occ,
    extreme_heat_occ
  )
  #return output----
  return(list(    actual_yield = yield$actual_yield,
                  marketable_yield = yield$marketable_yield,
                  quality_loss = yield$quality_loss,
                  growth_pot=gp))
}
