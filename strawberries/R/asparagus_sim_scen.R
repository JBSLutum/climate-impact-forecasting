source("asparagus/R/growth_potential.R")
source("asparagus/R/chill_requirement.R")
source("asparagus/R/season_length.R")
source("asparagus/R/yield_estimate.R")

####
#Simulation run with scenario wrapper----
#Simulation run#
asparagus_sim_scen<-function(...,risk_df=NULL,scenarios=NULL){
  if (is.null(scenarios)) stop("scenarios kam nicht an!")
  output<-list()
  #get names of scenarios
  scenario_cols <- setdiff(names(scenarios), c("Variable","param"))
  #cycle through all scenarios
  for (scen in scenario_cols){
    #ID for weather data
    id_range <- as.numeric(c(scenarios[[scen]]))
    id_draw <- runif(n=1,min=id_range[1], max=id_range[2])
    id=round(id_draw)
    
    #variable initiation----
    
    #weather variables
    #calculated weather variables are drawn randomly from a dataframe with
    #weather seasons from a weather generator, each scenario has at least 1400
    #seasons [id]
    
    water_stress_risk <- risk_df$drought_stress[id]
    insect_risk <- risk_df$insect_risk[id]
    disease_risk <- risk_df$disease_risk[id]
    pad <- risk_df$photosynhthesis_day[id]
    weather_damage_risk <- risk_df$risk_rain[id]
    growth_start_day <- risk_df$yday_speargrowth[id]
    days_till_harvest <- risk_df$yday_harvest_star[id] - growth_start_day
    actual_chill <- risk_df$accumulated_chill[id]
    late_frost_risk <- risk_df$frost_risk[id]
    temp_fluctuation_risk <- risk_df$diurnal_risk[id]
    extreme_rainfall_risk <- risk_df$rainharvest_risk[id]
    extreme_heat_risk <- risk_df$heatharvest_risk[id]
    Tsoil_mean<-risk_df$Tsoil_mean[id]
    
    #estimation input table
    #pad <- photosynthetic_active_days
    pad_need<-photosynthetic_active_days_needed
    
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
    #actual_chill <- chill_portions
    #growth_start_day <- growth_start_doy
    season_end_day <- season_end_doy
    standard_yield <- expected_yield
    #days_till_harvest <- speargrowth
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
    #weather variables####
    
    '  risk_df$drought_stress[id]
  risk_df$insect_risk[id]
  risk_df$disease_risk[id]
  risk_df$photosynhthesis_day[id]
  risk_df$risk_rain[id]
  risk_df$yday_speargrowth[id]
  risk_df$yday_harvest_star[id]
  risk_df$accumulated_chill[id]
  risk_df$frost_risk[id]
  risk_df$diurnal_risk[id]
  risk_df$rainharvest_risk[id]
  risk_df$heatharvest_risk[id]'
    
    
    #call growth function----
    # part 1: Calculate growth potential
    gp <- growth_potential(
      pad,
      pad_need,
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
      Tsoil_mean,
      
      late_frost_occ,
      temp_fluctuation_occ,
      extreme_rainfall_occ,
      extreme_heat_occ
    )
    
    #output list####
    outs <- list(
      actual_yield = yield$actual_yield,
      marketable_yield = yield$marketable_yield,
      water_stress_risk=water_stress_risk,
      insect_risk=insect_risk,
      disease_risk=disease_risk,
      photosynthetic_active_days=pad,
      weather_damage_risk=weather_damage_risk,
      growth_start_doy=growth_start_day,
      speargrowth=days_till_harvest,
      chill_portions=actual_chill,
      late_frost_risk=late_frost_risk,
      temp_fluctuation_risk=temp_fluctuation_risk,
      extreme_rainfall_risk=extreme_rainfall_risk,
      extreme_heat_risk=extreme_heat_risk,
      Tsoil_mean=Tsoil_mean,
      id=id
    )
    vars <- c("actual_yield",
              "marketable_yield",
              "water_stress_risk",
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
              "Tsoil_mean",
              "id")
    
    output[paste0(vars,"_",scen)]<-outs
  }
  
  
  #return output----
  return(output)
}
