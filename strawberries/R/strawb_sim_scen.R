#Strawberrie Yield Simulation for Scenarios

source("strawberries/R/summer.R")
source("strawberries/R/winter.R")
source("strawberries/R/spring.R")
source("strawberries/R/fruit.R")
source("strawberries/R/harvest.R")

####
#Simulation run with scenario wrapper----
#Simulation run#
strawb_sim_scen<-function(...,risk_df=NULL,scenarios=NULL){
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
    
    #Summer parameters
    #plant_date <- risk_df$summer_plant_date[id]
    summer_drought_stress_risk <- risk_df$summer_drought_stress[id]
    summer_insect_risk <- risk_df$summer_insect_risk[id]
    summer_disease_risk <- risk_df$summer_disease_risk[id]
    summer_mean_temp <- risk_df$summer_mean_temp[id]
    pad <- risk_df$pad[id]
    #winter parameters
    actual_chill <- risk_df$actual_chill[id]
    winter_mean_temp <- risk_df$winter_mean_temp[id]
    #spring parameters
    #spring_start <- risk_df$spring_start[id]
    spring_pollinator <- risk_df$spring_pollinator[id]
    spring_disease_risk <- risk_df$spring_disease_risk[id]
    spring_frost_risk <- risk_df$spring_frost_risk[id]
    spring_mean_temp <- risk_df$spring_mean_temp[id]
    #fruit parameters
    #fruit_start <- risk_df$f_start[id]
    fruit_frost_risk <- risk_df$fruit_frost_risk[id]
    fruit_drought_stress_risk <- risk_df$fruit_drought_stress[id]
    fruit_hail_risk <- risk_df$fruit_hail[id]
    fruit_sunburn_risk <- risk_df$fruit_sunburn[id]
    fruit_mean_temp <- risk_df$fruit_mean_temp[id]
    fruit_insect_risk <- risk_df$fruit_insect_risk[id]
    fruit_disease_risk <- risk_df$fruit_disease_risk[id]
    fruit_snail_risk <- risk_df$fruit_snail_risk[id]
    #harvest parameters
    #harvest_start <- risk_df$harvest_start[id]
    harvest_frost_risk <- risk_df$harvest_frost_risk[id]
    harvest_heat_risk <- risk_df$harvest_heat[id]
    harvest_hail_risk <- risk_df$harvest_hail[id]
    harvest_rain_risk <- risk_df$harvest_rain[id]
    harvest_fly_risk <- risk_df$harvest_fly[id]
    harvest_mean_temp <- risk_df$summer_mean_temp[id]
    
    
    #estimation input table
    #pad <- photosynthetic_active_days
    
    
    summer_drought_stress_occ <- chance_event(summer_drought_stress_risk,
                                     value_if = summer_drought_stress_dmg,
                                     value_if_not = 0)
    summer_insect_occ <- chance_event(summer_insect_risk,
                                value_if = summer_insect_dmg,
                                value_if_not = 0)
    
    summer_disease_occ <- chance_event(summer_disease_risk,
                                      value_if = summer_disease_dmg,
                                      value_if_not = 0)
    
    spring_disease_occ <- chance_event(spring_disease_risk,
                                       value_if = spring_disease_dmg,
                                       value_if_not = 0)
    spring_frost_occ <- chance_event(spring_frost_risk,
                                       value_if = spring_frost_dmg,
                                       value_if_not = 0)
    fruit_frost_occ <- chance_event(fruit_frost_risk,
                                       value_if = fruit_frost_dmg,
                                       value_if_not = 0)
    fruit_drought_stress_occ <- chance_event(fruit_drought_stress_risk,
                                       value_if = fruit_drought_stress_dmg,
                                       value_if_not = 0)
    fruit_hail_occ <- chance_event(fruit_hail_risk,
                                       value_if = fruit_hail_dmg,
                                       value_if_not = 0)
    fruit_sunburn_occ <- chance_event(fruit_sunburn_risk,
                                       value_if = fruit_sunburn_dmg,
                                       value_if_not = 0)
    fruit_insect_occ <- chance_event(fruit_insect_risk,
                                       value_if = fruit_insect_dmg,
                                       value_if_not = 0)
    fruit_disease_occ <- chance_event(fruit_disease_risk,
                                       value_if = fruit_disease_dmg,
                                       value_if_not = 0)
    
    fruit_snail_occ <- chance_event(fruit_snail_risk,
                                       value_if = fruit_snail_dmg,
                                       value_if_not = 0)
    harvest_frost_occ <- chance_event(harvest_frost_risk,
                                       value_if = harvest_frost_dmg,
                                       value_if_not = 0)
    harvest_heat_occ <- chance_event(harvest_heat_risk,
                                       value_if = harvest_heat_dmg,
                                       value_if_not = 0)
    harvest_hail_occ <- chance_event(harvest_hail_risk,
                                       value_if = harvest_hail_dmg,
                                       value_if_not = 0)
    harvest_rain_occ <- chance_event(harvest_rain_risk,
                                       value_if = harvest_rain_dmg,
                                       value_if_not = 0)
    harvest_fly_occ <- chance_event(harvest_fly_risk,
                                     value_if = harvest_fly_dmg,
                                     value_if_not = 0)
    
    pad_need<-photosynthetic_active_days_needed
    required_chill <- chill_need
    standard_yield <- expected_yield
    T_opt_fruit<-T_opt_fruit

    #call summer function----
    # part 1: Calculate yield potential
    yield_potential <- summer(
      pad,
      pad_need,
      summer_disease_occ,
      summer_insect_occ,
      summer_drought_stress_occ,
      summer_mean_temp
    )
    #call chill function----
    # part 2: Calculate chill ratio
    chill <- winter(
      required_chill,
      actual_chill,
      winter_mean_temp
    )
    #call spring function----
    # part 3: Determine blooming potential
    bloom_potential <- spring(
      #spring_start,
      spring_frost_occ,
      spring_disease_occ,
      spring_pollinator,
      spring_mean_temp
    )
    #call fruit count estimation function----
    # part 4: Estimate fruits
    fruit_quality <- fruit(
      fruit_snail_occ,
      fruit_disease_occ,
      fruit_insect_occ,
      fruit_sunburn_occ,
      fruit_hail_occ,
      fruit_drought_stress_occ,
      fruit_frost_occ,
      fruit_mean_temp,
      T_opt_fruit
    )
    #call harvest function
    # part 5: estimate harvested yield
    yield <- harvest(
      standard_yield,
      yield_potential,
      chill,
      bloom_potential,
      fruit_quality,
      harvest_rain_occ,
      harvest_hail_occ,
      harvest_frost_occ,
      harvest_heat_occ,
      harvest_fly_occ,
      harvest_mean_temp
    )
    
    #output list####
    outs <- list(
      actual_yield = yield$actual_yield,
      marketable_yield = yield$marketable_yield,
      #Summer parameters
      summer_drought_stress_risk=summer_drought_stress_risk,
      summer_insect_risk=summer_insect_risk,
      summer_disease_risk=summer_disease_risk,
      summer_mean_temp=summer_mean_temp,
      pad=pad,
      #winter parameters
      actual_chill=actual_chill,
      winter_mean_temp=winter_mean_temp,
      #spring parameters
      #spring_start=spring_start,
      spring_pollinator=spring_pollinator,
      spring_disease_risk=spring_disease_risk,
      spring_frost_risk=spring_frost_risk,
      spring_mean_temp=spring_mean_temp,
      #fruit parameters
      #fruit_start=fruit_start,
      fruit_frost_risk=fruit_frost_risk,
      fruit_drought_stress_risk=fruit_drought_stress_risk,
      fruit_hail_risk=fruit_hail_risk,
      fruit_sunburn_risk=fruit_sunburn_risk,
      fruit_mean_temp=fruit_mean_temp,
      fruit_insect_risk=fruit_insect_risk,
      fruit_disease_risk=fruit_disease_risk,
      fruit_snail_risk=fruit_snail_risk,
      T_opt_fruit=T_opt_fruit,
      #harvest parameters
      #harvest_start=harvest_start,
      harvest_frost_risk=harvest_frost_risk,
      harvest_heat_risk=harvest_heat_risk,
      harvest_hail_risk=harvest_hail_risk,
      harvest_rain_risk=harvest_rain_risk,
      harvest_fly_risk=harvest_fly_risk,
      harvest_mean_temp=harvest_mean_temp,
      id=id
    )
    vars <- c("actual_yield",
              "marketable_yield",
              "summer_drought_stress_risk",
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
              "harvest_mean_temp",
              "id")
    
    output[paste0(vars,"_",scen)]<-outs
  }
  
  
  #return output----
  return(output)
}
