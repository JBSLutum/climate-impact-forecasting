#risk functions

risk_heat <- function(Tmax, Tcrit_strong = 25, Tcrit_extreme = 30, risk_add_strong = 0.1, risk_add_extreme = 0.2){
  risk_vec <- ifelse(Tmax > Tcrit_extreme, yes = risk_add_extreme, 
                     no = ifelse(Tmax > Tcrit_strong, yes = risk_add_strong, no = 0))
  return(cumsum(risk_vec))
}

risk_rain <- function(Prec, P_crit = 25, risk_add = 0.1){
  risk_vec <- ifelse(Prec > P_crit, yes = risk_add, no = 0)
  return(cumsum(risk_vec))
}

risk_late_frost <- function(Tmean, T_crit = 0, risk_add = 0.05){
  risk_vec <- ifelse(Tmean < T_crit, yes = risk_add, no = 0)
  return(cumsum(risk_vec))
}

risk_hail <- function(Tmax,Tmin,Prec,Tupper,Tdif ,Pmin, risk_add)
{
  hail_days <- (Tmax>Tupper & (Tmax-Tmin)>Tdif & Prec >= Pmin )
  number_hail_days<-sum(hail_days)
  hail_risk<-risk_add*number_hail_days
  return(hail_risk)
}

risk_sunburn <- function(Tmax, Tmin, Prec, Tdif, Pmin, risk_add)
{
  sunburn<-(Tmax>30&(Tmax-Tmin)<10&Prec<Pmin)
  number_sunburn_days<-sum(sunburn)
  risk<-risk_add*number_sunburn_days
  return(risk)
}


get_chill <- function(weather, lat, i_start){
  if(is.na(i_start)) return(NA)
  
  weather %>% 
    select(DATE, Day, Month, Year, Tmin, Tmax) %>% 
    slice(1:i_start) %>% 
    stack_hourly_temps(latitude = lat) %>% 
    purrr::pluck('hourtemps') %>% 
    pull(Temp) %>% 
    chillR::Dynamic_Model(summ = TRUE) %>% 
    tail(test, n = 1) %>% 
    round(digits = 2) %>% 
    return()
}

get_insect_risk <- function(Tmean,
                            Prec, 
                            Tlower = 20, 
                            Tupper = 30, 
                            Pmin = 5, 
                            consecutive_days = 5, 
                            risk_day = 0.1, 
                            risk_additional_day = 0.05){
  
  #check if conditions fulfilled
  disease_condition <- (Tmean >= Tlower & Tmean <= Tupper & Prec < Pmin) %>% as.numeric()
  
  #check if conditions are consecutively fulfilled
  test <- rle(disease_condition)
  
  #placeholder for risk factor
  stress_factor <- rep(0, length(Tmean))
  
  #in case any risk factor is found
  if(any(test$values == 1 & test$lengths >= consecutive_days)){
    #iterae over consecutive output
    for(i in 1:length(test$lengths)){
      #in case condition not fulfilled, skip
      if(test$values[i] == 0 | test$lengths[i] < consecutive_days) next()
      d_start <- sum(test$lengths[1:i-1]) +1
      #end of dryspell
      d_end <- sum(test$lengths[1:i])
      
      risk_add <- c(rep(0, consecutive_days - 1), risk_day, rep(risk_additional_day, test$lengths[i] - consecutive_days))
      
      stress_factor[d_start:d_end] <- risk_add
      
    }
    
  }
  return(cumsum(stress_factor))
}

get_slugs_risk<-function(Tmin,Prec, Tlower, Pmin,
                          consecutive_days = 5, 
                          risk_day = 0.1, 
                          risk_additional_day = 0.05)
{
  slug_up<-(Tmin>Tlower&Prec>Pmin)%>% as.numeric()
  #check if conditions are consecutively fulfilled
  test <- rle(slug_up)
  
  #placeholder for risk factor
  stress_factor <- rep(0, length(Tmin))
  
  #in case any risk factor is found
  if(any(test$values == 1 & test$lengths >= consecutive_days)){
    #iterae over consecutive output
    for(i in 1:length(test$lengths)){
      #in case condition not fulfilled, skip
      if(test$values[i] == 0 | test$lengths[i] < consecutive_days) next()
      d_start <- sum(test$lengths[1:i-1]) +1
      #end of dryspell
      d_end <- sum(test$lengths[1:i])
      
      risk_add <- c(rep(0, consecutive_days - 1), risk_day, rep(risk_additional_day, test$lengths[i] - consecutive_days))
      
      stress_factor[d_start:d_end] <- risk_add
      
    }
    
  }
  return(cumsum(stress_factor))
}

get_fly_risk<-function(Tmax,Prec, Tlower, Tupper, Pmin,
                         consecutive_days = 3, 
                         risk_day = 0.05, 
                         risk_additional_day = 0.1)
{
  prec_yesterday <- dplyr::lag(Prec, default = 0)
  
  fly_active <- (prec_yesterday > Pmin & Tmax >= Tlower & Tmax <= Tupper) %>% as.numeric()
  
  #if fly active == 1, risk day
  stress_factor <- rep(0, length(Tmax))
  stress_factor[fly_active == 1] <- risk_day
  
  #check if we have consecutive risk
  test <- rle(fly_active)
  
  #in case any risk factor is found
  if(any(test$values == 1 & test$lengths >= consecutive_days)){
    #iterae over consecutive output
    for(i in 1:length(test$lengths)){
      #in case condition not fulfilled, skip
      if(test$values[i] == 0 | test$lengths[i] < consecutive_days) next()
      
      #get startpoint in original vector, when condition is fulfilled
      d_start <- sum(test$lengths[1:i-1]) +1
      #end of dryspell
      d_end <- sum(test$lengths[1:i])
      
      #add risk factor to column
      #             #cases that we skipped          #additional risk when conditions remain the same
      risk_add <- c(rep(risk_day, consecutive_days - 1), rep(risk_additional_day, test$lengths[i] - consecutive_days+1))
      
      #here we actually add risk values
      stress_factor[d_start:d_end] <- risk_add
      
    }
    
  }
  return(cumsum(stress_factor))
}


get_spring_pollinator <- function(Tmin,Tmax,Prec,Tlower,Tupper,Tdif,Pmin)
{
  good_polli_day <- (Tmax>Tlower & Tmax<=Tupper & (Tmax-Tmin)>=Tdif & Prec<=Pmin) %>% as.numeric()
  
  return((cumsum(good_polli_day)/length(good_polli_day)))
}

get_disease_risk <- function(Tmean, 
                             Prec,
                             Tlower = 15, 
                             Tupper = 25, 
                             Pmin = 5, 
                             consecutive_days = 5, 
                             risk_day = 0.1, 
                             risk_additional_day = 0.05){
  
  #check if conditions fulfilled
  disease_condition <- (Tmean >= Tlower & Tmean <= Tupper & Prec >= Pmin) %>% as.numeric()
  
  #check if conditions are consecutively fulfilled
  test <- rle(disease_condition)
  
  #placeholder for risk factor
  stress_factor <- rep(0, length(Tmean))
  
  #in case any risk factor is found
  if(any(test$values == 1 & test$lengths >= consecutive_days)){
    #iterae over consecutive output
    for(i in 1:length(test$lengths)){
      #in case condition not fulfilled, skip
      if(test$values[i] == 0 | test$lengths[i] < consecutive_days) next()
      d_start <- sum(test$lengths[1:i-1]) +1
      #end of dryspell
      d_end <- sum(test$lengths[1:i])
      
      risk_add <- c(rep(0, consecutive_days - 1), risk_day, rep(risk_additional_day, test$lengths[i] - consecutive_days))
      
      stress_factor[d_start:d_end] <- risk_add
    }
  }
  return(cumsum(stress_factor))
}

get_drought_stress_factor <- function(Prec,
                                      day_consec_dry = 5,
                                      prec_threhsold = 1,
                                      risk_dry = 0.1,
                                      risk_additional_day = 0.05){
  #this vector stores the final stress value
  stress_factor <- rep(0, length(Prec))
  
  #remove drizzle from the precipitation
  Prec_mod <- ifelse(Prec < prec_threhsold, yes = 0, no = Prec)
  
  #identify when there is no precipitation
  dry <- as.numeric(Prec_mod == 0)
  #check how often it is dry (so value = 1)
  test <- rle(dry)
  
  #check if there are any dry-spells fulfilling the conditions
  drought_screen <- test$values == 1 & test$lengths >= day_consec_dry
  
  #iterate over the dry_spells, add stress value depending on length
  for(i in 1:length(drought_screen)){
    if(drought_screen[i] == FALSE) next()
    #start of dry spell
    d_start <- sum(test$lengths[1:i-1]) +1
    #end of dryspell
    d_end <- sum(test$lengths[1:i])
    #dryspell length
    
    risk_add <- c(rep(0, day_consec_dry - 1), risk_dry, rep(risk_additional_day, test$lengths[i] - day_consec_dry))
    
    stress_factor[d_start:d_end] <- risk_add
    
  }
  return(cumsum(stress_factor))
}

#check if day is photosynthesis day
#calculate number of photosynthesis days
get_photosynthesis_days <- function(Tmean, Prec,
                                    lower_T = 15,
                                    upper_T = 30,
                                    max_P = 10){
  return(Tmean >= lower_T & Tmean <= upper_T & Prec <= max_P)
}


###call functions####

get_weather_indices <- function(weather,
                                latitude,
                                photosynday_temp_lower = 15,
                                photosynday_temp_upper = 30,
                                photosynday_prec_max = 15,
                                rain_cutoff=1,
                                droughtstress_consec_dry = 5,
                                droughtstress_risk_initial = 0.05,
                                droughtstress_risk_follow = 0.01,
                                diseaserisk_temp_lower = 15,
                                diseaserisk_temp_upper = 25,
                                diseaserisk_prec_min = 5,
                                diseaserisk_day_consec = 5,
                                diseaserisk_risk_initial = 0.1,
                                diseaserisk_risk_follow = 0.05,
                                insectrisk_temp_lower = 20,
                                insectrisk_temp_upper = 30,
                                insectrisk_prec_max = 5,
                                insectrisk_day_consec = 5,
                                insectrisk_risk_initial = 0.1,
                                insectrisk_risk_follow = 0.05,
                                rainrisk_prec_strong = 30,
                                rainrisk_prec_extreme = 60,
                                rainrisk_risk_strong = 0.1,
                                rainrisk_risk_extreme = 0.3,
                                rainrisk_risk_follow = 0.05,
                                frostrisk_temp_crit = 0,
                                frostrisk_risk_add = 0.05,
                                rainharvest_Pcrit = 25,
                                rainharvest_risk_add = 0.15,
                                heatharvest_Tcrit_srong = 30,
                                heatharvest_Tcrit_extreme = 35,
                                heatharvest_risk_add_strong = 0.05,
                                heatharvest_risk_add_extreme = 0.1,
                                polli_Tupper = 32,
                                polli_Tlower = 12,
                                polli_Tdif = 10,
                                polli_Pmin = 1,
                                hail_Tupper=25,
                                hail_Tdif=12,
                                hail_Pmin=5,
                                hail_risk_add=0.05,
                                sunburn_Tdif=10, 
                                sunburn_Pmin=1, 
                                sunburn_risk_add=0.1,
                                slug_Tlower=10,
                                slug_Pmin=2,
                                slug_consecutive_days = 3, 
                                slug_risk_day = 0.1, 
                                slug_risk_additional_day = 0.05,
                                fly_Tlower=20, 
                                fly_Tupper=28, 
                                fly_Pmin=2,
                                fly_consecutive_days = 3, 
                                fly_risk_day = 0.05, 
                                fly_risk_additional_day = 0.1
                                )
{
  leap_year <- FALSE
  if(nrow(weather) == 366) leap_year <- TRUE
  yday_subtract <- 365
  if(leap_year) yday_subtract <- 366
  
  weather_adj <- weather %>% 
    mutate(yday_plot = ifelse(yday >= 175, yes = yday - yday_subtract, no = yday),
           Tmean = (Tmin + Tmax) / 2)
  #summer parameters####
  weather_summer<-weather_adj %>% 
    filter(yday_plot > -151 & yday_plot < -59) %>% 
    mutate(
      summer_drought_stress_risk = get_drought_stress_factor(Prec,
                                                              day_consec_dry = droughtstress_consec_dry,
                                                              prec_threhsold = rain_cutoff,
                                                              risk_dry = droughtstress_risk_initial,
                                                              risk_additional_day = droughtstress_risk_follow),
      
      summer_insect_risk = get_insect_risk(Tmean = Tmean, 
                                            Prec = Prec,
                                            Tlower = insectrisk_temp_lower,
                                            Tupper = insectrisk_temp_upper,
                                            Pmin = insectrisk_prec_max,
                                            consecutive_days = insectrisk_day_consec,
                                            risk_day = insectrisk_risk_initial,
                                            risk_additional_day = insectrisk_risk_follow),
      
      summer_disease_risk = get_disease_risk(Tmean = Tmean, 
                                              Prec = Prec, 
                                              Tlower = diseaserisk_temp_lower,
                                              Tupper = diseaserisk_temp_upper, 
                                              Pmin = diseaserisk_prec_min,
                                              consecutive_days = diseaserisk_day_consec,
                                              risk_day = diseaserisk_risk_initial, 
                                              risk_additional_day = diseaserisk_risk_follow),
      
      ##summer_mean_temp=summer_mean_temp,
      
      pad = get_photosynthesis_days(Tmean = Tmean,
                                     Prec = Prec,
                                     lower_T = photosynday_temp_lower,
                                     upper_T = photosynday_temp_upper, 
                                     max_P = photosynday_prec_max)
    )
  summer_drought_stress_risk <- pmin(weather_summer$summer_drought_stress_risk[nrow(weather_summer)],1)
  summer_insect_risk <- pmin(weather_summer$summer_insect_risk[nrow(weather_summer)],1)
  summer_disease_risk <- pmin(weather_summer$summer_disease_risk[nrow(weather_summer)],1)
  #summer_mean_temp=summer_mean_temp 
  pad <- weather_summer$pad %>% sum()
  summer_mean_temp<-sum(weather_summer$Tmean)/nrow(weather_summer)
  

  #winter parameters####
weather_winter<-weather_adj %>% 
  filter(yday_plot >= -60 & yday_plot <= 90)
actual_chill <- get_chill(weather = weather_winter, lat = latitude, i_start = nrow(weather_winter))
winter_mean_temp<-sum((weather_winter$Tmax-weather_winter$Tmin))/nrow(weather_winter)

#winter_mean_temp=winter_mean_temp

#spring parameters####
#spring_start=spring_start 
weather_spring<-weather_adj %>% 
  filter(yday_plot > 60 & yday_plot <= 120) %>% 
  mutate(spring_pollinator = get_spring_pollinator(Tmin=Tmin,
                                           Tmax=Tmax,
                                           Prec=Prec,
                                           Tlower=polli_Tlower,
                                           Tupper=polli_Tupper,
                                           Tdif=polli_Tdif,
                                           Pmin=polli_Pmin
                                           ),
         spring_disease_risk = get_disease_risk(Tmean = Tmean, 
                                        Prec = Prec, 
                                        Tlower = diseaserisk_temp_lower,
                                        Tupper = diseaserisk_temp_upper, 
                                        Pmin = diseaserisk_prec_min,
                                        consecutive_days = diseaserisk_day_consec,
                                        risk_day = diseaserisk_risk_initial, 
                                        risk_additional_day = diseaserisk_risk_follow),
         spring_frost_risk = risk_late_frost(Tmean = Tmin,
                                     T_crit = frostrisk_temp_crit,
                                     risk_add = frostrisk_risk_add))

#save last value
#adjust pollinator activity. maybe sum? or mean?
spring_pollinator <- pmin(weather_spring$spring_pollinator[nrow(weather_spring)],1)
spring_disease_risk <- pmin(weather_spring$spring_disease_risk[nrow(weather_spring)],1)
spring_frost_risk <- pmin(weather_spring$spring_frost_risk[nrow(weather_spring)],1)
spring_mean_temp<-sum(weather_spring$Tmean)/nrow(weather_spring)

#fruit parameters#####
#fruit_start=fruit_start
weather_fruit<-weather_adj %>% 
  filter(yday_plot > 120 & yday_plot <= 150) %>% 
  mutate(fruit_frost_risk = risk_late_frost(Tmean = Tmin,
                                    T_crit = frostrisk_temp_crit,
                                    risk_add = frostrisk_risk_add),
         fruit_drought_stress_risk = get_drought_stress_factor(Prec,
                                                       day_consec_dry = droughtstress_consec_dry,
                                                       prec_threhsold = rain_cutoff,
                                                       risk_dry = droughtstress_risk_initial,
                                                       risk_additional_day = droughtstress_risk_follow),
         fruit_hail_risk = risk_hail(Tmax, Tmin, Prec,
                             Tupper=hail_Tupper,
                             Tdif=hail_Tdif,
                             Pmin=hail_Pmin,
                             risk_add=hail_risk_add),
         fruit_sunburn_risk = risk_sunburn(Tmax, Tmin, Prec, 
                                   Tdif=sunburn_Tdif, 
                                   Pmin=sunburn_Pmin, 
                                   risk_add=sunburn_risk_add),
         #fruit_tmean=fruit_tmean
         fruit_insect_risk = get_insect_risk(Tmean = Tmean, 
                                     Prec = Prec,
                                     Tlower = insectrisk_temp_lower,
                                     Tupper = insectrisk_temp_upper,
                                     Pmin = insectrisk_prec_max,
                                     consecutive_days = insectrisk_day_consec,
                                     risk_day = insectrisk_risk_initial,
                                     risk_additional_day = insectrisk_risk_follow),
         fruit_disease_risk = get_disease_risk(Tmean = Tmean, 
                                       Prec = Prec, 
                                       Tlower = diseaserisk_temp_lower,
                                       Tupper = diseaserisk_temp_upper, 
                                       Pmin = diseaserisk_prec_min,
                                       consecutive_days = diseaserisk_day_consec,
                                       risk_day = diseaserisk_risk_initial, 
                                       risk_additional_day = diseaserisk_risk_follow),
         fruit_snail_risk=get_slugs_risk(Tmin,Prec, Tlower=slug_Tlower, Pmin=slug_Pmin,
                                 consecutive_days = slug_consecutive_days, 
                                 risk_day = slug_risk_day, 
                                 risk_additional_day = slug_risk_additional_day)
)

#save last values. check if risks are cumulative or not!!!
fruit_frost_risk <- pmin(weather_fruit$fruit_frost_risk[nrow(weather_fruit)],1)
fruit_drought_stress_risk <- pmin(weather_fruit$fruit_drought_stress_risk[nrow(weather_fruit)],1)
fruit_hail_risk <- pmin(weather_fruit$fruit_hail_risk[nrow(weather_fruit)],1)
fruit_sunburn_risk <- pmin(weather_fruit$fruit_sunburn_risk[nrow(weather_fruit)],1)
fruit_insect_risk <- pmin(weather_fruit$fruit_insect_risk[nrow(weather_fruit)],1)
fruit_disease_risk <- pmin(weather_fruit$fruit_disease_risk[nrow(weather_fruit)],1)
fruit_snail_risk <- pmin(weather_fruit$fruit_snail_risk[nrow(weather_fruit)],1)
fruit_mean_temp<-sum(weather_fruit$Tmean)/nrow(weather_fruit)

weather_harvest<-weather_adj %>% 
  filter(!(yday_plot > -183 & yday_plot < 150)) %>% 
  mutate(
    #harvest parameters####
    #harvest_start=harvest_start
    harvest_frost_risk = risk_late_frost(Tmean = Tmin,
                                      T_crit = frostrisk_temp_crit,
                                      risk_add = frostrisk_risk_add),
    harvest_heat_risk = risk_heat(Tmax = Tmax,
                               Tcrit_strong = heatharvest_Tcrit_srong, 
                               Tcrit_extreme = heatharvest_Tcrit_extreme, 
                               risk_add_strong = heatharvest_risk_add_strong, 
                               risk_add_extreme = heatharvest_risk_add_extreme),
    harvest_hail_risk=risk_hail(Tmax, Tmin, Prec,
                             Tupper=hail_Tupper,
                             Tdif=hail_Tdif,
                             Pmin=hail_Pmin,
                             risk_add=hail_risk_add),
    harvest_rain_risk= risk_rain(Prec = Prec,
                                                 P_crit = rainharvest_Pcrit, 
                                                 risk_add = rainharvest_risk_add),
    harvest_fly_risk = get_fly_risk(Tmax, Prec, Tlower=fly_Tlower, Tupper=fly_Tupper, Pmin=fly_Pmin,
                                 consecutive_days = fly_consecutive_days, 
                                 risk_day = fly_risk_day, 
                                 risk_additional_day = fly_risk_additional_day)
)



harvest_frost_risk <- pmin(weather_harvest$harvest_frost_risk[nrow(weather_harvest)],1)
harvest_heat_risk <- pmin(weather_harvest$harvest_heat_risk[nrow(weather_harvest)],1)
harvest_hail_risk <- pmin(weather_harvest$harvest_hail_risk[nrow(weather_harvest)],1)
harvest_rain_risk <- pmin(weather_harvest$harvest_rain_risk[nrow(weather_harvest)],1)
harvest_fly_risk <- pmin(weather_harvest$harvest_fly_risk[nrow(weather_harvest)],1)
harvest_mean_temp<-sum(weather_harvest$Tmean)/nrow(weather_harvest)

output_list<-list(  summer_drought_stress_risk=summer_drought_stress_risk,
               summer_insect_risk=summer_insect_risk,
               summer_disease_risk=summer_disease_risk,
               pad=pad,
               actual_chill=actual_chill,
               winter_mean_temp=winter_mean_temp,
               spring_pollinator=spring_pollinator,
               spring_disease_risk=spring_disease_risk,
               spring_frost_risk=spring_frost_risk,
               fruit_frost_risk=fruit_frost_risk,
               fruit_drought_stress_risk=fruit_drought_stress_risk,
               fruit_hail_risk=fruit_hail_risk,
               fruit_sunburn_risk=fruit_sunburn_risk,
               fruit_insect_risk=fruit_insect_risk,
               fruit_disease_risk=fruit_disease_risk,
               fruit_snail_risk=fruit_snail_risk,
               harvest_frost_risk=harvest_frost_risk,
               harvest_heat_risk=harvest_heat_risk,
               harvest_hail_risk=harvest_hail_risk,
               harvest_rain_risk=harvest_rain_risk,
               harvest_fly_risk=harvest_fly_risk,
               summer_mean_temp=summer_mean_temp,
               spring_mean_temp=spring_mean_temp,
               fruit_mean_temp=fruit_mean_temp,
               harvest_mean_temp=harvest_mean_temp)
return(output_list)
}
