#functions to calculate the 


#use function by Van Wijk & De Vries (1963)
get_Tsoil <- function(Tmean, alpha, T_start = NULL,
                      foil_start = 'black',
                      t_soil_opt = 20, 
                      foil_thres = 0.1,
                      black_foil_i = NULL,
                      foil_tplus = 7){
  
  #if there is no starting temperature defined, just take first observation as a starting point
  if(is.null(T_start)) T_start <- Tmean[1]
  
  #alpha controls how inert the soil is, low alpha means slow warm-up but also slow heat loss
  #high alpha means quick heat up but also quick heat-loss
  #if one alpha value is supplied, then it is set for the whole temperature time series
  #otherwise the user can adjust alpha before the function call (for example modify alpha dependent if the soil is wet or dry)
  if(length(alpha) == 1) alpha <- rep(alpha, length(Tmean))
  
  #on days with black foil, the temperature (of the air...) is increased by a constant value to mimick higher temperatures under foil
  # if(is.null(black_foil_i) == FALSE){
  #    Tmean[black_foil_i] <- Tmean[black_foil_i] + foil_tplus
  # }
  
  #create a vector with temperature data, this will gradually replaced by the actually calculated temperature
  T_soil <- rep(T_start, length(Tmean))
  
  #foil indicates what kind of foil is currently on the dam
  #three possible conditions: no foil, white or black foil
  foil <- foil_start
  
  #t_soil_opt
  #optimium temperature of the soil, that the farmers want to maintain
  
  #foil_thres
  #tolerance of conditions outside the optimal temperature. if temperature below or above,
  #then foil management is triggered
  
  #counts how often foil is changed
  change_foil <- rep(0, length(Tmean))
  foil_was_changed <- 0
  foil_vec <- rep('black', length(Tmean))
  
  #iterate over all temperature entries. start at the second entry, as it always also includes the 
  #soil temperature at the time step before and the air temperature of the current day
  i <- 2
  while(i <= length(Tmean)){
    
    foil_was_changed <- 0
    
    #if soil temperature is too low and white foil (or no foil is one), apply black foil
    if (T_soil[i-1] < t_soil_opt*(1-foil_thres)&foil=="white"){
      foil<-"black"
      foil_was_changed <- 1
      } else if (T_soil[i-1]>t_soil_opt*(1+foil_thres)&foil=="black"){
      foil<-"white"
      foil_was_changed <- 1
      }
    
    #update change foil
    change_foil[i] <- change_foil[i-1] +foil_was_changed
    
    #update foil status
    foil_vec[i] <- foil
    
    if (foil=="black"){
      foil_tplus<-abs(foil_tplus)
      alpha[i]<-0.25
    } else if (foil=="white"){
      foil_tplus<- - abs(foil_tplus)
      alpha[i]<-0.15
    }
    
    
    
    
    T_soil[i] = T_soil[i-1] + alpha[i] * (Tmean[i] + foil_tplus - T_soil[i-1])
    i <- i+1
  }

  #return soil temperature
  return(list('T_soil' = T_soil,
              'change_foil' = change_foil,
              'foil' = foil_vec))
}
#could modify alpha to mimick effect of foils:
#black foil faster warming, so higher alpha
#white foil slower warming, so lower alpha
#rain: slower warming, so higher alpha


check_soil_wet <- function(Prec, rain_cutoff = 1, rain_lag = 1){
  #create a vector, that stores the condition if soil is wet
  soil_wet <- rep(FALSE, length(Prec))
  #create vector indicating if it rained
  rained <- Prec > rain_cutoff
  #iterate over the rain vector:
  #if it rained today or the day before (depending on the rain_lag value it can also be more days)
  #--> soil is wet
  #otherwise soil is dry
  i <- 1
  while(i <= length(Prec)){
    #index of start for the wetness check
    j <- i - rain_lag
    #indices to check from start to end of wetness check
    i_vec <- j:i
    #exclude indices that are outside of the vector(so smaller than 1 or larger than length of vector)
    i_vec <- i_vec[i_vec > 0 & i_vec <= length(Prec)]
    #if it rained on any of the days that were checked, then soil is wet
    if(any(rained[i_vec])) soil_wet[i] <- TRUE
    #go to the next day
    i <- i+1
  }
  return(soil_wet)
}


#----------------------------#
#factors during growth period
#----------------------------#

#check if day is photosynthesis day
#calculate number of photosynthesis days
get_photosynthesis_days <- function(Tmean, Prec,
                                    lower_T = 15,
                                    upper_T = 30,
                                    max_P = 10){
  return(Tmean >= lower_T & Tmean <= upper_T & Prec <= max_P)
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

get_extreme_rainfaill <- function(Prec, 
                                  Prec_strong = 30, 
                                  Prec_extreme = 60, 
                                  risk_strong = 0.1,
                                  risk_extreme = 0.3, 
                                  risk_additional_day = 0.05){
  check_rain <- ifelse(Prec >= Prec_extreme, yes = 'extreme', 
                       no = ifelse(Prec >= Prec_strong,
                                   yes = 'strong',
                                   no = 'normal'))
  
  #check if consecutive
  test <- rle(check_rain)
  
  risk_rain <- rep(0, length(Prec))
  
  if(any(test$values %in% c('strong', 'extreme'))){
    for(i in 1:length(test$lengths)){
      if(test$values[i] == 'normal') next()
      
      if(test$values[i] == 'extreme') p_add <- risk_extreme
      if(test$values[i] == 'strong') p_add <- risk_strong
      d_start <- sum(test$lengths[1:i-1]) +1
      risk_rain[d_start] <- p_add
      if(test$lengths[i] > 1){
        d_end <- sum(test$lengths[1:i])
        risk_rain[(d_start+1):d_end] <- risk_additional_day
      }
    }
  }
  return(cumsum(risk_rain))
}

#day when soil has 14°C for three consecutive days
#returns index when the condition is fulfilled first
get_speargrowth_start <-function(T_soil, T_crit = 14, consecutive_days = 3){
  check_soiltemp <- (T_soil >= T_crit) %>% as.numeric()
  
  test <- rle(check_soiltemp)
  
  i_test <- which(test$values == 1 & test$lengths >= consecutive_days) %>% min()
  i_start <- NA
  if(is.infinite(i_test) == FALSE){
    if(i_test == 1){
      i_start <- consecutive_days
    } else {
      i_start <- sum(test$lengths[1:(i_test-1)]) + consecutive_days
    }
  }
  
  return(i_start)
}

#get first harvest day
get_first_harvest <- function(T_soil, i_start, T_crit = 11, day_min = 7){
  day_til_harvest <- day_min
  i <- i_start
  if(is.na(i_start)) return(NA)
  wait<- TRUE
  while(wait){
    if(T_soil[i] >= T_crit) day_til_harvest <- day_til_harvest -1
    
    if(day_til_harvest != 0)     i <- i+1
    if(day_til_harvest == 0 | i > length(T_soil)) wait <- FALSE

  }
  return(i)
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

#-----------------------#
#risks during harvest 
#-----------------------#
risk_late_frost <- function(Tmean, T_crit = 0, risk_add = 0.1){
  risk_vec <- ifelse(Tmean < T_crit, yes = risk_add, no = 0)
  return(cumsum(risk_vec))
}

risk_amplitude <- function(Tmin, Tmax, delta_crit = 15, risk_add = 0.1){
  risk_vec <- ifelse(abs(Tmax - Tmin) > delta_crit, yes = risk_add, no = 0)
  return(cumsum(risk_vec))
}

risk_rain_harvest <- function(Prec, P_crit = 25, risk_add = 0.1){
  risk_vec <- ifelse(Prec > P_crit, yes = risk_add, no = 0)
  return(cumsum(risk_vec))
}

risk_heat_harvest <- function(Tmax, Tcrit_strong = 25, Tcrit_extreme = 30, risk_add_strong = 0.1, risk_add_extreme = 0.2){
  risk_vec <- ifelse(Tmax > Tcrit_extreme, yes = risk_add_extreme, 
                     no = ifelse(Tmax > Tcrit_strong, yes = risk_add_strong, no = 0))
  return(cumsum(risk_vec))
}


get_weather_indices <- function(weather,
                                latitude,
                                rain_cutoff = 1,
                                lag_days_soil_wet = 1,
                                alpha_dry = 0.2,
                                alpha_wet = 0.12,
                                soil_temp_star = 0.1,
                                black_foil_yday=c(41:200),
                                black_foil_tplus = 7,
                                t_soil_opt = 20,
                                foil_thres = 0.1,
                                foil_start = 'black',
                                photosynday_temp_lower = 15,
                                photosynday_temp_upper = 30,
                                photosynday_prec_max = 15,
                                droughtstress_consec_dry = 5,
                                droughtstress_risk_initial = 0.05,
                                droughtstress_risk_follow = 0.01,
                                diseaserisk_temp_lower = 15,
                                diseaserisk_temp_upper = 25,
                                diseaserisk_prec_min = 5,
                                diseaserisk_day_consec = 5,
                                diseaserisk_risk_initial = 0.2,
                                diseaserisk_risk_follow = 0.05,
                                insectrisk_temp_lower = 20,
                                insectrisk_temp_upper = 30,
                                insectrisk_prec_max = 5,
                                insectrisk_day_consec = 5,
                                insectrisk_risk_initial = 0.1,
                                insectrisk_risk_follow = 0.01,
                                rainrisk_prec_strong = 30,
                                rainrisk_prec_extreme = 60,
                                rainrisk_risk_strong = 0.1,
                                rainrisk_risk_extreme = 0.3,
                                rainrisk_risk_follow = 0.05,
                                speargrowth_temp_crit = 14,
                                speargrowth_day_consec = 3,
                                harvest_start_temp = 14,
                                harvest_start_consec = 7,
                                frostrisk_temp_crit = -4,
                                frostrisk_risk_add = 0.1,
                                riskdiurnal_delta_crit = 20,
                                riskdiurnal_risk_add = 0.1,
                                rainharvest_Pcrit = 25,
                                rainharvest_risk_add = 0.1,
                                heatharvest_Tcrit_srong = 30,
                                heatharvest_Tcrit_extreme = 35,
                                heatharvest_risk_add_strong = 0.05,
                                heatharvest_risk_add_extreme = 0.1){
  

  leap_year <- FALSE
  if(nrow(weather) == 366) leap_year <- TRUE
  yday_subtract <- 365
  if(leap_year) yday_subtract <- 366
  
  foil_i <- which(weather$yday %in% black_foil_yday)
  
  weather_adj <- weather %>% 
    mutate(yday_plot = ifelse(yday >= 175, yes = yday - yday_subtract, no = yday)) %>% 
    mutate(soil_wet = check_soil_wet(Prec = Prec, 
                                     rain_cutoff = rain_cutoff, 
                                     rain_lag = lag_days_soil_wet),
           Tmean = (Tmin + Tmax) / 2,
           #Tmean_foil = (0.3*Tmin + 0.7*Tmax),
           alpha = ifelse(soil_wet, yes = alpha_wet, no = alpha_dry))
  
  weather_adj <- weather_adj %>%
    mutate(Tmean_foil = ifelse(yday_plot < 40, 0, Tmean))
  
  soil_out <- get_Tsoil(Tmean = weather_adj$Tmean_foil,
                        black_foil_i = foil_i,
                        foil_thres = foil_thres,
                        foil_start = foil_start,
                        t_soil_opt = t_soil_opt,
                        foil_tplus =  black_foil_tplus,
                        T_start =  soil_temp_star,
                        alpha = weather_adj$alpha)
  
  weather_adj$T_soil <- soil_out$T_soil
  weather_adj$change_foil <- soil_out$change_foil
  weather_adj$foil <- soil_out$foil
  

  
  # weather_adj %>% 
  #   ggplot(aes(x = yday_plot)) +
  #   geom_line(aes(y = T_soil)) +
  #   geom_ribbon(aes(ymin = t_soil_opt * (1-foil_thres),
  #                   ymax = t_soil_opt * (1+foil_thres)),
  #               alpha = 0.2) +
  #   geom_tile(aes(y =30, fill = foil))

  
  #example foil
  
  # #put foil in the first week of Jan
  # foil_i <- which(weather$yday %in% (1:7))
  # weather_adj <- weather_adj %>% 
  #   mutate(T_soil = get_Tsoil(Tmean,
  #                             T_start =  soil_temp_star,
  #                             black_foil_i=black_foil_i,
  #                             black_foil_tplus =black_foil_tplus,  
  #                             alpha = alpha))
  # # 
  # #check example when alpha is set higher
  # alpha_higher <- weather_adj$alpha
  # alpha_higher[foil_i] <- alpha_higher[foil_i] * (1+0.4)
  # 
  # weather_adj <- weather_adj %>% 
  #   mutate(alpha_mod = alpha_higher,
  #          T_soil_foilv2 = get_Tsoil(Tmean,
  #                                  T_start =  soil_temp_star,
  #                                  black_foil_tplus =black_foil_tplus,  
  #                                  alpha = alpha_mod))
  # 
  # ggplot(weather_adj, aes(x = yday)) +
  #   geom_rect(aes(xmin = 1, xmax = 7, ymin = -Inf, ymax = Inf), alpha = 0.3, fill = 'steelblue') +
  #   geom_line(aes(y = T_soil, col = 'T_soil')) +
  #   geom_line(aes(y = T_soil_foil,  col = 'T_soil_foil')) +
  #   geom_line(aes(y = T_soil_foilv2,  col = 'T_soil_foilv2')) +
  #   xlim(c(0, 90))
  # ggsave('example_warming.jpeg')
  
  #---------------#
  #risk vegetative period
  #---------------#
  
  # weather_vegetation %>% 
  #   pivot_longer(cols = c('Tmin', 'Tmax', 'Tmean', 'T_soil')) %>% 
  #   ggplot(aes(x =yday_plot, y = value, col = name)) +
  #   geom_line()
  
  weather_vegetation <- weather_adj %>% 
    filter(yday_plot < -60) %>% 
    mutate(photosynhthesis_day = get_photosynthesis_days(Tmean = Tmean,
                                                         Prec = Prec,
                                                        lower_T = photosynday_temp_lower,
                                                        upper_T = photosynday_temp_upper, 
                                                        max_P = photosynday_prec_max),
           drought_stress = get_drought_stress_factor(Prec,
                                                      day_consec_dry = droughtstress_consec_dry,
                                                      prec_threhsold = rain_cutoff,
                                                      risk_dry = droughtstress_risk_initial,
                                                      risk_additional_day = droughtstress_risk_follow),
           disease_risk = get_disease_risk(Tmean = Tmean, 
                                           Prec = Prec, 
                                           Tlower = diseaserisk_temp_lower,
                                           Tupper = diseaserisk_temp_upper, 
                                           Pmin = diseaserisk_prec_min,
                                           consecutive_days = diseaserisk_day_consec,
                                           risk_day = diseaserisk_risk_initial, 
                                           risk_additional_day = diseaserisk_risk_follow),
           insect_risk = get_insect_risk(Tmean = Tmean, 
                                         Prec = Prec,
                                         Tlower = insectrisk_temp_lower,
                                         Tupper = insectrisk_temp_upper,
                                         Pmin = insectrisk_prec_max,
                                         consecutive_days = insectrisk_day_consec,
                                         risk_day = insectrisk_risk_initial,
                                         risk_additional_day = insectrisk_risk_follow),
           risk_rain = get_extreme_rainfaill(Prec = Prec, 
                                             Prec_strong = rainrisk_prec_strong,
                                             Prec_extreme = rainrisk_prec_extreme,
                                             risk_strong = rainrisk_risk_strong,
                                             risk_extreme = rainrisk_risk_extreme,
                                             risk_additional_day = rainrisk_risk_follow)) 
  
  drought_stress <- weather_vegetation$drought_stress[nrow(weather_vegetation)]
  insect_risk <- weather_vegetation$insect_risk[nrow(weather_vegetation)]
  disease_risk <- weather_vegetation$disease_risk[nrow(weather_vegetation)]
  photosynhthesis_day <- weather_vegetation$photosynhthesis_day %>% sum()
  risk_rain <- weather_vegetation$risk_rain[nrow(weather_vegetation)]  
  
  #-------------------#
  #determine start of harvest
  #-------------------#
  
  weather_sub_chill <- weather_adj %>% 
    filter(yday_plot >= -60)
  weather_sub <- weather_adj %>% 
    filter(yday_plot >= 40)
  
  #index when speargrowth condition is fulfilled, 
  i_start <- get_speargrowth_start(T_soil = weather_sub$T_soil, 
                                   T_crit = speargrowth_temp_crit, 
                                   consecutive_days = speargrowth_day_consec)
  
  i_havest <- NA
  yday_speargrowth <- NA
  yday_harvest_star <- NA
  accumulated_chill <- NA
  if(is.null(i_start) == FALSE){
    
    yday_speargrowth <- weather_sub$yday[i_start] 
    #index when first harvest is reached
    i_havest <- get_first_harvest(T_soil = weather_sub$T_soil, 
                                  i_start = i_start,
                                  T_crit = harvest_start_temp,
                                  day_min = harvest_start_consec)
    
    #i_start refers to the index of weather_sub
    #adjust i_start for weather_sub:chill (because it determines when chill accumulation stops)
    i_stop_chill <- i_start + (nrow(weather_sub_chill) - nrow(weather_sub))
    
    #calculate accumulated chill until speargrowth start?
    accumulated_chill <- get_chill(weather = weather_sub_chill, lat = latitude, i_start = i_stop_chill)
    
  }
  
  
  #-------------------#
  #risk during harvest#
  #-------------------#
  
  frost_risk <- NA
  diurnal_risk <- NA
  rainharvest_risk <- NA
  heatharvest_risk <- NA
  if(is.na(i_havest) == FALSE){
    yday_harvest_star <- weather_sub$yday[i_havest]
    
    weather_harvest <- weather_sub[i_havest:nrow(weather_sub),] %>% 
      mutate(frost_risk = risk_late_frost(Tmean = Tmin,
                                          T_crit = frostrisk_temp_crit,
                                          risk_add = frostrisk_risk_add),
             diurnal_risk = risk_amplitude(Tmin = Tmin, 
                                           Tmax = Tmax,
                                           delta_crit = riskdiurnal_delta_crit,
                                           risk_add = riskdiurnal_risk_add),
             rainrisk_harvest = risk_rain_harvest(Prec = Prec,
                                                  P_crit = rainharvest_Pcrit, 
                                                  risk_add = rainharvest_risk_add),
             heatrisk_harvest = risk_heat_harvest(Tmax = Tmax,
                                                  Tcrit_strong = heatharvest_Tcrit_srong, 
                                                  Tcrit_extreme = heatharvest_Tcrit_extreme, 
                                                  risk_add_strong = heatharvest_risk_add_strong, 
                                                  risk_add_extreme = heatharvest_risk_add_extreme))
    
    frost_risk <- weather_harvest$frost_risk[nrow(weather_harvest)]
    diurnal_risk <- weather_harvest$diurnal_risk[nrow(weather_harvest)]
    rainharvest_risk <- weather_harvest$rainrisk_harvest[nrow(weather_harvest)]
    heatharvest_risk <- weather_harvest$heatrisk_harvest[nrow(weather_harvest)]
  }
  
  # weather_harvest %>%
  #   mutate(diurnal_risk = diurnal_risk * 15) %>% 
  #   pivot_longer(cols = c('Tmin', 'Tmax', 'diurnal_risk')) %>%
  #   ggplot(aes(x =yday_plot, y = value, col = name)) +
  #   geom_line()

  #cap the risk indices, so that they are between 0.0 and 1.0
  
  drought_stress <- ifelse(drought_stress > 1, yes = 1, no = drought_stress)
  insect_risk <- ifelse(insect_risk > 1, yes = 1, no = insect_risk)
  disease_risk <- ifelse(disease_risk > 1, yes = 1, no = disease_risk)
  risk_rain <- ifelse(risk_rain > 1, yes = 1, no = risk_rain)
  frost_risk <- ifelse(frost_risk > 1, yes = 1, no = frost_risk)
  diurnal_risk <- ifelse(diurnal_risk > 1, yes = 1, no = diurnal_risk)
  rainharvest_risk <- ifelse(rainharvest_risk > 1, yes = 1, no = rainharvest_risk)
  heatharvest_risk <- ifelse(heatharvest_risk > 1, yes = 1, no = heatharvest_risk)
  
  #get mean soil temperature
  Tsoil_mean <- weather_harvest$T_soil %>% mean()
  
  
  output_list <- list(drought_stress = drought_stress,
                      insect_risk = insect_risk,
                      disease_risk = disease_risk,
                      photosynhthesis_day = photosynhthesis_day,
                      risk_rain = risk_rain,
                      yday_speargrowth = yday_speargrowth,
                      yday_harvest_star = yday_harvest_star,
                      accumulated_chill = accumulated_chill,
                      frost_risk = frost_risk,
                      diurnal_risk = diurnal_risk,
                      rainharvest_risk = rainharvest_risk,
                      heatharvest_risk = heatharvest_risk,
                      Tsoil_mean = Tsoil_mean)
  
  return(output_list)
}


rain_cutoff = 1
lag_days_soil_wet = 1
alpha_dry = 0.2
alpha_wet = 0.12
soil_temp_star = 0.1
black_foil_tplus = 7
black_foil_yday=c(41:200)
t_soil_opt = 20
foil_thres = 0.1
foil_start = 'black'
photosynday_temp_lower = 15
photosynday_temp_upper = 30
photosynday_prec_max = 10
droughtstress_consec_dry = 5
droughtstress_risk_initial = 0.05
droughtstress_risk_follow = 0.01
diseaserisk_temp_lower = 15
diseaserisk_temp_upper = 25
diseaserisk_prec_min = 5
diseaserisk_day_consec = 5
diseaserisk_risk_initial = 0.2
diseaserisk_risk_follow = 0.05
insectrisk_temp_lower = 20
insectrisk_temp_upper = 30
insectrisk_prec_max = 5
insectrisk_day_consec = 5
insectrisk_risk_initial = 0.1
insectrisk_risk_follow = 0.01
rainrisk_prec_strong = 30
rainrisk_prec_extreme = 60
rainrisk_risk_strong = 0.1
rainrisk_risk_extreme = 0.3
rainrisk_risk_follow = 0.05
speargrowth_temp_crit = 14
speargrowth_day_consec = 3
harvest_start_temp = 14
harvest_start_consec = 7
frostrisk_temp_crit = -4
frostrisk_risk_add = 0.05
riskdiurnal_delta_crit = 15
riskdiurnal_risk_add = 0.05
rainharvest_Pcrit = 25
rainharvest_risk_add = 0.1
heatharvest_Tcrit_srong = 30
heatharvest_Tcrit_extreme = 35
heatharvest_risk_add_strong = 0.05
heatharvest_risk_add_extreme = 0.1
