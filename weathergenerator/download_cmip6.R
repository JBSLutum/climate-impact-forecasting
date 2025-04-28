#download cmip6 for jb
library(chillR)
library(LarsChill)

weather <- read.csv('data/weather-koln-bonn.csv')
colnames(weather)
station_info <- read.csv('data/weather-station-info.csv')
colnames(station_info) <- c('longitude', 'latitude', 'station_name', 'id')

area <- c(52, 6, 50, 8)

chillR::download_cmip6_ecmwfr(
  scenarios = c('ssp126', 'ssp245', 'ssp370', 'ssp585') ,
  area =  area,
  model = 'default',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax', 'Prec'),
  year_start = 2015,
  year_end = 2100)

chillR::download_baseline_cmip6_ecmwfr(
  area =  area,
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax', 'Prec'),
  year_start = 1985,
  year_end = 2014)


#extract data, create change scenarios
extracted <- chillR::extract_cmip6_data(stations = station_info, 
                                        variable = c('Tmin', 'Tmax', 'Prec')) 
 
scen_list <- LarsChill::custom_gen_rel_change_scenario(downloaded_list = extracted,
                                                       variable = c('Tmin', 'Tmax', 'Prec'), 
                                            scenarios = c(2075), 
                                            reference_period = c(1986:2014),
                                            future_window_width = 30) %>% 
  chillR::convert_scen_information(give_structure = FALSE)

#temperature scenario for the locally observed weather, representative for
#the whole observation period
scen_observed <- LarsChill::custom_temperature_scenario_from_records(weather = weather, 
                                                                     year = 2001.5, 
                                                                     variable = c('Tmin', 'Tmax', 'Prec'))
#temperature scenario of observed weather that corresponds to the reference years
#of the CMIP6 data
scen_2000 <- LarsChill::custom_temperature_scenario_from_records(weather = weather, 
                                                                 year = 2000, 
                                                                 variable = c('Tmin', 'Tmax', 'Prec'))

#correction for the cmip6 scenario, so that is corresponds to the observation period
base <- chillR::temperature_scenario_baseline_adjustment(scen_observed,
                                                         scen_2000, 
                                                         required_variables = c('Tmin', 'Tmax', 'Prec'))

#adjust the cmip6 change scenarios, so that it corresponds to the observation peroid
adjusted_list <- chillR::temperature_scenario_baseline_adjustment(base,
                                                                  scen_list,
                                                                  temperature_check_args=
                                                                    list( scenario_check_thresholds = c(-5, 15)))

dir.create('data/future_weather')
dir_name <- 'data/future_weather/'

#remove / from station name
names(adjusted_list) <- gsub(x = names(adjusted_list), pattern = '/', replacement = '_')
station_info$station_name <- gsub(x =station_info$station_name, pattern = '/', replacement = '_')

purrr::walk(1:length(adjusted_list), function(i){
  
  #create file name
  fname <- paste0(dir_name, 'future-weather.', names(adjusted_list)[i], '.csv')
  
  #check if files exist, if so, next
  if(file.exists(fname)) return(NULL)
  
  gen_weather <- LarsChill::temperature_generation_rmawgen_prec(weather = weather,
                                                 years = c(1980, 2023), 
                                                 sim_years = c(2000, 2100), 
                                                 temperature_scenario = adjusted_list[i])
  
  #round to remove digits
  gen_weather[[1]]$Tmin <- round(gen_weather[[1]]$Tmin, digits = 2)
  gen_weather[[1]]$Tmax <- round(gen_weather[[1]]$Tmax, digits = 2)
  gen_weather[[1]]$Prec <- round(gen_weather[[1]]$Prec, digits = 2)
  
  write.csv(gen_weather[[1]], file = fname)
  
}, .progress = TRUE)
