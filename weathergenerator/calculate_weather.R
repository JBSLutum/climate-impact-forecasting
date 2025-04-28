#calculate stuff for jb
library(chillR)
library(tidyverse)

flist <- list.files('weathergenerator/weather/future_weather/', full.names = TRUE)

future_weather <- purrr::map(1:length(flist), function(i){
  
  name_split <- flist[i] %>% 
    str_split('/') %>% 
    purrr::map(4) %>% 
    str_split('\\.')
  
  read.csv(flist[i]) %>% 
    select(-X) %>% 
    mutate(ssp = name_split[[1]][[3]],
           gcm = name_split[[1]][[4]],
           scenario_year = name_split[[1]][[5]],
           yday = lubridate::yday(DATE),
             #add season, new season starts june 24
           season = ifelse(yday >= 175,
                           yes = Year  +1,
                           no = Year)) %>% 
    return()
})

future_weather <- do.call('rbind', future_weather) 
future_weather$id = paste(future_weather$ssp, future_weather$gcm, future_weather$scenario_year, sep = '--')


#weed out incomplete seasons
drop_list <- future_weather %>% 
  group_by(id, season) %>% 
  summarise(n = n()) %>% 
  filter(n < 365)

future_weather <- future_weather %>% 
  filter(!(paste(id, season) %in% paste(drop_list$id, drop_list$season)))

source('functions/weather_indices_function.R')

weather_info <- read.csv('weathergenerator/weather/weather-station-info.csv')
lat <- weather_info$lat[1]
example_season <- read.csv('weathergenerator/example_season.csv')

#test for one season
get_weather_indices(weather = example_season, 
                    latitude = lat)

#add unique id to each seaoson
future_weather$id_seaon <- paste(future_weather$id, future_weather$season, sep = '--')

#split by each season
test <- split(x = future_weather, f = future_weather$id_seaon) 

# test_out <- purrr::map(test[1:10], function(x) get_weather_indices(x, lat),
#                        .progress = TRUE)

#apply function to each season
test_out <- purrr::map(test, function(x) get_weather_indices(x, lat),
                       .progress = TRUE)

#get ssp names
ssp <- names(test) %>% 
  str_split('--') %>% 
  purrr::map_chr(1)

#extract the individual indices
drought_stress <- purrr::map_dbl(test_out, 'drought_stress')
insect_risk <- purrr::map_dbl(test_out, 'insect_risk')
disease_risk <- purrr::map_dbl(test_out, 'disease_risk')
photosynhthesis_day <- purrr::map_dbl(test_out, 'photosynhthesis_day')
risk_rain <- purrr::map_dbl(test_out, 'risk_rain')

l <- purrr::map_int(test_out, function(x) length(x$yday_speargrowth)) 
fix_out <- which(l!=1)
for(i in fix_out){
  test_out[[i]]$yday_speargrowth <- NA
}
yday_speargrowth <- purrr::map_dbl(test_out, 'yday_speargrowth')

l <- purrr::map_int(test_out, function(x) length(x$yday_harvest_star)) 
fix_out <- which(l!=1)
for(i in fix_out){
  test_out[[i]]$yday_harvest_star <- NA
}
yday_harvest_star <- purrr::map_dbl(test_out, 'yday_harvest_star')
accumulated_chill <- purrr::map_dbl(test_out, 'accumulated_chill')
frost_risk <- purrr::map_dbl(test_out, 'frost_risk')
diurnal_risk <- purrr::map_dbl(test_out, 'diurnal_risk')
rainharvest_risk <- purrr::map_dbl(test_out, 'rainharvest_risk')
heatharvest_risk <- purrr::map_dbl(test_out, 'heatharvest_risk')

risk_df <- data.frame(ssp,
           drought_stress, 
           insect_risk,
           disease_risk,
           photosynhthesis_day,
           risk_rain,
           yday_speargrowth,
           yday_harvest_star,
           accumulated_chill,
           frost_risk,
           diurnal_risk,
           rainharvest_risk,
           heatharvest_risk
) %>% 
  mutate(id = 1:length(heatharvest_risk))

risk_df %>% 
  group_by(ssp) %>% 
  summarise(n = n())

risk_df %>% 
  group_by(ssp) %>% 
  summarise(id_min = min(id),
            id_max = max(id))

write.csv(risk_df, "weathergenerator/risk_df.csv")




risk_df %>% 
  ggplot(aes(fill = ssp, x = drought_stress)) +
  geom_density(alpha = 0.3)

risk_df %>% 
  ggplot(aes(fill = ssp, x = insect_risk)) +
  geom_density(alpha = 0.3)

risk_df %>% 
  ggplot(aes(fill = ssp, x = photosynhthesis_day)) +
  geom_density(alpha = 0.3)

risk_df %>% 
  ggplot(aes(fill = ssp, x = disease_risk)) +
  geom_density(alpha = 0.3)

risk_df %>% 
  ggplot(aes(fill = ssp, x = risk_rain)) +
  geom_density(alpha = 0.3)

risk_df %>% 
  drop_na() %>% 
  ggplot(aes(fill = ssp, x = yday_harvest_star)) +
  geom_density(alpha = 0.3)

risk_df %>% 
  drop_na() %>% 
  ggplot(aes(fill = ssp, x = yday_speargrowth)) +
  geom_density(alpha = 0.3)

risk_df %>% 
  drop_na() %>% 
  ggplot(aes(fill = ssp, x = accumulated_chill)) +
  geom_density(alpha = 0.3)


#combined plot
risk_df %>% 
  pivot_longer(cols = drought_stress:heatharvest_risk) %>% 
  drop_na() %>% 
  ggplot(aes(fill = ssp, x = value)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~name, scales = 'free') +
  theme_bw(base_size = 15)
ggsave('combined_indices.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')

#speargrowth start is always the same, weird
#sometimes very little chill, because speargrowth start is sometimes very early
#rain does not play a big role, 



#split by ssp
future_weather <- split(x = future_weather, f = future_weather$ssp)

example_season <- future_weather %>% 
  filter(season == 2001, ssp == 'ssp126',
         gcm == 'ACCESS-CM2')



example_season <- example_season %>% 
  mutate(yday_plot = ifelse(yday >= 175, yes = yday - yday_subtract, no = yday)) 


write.csv(example_season, file = 'example_season.csv', row.names = FALSE)






# #threshold what is considered to be rain
# rain_cutoff <- 1
# alpha_soil <- 0.2
# alpha_wet_reduce <- 0.8
# alpha_wet <- alpha_soil * alpha_wet_reduce
# 
# #calculate soil temperature etc
# example_season <- example_season %>% 
#   mutate(soil_wet = check_soil_wet(Prec),
#          Tmean = (Tmin + Tmax) / 2,
#          alpha = ifelse(soil_wet, yes = alpha_wet, no = alpha_soil),
#          T_soil = get_Tsoil(Tmean, alpha = alpha)) 
# 
# #plot evolution of temperatures
# example_season %>% 
# ggplot(aes(x = yday_plot)) +
#   geom_line(aes(y = Tmax, col = 'Tmax')) +
#   geom_line(aes(y = Tmin, col = 'Tmin')) +
#   geom_line(aes(y = T_soil, col = 'T_soil')) +
#   geom_col(aes(y = Prec, fill = 'Prec')) +
#   scale_fill_manual(values = 'grey50') +
#   theme_bw()
# ggsave('example_temperature.jpeg', height = 10, width = 15,
#        unit = 'cm', device = 'jpeg')








#------------------------#
#period since harvest
#------------------------#

weather_harvest <- weather_sub[i_havest:nrow(weather_sub),]




