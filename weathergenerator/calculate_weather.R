#calculate stuff for jb
library(chillR)
c

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

hist_weather <- read.csv('weathergenerator/weather_2020_koeln-bonn.csv') %>% 
  mutate(scenario_year = 2020, 
         ssp = 'historical',
         gcm = 'historical',
         yday = lubridate::yday(DATE),
         season = ifelse(yday >= 175,
                         yes = Year  +1,
                         no = Year),
         id = paste(ssp, gcm, scenario_year, sep = '--'))
drop_list <- hist_weather %>% 
  group_by(id, season) %>% 
  summarise(n = n()) %>% 
  filter(n < 365)
hist_weather <- hist_weather %>% 
  filter(!(paste(id, season) %in% paste(drop_list$id, drop_list$season)))

#combine hist and future weather
weather_combined <- future_weather %>% 
  rbind(hist_weather)

source('functions/weather_indices_function.R')

weather_info <- read.csv('weathergenerator/weather/weather-station-info.csv')
lat <- weather_info$lat[1]
example_season <- read.csv('weathergenerator/example_season.csv')

#test for one season
get_weather_indices(weather = example_season, 
                    latitude = lat)

#add unique id to each seaoson
weather_combined$id_seaon <- paste(weather_combined$id, weather_combined$season, sep = '--')

#split by each season
test <- split(x = weather_combined, f = weather_combined$id_seaon) 

# test_out <- purrr::map(test[1:10], function(x) get_weather_indices(x, lat),
#                        .progress = TRUE)

#apply function to each season
test_out <- purrr::map(test, function(x) get_weather_indices(weather = x, latitude = lat),
                       .progress = TRUE)
#weather <- test[[1967]]
#x <- test[3438]

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
Tsoil_mean <- purrr::map_dbl(test_out, 'Tsoil_mean')

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
           heatharvest_risk,
           Tsoil_mean
) %>% 
  mutate(id = 1:length(Tsoil_mean))

risk_df %>% 
  group_by(ssp) %>% 
  summarise(n = n())

risk_df %>% 
  group_by(ssp) %>% 
  summarise(id_min = min(id),
            id_max = max(id))

id <- 1495
risk_df$disease_risk[id]

write.csv(risk_df,"weathergenerator/risk_df.csv")
risk_df<-read.csv("weathergenerator/risk_df.csv")


# risk_df %>% 
#   ggplot(aes(fill = ssp, x = drought_stress)) +
#   geom_density(alpha = 0.3)
# 
# risk_df %>% 
#   ggplot(aes(fill = ssp, x = insect_risk)) +
#   geom_density(alpha = 0.3)
# 
# risk_df %>% 
#   ggplot(aes(fill = ssp, x = photosynhthesis_day)) +
#   geom_density(alpha = 0.3)
# 
# risk_df %>% 
#   ggplot(aes(fill = ssp, x = disease_risk)) +
#   geom_density(alpha = 0.3)
# 
# risk_df %>% 
#   ggplot(aes(fill = ssp, x = risk_rain)) +
#   geom_density(alpha = 0.3)
# 
# risk_df %>% 
#   drop_na() %>% 
#   ggplot(aes(fill = ssp, x = yday_harvest_star)) +
#   geom_density(alpha = 0.3)
# 
# risk_df %>% 
#   drop_na() %>% 
#   ggplot(aes(fill = ssp, x = yday_speargrowth)) +
#   geom_density(alpha = 0.3)
# 
# risk_df %>% 
#   drop_na() %>% 
#   ggplot(aes(fill = ssp, x = accumulated_chill)) +
#   geom_density(alpha = 0.3)


#combined plot
risk_df %>% 
  select(-id)%>% 
  pivot_longer(cols = drought_stress:Tsoil_mean) %>% 
  drop_na() %>% 
  ggplot(aes(fill = ssp, x = value)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~name, scales = 'free') +
  theme_bw(base_size = 15)
ggsave('combined_indices.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')

risk_df %>% 
  select(-id, -yday_speargrowth)%>%
  pivot_longer(cols = c(-ssp,-X)) %>%
  drop_na() %>% 
  ggplot(aes(fill = ssp, y = value, x=ssp)) +
  geom_boxplot(alpha = 0.3) +
  facet_wrap(~name, scales = 'free') +
  theme_bw(base_size = 15)+
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(), legend.position = "bottom")
ggsave('asparagus/Figures/combined_indices.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')

risk_df %>% 
  select(ssp, X,photosynhthesis_day, yday_harvest_star, Tsoil_mean)%>%
  pivot_longer(cols = c(-ssp,-X)) %>%
  drop_na() %>% 
  ggplot(aes(fill = ssp, y = value, x=ssp)) +
  geom_boxplot(alpha = 0.3) +
  facet_wrap(~name, scales = 'free', labeller = labeller(name=
                                                           c("photosynhthesis_day"="Anzahl\nPhotosynthese Tage", 
                                                             "yday_harvest_star"= "Tage des Jahres\nErntestart",
                                                             "Tsoil_mean"="Mittlere\nBodentemperatur"))) +
  theme_bw(base_size = 15)+
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(), legend.position = "bottom")
ggsave('asparagus/Figures/weather_grow.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')

risk_df %>% 
  select(ssp, X,frost_risk,heatharvest_risk, insect_risk)%>%
  pivot_longer(cols = c(-ssp,-X)) %>%
  drop_na() %>% 
  ggplot(aes(fill = ssp, y = value, x=ssp)) +
  geom_boxplot(alpha = 0.3) +
  facet_wrap(~name, scales = 'free', labeller = labeller(name=
                                                           c("frost_risk"="Risiko für\nSpätfrost", 
                                                             "heatharvest_risk"= "Risiko für\nErnteverzögerung\ndurch Hitze",
                                                             "insect_risk"="Risiko für\nInsektenbefall"))) +
  theme_bw(base_size = 15)+
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(), legend.position = "bottom")
ggsave('asparagus/Figures/weather_risks.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')


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

library(grid)
drop(weather_plot)
weather_combined$Tmean<-weather_combined$Tmax - weather_combined$Tmin
weather_plot <- weather_combined %>% filter(ssp != "historical") %>% 
   group_by(yday, gcm, ssp, id) %>%
   summarise(Mean_Tmax = median(Tmax, na.rm = TRUE), .groups = "drop")

ggplot(weather_plot, aes(y=Mean_Tmax, x=yday, col=gcm))+
  geom_line(linewidth=0.5, alpha=0.5)+
  facet_wrap(~ssp, ncol=2, nrow=2)+
  theme_minimal()+
  theme(legend.position = "none")




#------------------------#
#period since harvest
#------------------------#

weather_harvest <- weather_sub[i_havest:nrow(weather_sub),]




