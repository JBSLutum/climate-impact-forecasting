#plot results

library(raster)
library(rnaturalearthdata)
library(rnaturalearth)
library(tidyverse)
germany_states <- rnaturalearth::ne_states(country = "Germany", returnclass = "sf")
netherland <- rnaturalearth::ne_countries(country = "netherlands", returnclass = "sf", scale = 50)
belgium <- rnaturalearth::ne_countries(country = "belgium", returnclass = "sf", scale = 50)
germany <- rnaturalearth::ne_countries(country = "germany", returnclass = "sf", scale = 50)

# Filter for NRW
nrw <- filter(germany_states, name == "Nordrhein-Westfalen")
nrw <- sf::st_transform(nrw, crs = crs('+proj=longlat +datum=WGS84 +no_defs'))  # Match CRS

pixel_df <- read.csv('weathergenerator/pixel_id.csv')


#
mean_yield <- read.csv('asparagus/MC_results_NRW/means/sim_mean_compare_nrw.csv')

#merge with row id
merge(mean_yield, pixel_df, by = 'id') %>% 
  select(-X, -temperature) %>% 
  pivot_longer(cols = ssp1:ssp5, names_to = 'scenario') %>% 
  mutate(scen_pretty = factor(scenario, levels = c('ssp1', 'ssp2', 'ssp3', 'ssp5'),
                              labels = c('SSP1', 'SSP2', 'SSP3', 'SSP5'))) %>% 
  #raster::rasterFromXYZ() %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  geom_sf(data = germany_states, fill = NA) +
  #geom_sf(data = germany, fill = NA, lwd = 1.2) +
  geom_sf(data = netherland, fill = NA)+
  geom_sf(data = belgium, fill = NA)+
  coord_sf(xlim = c(5.7, 9.5), ylim = c(50.3, 52.6)) +
  scale_fill_viridis_c() +
  ylab('Latitude') +
  xlab('Longitude') +
  # north arrow
  ggspatial::annotation_north_arrow(
    location = "tr",      # top right
    which_north = "true",
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm"),
    pad_x = unit(1, "cm"),
    pad_y = unit(5, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  
  # scale bar
  ggspatial::annotation_scale(
    location = "bl",      # bottom left
    width_hint = 0.25,    # how wide the scale bar is relative to plot width
    line_width = 0.7,
    text_cex = 0.9,
    pad_x = unit(4, "cm"),
    pad_y = unit(0.2, "cm")
  ) +
  facet_wrap(~scen_pretty) +
  #facet_grid(~scen_pretty) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )
ggsave('plot_maps.jpeg', height = 20, width = 25, units = 'cm', device = 'jpeg')
