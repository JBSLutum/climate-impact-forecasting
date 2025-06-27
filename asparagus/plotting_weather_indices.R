library(tidyverse)
library(patchwork)
library(cowplot)

risk_df<-read.csv("weathergenerator/risk_df.csv")


r1<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = drought_stress)) +
  geom_boxplot()+
  ggtitle("Risk for\ndrought stress")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(fill="Szenarios")

r2<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = insect_risk)) +
  geom_boxplot()+
  ggtitle("Risk for\ninsect stress")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(fill="Szenarios")

r3<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = disease_risk)) +
  geom_boxplot()+
  ggtitle("Risk for\ndisease stress")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(fill="Szenarios")

r4<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = risk_rain)) +
  geom_boxplot()+
  ggtitle("Risk for\nweather event stress")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(fill="Szenarios")

r5<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = rainharvest_risk)) +
  geom_boxplot()+
  ggtitle("Risk for\nrain harvest delays")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(fill="Szenarios")

r6<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = heatharvest_risk)) +
  geom_boxplot()+
  ggtitle("Risk for\nheat harvest delays")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(fill="Szenarios")

r7<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = frost_risk)) +
  geom_boxplot()+
  ggtitle("Risk for\nlate frost")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(fill="Szenarios")

r8<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = diurnal_risk)) +
  geom_boxplot()+
  ggtitle("Risk for\ntemperature changes")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(fill="Szenarios")

g1<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = photosynhthesis_day)) +
  geom_boxplot()+
  ggtitle("Number of\nphotosynthesis days")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  labs(fill="Szenarios")

g2<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = yday_harvest_star)) +
  geom_boxplot()+
  ggtitle("Day of the year\nof harvest start")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  labs(fill="Szenarios")

g3<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = accumulated_chill)) +
  geom_boxplot()+
  ggtitle("Accumulated chill\nover winter")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  labs(fill="Szenarios")

g4<-risk_df %>%
  ggplot(aes(fill = ssp, x = ssp , y = Tsoil_mean)) +
  geom_boxplot()+
  ggtitle("Mean soil temperature\nduring harvest time")+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))+
  labs(fill="Szenarios")

# Extrahiere NUR die Legende


(r1|r2|r3|r4)/
  (r5|r6|r7|r8)/
  (g1|g2|g3|g4)+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave('asparagus/Figures/combined_indices.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')



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