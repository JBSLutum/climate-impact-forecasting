#Load required packages####
library(ggtext)
library(png)
library(grid)
library(patchwork)
library(tidyverse)
library(decisionSupport)
library(dplyr)
library(ggh4x)

#Prepare formatting functions####
options(scipen=100000)
remove_x <- theme(
  axis.text.x = element_blank(),
  #axis.ticks.x = element_blank(),
  axis.title.x = element_blank()
)

remove_title<-theme(strip.text.x = element_blank())

remove_legend<-theme(legend.position = "none")

remove_y <- theme(
  axis.text.y = element_blank(),
  #axis.ticks.y = element_blank(),
  axis.title.y = element_blank()
)
remove_y_only_title<-theme(axis.title.y = element_blank())
remove_x_only_title<-theme(axis.title.x = element_blank())

#read saved Monte Carlo simulation results
sim_results_today<-readRDS("asparagus/MC_results/MC_results_today.RDS")
sim_results_126<-readRDS("asparagus/MC_results/MC_results_126.RDS")
sim_results_245<-readRDS("asparagus/MC_results/MC_results_245.RDS")
sim_results_370<-readRDS("asparagus/MC_results/MC_results_370.RDS")
sim_results_585<-readRDS("asparagus/MC_results/MC_results_585.RDS")


#Function to sort the PLS-Results
source("functions/Sorting_VIP_data.R")

#Read in model input data
sim_today_input<- read.csv("asparagus/asparagus_today.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
sim_126_input<- read.csv("asparagus/asparagus_126.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
sim_245_input<- read.csv("asparagus/asparagus_245.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
sim_370_input<- read.csv("asparagus/asparagus_370.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
sim_585_input<- read.csv("asparagus/asparagus_585.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")

#delete rows with NA
'sim_results_today$y <- sim_results_today$y[complete.cases(sim_results_today$x), ]
sim_results_today$x <- sim_results_today$x[complete.cases(sim_results_today$x), ]
sim_results_126$y <- sim_results_126$y[complete.cases(sim_results_126$x), ]
sim_results_126$x <- sim_results_126$x[complete.cases(sim_results_126$x), ]
sim_results_245$y <- sim_results_245$y[complete.cases(sim_results_245$x), ]
sim_results_245$x <- sim_results_245$x[complete.cases(sim_results_245$x), ]
sim_results_370$y <- sim_results_370$y[complete.cases(sim_results_370$x), ]
sim_results_370$x <- sim_results_370$x[complete.cases(sim_results_370$x), ]
sim_results_585$y <- sim_results_585$y[complete.cases(sim_results_585$x), ]
sim_results_585$x <- sim_results_585$x[complete.cases(sim_results_585$x), ]'


####pls+vip+plot marketable yield####
#PLS regression to compute the Variable Importance in Projection (VIP) for high-quality yield#
pls_result_today_yield <- plsr.mcSimulation(object = sim_results_today,
                                                  resultName = names(sim_results_today$y)[2], ncomp = 1)
pls_result_126_yield <- plsr.mcSimulation(object = sim_results_126,
                                          resultName = names(sim_results_126$y)[2], ncomp = 1)
pls_result_245_yield <- plsr.mcSimulation(object = sim_results_245,
                                            resultName = names(sim_results_245$y)[2], ncomp = 1)
pls_result_370_yield <- plsr.mcSimulation(object = sim_results_370,
                                          resultName = names(sim_results_370$y)[2], ncomp = 1)
pls_result_585_yield <- plsr.mcSimulation(object = sim_results_585,
                                          resultName = names(sim_results_585$y)[2], ncomp = 1)


#restructure PLS results
pls_result_today_yield_table<-VIP_table(pls_result_today_yield, input_table = sim_today_input, threshold = 0)
pls_result_126_yield_table<-VIP_table(pls_result_126_yield, input_table = sim_126_input, threshold = 0)
pls_result_245_yield_table<-VIP_table(pls_result_245_yield, input_table = sim_245_input, threshold = 0)
pls_result_370_yield_table<-VIP_table(pls_result_370_yield, input_table = sim_370_input, threshold = 0)
pls_result_585_yield_table<-VIP_table(pls_result_585_yield, input_table = sim_585_input, threshold = 0)



#separate the variable descriptions
variablen_VIP<-pls_result_today_yield_table$Description

#extract important values from the PLS results
VIP_yieldsim1<-pls_result_today_yield_table$VIP
Coef_yieldsim1<-pls_result_today_yield_table$Coefficient
VIP_yieldsim126<-pls_result_126_yield_table$VIP
Coef_yieldsim126<-pls_result_126_yield_table$Coefficient
VIP_yieldsim245<-pls_result_245_yield_table$VIP
Coef_yieldsim245<-pls_result_245_yield_table$Coefficient
VIP_yieldsim370<-pls_result_370_yield_table$VIP
Coef_yieldsim370<-pls_result_370_yield_table$Coefficient
VIP_yieldsim585<-pls_result_585_yield_table$VIP
Coef_yieldsim585<-pls_result_585_yield_table$Coefficient

#create a data frame, with variable description, VIP and Coefficient 
VIP_yield_sim_all<-data.frame(variablen_VIP,
                              VIP_yieldsim1,
                              VIP_yieldsim126,
                              VIP_yieldsim245,
                              VIP_yieldsim370,
                              VIP_yieldsim585)
Coef_yield_sim_all<-data.frame(variablen_VIP,
                              Coef_yieldsim1,
                              Coef_yieldsim126,
                              Coef_yieldsim245,
                              Coef_yieldsim370,
                              Coef_yieldsim585)
VIP_and_Coef_yield<-data.frame(variablen_VIP,
                               VIP_yieldsim1,
                               Coef_yieldsim1,
                               VIP_yieldsim126,
                               Coef_yieldsim126,
                               VIP_yieldsim245,
                               Coef_yieldsim245,
                               VIP_yieldsim370,
                               Coef_yieldsim370,
                               VIP_yieldsim585,
                               Coef_yieldsim585)

#set the threshold for important variables to a VIP > 1
VIP_and_Coef_yield_threshold<-subset(VIP_and_Coef_yield, abs(VIP_and_Coef_yield$VIP_yieldsim1)>1|
                                       abs(VIP_and_Coef_yield$VIP_yieldsim126)>1|
                                       abs(VIP_and_Coef_yield$VIP_yieldsim245)>1|
                                       abs(VIP_and_Coef_yield$VIP_yieldsim370)>1|
                                       abs(VIP_and_Coef_yield$VIP_yieldsim585)>1)

#Restructure data frame
VIP_and_Coef_yield_threshold_longer<-VIP_and_Coef_yield_threshold%>%
  pivot_longer(cols = !variablen_VIP, names_to = c(".value","yieldsim"), names_sep = "_")

#add new column with the information if the coefficient is positive or negative
VIP_and_Coef_yield_threshold_longer$PosNeg<-ifelse(VIP_and_Coef_yield_threshold_longer$Coef>0,"positive","negative")

#add new column for VIP where all variables with a VIP <1 get the value "NA"
VIP_and_Coef_yield_threshold_longer$VIP_threshold_corr<-ifelse(VIP_and_Coef_yield_threshold_longer$VIP>=1,VIP_and_Coef_yield_threshold_longer$VIP, NA )

#could read in images as labels for the plot
labels <- c("today","126", "245", "370","585")

#Plot the VIP results
png("asparagus/Figures/VIP_yield.png", pointsize=10, width=4500, height=6100, res=600)

ggplot(VIP_and_Coef_yield_threshold_longer, aes(yieldsim, forcats::fct_rev(variablen_VIP), color = PosNeg, size = VIP_threshold_corr)) +
  geom_point(shape = 16, stroke = 0) +
  geom_hline(yintercept = seq(.5, 30.5, 1), linewidth = .2, color= "gray75") +
  #scale_x_discrete() +
  scale_radius(range = c(1, 9)) +
  scale_color_manual(values = c("negative"="red", "positive"="blue"))  +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(color = "black"),
        axis.text.x = ggtext::element_markdown()) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
                             label.position = "bottom",
                             title.position = "top", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, order = 2)) +
  labs(size = "Area = VIP", color = "Coefficient", x = NULL, y = NULL)+
  scale_x_discrete(labels=labels, position = "top")+
  geom_vline(xintercept = seq(0.5,5.5,1), linewidth=.2, color="gray75")+
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot")+
  labs(caption = "*This variable has a negative value. A less negative i.e. higher value positively influences\nthe high-quality yield, as the fruit growth is inhibited less strongly.")

dev.off()
####pls+vip+plot total yield####
#PLS regression to compute the Variable Importance in Projection (VIP) for total yield#
pls_result_today_tyield <- plsr.mcSimulation(object = sim_results_today,
                                           resultName = names(sim_results_today$y)[1], ncomp = 1)
pls_result_245_tyield <- plsr.mcSimulation(object = sim_results_245,
                                         resultName = names(sim_results_245$y)[1], ncomp = 1)
pls_result_370_tyield <- plsr.mcSimulation(object = sim_results_370,
                                         resultName = names(sim_results_370$y)[1], ncomp = 1)
pls_result_585_tyield <- plsr.mcSimulation(object = sim_results_585,
                                         resultName = names(sim_results_585$y)[1], ncomp = 1)


#restructure PLS results
pls_result_today_tyield_table<-VIP_table(pls_result_today_tyield, input_table = sim_today_input, threshold = 0)
pls_result_245_tyield_table<-VIP_table(pls_result_245_tyield, input_table = sim_245_input, threshold = 0)
pls_result_370_tyield_table<-VIP_table(pls_result_370_tyield, input_table = sim_370_input, threshold = 0)
pls_result_585_tyield_table<-VIP_table(pls_result_585_tyield, input_table = sim_585_input, threshold = 0)



#separate the variable descriptions
variablen_VIP<-pls_result_today_tyield_table$Description

#extract important values from the PLS results
VIP_tyieldsim1<-pls_result_today_tyield_table$VIP
Coef_tyieldsim1<-pls_result_today_tyield_table$Coefficient
VIP_tyieldsim245<-pls_result_245_tyield_table$VIP
Coef_tyieldsim245<-pls_result_245_tyield_table$Coefficient
VIP_tyieldsim370<-pls_result_370_tyield_table$VIP
Coef_tyieldsim370<-pls_result_370_tyield_table$Coefficient
VIP_tyieldsim585<-pls_result_585_tyield_table$VIP
Coef_tyieldsim585<-pls_result_585_tyield_table$Coefficient

#create a data frame, with variable description, VIP and Coefficient 
VIP_tyield_sim_all<-data.frame(variablen_VIP,
                             VIP_tyieldsim1,
                             VIP_tyieldsim245,
                             VIP_tyieldsim370,
                             VIP_tyieldsim585)
Coef_tyield_sim_all<-data.frame(variablen_VIP,
                              Coef_tyieldsim1,
                              Coef_tyieldsim245,
                              Coef_tyieldsim370,
                              Coef_tyieldsim585)
VIP_and_Coef_tyield<-data.frame(variablen_VIP,
                              VIP_tyieldsim1,
                              Coef_tyieldsim1,
                              VIP_tyieldsim245,
                              Coef_tyieldsim245,
                              VIP_tyieldsim370,
                              Coef_tyieldsim370,
                              VIP_tyieldsim585,
                              Coef_tyieldsim585)

#set the threshold for important variables to a VIP > 1
VIP_and_Coef_tyield_threshold<-subset(VIP_and_Coef_tyield, abs(VIP_and_Coef_tyield$VIP_tyieldsim1)>0.5|
                                      abs(VIP_and_Coef_tyield$VIP_tyieldsim245)>0.5|
                                      abs(VIP_and_Coef_tyield$VIP_tyieldsim370)>0.5|
                                      abs(VIP_and_Coef_tyield$VIP_tyieldsim585)>0.5)

#Restructure data frame
VIP_and_Coef_tyield_threshold_longer<-VIP_and_Coef_tyield_threshold%>%
  pivot_longer(cols = !variablen_VIP, names_to = c(".value","tyieldsim"), names_sep = "_")

#add new column with the information if the coefficient is positive or negative
VIP_and_Coef_tyield_threshold_longer$PosNeg<-ifelse(VIP_and_Coef_tyield_threshold_longer$Coef>0,"positive","negative")

#add new column for VIP where all variables with a VIP <1 get the value "NA"
VIP_and_Coef_tyield_threshold_longer$VIP_threshold_corr<-ifelse(VIP_and_Coef_tyield_threshold_longer$VIP>=0.5,VIP_and_Coef_tyield_threshold_longer$VIP, NA )

#could read in images as labels for the plot
labels <- c("today", "245", "370","585")

#Plot the VIP results
png("asparagus/Figures/VIP_tyield.png", pointsize=10, width=4500, height=6100, res=600)

ggplot(VIP_and_Coef_tyield_threshold_longer, aes(tyieldsim, forcats::fct_rev(variablen_VIP), color = PosNeg, size = VIP_threshold_corr)) +
  geom_point(shape = 16, stroke = 0) +
  geom_hline(yintercept = seq(.5, 30.5, 1), linewidth = .2, color= "gray75") +
  #scale_x_discrete() +
  scale_radius(range = c(0.5, 9)) +
  scale_color_manual(values = c("negative"="red", "positive"="blue"))  +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(color = "black"),
        axis.text.x = ggtext::element_markdown()) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
                             label.position = "bottom",
                             title.position = "top", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, order = 2)) +
  labs(size = "Area = VIP", color = "Coefficient", x = NULL, y = NULL)+
  scale_x_discrete(labels=labels, position = "top")+
  geom_vline(xintercept = seq(0.5,5.5,1), linewidth=.2, color="gray75")+
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot")
  #labs(caption = "")

dev.off()

####pls+vip+plot quality####
#PLS regression to compute the Variable Importance in Projection (VIP) for quality#
pls_result_today_qual <- plsr.mcSimulation(object = sim_results_today,
                                            resultName = names(sim_results_today$y)[3], ncomp = 1)
pls_result_245_qual <- plsr.mcSimulation(object = sim_results_245,
                                          resultName = names(sim_results_245$y)[3], ncomp = 1)
pls_result_370_qual <- plsr.mcSimulation(object = sim_results_370,
                                          resultName = names(sim_results_370$y)[3], ncomp = 1)
pls_result_585_qual <- plsr.mcSimulation(object = sim_results_585,
                                          resultName = names(sim_results_585$y)[3], ncomp = 1)


#restructure PLS results
pls_result_today_qual_table<-VIP_table(pls_result_today_qual, input_table = sim_today_input, threshold = 0)
pls_result_245_qual_table<-VIP_table(pls_result_245_qual, input_table = sim_245_input, threshold = 0)
pls_result_370_qual_table<-VIP_table(pls_result_370_qual, input_table = sim_370_input, threshold = 0)
pls_result_585_qual_table<-VIP_table(pls_result_585_qual, input_table = sim_585_input, threshold = 0)



#separate the variable descriptions
variablen_VIP<-pls_result_today_qual_table$Description

#extract important values from the PLS results
VIP_qualsim1<-pls_result_today_qual_table$VIP
Coef_qualsim1<-pls_result_today_qual_table$Coefficient
VIP_qualsim245<-pls_result_245_qual_table$VIP
Coef_qualsim245<-pls_result_245_qual_table$Coefficient
VIP_qualsim370<-pls_result_370_qual_table$VIP
Coef_qualsim370<-pls_result_370_qual_table$Coefficient
VIP_qualsim585<-pls_result_585_qual_table$VIP
Coef_qualsim585<-pls_result_585_qual_table$Coefficient

#create a data frame, with variable description, VIP and Coefficient 
VIP_qual_sim_all<-data.frame(variablen_VIP,
                              VIP_qualsim1,
                              VIP_qualsim245,
                              VIP_qualsim370,
                              VIP_qualsim585)
Coef_qual_sim_all<-data.frame(variablen_VIP,
                               Coef_qualsim1,
                               Coef_qualsim245,
                               Coef_qualsim370,
                               Coef_qualsim585)
VIP_and_Coef_qual<-data.frame(variablen_VIP,
                               VIP_qualsim1,
                               Coef_qualsim1,
                               VIP_qualsim245,
                               Coef_qualsim245,
                               VIP_qualsim370,
                               Coef_qualsim370,
                               VIP_qualsim585,
                               Coef_qualsim585)

#set the threshold for important variables to a VIP > 1
VIP_and_Coef_qual_threshold<-subset(VIP_and_Coef_qual, abs(VIP_and_Coef_qual$VIP_qualsim1)>0.5|
                                       abs(VIP_and_Coef_qual$VIP_qualsim245)>0.5|
                                       abs(VIP_and_Coef_qual$VIP_qualsim370)>0.5|
                                       abs(VIP_and_Coef_qual$VIP_qualsim585)>0.5)

#Restructure data frame
VIP_and_Coef_qual_threshold_longer<-VIP_and_Coef_qual_threshold%>%
  pivot_longer(cols = !variablen_VIP, names_to = c(".value","qualsim"), names_sep = "_")

#add new column with the information if the coefficient is positive or negative
VIP_and_Coef_qual_threshold_longer$PosNeg<-ifelse(VIP_and_Coef_qual_threshold_longer$Coef>0,"positive","negative")

#add new column for VIP where all variables with a VIP <1 get the value "NA"
VIP_and_Coef_qual_threshold_longer$VIP_threshold_corr<-ifelse(VIP_and_Coef_qual_threshold_longer$VIP>=0.5,VIP_and_Coef_qual_threshold_longer$VIP, NA )

#could read in images as labels for the plot
labels <- c("today", "245", "370","585")

#Plot the VIP results
png("asparagus/Figures/VIP_qual.png", pointsize=10, width=4500, height=6100, res=600)

ggplot(VIP_and_Coef_qual_threshold_longer, aes(qualsim, forcats::fct_rev(variablen_VIP), color = PosNeg, size = VIP_threshold_corr)) +
  geom_point(shape = 16, stroke = 0) +
  geom_hline(yintercept = seq(.5, 30.5, 1), linewidth = .2, color= "gray75") +
  #scale_x_discrete() +
  scale_radius(range = c(0.5, 9)) +
  scale_color_manual(values = c("negative"="red", "positive"="blue"))  +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(color = "black"),
        axis.text.x = ggtext::element_markdown()) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
                             label.position = "bottom",
                             title.position = "top", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, order = 2)) +
  labs(size = "Area = VIP", color = "Coefficient", x = NULL, y = NULL)+
  scale_x_discrete(labels=labels, position = "top")+
  geom_vline(xintercept = seq(0.5,5.5,1), linewidth=.2, color="gray75")+
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot")+
  labs(caption = "*This variable has a negative value. A less negative i.e. higher value positively influences\nthe high-quality yield, as the fruit growth is inhibited less strongly.")

dev.off()

####yield graphs####
#loading yield data
results_marktyield_today<-sim_results_today$y[c(1,2)]
results_marktyield_126<-sim_results_126$y[c(1,2)]
results_marktyield_245<-sim_results_245$y[c(1,2)]
results_marktyield_370<-sim_results_370$y[c(1,2)]
results_marktyield_585<-sim_results_585$y[c(1,2)]
#adding row for scenario identification
results_marktyield_today$scenario<-as.character("Year 2020")
results_marktyield_126$scenario<-as.character("Year 2075\nSSP126")
results_marktyield_245$scenario<-as.character("Year 2075\nSSP245")
results_marktyield_370$scenario<-as.character("Year 2075\nSSP370")
results_marktyield_585$scenario<-as.character("Year 2075\nSSP585")
#all together
results_yield_all<-rbind(results_marktyield_today,results_marktyield_126,results_marktyield_245,results_marktyield_370,results_marktyield_585)
#rename column
names(results_yield_all)<-c("total_yield", "marketable_yield", "scenario")
#easy ggplot
'ggplot(results_yield_all, aes(x=scenario, y=total_yield, fill=scenario))+
  geom_boxplot()'
#direktvergleich
results_yield_all_longer<- pivot_longer(results_yield_all, cols = c(total_yield, marketable_yield))
results_yield_all_longer$name<-factor(results_yield_all_longer$name, levels= c("total_yield","marketable_yield"))

results_yield_all_longer <- results_yield_all_longer %>%
  mutate(period = ifelse(scenario == "Year 2020", "2020", "2075"))
results_yield_all_longer$period<- as.factor(results_yield_all_longer$period)
results_yield_all_longer$scenario<- as.factor(results_yield_all_longer$scenario)

#mittelwerte
summary_df <- results_yield_all_longer %>%
  group_by(scenario, name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = name, values_from = mean_value) %>%
  mutate(percent = (`marketable_yield` / `total_yield`) * 100) 


ggplot(results_yield_all_longer, aes(x=scenario, y=value, fill=name))+
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_grid(~period,scales="free_x",space="free_x")+
  geom_text(data = summary_df,
            aes(x = scenario,
                y = max(results_yield_all_longer$value, na.rm = TRUE) + 1,
                label = paste0(round(percent), "%")),
            inherit.aes = FALSE,
            size = 4)+
  theme(legend.title = element_blank(),
        legend.position = "right",
        strip.background = element_rect(fill = "lightgrey"),
        strip.text = element_text(size = 12, face = "bold"))+
  scale_x_discrete(name="Climate scenario")+
  scale_y_continuous(name="Yield dt/ha")+
  geom_hline(yintercept = 56.3, linetype="dashed", )+
  annotate(geom="text", x =  -Inf, y=55, label=c("56.3 dt/ha\nYield 2020"), color="black", 
           fontface="plain",hjust = 0,vjust=1, angle=0,size=3)

ggplot(results_yield_all_longer, aes(x = scenario, y = value, fill = name)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_grid(~ period, scales = "free_x", space = "free_x") +
  theme(strip.background = element_blank())+
  scale_x_discrete(name="Climate scenario")+
  scale_y_continuous(name="Yield dt/ha")+
  geom_hline(yintercept = 56.3, linetype="dashed", )
  
  

ggplot(results_yield_all_longer, aes(x=scenario, y=value, fill=name))+
  geom_violin(position = position_dodge(width = 0.8)) +
  geom_boxplot(width=0.1,position = position_dodge(width = 0.8))+
  theme(legend.title = element_blank(),legend.position = "right")+
  scale_x_discrete(name="Climate scenario")+
  scale_y_continuous(name="Yield dt/ha")+
  facet_grid(~ period, scales = "free_x", space = "free_x") +
  geom_hline(yintercept = 60, linetype="dashed", )+
  annotate(geom="text", x =  -Inf, y=55, label=c("60 dt/ha\nYield 2020"), color="black", fontface="plain",hjust = 0,vjust=1, angle=0,size=3)



#devtools::install_github("psyteachr/introdataviz")
library(introdataviz)
ggplot(results_yield_all_longer, aes(y=value, x=scenario, fill=name))+
  geom_split_violin()+
  geom_boxplot(width=0.5)+
  theme(legend.title = element_blank(),legend.position = "top")+
  scale_x_discrete(name="Climate scenario")+
  scale_y_continuous(name="Yield dt/ha")+
  geom_hline(yintercept = 60, linetype="dashed",linewidth=1 )+
  facet_grid(~ period, scales = "free_x", space = "free_x")
  annotate(geom="text", x = -Inf, y=55, label=c("60 dt/ha\nYield 2020"), color="black", fontface="plain",hjust = 0,vjust=1, angle=0, size =3)



# library(ggstance)
# ggplot(results_yield_all_longer, aes(x=value, fill=name))+
#   geom_boxploth(aes(x = value, y = 2.5), width = 5)+
#   facet_wrap(~scenario, ncol=1)
# 
ggplot(results_yield_all_longer, aes(x=value,fill=name))+
  geom_histogram(position="identity", binwidth = 1, alpha=.5)+
  theme(legend.title = element_blank(),legend.position = "right")+
  scale_x_continuous(name="Yield dt/ha")+
  scale_y_continuous(name="")+
  geom_vline(xintercept = 60, linetype="dashed", )+
  facet_wrap(~scenario, ncol=1)

ggplot(results_yield_all_longer, aes(x=value,fill=name))+
  geom_density()+
  facet_wrap(~scenario, ncol=1)


