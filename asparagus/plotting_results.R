#Load required packages####
library(ggtext)
library(png)
library(grid)
library(patchwork)
library(tidyverse)
library(decisionSupport)

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
sim_results_245<-readRDS("asparagus/MC_results/MC_results_245.RDS")
sim_results_370<-readRDS("asparagus/MC_results/MC_results_370.RDS")
sim_results_585<-readRDS("asparagus/MC_results/MC_results_585.RDS")


#Function to sort the PLS-Results
source("functions/Sorting_VIP_data.R")

#Read in model input data
sim_today_input<- read.csv("asparagus/asparagus_today.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
sim_245_input<- read.csv("asparagus/asparagus_245.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
sim_370_input<- read.csv("asparagus/asparagus_370.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")
sim_585_input<- read.csv("asparagus/asparagus_585.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")



#PLS regression to compute the Variable Importance in Projection (VIP) for high-quality yield#
pls_result_today_yield <- plsr.mcSimulation(object = sim_results_today,
                                                  resultName = names(sim_results_today$y)[2], ncomp = 1)
pls_result_245_yield <- plsr.mcSimulation(object = sim_results_245,
                                            resultName = names(sim_results_245$y)[2], ncomp = 1)
pls_result_370_yield <- plsr.mcSimulation(object = sim_results_370,
                                          resultName = names(sim_results_370$y)[2], ncomp = 1)
pls_result_585_yield <- plsr.mcSimulation(object = sim_results_585,
                                          resultName = names(sim_results_585$y)[2], ncomp = 1)


#restructure PLS results
pls_result_today_yield_table<-VIP_table(pls_result_today_yield, input_table = sim_today_input, threshold = 0)
pls_result_245_yield_table<-VIP_table(pls_result_245_yield, input_table = sim_245_input, threshold = 0)
pls_result_370_yield_table<-VIP_table(pls_result_370_yield, input_table = sim_370_input, threshold = 0)
pls_result_585_yield_table<-VIP_table(pls_result_585_yield, input_table = sim_585_input, threshold = 0)



#separate the variable descriptions
variablen_VIP<-pls_result_today_yield_table$Description

#extract important values from the PLS results
VIP_yieldsimtoday<-pls_result_today_yield_table$VIP
Coef_yieldsimtoday<-pls_result_today_yield_table$Coefficient
VIP_yieldsim245<-pls_result_245_yield_table$VIP
Coef_yieldsim245<-pls_result_245_yield_table$Coefficient
VIP_yieldsim370<-pls_result_370_yield_table$VIP
Coef_yieldsim370<-pls_result_370_yield_table$Coefficient
VIP_yieldsim585<-pls_result_585_yield_table$VIP
Coef_yieldsim585<-pls_result_585_yield_table$Coefficient

#create a data frame, with variable description, VIP and Coefficient 
VIP_yield_sim_all<-data.frame(variablen_VIP,
                              VIP_yieldsimtoday,
                              VIP_yieldsim245,
                              VIP_yieldsim370,
                              VIP_yieldsim585)
Coef_yield_sim_all<-data.frame(variablen_VIP,
                              Coef_yieldsimtoday,
                              Coef_yieldsim245,
                              Coef_yieldsim370,
                              Coef_yieldsim585)
VIP_and_Coef_yield<-data.frame(variablen_VIP,
                               VIP_yieldsimtoday,
                               Coef_yieldsimtoday,
                               VIP_yieldsim245,
                               Coef_yieldsim245,
                               VIP_yieldsim370,
                               Coef_yieldsim370,
                               VIP_yieldsim585,
                               Coef_yieldsim585)

#set the threshold for important variables to a VIP > 1
VIP_and_Coef_yield_threshold<-subset(VIP_and_Coef_yield, abs(VIP_and_Coef_yield$VIP_yieldsimtoday)>0.5|
                                       abs(VIP_and_Coef_yield$VIP_yieldsim245)>0.5|
                                       abs(VIP_and_Coef_yield$VIP_yieldsim370)>0.5|
                                       abs(VIP_and_Coef_yield$VIP_yieldsim585)>0.5)

#Restructure data frame
VIP_and_Coef_yield_threshold_longer<-VIP_and_Coef_yield_threshold%>%
  pivot_longer(cols = !variablen_VIP, names_to = c(".value","yieldsim"), names_sep = "_")

#add new column with the information if the coefficient is positive or negative
VIP_and_Coef_yield_threshold_longer$PosNeg<-ifelse(VIP_and_Coef_yield_threshold_longer$Coef>0,"positive","negative")

#add new column for VIP where all variables with a VIP <1 get the value "NA"
VIP_and_Coef_yield_threshold_longer$VIP_threshold_corr<-ifelse(VIP_and_Coef_yield_threshold_longer$VIP>=0.5,VIP_and_Coef_yield_threshold_longer$VIP, NA )

#could read in images as labels for the plot
labels <- c("2025", "2075_SSP245", "2075_SSP370","2075_SSP585")

#Plot the VIP results
png("asparagus/Figures/VIP_yield.png", pointsize=10, width=4500, height=6100, res=600)

ggplot(VIP_and_Coef_yield_threshold_longer, aes(yieldsim, forcats::fct_rev(variablen_VIP), color = PosNeg, size = VIP_threshold_corr)) +
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
results_marktyield_245<-sim_results_245$y[c(1,2)]
results_marktyield_370<-sim_results_370$y[c(1,2)]
results_marktyield_585<-sim_results_585$y[c(1,2)]
#adding row for scenario identification
results_marktyield_today$scenario<-as.character("2025")
results_marktyield_245$scenario<-as.character("2075_245")
results_marktyield_370$scenario<-as.character("2075_370")
results_marktyield_585$scenario<-as.character("2075_585")
#all together
results_yield_all<-rbind(results_marktyield_today,results_marktyield_245,results_marktyield_370,results_marktyield_585)
#rename column
names(results_yield_all)<-c("total_yield", "marketable_yield", "scenario")
#easy ggplot
ggplot(results_yield_all, aes(x=scenario, y=total_yield, fill=scenario))+
  geom_boxplot()
#direktvergleich
results_yield_all_longer<- pivot_longer(results_yield_all, cols = c(total_yield, marketable_yield))

ggplot(results_yield_all_longer, aes(x=scenario, y=value, fill=name))+
  geom_boxplot()
