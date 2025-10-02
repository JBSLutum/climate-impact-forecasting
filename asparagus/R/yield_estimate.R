library(decisionSupport)

#Harvest Season####
#This function estimates the yield of asparagus over the season
#the total yield that comes from the field is determined by:
#accumulated growth potential, if the chill requirement is met, and the season length
#the marketable yield is determined by the quality of the spears
#weather influences the quality directly via frost, heat and rapid temperature fluctuations
#and indirectly by prohibiting harvesting on time because of rain or heat
#the fieldworker cant get on the field
yield_estimate <- function(
    standard_yield,
    gp,       #growth potential 0-1
    chill,            # chill ratio 0-1
    season_days,          # actual season length
    standard_season_length, # reference full-length season
    Tsoil_mean, #average soil temp over harvest season
    
    # Quality risks
    late_frost_occ,
    temp_fluctuation_occ,
    extreme_rainfall_occ,
    extreme_heat_occ
) {
  #  Base yield under perfect conditions scaled by growth factors
  season_factor <- season_days / standard_season_length
  season_factor <- max(min(season_factor, 1), 0.1)
 
  #Soil temperature influence, ideal is 20 degree, temperature over or under are
  #give reduction via Gaussian response function
  T_opt<-20
  sigma<-5
  #Tsoil_mean<-15
  temp_factor<-exp(-((Tsoil_mean - T_opt)^2) / (2 * sigma^2))
  
  actual_yield <- standard_yield * gp * chill * season_factor *temp_factor
  

  
  
  # Quality risk evaluation
  total_quality_loss <- 1
  
    total_quality_loss <- total_quality_loss * chill
  
    total_quality_loss <- total_quality_loss -late_frost_occ
  
    total_quality_loss <- total_quality_loss -temp_fluctuation_occ
  
    total_quality_loss <- total_quality_loss -extreme_rainfall_occ
  
    total_quality_loss <- total_quality_loss -extreme_heat_occ
  
  
  # Cap loss to max 100%
  total_quality_loss <- max(min(total_quality_loss, 1), 0.1)
  
  # Marketable yield after quality loss
  marketable_yield <- actual_yield * total_quality_loss
  
  # Return output
  return(list(
    actual_yield = actual_yield/1000,
    marketable_yield = marketable_yield/1000,
    quality_loss = total_quality_loss
  ))
}




####make_variables####
# make_variables<-function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
# 
# make_variables(estimate_read_csv("asparagus/asparagus_today.csv"))

