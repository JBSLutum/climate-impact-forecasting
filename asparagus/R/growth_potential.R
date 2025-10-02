#Growth Potential####
#This function calculates the growth potential the asparagus plant accumulates
#during their vegetative crowd phase over the summer
#time point 24th of June till November

growth_potential <- function(pad,
                             pad_need,
                             water_stress_occ, 
                             disease_occ, 
                             insect_damage_occ, 
                             weather_damage_occ) {
  
  # Base growth potential from PAD
  if (pad[1] >= pad_need) {
    potential <- 1  # 100%
  } else {
    potential <- pad / 100  # Scale proportionally
  }
  
  # Apply damage reductions if they occurred
  
  potential <- potential *(1-water_stress_occ)
  
  potential <- potential  *(1-disease_occ)
  
  potential <- potential  *(1-insect_damage_occ)
  
  potential <- potential  *(1-weather_damage_occ)
  
  
  # Ensure potential stays within [0, 1]
  potential <- max(min(potential, 1), 0.1)
  
  return(potential)
}