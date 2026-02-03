#summer function
summer<-function(
    pad,
    pad_need,
    summer_disease_occ,
    summer_insect_occ,
    summer_drought_stress_occ,
    summer_mean_temp
  )
{
  # Base growth potential from PAD
  if (pad >= pad_need) {
    potential <- pad/pad_need  # 100%
  } else {
    potential <- pad / 100  # Scale proportionally
  }
  potential <- max(min(potential, 1.2), 0.1)
  sigma<-5
  # Apply damage reductions if they occurred
  temp_factor<-exp(-((summer_mean_temp - 15)^2) / (2 * sigma^2))
  potential <- 1 * temp_factor
  
  # Apply damage reductions if they occurred
  
  potential <- potential -summer_disease_occ
  
  potential <- potential  -summer_insect_occ
  
  potential <- potential  -summer_drought_stress_occ
  
  
  
  # Ensure potential stays within [0, 1]
  potential <- max(min(potential, 1.3), 0.1)
  
  return(potential)
}