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
  
  # Apply damage reductions if they occurred
  
  potential <- potential *(1-summer_disease_occ)
  
  potential <- potential  *(1-summer_insect_occ)
  
  potential <- potential  *(1-summer_drought_stress_occ)
  
  
  # Ensure potential stays within [0, 1]
  potential <- max(min(potential, 2), 0.1)
  
  return(potential)
}