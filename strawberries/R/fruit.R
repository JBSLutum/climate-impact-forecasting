#fruit
fruit<-function(
    fruit_snail_occ,
    fruit_disease_occ,
    fruit_insect_occ,
    fruit_sunburn_occ,
    fruit_hail_occ,
    fruit_drought_stress_occ,
    fruit_frost_occ,
    fruit_mean_temp,
    T_opt_fruit
    )
{
  sigma<-5
  # Apply damage reductions if they occurred
  temp_factor<-exp(-((fruit_mean_temp - T_opt_fruit)^2) / (2 * sigma^2))
  quality <- 1 + temp_factor
  quality <- quality * (1-fruit_snail_occ)
  
  quality <- quality  * (1-fruit_disease_occ)
  
  quality <- quality  * (1-fruit_insect_occ)
  
  quality <- quality * (1-fruit_sunburn_occ)
  
  quality <- quality  * (1-fruit_hail_occ)
  
  quality <- quality  * (1-fruit_drought_stress_occ)
  
  quality <- quality  * (1-fruit_frost_occ)
  
  # Ensure potential stays within [0, 1]
  quality <- max(min(quality, 2), 0.1)
  
  return(quality)
}