#spring
spring<-function(
  spring_frost_occ,
  spring_disease_occ,
  spring_pollinator,
  spring_mean_temp
)
{
  # Apply damage reductions if they occurred
  potential <- 1
  sigma<-5
  # Apply damage reductions if they occurred
  temp_factor<-exp(-((spring_mean_temp - 8.5)^2) / (2 * sigma^2))
  potential <- 1 + temp_factor
  if (spring_frost_occ>0.1){
  potential <- potential *(1-spring_frost_occ)}
  else { potential <- potential + spring_frost_occ}
  
  potential <- potential  -spring_disease_occ
  
  potential <- potential  * (spring_pollinator/0.25)
  
  
  # Ensure potential stays within [0, 1]
  potential <- max(min(potential, 1.3), 0.1)
  
  return(potential)
}