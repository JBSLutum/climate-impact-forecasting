#winter

#This function checks if the chill requirement of the plant is met
#Since the plant would grow even with no chill accumulated the output is a ratio
#time point November till Spring
winter <- function(required_chill, actual_chill, winter_mean_temp) {
  if (required_chill > 0){
    chillratio<-actual_chill/required_chill
    chillratio <- max(min(chillratio, 2), 0.1)}
  else {chillratio <- 1}
  # Störung durch zu warmen Winter
  # Schwellen
  # <= 4°C: kein Stress
  # 4–7°C: zunehmend störend
  # >= 7°C: klar störend
  
  if (winter_mean_temp <= 4) {
    warm_penalty <- 1.1
  } else if (winter_mean_temp <= 7) {
    warm_penalty <- 1 - 0.1 * (winter_mean_temp - 4) / 3
    # linearer Abzug bis max -0.1
  } else {
    warm_penalty <- 0.9
  }
  
  #Gesamtfaktor
  chillfactor <- chillratio * warm_penalty
  
  return(chillfactor)
}