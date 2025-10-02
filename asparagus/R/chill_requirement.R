#Chill Requirement####
#This function checks if the chill requirement of the plant is met
#Since the plant would grow even with no chill accumulated the output is a ratio
#time point November till Spring
chill_requirement <- function(required_chill, actual_chill) {
  if (required_chill > 0){
    chillratio<-actual_chill/required_chill
    chillratio <- max(min(chillratio, 1), 0.1)}
  else {chillratio <- 1}
  return(chillratio)
}