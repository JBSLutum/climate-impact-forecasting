#Season Length####
#This function calculates the length of a season
#as soon as growth conditions (soil temp > 14°) are met the spear starts growing
#the season end in Germany is additionally the 24th of June
#time point spring
season_length <- function(growth_start_day,days_till_harvest,season_end_day) {
  days <- season_end_day - (growth_start_day + days_till_harvest)  # Inclusive the start days for first spear growth
  days <- max(days, 0)  # Prevent negative values if start is after end
  return(days)
}
