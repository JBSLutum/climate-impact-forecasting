#yield

harvest <- function(standard_yield,
                    yield_potential,
                    chill,
                    bloom_potential,
                    fruit_quality,
                    harvest_rain_occ,
                    harvest_hail_occ,
                    harvest_frost_occ,
                    harvest_heat_occ,
                    harvest_fly_occ,
                    harvest_mean_temp)
{
  actual_yield <-
    standard_yield * yield_potential * chill * bloom_potential * fruit_quality
  
  
  
  
  # Quality risk evaluation
  total_quality_loss <- 1
  
  total_quality_loss <- total_quality_loss * chill
  
  total_quality_loss <- total_quality_loss - harvest_rain_occ
  
  total_quality_loss <- total_quality_loss - harvest_hail_occ
  
  total_quality_loss <- total_quality_loss - harvest_frost_occ
  
  total_quality_loss <- total_quality_loss - harvest_heat_occ
  
  total_quality_loss <- total_quality_loss - harvest_fly_occ
  
  
  # Cap loss to max 100%
  total_quality_loss <- max(min(total_quality_loss, 0.9), 0.1)
  
  # Marketable yield after quality loss
  marketable_yield <- actual_yield * total_quality_loss
  
  # Return output
  return(
    list(
      actual_yield = actual_yield,
      marketable_yield = marketable_yield,
      quality_loss = total_quality_loss
    )
  )
}