
extract_outcomes = function(output){
  
  #How long until the ARG is above a threshold in 50% of compartments
  #(1 animal for farm animals, 1 fish for fish, 1 animal for wildlife, 1 animal for peridomestic, 100 cfu/mL or g for water and soil)
  min_time_Fish = min(which(output$FishCr > 1))
  min_time_Farm = min(which(output$FarmCr > 1))
  min_time_Wild = min(which(output$WildCr > 1))
  min_time_Pet = min(which(output$PetCr > 1))
  min_time_Water = min(which(output$WaterCr > 100))
  min_time_Soil = min(which(output$SoilCr > 100))
  # order the times, the one where 50% of compartments are above the threshold is the one where the 3rd compartment is above the threshold
  min_time_all = sort(c(min_time_Fish, min_time_Farm, min_time_Wild, min_time_Pet, min_time_Water, min_time_Soil))[3]
  
  #Prevalence in animals and prevalence in water+soil after 1 week
  prev_animals_1week = sum(output$FishCr[7],
                           output$FarmCr[7],
                           output$WildCr[7],
                           output$PetCr[7])/
    sum(c(output$FishN[7],output$FishCs[7],output$FishCr[7],
          output$FarmN[7],output$FarmCs[7],output$FarmCr[7],
          output$WildN[7],output$WildCs[7],output$WildCr[7],
          output$PetN[7],output$PetCs[7],output$PetCr[7]))
  prev_envir_1week = sum(output$WaterCr[7],output$SoilCr[7])/
    sum(c(output$WaterCs[7],output$WaterCr[7],
          output$SoilCs[7],output$SoilCr[7]))
  
  #Prevalence in animals and prevalence in water+soil after 30 days
  prev_animals_1month = sum(output$FishCr[30],
                            output$FarmCr[30],
                            output$WildCr[30],
                            output$PetCr[30])/
    sum(c(output$FishN[30],output$FishCs[30],output$FishCr[30],
          output$FarmN[30],output$FarmCs[30],output$FarmCr[30],
          output$WildN[30],output$WildCs[30],output$WildCr[30],
          output$PetN[30],output$PetCs[30],output$PetCr[30]))
  prev_envir_1month = sum(output$WaterCr[30],output$SoilCr[30])/
    sum(c(output$WaterCs[30],output$WaterCr[30],
          output$SoilCs[30],output$SoilCr[30]))
  
  outcomes = list(min_time_all = min_time_all,
                  prev_animals_1week = prev_animals_1week,
                  prev_envir_1week = prev_envir_1week,
                  prev_animals_1month = prev_animals_1month,
                  prev_envir_1month = prev_envir_1month)
  
  return(outcomes)
  
}

extract_outcomes(output)

extract_outcomes2 = function(output){
  
  #How long until the ARG is above 25% for each compartment
  min_time_Fish = min(which(output$FishCr/(output$FishN+output$FishCs+output$FishCr) > 0.25))
  min_time_Farm = min(which(output$FarmCr/(output$FarmN+output$FarmCs+output$FarmCr) > 0.25))
  min_time_Wild = min(which(output$WildCr/(output$WildN+output$WildCs+output$WildCr) > 0.25))
  min_time_Pet = min(which(output$PetCr/(output$PetN+output$PetCs+output$PetCr) > 0.25))
  min_time_Water = min(which(output$WaterCr/(output$WaterCs+output$WaterCr) > 0.25))
  min_time_Soil = min(which(output$SoilCr/(output$SoilCs+output$SoilCr) > 0.25))

  #Prevalence all compartments at the end of the simulation
  prev_Fish = output$FishCr[nrow(output)]/(output$FishN[nrow(output)]+output$FishCs[nrow(output)]+output$FishCr[nrow(output)])
  prev_Farm = output$FarmCr[nrow(output)]/(output$FarmN[nrow(output)]+output$FarmCs[nrow(output)]+output$FarmCr[nrow(output)])
  prev_Pet = output$PetCr[nrow(output)]/(output$PetN[nrow(output)]+output$PetCs[nrow(output)]+output$PetCr[nrow(output)])
  prev_Wild = output$WildCr[nrow(output)]/(output$WildN[nrow(output)]+output$WildCs[nrow(output)]+output$WildCr[nrow(output)])
  prev_Water = output$WaterCr[nrow(output)]/(output$WaterCs[nrow(output)]+output$WaterCr[nrow(output)])
  prev_Soil = output$SoilCr[nrow(output)]/(output$SoilCs[nrow(output)]+output$SoilCr[nrow(output)])

  outcomes = list(min_time_Fish = min_time_Fish,
                  min_time_Farm = min_time_Farm,
                  min_time_Wild = min_time_Wild,
                  min_time_Pet = min_time_Pet,
                  min_time_Water = min_time_Water,
                  min_time_Soil = min_time_Soil,
                  prev_Fish = prev_Fish,
                  prev_Farm = prev_Farm,
                  prev_Pet = prev_Pet,
                  prev_Wild = prev_Wild,
                  prev_Water = prev_Water,
                  prev_Soil = prev_Soil)
  
  return(outcomes)
  
}
