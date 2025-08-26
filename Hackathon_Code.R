#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Team 3: Animal - Environment
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# libraries
library("deSolve")
library("ggplot2")
library("dplyr")

initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 19, FarmCr = 1, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 1e6, WaterCr = 0, 
                   SoilCs = 1e11, SoilCr = 0)


parameters <- c(beta_FishS = 0.5,    # transmission rate fish
                beta_FarmS = 0.5,    # transmission rate farm animals
                beta_WildS = 0.01,    # transmission rate wild animals local
                beta_PetS = 0.05,     # transmission rate pets and peridomestic animals
                fc = 0.99,            # fitness cost of resistance
                bacInterference = 0.5, # bacterial interference for superinfection
                gammaFish = 1/60,     # carriage cessation in fish (1/days)
                gammaFishABX = 1/7,  # additional carriage cessation if treated with ABX (1/days)
                gammaFarm = 1/30,     # carriage cessation in farm animals (1/days)
                gammaFarmABX = 1/7,  # additional carriage cessation if treated with ABX (1/days)
                gammaWild = 1/30,     # carriage cessation in wildlife animals (1/days)
                gammaWildABX = 1/7,  # additional carriage cessation if treated with ABX (1/days)
                gammaPet = 1/30,      # carriage cessation in pets and peridomestic animals (1/days)
                gammaPetABX = 1/7,   # additional carriage cessation if treated with ABX (1/days)
                FishLoss = 1e-7,       # rate of ARG loss in fish
                FarmLoss = 1e-7,       # rate of ARG loss in farm animals
                WildLoss = 1e-7,       # rate of ARG loss in wildlife
                PetLoss = 1e-7,       # rate of ARG loss in pets and peridomestic animals
                
                FishExp = 0.5,         # proportion of farm fish exposed to abx
                FarmExp = 0.5,         # proportion of farm animals exposed to abx
                WildExp = 0.5,         # proportion of wild animals exposed to abx
                PetExp = 0.5,         # proportion of pets and peridomestic animals exposed to abx
                
                WaterGrowthCs = 0.1,   # growth rate of S bacteria in water
                WaterGrowthCr = 0.1,   # growth rate of R bacteria in water
                WaterCarrying = 1e6, # carrying capacity of bacteria in water
                WaterDecay = 0.01,      # decay rate of bacteria in water
                WaterHGT = 1e-5,        # rate of HGT in water
                WaterLoss = 0.05,       # rate of ARG loss in water
                WaterAbx = 0.001,       # rate of antibiotic exposure in water
                
                SoilGrowthCs = 0.1,   # growth rate of S bacteria in soil
                SoilGrowthCr = 0.1,   # growth rate of R bacteria in soil
                SoilCarrying = 1e11, # carrying capacity of bacteria in soil
                SoilDecay = 0.01,      # decay rate of bacteria in soil
                SoilHGT = 1e-6,        # rate of HGT in soil
                SoilLoss = 0.001,       # rate of ARG loss in soil
                SoilAbx = 0.001      # rate of antibiotic exposure in soil
)



sim_time <- seq(from = 0, to = 365, by = 1)  # model simulation time, days

############################################ MODEL ######################################

AE_model <- function(time, state, parameters) {  
  with(as.list(c(state, parameters)), { 
    
    # 16 compartments
    N <- FishN + FishCs + FishCr + FarmN + FarmCs + FarmCr +
      WildN + WildCs + WildCr + PetN + PetCs + PetCr
      
    #print(sum(N))
    
    ########### Forces of infection:
    lambdaFishS <- beta_FishS*(FishCs)/(FishN+FishCs+FishCr)
    lambdaFishR <- beta_FishS*fc*(FishCr)/(FishN+FishCs+FishCr)
    
    lambdaFarmS <- beta_FarmS*(FarmCs)/(FarmN+FarmCs+FarmCr)
    lambdaFarmR <- beta_FarmS*fc*(FarmCr)/(FarmN+FarmCs+FarmCr)
    
    lambdaWildS <- beta_WildS*(WildCs)/(WildN+WildCs+WildCr)
    lambdaWildR <- beta_WildS*fc*(WildCr)/(WildN+WildCs+WildCr)

    lambdaPetS <- beta_PetS*(PetCs)/(PetN+PetCs+PetCr)
    lambdaPetR <- beta_PetS*fc*(PetCr)/(PetN+PetCs+PetCr)
    
    
    FarmtoPet = 0.01
    FarmtoWild = 0.01
    FarmtoWater = 0.01
    FarmtoSoil = 0.01
    
    PettoFarm = FarmtoPet
    PettoWild = 0.01
    PettoWater = 0.01
    PettoSoil = 0.01
    
    WildtoFarm = FarmtoWild
    WildtoPet = PettoWild
    WildtoFish = 0.01
    WildtoWater = 0.01
    WildtoSoil = 0.01
    
    FishtoWild = WildtoFish
    FishtoWater = 0.01
    
    WatertoFarm = 0.01
    WatertoPet = 0.01
    WatertoWild = 0.01
    WatertoFish = 0.01
    WatertoSoil = 0.01
    
    SoiltoFarm = 0.01
    SoiltoPet = 0.01
    SoiltoWild = 0.01
    SoiltoWater = 0.01
    
########################################## Model equations: #############################################
    
    dFishN <- -(lambdaFishS + lambdaFishR)*FishN + (gammaFish+gammaFishABX*FishExp)*FishCs + gammaFish*FishCr
    
    dFishCs <- lambdaFishS*FishN - (gammaFish+gammaFishABX*FishExp)*FishCs - lambdaFishR*bacInterference*FishCs + lambdaFishS*bacInterference*FishCr + FishLoss*FishCr -
      FishCs*(WildtoFish*WildCr/(WildCr+WildCs+WildN) + WatertoFish*(WaterCr/WaterCarrying))
      
    dFishCr <- lambdaFishR*FishN - gammaFish*FishCr + lambdaFishR*bacInterference*FishCs - lambdaFishS*bacInterference*FishCr - FishLoss*FishCr +
      FishCs*(WildtoFish*WildCr/(WildCr+WildCs+WildN) + WatertoFish*(WaterCr/WaterCarrying))
    
    
    dFarmN <- -(lambdaFarmS + lambdaFarmR)*FarmN + (gammaFarm+gammaFarmABX*FarmExp)*FarmCs + gammaFarm*FarmCr
    
    dFarmCs <- lambdaFarmS*FarmN - (gammaFarm+gammaFarmABX*FarmExp)*FarmCs - lambdaFarmR*bacInterference*FarmCs + lambdaFarmS*bacInterference*FarmCr + FarmLoss*FarmCr -
      FarmCs*(PettoFarm*PetCr/(PetCr+PetCs+PetN) + WildtoFarm*WildCr/(WildCr+WildCs+WildN) + WatertoFarm*(WaterCr/WaterCarrying) + SoiltoFarm*(SoilCr/SoilCarrying))
    
    dFarmCr <- lambdaFarmR*FarmN - gammaFarm*FarmCr + lambdaFarmR*bacInterference*FarmCs - lambdaFarmS*bacInterference*FarmCr - FarmLoss*FarmCr +
      FarmCs*(PettoFarm*PetCr/(PetCr+PetCs+PetN) + WildtoFarm*WildCr/(WildCr+WildCs+WildN) + WatertoFarm*(WaterCr/WaterCarrying) + SoiltoFarm*(SoilCr/SoilCarrying))
    

    dWildN <- -(lambdaWildS + lambdaWildR)*WildN + (gammaWild+gammaWildABX*WildExp)*WildCs + gammaWild*WildCr
      
    dWildCs <- lambdaWildS*WildN - (gammaWild+gammaWildABX*WildExp)*WildCs - lambdaWildR*bacInterference*WildCs + lambdaWildS*bacInterference*WildCr + WildLoss*WildCr - 
      WildCs*(FarmtoWild*FarmCr/(FarmCr+FarmCs+FarmN) + PettoWild*PetCr/(PetCr+PetCs+PetN) + FishtoWild*FishCr/(FishCr+FishCs+FishN) + WatertoWild*(WaterCr/WaterCarrying) + SoiltoWild*(SoilCr/SoilCarrying))
    
    dWildCr <- lambdaWildR*WildN - gammaWild*WildCr + lambdaWildR*bacInterference*WildCs - lambdaWildS*bacInterference*WildCr - WildLoss*WildCr +
      WildCs*(FarmtoWild*FarmCr/(FarmCr+FarmCs+FarmN) + PettoWild*PetCr/(PetCr+PetCs+PetN) + FishtoWild*FishCr/(FishCr+FishCs+FishN) + WatertoWild*(WaterCr/WaterCarrying) + SoiltoWild*(SoilCr/SoilCarrying))
    
    
    dPetN <- -(lambdaPetS + lambdaPetR)*PetN + (gammaPet+gammaPetABX*PetExp)*PetCs + gammaPet*PetCr
    
    dPetCs <- lambdaPetS*PetN - (gammaPet+gammaPetABX*PetExp)*PetCs - lambdaPetR*bacInterference*PetCs + lambdaPetS*bacInterference*PetCr + PetLoss*PetCr -
      PetCs*(FarmtoPet*FarmCr/(FarmCr+FarmCs+FarmN) + WildtoPet*WildCr/(WildCr+WildCs+WildN) + WatertoPet*(WaterCr/WaterCarrying) + SoiltoPet*(SoilCr/SoilCarrying))
    
    dPetCr <- lambdaPetR*PetN - gammaPet*PetCr + lambdaPetR*bacInterference*PetCs - lambdaPetS*bacInterference*PetCr - PetLoss*PetCr +
      PetCs*(FarmtoPet*FarmCr/(FarmCr+FarmCs+FarmN) + WildtoPet*WildCr/(WildCr+WildCs+WildN) + WatertoPet*(WaterCr/WaterCarrying) + SoiltoPet*(SoilCr/SoilCarrying))
    
    
    dWaterCs <- WaterCs*WaterGrowthCs*(1-(WaterCs+WaterCr)/WaterCarrying) - WaterCs*WaterDecay - WaterCs*WaterHGT*(WaterCr/(WaterCs+WaterCr)) + WaterLoss*WaterCr - WaterCs*WaterAbx -
      WaterCs*(FarmtoWater*FarmCr/(FarmCr+FarmCs+FarmN) + PettoWater*PetCr/(PetCr+PetCs+PetN) + WildtoWater*WildCr/(WildCr+WildCs+WildN) + FishtoWater*FishCr/(FishCr+FishCs+FishN) + SoiltoWater*(SoilCr/SoilCarrying))
    
    dWaterCr <- WaterCr*WaterGrowthCr*fc*(1-(WaterCs+WaterCr)/WaterCarrying) - WaterCr*WaterDecay + WaterCs*WaterHGT*(WaterCr/(WaterCs+WaterCr)) - WaterLoss*WaterCr +
      WaterCs*(FarmtoWater*FarmCr/(FarmCr+FarmCs+FarmN) + PettoWater*PetCr/(PetCr+PetCs+PetN) + WildtoWater*WildCr/(WildCr+WildCs+WildN) + FishtoWater*FishCr/(FishCr+FishCs+FishN) + SoiltoWater*(SoilCr/SoilCarrying))
    
    
    dSoilCs <- SoilCs*SoilGrowthCs*(1-(SoilCs+SoilCr)/SoilCarrying) - SoilCs*SoilDecay - SoilCs*SoilHGT*(SoilCr/(SoilCs+SoilCr)) + SoilLoss*SoilCr - SoilCs*SoilAbx -
      SoilCs*(FarmtoSoil*FarmCr/(FarmCr+FarmCs+FarmN) + PettoSoil*PetCr/(PetCr+PetCs+PetN) + WildtoSoil*FishCr/(WildCr+WildCs+WildN) + WatertoSoil*(WaterCr/WaterCarrying))
    
    dSoilCr <- SoilCr*SoilGrowthCr*fc*(1-(SoilCs+SoilCr)/SoilCarrying) - SoilCr*SoilDecay + SoilCs*SoilHGT*(SoilCr/(SoilCs+SoilCr)) - SoilLoss*SoilCr +
      SoilCs*(FarmtoSoil*FarmCr/(FarmCr+FarmCs+FarmN) + PettoSoil*PetCr/(PetCr+PetCs+PetN) + WildtoSoil*FishCr/(WildCr+WildCs+WildN) + WatertoSoil*(WaterCr/WaterCarrying))
    

    return(list(c(dFishN, dFishCs, dFishCr, 
                  dFarmN, dFarmCs, dFarmCr, 
                  dWildN, dWildCs, dWildCr, 
                  dPetN, dPetCs, dPetCr, 
                  dWaterCs, dWaterCr, 
                  dSoilCs, dSoilCr))) 
  })
  
}


###############################################################################################
# Solving the differential equations using the ode integration algorithm
output <- as.data.frame(ode(y = initial_state, 
                                times = sim_time, 
                                func = AE_model, 
                                parms = parameters#, 
                                #method = "euler"
                            ))
# View(output)
