
library(ggplot2)
library(cowplot)
library(reshape2)

colors <- c("FISH_R" = "blue", 
            "FARM_R" = "brown", 
            "WILD_R" = "forestgreen", 
            "PET_R" = "grey30", 
            "WATER_R" = "cornflowerblue", 
            "SOIL_R" = "orange3"
)

# INCREASE WATERBODY TEMP
climate1_parameters = c(beta_FarmS = 0.25,    # transmission rate farm animals
                        beta_PetS = 0.14,     # transmission rate pets and peridomestic animals
                        beta_WildS = 0.05,    # transmission rate wild animals local
                        beta_FishS = 0.65*1.25,    # transmission rate fish
                        fc = 0.99,            # relative fitness of resistant strains
                        gammaFarm = 1/30,     # carriage cessation in farm animals (1/days)
                        gammaFarmABX = 1/7,  # additional carriage cessation if treated with ABX (1/days)
                        gammaPet = 1/30,      # carriage cessation in pets and peridomestic animals (1/days)
                        gammaPetABX = 1/7,   # additional carriage cessation if treated with ABX (1/days)
                        gammaWild = 1/30,     # carriage cessation in wildlife animals (1/days)
                        gammaWildABX = 1/7,  # additional carriage cessation if treated with ABX (1/days)
                        gammaFish = 1/(10*1.25),     # carriage cessation in fish (1/days)
                        gammaFishABX = 1/(7*1.25),  # additional carriage cessation if treated with ABX (1/days)
                        FarmLoss = 10e-6,       # rate of ARG loss in farm animals
                        PetLoss = 10e-6,       # rate of ARG loss in pets and peridomestic animals
                        WildLoss = 10e-6,       # rate of ARG loss in wildlife
                        FishLoss = 10e-6,       # rate of ARG loss in fish
                        
                        FarmExp = 0.5,         # proportion of farm animals exposed to abx
                        PetExp = 0.15,         # proportion of pets and peridomestic animals exposed to abx
                        WildExp = 0.025,         # proportion of wild animals exposed to abx
                        FishExp = 0.5*1.25,         # proportion of farm fish exposed to abx
                        
                        WaterGrowthCs = 0.2*1.25,   # growth rate of S bacteria in water
                        WaterGrowthCr = 0.2*1.25,   # growth rate of R bacteria in water
                        WaterCarrying = 10e5*1.25, # carrying capacity of bacteria in water
                        WaterDecay = 0.02,      # decay rate of bacteria in water
                        WaterHGT = 10e-6*1.25,        # rate of HGT in water
                        WaterLoss = 0.05,       # rate of ARG loss in water
                        WaterAbx = 0.001,       # rate of antibiotic exposure in water
                        
                        SoilGrowthCs = 0.1,   # growth rate of S bacteria in soil
                        SoilGrowthCr = 0.1,   # growth rate of R bacteria in soil
                        SoilCarrying = 10e10, # carrying capacity of bacteria in soil
                        SoilDecay = 0.01,      # decay rate of bacteria in soil
                        SoilHGT = 10e-7,        # rate of HGT in soil
                        SoilLoss = 0.05,       # rate of ARG loss in soil
                        SoilAbx = 0.001,      # rate of antibiotic exposure in soil
                        
                        FarmtoPet = 0.02,
                        FarmtoWild = 0.01,
                        FarmtoFish = 0,
                        FarmtoWater = 0.01,
                        FarmtoSoil = 0.01,
                        
                        PettoFarm = 0.02,
                        PettoWild = 0.01,
                        PettoFish = 0,
                        PettoWater = 0.01,
                        PettoSoil = 0.01,
                        
                        WildtoFarm = 0.01,
                        WildtoPet = 0.01,
                        WildtoFish = 0.005,
                        WildtoWater = 0.01,
                        WildtoSoil = 0.01,
                        
                        FishtoFarm = 0,
                        FishtoPet = 0,
                        FishtoWild = 0.005,
                        FishtoWater = 0.02,
                        FishtoSoil = 0,
                        
                        WatertoFarm = 0.01,
                        WatertoPet = 0.01,
                        WatertoWild = 0.01,
                        WatertoFish = 0.02,
                        WatertoSoil = 0.01,
                        
                        SoiltoFarm = 0.01,
                        SoiltoPet = 0.01,
                        SoiltoWild = 0.01,
                        SoiltoFish = 0,
                        SoiltoWater = 0.01
)


# start in terrestrial farm animals
initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 19, FarmCr = 1, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 1e6, WaterCr = 0, 
                   SoilCs = 1e11, SoilCr = 0)

output_ter_farm <- as.data.frame(ode(y = initial_state, 
                                     times = sim_time, 
                                     func = AE_model, 
                                     parms = climate1_parameters))
output_ter_farm = extract_outcomes2(output_ter_farm)
output_ter_farm$scenario = "Increased water temperature"


# start in water
initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 20, FarmCr = 0, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 0.99e6, WaterCr = 1e4, 
                   SoilCs = 1e11, SoilCr = 0)

output_water <- as.data.frame(ode(y = initial_state, 
                                  times = sim_time, 
                                  func = AE_model, 
                                  parms = climate1_parameters))
output_water = extract_outcomes2(output_water)
output_water$scenario = "Increased water temperature"


# INCREASE EXTREME EVENTS
climate2_parameters = c(beta_FarmS = 0.25,    # transmission rate farm animals
                        beta_PetS = 0.14,     # transmission rate pets and peridomestic animals
                        beta_WildS = 0.05,    # transmission rate wild animals local
                        beta_FishS = 0.65,    # transmission rate fish
                        fc = 0.99,            # relative fitness of resistant strains
                        gammaFarm = 1/30,     # carriage cessation in farm animals (1/days)
                        gammaFarmABX = 1/7,  # additional carriage cessation if treated with ABX (1/days)
                        gammaPet = 1/30,      # carriage cessation in pets and peridomestic animals (1/days)
                        gammaPetABX = 1/7,   # additional carriage cessation if treated with ABX (1/days)
                        gammaWild = 1/30,     # carriage cessation in wildlife animals (1/days)
                        gammaWildABX = 1/7,  # additional carriage cessation if treated with ABX (1/days)
                        gammaFish = 1/(10),     # carriage cessation in fish (1/days)
                        gammaFishABX = 1/(7),  # additional carriage cessation if treated with ABX (1/days)
                        FarmLoss = 10e-6,       # rate of ARG loss in farm animals
                        PetLoss = 10e-6,       # rate of ARG loss in pets and peridomestic animals
                        WildLoss = 10e-6,       # rate of ARG loss in wildlife
                        FishLoss = 10e-6,       # rate of ARG loss in fish
                        
                        FarmExp = 0.5*1.25,         # proportion of farm animals exposed to abx
                        PetExp = 0.15*1.25,         # proportion of pets and peridomestic animals exposed to abx
                        WildExp = 0.025*1.25,         # proportion of wild animals exposed to abx
                        FishExp = 0.5*1.25,         # proportion of farm fish exposed to abx
                        
                        WaterGrowthCs = 0.2*1.25,   # growth rate of S bacteria in water
                        WaterGrowthCr = 0.2*1.25,   # growth rate of R bacteria in water
                        WaterCarrying = 10e5*1.25, # carrying capacity of bacteria in water
                        WaterDecay = 0.02*0.75,      # decay rate of bacteria in water
                        WaterHGT = 10e-6*1.25,        # rate of HGT in water
                        WaterLoss = 0.05,       # rate of ARG loss in water
                        WaterAbx = 0.001*1.25,       # rate of antibiotic exposure in water
                        
                        SoilGrowthCs = 0.1*1.25,   # growth rate of S bacteria in soil
                        SoilGrowthCr = 0.1*1.25,   # growth rate of R bacteria in soil
                        SoilCarrying = 10e10, # carrying capacity of bacteria in soil
                        SoilDecay = 0.01*1.25,      # decay rate of bacteria in soil
                        SoilHGT = 10e-7*1.25,        # rate of HGT in soil
                        SoilLoss = 0.05,       # rate of ARG loss in soil
                        SoilAbx = 0.001,      # rate of antibiotic exposure in soil
                        
                        FarmtoPet = 0.02,
                        FarmtoWild = 0.01,
                        FarmtoFish = 0,
                        FarmtoWater = 0.01*1.25,
                        FarmtoSoil = 0.01*1.25,
                        
                        PettoFarm = 0.02,
                        PettoWild = 0.01,
                        PettoFish = 0,
                        PettoWater = 0.01*1.25,
                        PettoSoil = 0.01*1.25,
                        
                        WildtoFarm = 0.01*0.75,
                        WildtoPet = 0.01,
                        WildtoFish = 0.005,
                        WildtoWater = 0.01*1.25,
                        WildtoSoil = 0.01*1.25,
                        
                        FishtoFarm = 0,
                        FishtoPet = 0,
                        FishtoWild = 0.005,
                        FishtoWater = 0.02*1.25,
                        FishtoSoil = 0,
                        
                        WatertoFarm = 0.01*1.25,
                        WatertoPet = 0.01*1.25,
                        WatertoWild = 0.01,
                        WatertoFish = 0.02*1.25,
                        WatertoSoil = 0.01*1.25,
                        
                        SoiltoFarm = 0.01,
                        SoiltoPet = 0.01,
                        SoiltoWild = 0.01,
                        SoiltoFish = 0,
                        SoiltoWater = 0.01*1.25
)


# start in terrestrial farm animals
initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 19, FarmCr = 1, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 1e6, WaterCr = 0, 
                   SoilCs = 1e11, SoilCr = 0)

output_ter_farm2 <- as.data.frame(ode(y = initial_state, 
                                     times = sim_time, 
                                     func = AE_model, 
                                     parms = climate2_parameters))
output_ter_farm2 = extract_outcomes2(output_ter_farm2)
output_ter_farm2$scenario = "Increased extreme events"



# start in water
initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 20, FarmCr = 0, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 0.99e6, WaterCr = 1e4, 
                   SoilCs = 1e11, SoilCr = 0)

output_water2 <- as.data.frame(ode(y = initial_state, 
                                  times = sim_time, 
                                  func = AE_model, 
                                  parms = climate2_parameters))
output_water2 = extract_outcomes2(output_water2)
output_water2$scenario = "Increased extreme events"

# extract baseline outcomes
output_water_baseline = extract_outcomes2(output_water_baseline)
output_water_baseline$scenario = "Baseline"
output_water_baseline$seed = "Water"
output_ter_farm_baseline = extract_outcomes2(output_ter_farm_baseline)
output_ter_farm_baseline$scenario = "Baseline"

# compare climate change outcomes to baseline
output_ter_farm = bind_rows(output_ter_farm, output_ter_farm_baseline, output_ter_farm2)
output_ter_farm = melt(output_ter_farm, id.vars = c("scenario"))
output_water = bind_rows(output_water_baseline, output_water, output_water2)
output_water = melt(output_water, id.vars = c("scenario"))

# plot different outcomes as a bar chart with x axis for each scnario
ggplot(output_ter_farm, aes(x=variable, y=value, fill=scenario)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x="Scenario", y="Prevalence of resistance\nin animals after 1 month",
       title="Seed in terrestrial farm animals") +
  theme_classic() + 
  theme(legend.position="none",
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) + 
  scale_fill_manual(values=c("Baseline" = "grey70", "Increased water temperature" = "cornflowerblue", "Increased extreme events" = "orange3"))


# Fig_1b <- ggplot(output_water, aes(x=time)) + 
#   geom_line(aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1) +  
#   geom_line(aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1) +  
#   geom_line(aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1) +
#   geom_line(aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1) +
#   geom_line(aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed") +  
#   geom_line(aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed") +
#   #baseline in faded
#   geom_line(data = output_water_baseline, aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1, alpha = 0.2) +  
#   geom_line(data = output_water_baseline, aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1, alpha = 0.2) +  
#   geom_line(data = output_water_baseline, aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1, alpha = 0.2) +
#   geom_line(data = output_water_baseline, aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1, alpha = 0.2) +
#   geom_line(data = output_water_baseline, aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed", alpha = 0.2) +  
#   geom_line(data = output_water_baseline, aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed", alpha = 0.2) +
#   facet_grid(rows = vars(scenario)) +
#   theme_classic() + 
#   scale_x_continuous(breaks=seq(0, 365, 60)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
#   labs(x="Time (days)", y="Prevalence of resistance\nwithin ecological compartment",
#        title="Seed in water") +
#   theme(legend.position="top", #legend.position="right",#legend.position = c(.8,.88) c(.8,.8)
#         legend.background=element_blank(),
#         legend.title=element_text(size=12), 
#         legend.text=element_text(size=12),
#         axis.text.x = element_text(size = 11),
#         axis.text.y = element_text(size = 11),
#         axis.title.x = element_text(size = 11),
#         axis.title.y = element_text(size = 11)) + 
#   scale_color_manual(name = "Compartments:", values = colors,
#                      labels = c("Terr. farm animals", "Farmed fish", "Pet & peridom.", "Soil", "Water"," Wild Animals"))




plot_grid(plot_grid(Fig_1a+guides(colour="none"), Fig_1b+guides(colour="none"),
                    labels = c("a)", "b)"), ncol=2),
          get_legend(Fig_1a+theme(legend.position="right")),
          ncol=1, rel_heights = c(1, 0.1))

ggsave("fig_scenarios_climate.png", bg="white", height = 8, width=12)
