
library(ggplot2)
library(cowplot)

colors <- c("FISH_R" = "blue", 
            "FARM_R" = "brown", 
            "WILD_R" = "forestgreen", 
            "PET_R" = "grey30", 
            "WATER_R" = "cornflowerblue", 
            "SOIL_R" = "orange3"
)

# turn off all between-compartment connections
alt_parameters[36:50] = 0

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
                                     parms = alt_parameters))

Fig_1a <- ggplot(output_ter_farm, aes(x=time)) + 
  # geom_line(aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1) +  
  geom_line(aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1) +  
  # geom_line(aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1) +
  # geom_line(aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1) +
  # geom_line(aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed") +  
  # geom_line(aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(0, 365, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x="Time (days)", y="Prevalence of resistance\nwithin ecological compartment",
       title="Terrestrial farm animals") +
  theme(legend.position="top", #legend.position="right",#legend.position = c(.8,.88) c(.8,.8)
        legend.background=element_blank(),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) + 
  scale_color_manual(name = "Compartments:", values = colors,
                     labels = c("Terr. farm animals", "Farmed fish", "Pet & peridom.", "Soil", "Water"," Wild Animals"))


# start in farm fish
initial_state <- c(FishN = 100, FishCs = 19, FishCr = 1, 
                   FarmN = 100, FarmCs = 20, FarmCr = 0, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 1e6, WaterCr = 0, 
                   SoilCs = 1e11, SoilCr = 0)

output_farm_fish <- as.data.frame(ode(y = initial_state, 
                                      times = sim_time, 
                                      func = AE_model, 
                                      parms = alt_parameters))

Fig_1b <- ggplot(output_farm_fish, aes(x=time)) + 
  geom_line(aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1) +  
  # geom_line(aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1) +  
  # geom_line(aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1) +
  # geom_line(aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1) +
  # geom_line(aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed") +  
  # geom_line(aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(0, 365, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x="Time (days)", y="Prevalence of resistance\nwithin ecological compartment",
       title="Farm fish") +
  theme(legend.position="top", #legend.position="right",#legend.position = c(.8,.88) c(.8,.8)
        legend.background=element_blank(),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) + 
  scale_color_manual(name = "Compartments:", values = colors,
                     labels = c("Terr. farm animals", "Farmed fish", "Pet & peridom.", "Soil", "Water"," Wild Animals"))

# start in wild
initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 20, FarmCr = 0, 
                   WildN = 100, WildCs = 19, WildCr = 1,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 1e6, WaterCr = 0, 
                   SoilCs = 1e11, SoilCr = 0)

output_wild <- as.data.frame(ode(y = initial_state, 
                                 times = sim_time, 
                                 func = AE_model, 
                                 parms = alt_parameters))

Fig_1c <- ggplot(output_wild, aes(x=time)) + 
  # geom_line(aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1) +  
  # geom_line(aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1) +  
  geom_line(aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1) +
  # geom_line(aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1) +
  # geom_line(aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed") +  
  # geom_line(aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(0, 365, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x="Time (days)", y="Prevalence of resistance\nwithin ecological compartment",
       title="Wildlife") +
  theme(legend.position="top", #legend.position="right",#legend.position = c(.8,.88) c(.8,.8)
        legend.background=element_blank(),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) + 
  scale_color_manual(name = "Compartments:", values = colors,
                     labels = c("Terr. farm animals", "Farmed fish", "Pet & peridom.", "Soil", "Water"," Wild Animals"))

# start in pets
initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 20, FarmCr = 0, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 19, PetCr = 1,
                   WaterCs = 1e6, WaterCr = 0, 
                   SoilCs = 1e11, SoilCr = 0)

output_pet <- as.data.frame(ode(y = initial_state, 
                                times = sim_time, 
                                func = AE_model, 
                                parms = alt_parameters))

Fig_1d <- ggplot(output_pet, aes(x=time)) + 
  # geom_line(aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1) +  
  # geom_line(aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1) +  
  # geom_line(aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1) +
  geom_line(aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1) +
  # geom_line(aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed") +  
  # geom_line(aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(0, 365, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x="Time (days)", y="Prevalence of resistance\nwithin ecological compartment",
       title="Pets & peridom.") +
  theme(legend.position="top", #legend.position="right",#legend.position = c(.8,.88) c(.8,.8)
        legend.background=element_blank(),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) + 
  scale_color_manual(name = "Compartments:", values = colors,
                     labels = c("Terr. farm animals", "Farmed fish", "Pet & peridom.", "Soil", "Water"," Wild Animals"))


# start in water
initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 20, FarmCr = 0, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 0.9e6, WaterCr = 1e5, 
                   SoilCs = 1e11, SoilCr = 0)

output_water <- as.data.frame(ode(y = initial_state, 
                                  times = sim_time, 
                                  func = AE_model, 
                                  parms = alt_parameters))

Fig_1e <- ggplot(output_water, aes(x=time)) + 
  # geom_line(aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1) +  
  # geom_line(aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1) +  
  # geom_line(aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1) +
  # geom_line(aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1) +
  geom_line(aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed") +  
  # geom_line(aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(0, 365, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x="Time (days)", y="Prevalence of resistance\nwithin ecological compartment",
       title="Water") +
  theme(legend.position="top", #legend.position="right",#legend.position = c(.8,.88) c(.8,.8)
        legend.background=element_blank(),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) + 
  scale_color_manual(name = "Compartments:", values = colors,
                     labels = c("Terr. farm animals", "Farmed fish", "Pet & peridom.", "Soil", "Water"," Wild Animals"))

# start in soil
initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 20, FarmCr = 0, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 1e6, WaterCr = 0, 
                   SoilCs = 0.9e11, SoilCr = 1e10)

output_soil <- as.data.frame(ode(y = initial_state, 
                                 times = sim_time, 
                                 func = AE_model, 
                                 parms = alt_parameters))

Fig_1f <- ggplot(output_soil, aes(x=time)) + 
  # geom_line(aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1) +  
  # geom_line(aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1) +  
  # geom_line(aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1) +
  # geom_line(aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1) +
  # geom_line(aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed") +  
  geom_line(aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(0, 365, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x="Time (days)", y="Prevalence of resistance\nwithin ecological compartment",
       title="Soil") +
  theme(legend.position="top", #legend.position="right",#legend.position = c(.8,.88) c(.8,.8)
        legend.background=element_blank(),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) + 
  scale_color_manual(name = "Compartments:", values = colors,
                     labels = c("Terr. farm animals", "Farmed fish", "Pet & peridom.", "Soil", "Water"," Wild Animals"))

Fig_legend <- ggplot(output_soil, aes(x=time)) + 
  geom_line(aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1) +
  geom_line(aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1) +
  geom_line(aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1) +
  geom_line(aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1) +
  geom_line(aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed") +
  geom_line(aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(0, 365, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x="Time (days)", y="Prevalence of resistance\nwithin ecological compartment",
       title="Soil") +
  theme(legend.position="top", #legend.position="right",#legend.position = c(.8,.88) c(.8,.8)
        legend.background=element_blank(),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) + 
  scale_color_manual(name = "Compartments:", values = colors,
                     labels = c("Terr. farm animals", "Farmed fish", "Pet & peridom.", "Soil", "Water"," Wild Animals"))


plot_grid(plot_grid(Fig_1a+guides(colour="none"), Fig_1b+guides(colour="none"),
                    Fig_1c+guides(colour="none"), Fig_1d+guides(colour="none"),
                    Fig_1e+guides(colour="none"), Fig_1f+guides(colour="none"),
                    labels = c("a)", "b)", "c)", "d)", "e)", "f)")),
          get_legend(Fig_legend+theme(legend.position="bottom")),
          ncol=1, rel_heights = c(1, 0.1))

ggsave("fig_scenarios_indiv_comp.png", bg="white", height = 8, width=12)
