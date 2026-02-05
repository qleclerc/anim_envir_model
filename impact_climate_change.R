
library(ggplot2)
library(cowplot)

colors <- c("FISH_R" = "blue", 
            "FARM_R" = "brown", 
            "WILD_R" = "forestgreen", 
            "PET_R" = "grey30", 
            "WATER_R" = "cornflowerblue", 
            "SOIL_R" = "orange3"
)


# baseline
initial_state <- c(FishN = 100, FishCs = 20, FishCr = 0, 
                   FarmN = 100, FarmCs = 19, FarmCr = 1, 
                   WildN = 100, WildCs = 20, WildCr = 0,
                   PetN = 100, PetCs = 20, PetCr = 0,
                   WaterCs = 1e6, WaterCr = 0, 
                   SoilCs = 1e11, SoilCr = 0)

output_baseline <- as.data.frame(ode(y = initial_state, 
                                     times = sim_time, 
                                     func = AE_model, 
                                     parms = parameters))

Fig_1a <- ggplot(output_baseline, aes(x=time)) + 
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
       title="Baseline conditions") +
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


# increase...
parameters["FarmtoWild"] = parameters["FarmtoWild"]*2

output_incr_farmtowild <- as.data.frame(ode(y = initial_state, 
                                      times = sim_time, 
                                      func = AE_model, 
                                      parms = parameters))


Fig_1b <- ggplot(output_incr_farmtowild, aes(x=time)) + 
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
       title="Increased wildlife contacts") +
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

# increase...
output_scenario <- as.data.frame(ode(y = initial_state, 
                                  times = sim_time, 
                                  func = AE_model, 
                                  parms = parameters))

Fig_1c <- ggplot(output_scenario, aes(x=time)) + 
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
       title="Increase in...") +
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


plot_grid(Fig_1a+guides(colour="none"), Fig_1b+guides(colour="none"),
          Fig_1c+guides(colour="none"), get_legend(Fig_1a+theme(legend.position="right")),
          labels = c("a)", "b)", "c)"))

# ggsave("fig_scenarios.png", bg="white", height = 8, width=12)
