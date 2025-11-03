
###############################################################################################

# PLOT THE MODEL OUTPUT:

colors <- c("FISH_R" = "blue", 
            "FARM_R" = "brown", 
            "WILD_R" = "forestgreen", 
            "PET_R" = "grey30", 
            "WATER_R" = "cornflowerblue", 
            "SOIL_R" = "orange3"
)

Fig_1 <- ggplot(output, aes(x=time)) + 
  geom_line(aes(y=(FishCr)/(FishN+FishCs+FishCr), color="FISH_R"), linewidth=1) +  
  geom_line(aes(y=(FarmCr)/(FarmN+FarmCs+FarmCr), color="FARM_R"), linewidth=1) +  
  geom_line(aes(y=(WildCr)/(WildN+WildCs+WildCr), color="WILD_R"), linewidth=1) +
  geom_line(aes(y=(PetCr)/(PetN+PetCs+PetCr), color="PET_R"), linewidth=1) +
  geom_line(aes(y=(WaterCr)/(WaterCs+WaterCr), color="WATER_R"), linewidth=1, linetype = "dashed") +  
  geom_line(aes(y=(SoilCr)/(SoilCs+SoilCr), color="SOIL_R"), linewidth=1, linetype = "dashed") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(0, 365, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x="Time (days)", y="Prevalence of resistance within ecological compartment") +
  theme(legend.position="top", #legend.position="right",#legend.position = c(.8,.88) c(.8,.8)
        legend.background=element_blank(),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 14, face = "plain")) + 
  scale_color_manual(name = "Compartments:", values = colors,
                     labels = c("Farm animals", "Farmed fish", "Pet & peridom.", "Soil", "Water"," Wild Animals"))

Fig_1
#,labels = c("FISH_R", "FARM_R", "WILD_R", "WATER_R","SOIL_R")
#,labels = c("Farm animals", "Farmed fish", "Soil", "Water"," Wild Animals")

# ggsave("Fig_1.png", height = 6, width = 8)
