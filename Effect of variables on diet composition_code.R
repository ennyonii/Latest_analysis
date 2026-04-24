############INFLUENCE OF RAINFALL AND TERRESTRIAL BIOMASS ON PREY COMPOSITION#############
############ENIOLA OLU-AYORINDE###################
############2026-23-04##############################
#TO CALCULATE how EVI and rainfall(p7) affect PREY COMPOSITION (bray curtis and NMDS) of Hemigrammus marginatus

library(readxl)
HMR_IAI <- read_excel("generated_data/HMR_IAI.xlsx")
View(HMR_IAI)
HMR_IAI_df <- as.data.frame(HMR_IAI)
#TO Set the first column as row names and remove it from columns
rownames(HMR_IAI_df) <- HMR_IAI_df[[1]]
HMR_IAI_df <- HMR_IAI_df[, -1]  # Drop the 'Prey' column
View(HMR_IAI_df)

prey<-t(HMR_IAI_df)
write.csv(prey, "prey.csv")
View(prey)
library(readr)
prey_with_location <- read_csv("raw_data/prey_with_location.csv")
View(prey_with_location)
#Hellinger transformation for IAI to standardize it
library(vegan)
prey_hellinger <- decostand(prey, method = "hellinger")
nmds <- metaMDS(prey_hellinger, distance = "bray", autotransform = FALSE)
dist_matrix <- vegdist(prey_hellinger, method = "bray")
View(nmds)

#To fit environmental data on prey data
library(readr)
library(dplyr)
prey_data <- read_csv("raw_data/prey_with_location.csv")
env_data <- read_csv("raw_data/biomass_with_ratio.csv")
#Join the data using BOTH Location and Month
env_data_aligned <- prey_data %>%
  left_join(env_data, by = c("Location", "Month"))

# 5. Check the result
View(env_data_aligned)

en <- envfit(nmds, env_data_aligned[, c("p7", "evi_mean")], 
             permutations = 999, na.rm = TRUE)
print(en)

site_scores <- as.data.frame(scores(nmds, display = "sites"))

site_scores$Location <- env_data_aligned$Location
# 5. Extract vector coordinates
vec_coords <- as.data.frame(scores(en, display = "vectors")) * ordiArrowMul(en)

# 6. Plotting
NMDS_plot <- ggplot() +
  # Use the data directly from site_scores so every fish is represented
  geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2, color = Location), size = 3) +
  geom_segment(data = vec_coords, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), color = "black", linewidth = 1) +
  geom_text(data = vec_coords, aes(x = NMDS1, y = NMDS2, label = rownames(vec_coords)),
            color = "black", vjust = -1, fontface = "bold") +
  scale_color_brewer(palette = "Set1") + 
  theme_bw() +
  labs(subtitle = paste("Stress:", round(nmds$stress, 3)),
       color = "Study Sites")

print(NMDS_plot)
ggsave(
  "NMDS_plot.png",
  plot = NMDS_plot,       
  width = 10,     
  height = 7,      
  dpi = 300
)
getwd()
