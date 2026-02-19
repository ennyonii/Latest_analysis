library(readxl)
data_bp <- read_excel("C:/Desktop/MY THESIS/data_bp.xlsx")
View(data_bp)
Levins_index <- data_bp$Levins_index
Rainfall <- data_bp$`precipitation(mm)`
Month <- data_bp$Month
biomass_densityy <- data_bp$`biomass_density (Mg/ha)`

#STAT ANALYSIS FOR THE NICHE WIDTH DEPENDENCE ON rainfall and biomass density  QUESTION
install.packages("DHARMa")
library("DHARMa")
anova(lm(Levins_index ~ Rainfall + biomass_densityy))

anova(lm(Levins_index ~ Rainfall*biomass_densityy)) # dependent~independent

model_1 <- lm(Levins_index ~ Rainfall*biomass_densityy)
summary(model_1)
model_8 <- lm(Levins_index ~ Rainfall+biomass_densityy)
summary(model_8)

#DHARMa's test
res <- simulateResiduals(fittedModel = model_1)
plot(res)
testDispersion(res)
testOutliers(res)

library(ggplot2)

ggplot(data_bp, aes(x = biomass_densityy, y = Levins_index, color = Rainfall)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Biomass Density",
    y = "Levins Index",
    color = "Precipitation"
  ) +
  scale_color_gradient(low = "blue", high = "red") +  # More distinct color gradient
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8.7)
  )


anova(lm(Levins_index ~ biomass_densityy))
Model_2 <- lm(Levins_index ~ biomass_densityy)
summary(Model_2)
#DHARMa's test
res_2 <- simulateResiduals(fittedModel = Model_2)
plot(res)
testDispersion(res)
testOutliers(res)

library(ggplot2)

ggplot(data_bp, aes(x = biomass_densityy, y = Levins_index)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    x = "Biomass Density",
    y = "Levins Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9.0)
  )

model_6 <- lm(Levins_index ~ Rainfall)
summary(model_6)


#to find ratio of aquatic to terrestial biomass
library(readxl)
latest_biomass_dataaAA <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAA)
library(tidyverse)
latest_biomass_data_n <- latest_biomass_dataaAA %>% mutate(ratio = Aquatic_Biomass/Terrestrial_Biomass)
View(latest_biomass_data_n)
write.csv(latest_biomass_data_n, "biomass_with_ratio.csv" , row.names = FALSE)
getwd()

#TO CALCULATE biomass ratio dependence on woody biomass density and rainfall (continous variable)

ratioo <- log(ratio)
clean_modeling_data <- latest_biomass_data_n[-1, ]
model_3 <- lm(ratioo ~ log(biomass_density) * log(Precipitation), data = clean_modeling_data)
summary(model_3)
model_4 <- lm(ratioo ~ biomass_density, data = clean_modeling_data)
summary(model_4)
model_5 <- lm(ratioo ~ Precipitation, data = clean_modeling_data)
summary(model_5)

library(ggplot2)

ggplot(clean_modeling_data, aes(x = biomass_density, y = ratioo)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Effect of Biomass Density on Ratio",
    x = "Biomass Density",
    y = "Biomass Ratio(Aquatic/Terrestial)"
  ) +
  theme_minimal()


ggplot(clean_modeling_data, aes(x = Precipitation, y = ratioo)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Effect of precipitation on Ratio",
    x = "Precipitation",
    y = "Biomass Ratio(Aquatic/Terrestial)"
  ) +
  theme_minimal()


ggplot(clean_modeling_data, aes(x = biomass_density, y = log(ratio), color = Precipitation)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Effeect of Biomass Density and Precipitation on Biomass Ratio",
    x = "Biomass Density",
    y = "Biomass Ratio(Aquatic/Terrestial)",
    color = "Precipitation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8.5)
  )


#MAIN SOURCE of Biomass, BASED ON PRECIPITATION, LINEAR REGRESSION
#linear model
lm(log(ratio) ~ Precipitation, data = clean_modeling_data)

#plotting the analysis results
library(ggplot2)
ggplot(clean_modeling_data, aes(x = Precipitation, y = log(ratio))) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship Between Precipitation and Biomass Ratio",
    x = "Precipitation (mm)",
    y = "Log(Biomass Ratio)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9.5))

view(clean_modeling_data)

#does the source of biomass become more aquatic or terrestial as rainfall increases or reduces?
#Regression model
clean_modeling_data$Main_Source <- factor(clean_modeling_data$Main_Source)
View(clean_modeling_data)

# Fit the logistic regression model glm
model <- glm(Main_Source ~ Precipitation, data = clean_modeling_data, family = binomial)
summary(model)

#plotting the Analysis results
library(ggplot2)

# Create predicted probabilities
clean_modeling_data$predicted_prob <- predict(glm(Main_Source ~ Precipitation, 
                                         family = binomial, data = clean_modeling_data), 
                                     type = "response")

# Plot
ggplot(clean_modeling_data, aes(x = Precipitation, y = predicted_prob)) +
  geom_point(aes(color = Main_Source), size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "blue") +
  labs(
    title = "Effect of Precipitation on Biomass Source",
    x = "Precipitation (mm)",
    y = "Probability that Main Source is Aquatic"
  ) +
  theme_minimal()


#Extra
library(readxl)
latest_biomass_dataaAA <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAA)
# Rename columns for easier use (remove the leading X)
names(latest_biomass_dataaAA)[1] <- "Sample_ID"

# --- 2. DATA PREPARATION: CATEGORIZE PRECIPITATION ---
# Updated breaks and labels to match the user's request (0-50, 50-100, 100-150, >150)
latest_biomass_dataaAA$Precipitation_Category <- cut(latest_biomass_dataaAA$Precipitation, 
                                                     breaks = c(-Inf, 50, 100, 150, Inf),
                                                     labels = c("Low (0-50 mm)", "Moderate (50-100 mm)", "High (100-150 mm)", "Very High (>150 mm)"),
                                                     right = TRUE,
                                                     include.lowest = TRUE)

library(tidyverse)
install.packages("reshape2")
library(reshape2)

# --- 3. CALCULATE TOTAL BIOMASS CONTRIBUTION (unchanged) ---

# Calculate the grand totals (similar to the Python script)
total_aquatic_biomass <- sum(latest_biomass_dataaAA$Aquatic_Biomass)
total_terrestrial_biomass <- sum(latest_biomass_dataaAA$Terrestrial_Biomass)
grand_total_biomass <- total_aquatic_biomass + total_terrestrial_biomass

# Calculate the percentage contribution
percent_aquatic <- (total_aquatic_biomass / grand_total_biomass) * 100
percent_terrestrial <- (total_terrestrial_biomass / grand_total_biomass) * 100

# Print the overall result
cat("--- Overall Biomass Contribution ---\n")
cat(sprintf("Total Aquatic (Autochthonous) Biomass: %.2f (%.2f%%)\n", total_aquatic_biomass, percent_aquatic))
cat(sprintf("Total Terrestrial (Allochthonous) Biomass: %.2f (%.2f%%)\n", total_terrestrial_biomass, percent_terrestrial))
cat("\nConclusion: The main source of biomass overall is Aquatic.\n")
cat("--------------------------------------\n\n")

# --- 4. PREPARE DATA FOR BOXPLOT (Long Format) ---

# We need to transform the data from 'wide' format (two columns for biomass) 
# into 'long' format (one column for the value, one column for the source type).

latest_biomass_dataaAA_long <- melt(latest_biomass_dataaAA, 
                                    id.vars = c("Sample_ID", "Location", "Month", "Season", "Precipitation"),
                                    measure.vars = c("Aquatic_Biomass", "Terrestrial_Biomass"),
                                    variable.name = "Biomass_Source",
                                    value.name = "Biomass_Value")

# Clean up the variable names in the new column
latest_biomass_dataaAA_long$Biomass_Source <- gsub("_Biomass", "", latest_biomass_dataaAA_long$Biomass_Source)

# --- 5. GENERATE BOXPLOT BY PRECIPITATION CATEGORY ---

cat("\n--- Boxplot by Precipitation Category ---\n")
# Plot to show the distribution split by the new Precipitation Category

boxplot_precip <- ggplot(latest_biomass_dataaAA_long, aes(x = Biomass_Source, y = Biomass_Value, fill = Biomass_Source)) +
  
  # Add the boxplot layer
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.fill = "red") +
  geom_jitter(width = 0.2, color = "black", alpha = 0.6) +
  
  # Facet by the new Precipitation Category
  facet_wrap(~ Precipitation, ncol = 4) +
  
  # Title and Labels
  labs(
    title = "Biomass Distribution by Source across Precipitation Categories",
    subtitle = "Visualizing the impact of increasing rainfall on diet source",
    x = "Biomass Source Type",
    y = "Biomass Value (Units)",
    fill = "Source"
  ) +
  
  # Set colors
  scale_fill_manual(values = c("Aquatic" = "#0072B2", "Terrestrial" = "#D55E00")) +
  
  # Set the theme
  theme_bw() +
  
  # Customize appearance
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Print the seasonal boxplot object
print(boxplot_precip)
#Extra ends

#Extra 2 - determining dominant food source by location
library(ggplot2)
library(reshape2)
library(dplyr)

# --- Transform data to long format ---
latest_biomass_dataaAA_long <- melt(
  latest_biomass_dataaAA,
  id.vars = c("Sample_ID", "Location", "Month", "Season", "Precipitation"),
  measure.vars = c("Aquatic_Biomass", "Terrestrial_Biomass"),
  variable.name = "Biomass_Source",
  value.name = "Biomass_Value"
)

# Clean up the variable names
latest_biomass_dataaAA_long$Biomass_Source <- gsub("_Biomass", "", latest_biomass_dataaAA_long$Biomass_Source)

# --- Boxplot comparing sources across all 7 sites ---
boxplot_site <- ggplot(latest_biomass_dataaAA_long,
                       aes(x = Location, y = Biomass_Value, fill = Biomass_Source)) +
  
  geom_boxplot(alpha = 0.85, outlier.shape = 21, outlier.fill = "red",
               position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              color = "black", alpha = 0.4, size = 1.5) +
  
  labs(
    title = "Comparison of Aquatic vs Terrestrial Biomass Across 7 Freshwater Sites",
    subtitle = "Determining dominant biomass source by location",
    x = "Sampling Site",
    y = "Biomass Value (units)",
    fill = "Biomass Source"
  ) +
  
  scale_fill_manual(values = c("Aquatic" = "#0072B2", "Terrestrial" = "#D55E00")) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(boxplot_site)
# Save the plot as a high-quality PNG file
ggsave(
  filename = "Biomass_Source_by_Site.png",  # file name
  plot = boxplot_site,                      # your ggplot object    # folder path (change to where you want it saved)
  width = 10,                               # width in inches
  height = 6,                               # height in inches
  dpi = 300                                 # resolution for print quality
)
#Extra_2 ends


#TO CALCULATE how EVI and rainfall(p7) affect PREY COMPOSITION (bray curtis and NMDS)
library(readxl)
HMR_IAI <- read_excel("C:/Users/eniol/OneDrive/Desktop/MY THESIS/HMR_IAI.xlsx")
HMR_IAI_df <- as.data.frame(HMR_IAI)
#TO Set the first column as row names and remove it from columns
rownames(HMR_IAI_df) <- HMR_IAI_df[[1]]
HMR_IAI_df <- HMR_IAI_df[, -1]  # Drop the 'Prey' column
View(HMR_IAI_df)

prey<-t(HMR_IAI_df)
write.csv(prey, "prey.csv")
View(prey)
library(readr)
prey_with_location <- read_csv("C:/Desktop/MY THESIS/prey.csv")
View(prey_with_location)
write.csv(prey_with_location, "prey_with_location.csv")
#Hellinger transformation for IAI to standardize it
library(vegan)
prey_hellinger <- decostand(prey, method = "hellinger")
nmds <- metaMDS(prey_hellinger, distance = "bray", autotransform = FALSE)
dist_matrix <- vegdist(prey_hellinger, method = "bray")
View(nmds)

vec<-c("CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC",
       "ATLANTIC","ATLANTIC")
colors <- c("blue", "red")  # adjust for your groups
group_colors <- colors[vec]  # assigns color to each point

#To fit environmental data on prey data
library(readr)
biomass_with_ratio <- read_csv("raw_data/biomass_with_ratio.csv")
View(biomass_with_ratio)
env_data <- biomass_with_ratio
prey_with_location <- read_csv("C:/Desktop/MY THESIS/prey_with_location.csv")
View(env_data$Location)
View(prey_with_location)
env_data_aligned <- env_data[match(prey_with_location$Location, env_data$Location), ]
View(env_data_aligned)


all(prey_with_location$Location == env_data_aligned$Location)
en <- envfit(nmds, env_data_aligned[, c("p7", "evi_mean")], 
             permutations = 999, na.rm = TRUE)
print(en)

#Plot
library(ggplot2)
site_scores <- as.data.frame(scores(nmds, display = "sites"))
site_scores$Site <- c("Curralinho", "Curralinho", "Curralinho", "Curralinho", 
                                          "Ilha", "Ilha", "Ilha", "Ilha", 
                                          "Niterói", "Niterói", "Niterói", "Niterói",
                                          "Xingó", "Xingó", "Xingó", 
                                          "Amparo", "Amparo", "Amparo", 
                                          "Gararu", "Gararu", "Gararu", 
                                          "Propriá", "Propriá", "Propriá")
# 2. Extract vector coordinates
vec_coords <- as.data.frame(scores(en, display = "vectors")) * ordiArrowMul(en)
rownames(vec_coords) <- c("p7", "evi_mean")

# 3. Final ggplot with Colors
NMDS_plot <- ggplot() +
  # Plot points colored by Site
  geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2, color = Site), size = 3) +
  # Add arrows for significant environmental variables
  geom_segment(data = vec_coords, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), color = "black", size = 1) +
  geom_text(data = vec_coords, aes(x = NMDS1, y = NMDS2, label = rownames(vec_coords)),
            color = "black", vjust = -1) +
  # Use a professional color palette
  scale_color_brewer(palette = "Set1") + 
  theme_bw() +
  labs( subtitle = paste("Stress:", round(nmds$stress, 2)),
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
#Extra 3 - CAP ORDINATION and SIMPER
library(readxl)
latest_biomass_dataaAAP <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAAP)

library(vegan)

# 1. Run CAP analysis (using original column names)
cap_model <- capscale(prey_bray ~ biomass_density + Precipitation, data = latest_biomass_dataaAAP)

# 2. Extract site scores
site_scores <- scores(cap_model, display = "sites", choices = 1:2)

# 3. Prepare site labels from Location
site_labels <- latest_biomass_dataaAAP$Location

# 4. Prepare colors for each unique Location
locations <- as.factor(latest_biomass_dataaAAP$Location)
location_levels <- levels(locations)
location_colors <- rainbow(length(location_levels))[locations]

# 5. Make a copy of the environmental data with short names
env_short <- latest_biomass_dataaAAP[, c("biomass_density", "Precipitation")]
colnames(env_short) <- c("BD", "Prep")

# 6. Fit environmental vectors with short names
envfit_res <- envfit(cap_model, env_short)

# 7. Plot the ordination
plot(site_scores, type = "n", main = "CAP Ordination (labels: Location)",
     xlab = "CAP1", ylab = "CAP2")

# 8. Add points, colored by location
points(site_scores, pch = 21, bg = location_colors, cex = 1.3)


# 10. Add environmental vectors with short labels and small font
plot(envfit_res, col = "red", add = TRUE, cex = 0.6)

# 11. Add a legend for locations
legend("bottomright", legend = location_levels, pt.bg = rainbow(length(location_levels)),
       pch = 21, title = "Location", cex = 0.7)


# 1. Set a wider right margin and enable plotting outside plot region
par(mar = c(5, 4, 3, 7), xpd = TRUE)  # 8 gives enough space on the right

# 2. Plot your ordination as usual
plot(site_scores, type = "n", main = "CAP Ordination",
     xlab = "CAP1", ylab = "CAP2")
points(site_scores, pch = 21, bg = location_colors, cex = 1.3)
plot(envfit_res, col = "red", add = TRUE, cex = 0.6)

# 3. Add the legend outside the plot
legend("bottomright", inset = c(-0.25, 0),  # Adjust -0.25 as needed for your window
       legend = location_levels, pt.bg = rainbow(length(location_levels)),
       pch = 21, title = "Location", cex = 0.7)

ggsave("CAP.png", width = 12, height = 6, units = "in", dpi = 300)


nrow(prey)
length(vec)

#to calculate SIMPER -to determine which species contribute most to the observed dissimilarity between groups of samples 
species_data <- prey[, 1:64]
View(species_data)
vec<-c("CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC",
       "ATLANTIC","ATLANTIC")
simper_result <- simper(species_data, vec)
summary(simper_result)
# View all group comparisons
names(simper_result)
# Extract the result for the Forest vs Caatinga comparison
res <- simper_result[["CAATINGA_ATLANTIC"]]  # use the actual name shown by names(simper_result)

# Convert to data frame
res_df <- as.data.frame(res)
res_df$species <- rownames(res_df)
top_species <- res_df[order(-res_df$average), ][1:10, ]

#Extra 3 ends - CAP ordination & SIMPER ends 


#Extra 4 - NMDS 1, NMDS2 AND SIMPER ON ONE PLOT TO SEE THE SPECIES(DIET COMPOSITION) CONTRIBUTING MAJORLY TO THE SIMILARITIES BETWEEN THE 2 BIOMES

library(vegan)
library(ggplot2)
library(dplyr)


nmds <- metaMDS(species_data, distance = "bray", k = 2, trymax = 100)

# Group info: group should be a factor matching the rows of species_data
group <- factor(vec)  # e.g., c("Forest", "Forest", ..., "Caatinga", ...)
site_scores <- as.data.frame(scores(nmds, display = "sites"))
site_scores$group <- group
species_fit <- envfit(nmds, species_data, permutations = 999)
species_vectors <- as.data.frame(species_fit$vectors$arrows)
View(species_vectors)
# Keep only significant species if you want:
species_vectors$pvals <- species_fit$vectors$pvals
top_simper <- res_df[order(-res_df$average), ][1:10, ]
top_species <- rownames(top_simper)

# Subset species_vectors to only those species
top_species_vectors <- species_vectors %>%
  filter(rownames(species_vectors) %in% top_species) %>%
  mutate(species = rownames(.))
ggplot() +
  # NMDS points
  geom_point(data = site_scores, 
             aes(x = NMDS1, y = NMDS2, color = group), size = 3) +
  
  # Species arrows
  geom_segment(data = top_species_vectors, 
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "darkgreen") +
  
  # Species labels
  geom_text(data = top_species_vectors, 
            aes(x = NMDS1, y = NMDS2, label = species), 
            hjust = 0.5, vjust = -0.8, color = "darkgreen", size = 3) +
  
  # Theme
  theme_minimal() +
  labs(title = "Top Species causing dissimilarity",
       x = "NMDS1", y = "NMDS2", color = "Biome") +
  theme(legend.position = "right")

install.packages("ggrepel")
library("ggrepel") # for smarter text placement

ggplot() +
  # NMDS points
  geom_point(data = site_scores, 
             aes(x = NMDS1, y = NMDS2, color = group), size = 3) +
  
  # Species arrows
  geom_segment(data = top_species_vectors, 
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "darkgreen") +
  
  # Improved labels with ggrepel to avoid overlap
  ggrepel::geom_text_repel(data = top_species_vectors, 
                           aes(x = NMDS1, y = NMDS2, label = species),
                           color = "darkgreen", size = 3, 
                           box.padding = 0.3, point.padding = 0.2,
                           segment.color = "grey50", max.overlaps = Inf) +
  
  # Axis and title
  labs(
    title = "Top Species Causing Dissimilarity",
    x = "NMDS1", y = "NMDS2", color = "Biome"
  ) +
  
  # Theme adjustments
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

#Extra 4 ends

#for Resource RICHNESS ANALYSIS
library(readxl)
HMR_IAI <- read_excel("C:/Desktop/MY THESIS/HMR_IAI.xlsx")
View(HMR_IAI)

non_zero_counts <- sapply(HMR_IAI[, c("CJ", "CA", "CJL", "CO", "IJ", "IA", "IJL", "IO", "NA", "NJ",
                                      "NJL", "NO","UJ", "UJL", "UO", "AA", "AJ", "AJL", 
                                      "GA", "GJL", "GO", "PA", "PJ", "PJL")], function(x) sum(x != 0))


View(non_zero_counts)
#non_zero_counts = resource richness, it counts the numbers of columns that have numbers more than zero for each sampling unit, so, out of the 68 food items, how many were consumed in CJ, CO, etc, for biomass, it adds them)
library(readxl)
latest_biomass_dataaAA <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAA)

Model_6 <- lm(Resource_richness ~ biomass_density * Precipitation, data = latest_biomass_dataaAA)
summary(Model_6)

Model_7 <- lm(Resource_richness ~ Precipitation, data = latest_biomass_dataaAA)
summary(Model_7)
res <- simulateResiduals(fittedModel = Model_6)
plot(res)
testDispersion(res)
testOutliers(res)

library(ggplot2)
Resource_rich <-  ggplot(latest_biomass_dataaAA, aes(x = biomass_density, y = Resource_richness, color = Precipitation)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    x = "Biomass Density",
    y = "Resource richness",
    color = "Precipitation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8.7, face = "bold") 
  )
print(Resource_rich)
ggsave(
  filename = "Resource_rich_graph.png",  
  plot = Resource_rich,                      
  width = 10,                               
  height = 6,                               
  dpi = 300                                 
)

#SUPPLEMENTARY INFORMATION - Appendix 1 
#% IAI FOR ALL LOCATIONS IN RAINY AND DRY SEASON
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
HMR_IAI <- read_excel("C:/Desktop/MY THESIS/HMR_IAI.xlsx")
View(HMR_IAI)

first_col_name <- colnames(HMR_IAI)[1]
data_prepared <- HMR_IAI %>%
  rename(Food_Item = all_of(first_col_name))

location_codes <- colnames(data_prepared)[-1]

data_long <- data_prepared %>%
  pivot_longer(
    cols = all_of(location_codes), # Gather all columns except the 'Food_Item' column
    names_to = "Code",           # This new column will contain the headers 'CJ', 'CA', etc.
    values_to = "IAI"            # This new column will contain the IAI values
  ) %>%
  # Remove rows where IAI is not a number or is zero, as they don't contribute.
  filter(!is.na(IAI) & IAI > 0)
print("--- Data correctly reshaped into long format ---")
print(head(data_long))
View(data_long)

data_mapped <- data_long %>%
  mutate(
    True_Location = case_when(
      Code %in% c("CJ", "CA", "CJL", "CO") ~ "Curralinho",
      Code %in% c("IJ", "IA", "IJL", "IO") ~ "Ilha do Ouro",
      Code %in% c("NA", "NJ", "NJL", "NO") ~ "Niterói",
      Code %in% c("UJ", "UJL", "UO")       ~ "UHE Xingó",
      Code %in% c("AA", "AJL", "AJ")       ~ "Amparo",
      Code %in% c("GA", "GJL", "GO")       ~ "Gararu",
      Code %in% c("PA", "PJL", "PJ")       ~ "Própria",
      TRUE                                 ~ "Unknown"
    ),
    True_Season = case_when(
      Code %in% c("CJ", "CA", "IJ", "IA", "NA", "NJ", "UJ", "AA", "AJL", "GA", "GJL", "PA", "PJL") ~ "Rainy",
      Code %in% c("CJL", "CO", "IJL", "IO", "NJL", "NO", "UJL", "UO", "AJ", "GO", "PJ")          ~ "Dry",
      TRUE                                                                                      ~ "Unknown"
    )
  )
appendix_table <- data_mapped %>%
  filter(True_Location != "Unknown" & True_Season != "Unknown") %>%
  # Group by the new clean Location and Season columns.
  group_by(True_Location, True_Season) %>%
  # Calculate the total IAI for each group (e.g., total for Curralinho_Rainy).
  mutate(Total_IAI_in_Group = sum(IAI, na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate the percentage for each food item within its group.
  mutate(IAI_Percentage = round((IAI / Total_IAI_in_Group) * 100, 2)) %>%
  # Create the final column headers (e.g., "Curralinho_Rainy").
  mutate(Location_Season = paste(True_Location, True_Season, sep = "_")) %>%
  # Handle cases where multiple codes belong to the same group (e.g., CJ and CA).
  group_by(Food_Item, Location_Season) %>%
  summarise(IAI_Percentage = sum(IAI_Percentage), .groups = 'drop') %>%
  # Pivot back to the final wide format for the appendix.
  pivot_wider(
    names_from = Location_Season,
    values_from = IAI_Percentage,
    values_fill = 0 # Fill empty cells with 0
  ) %>%
  # Arrange alphabetically for a clean look.
  arrange(Food_Item)

View(appendix_table) 
write.csv(appendix_table, "IAI % IN 7 LOCATIONS.csv", row.names = FALSE) 

#AQUATIC or TERRESTIAL
library(dplyr)
library(tibble)

food_items <- c(
  "Acari", "Amphipoda", "Aranae", "Baetidae", "Caenidae", "Calamoceratidae", "Ceratopogonidae", 
  "Chaoboridae", "Chironomidae (L)", "Chironomidae(pupa)", "Cladocera", "Colembola", 
  "Coleoptera", "Coleoptera (larva)", "Copepoda", "Corixidae", "Diptera (adult)", 
  "Diptera (pupa)", "Dytiscidae", "Elmidae", "Empididae", "Ephemeroptera", "Filamentous Algae", 
  "Formicidae", "Gerrridae", "Helicopsychidae", "Hemiptera", "Hydracarina", "Hydropsychidae", 
  "Hymenoptera", "Insect Remains", "Invertebrate eggs", "Isoptera", "Lampyridae", "Leptoceridae", 
  "Leptohyphes", "Leptophlebiidae", "Libellulidae", "Naucoridae", "Noteridae", "Odonata (N)", 
  "Organic matter", "Orthoptera", "Ostracoda", "Philopotamidae", "Plant Matter", "Plecoptera", 
  "Pleidae", "Pseudoscorpionida", "Psocoptera", "Psychodidae", "Pyralidae", "Scales", 
  "Sediments", "Seeds and Fruits", "Shrimp (L)", "Simullidae", "Staphynilidae", "Thysanura", 
  "Tingidae", "Trichoptera", "Trichoptera (L)", "Vellidae", "Vespidae"
)

# Define aquatic items (everything else will be classified as terrestrial)
aquatic_items <- c(
  "Amphipoda", "Baetidae", "Caenidae", "Calamoceratidae", "Ceratopogonidae", "Chaoboridae", 
  "Chironomidae (L)", "Chironomidae(pupa)", "Cladocera", "Copepoda", "Corixidae", 
  "Dytiscidae", "Elmidae", "Empididae", "Ephemeroptera", "Filamentous Algae", 
  "Gerrridae", "Helicopsychidae", "Hemiptera", "Hydracarina", "Hydropsychidae", 
  "Leptoceridae", "Leptohyphes", "Leptophlebiidae", "Libellulidae", "Naucoridae", 
  "Noteridae", "Odonata (N)", "Ostracoda", "Philopotamidae", "Plecoptera", 
  "Pleidae", "Shrimp (L)", "Simullidae", "Trichoptera", "Trichoptera (L)", "Vellidae"
)

# Create dataframe and classify
food_df <- tibble(Food_Item = food_items) %>%
  mutate(Habitat = ifelse(Food_Item %in% aquatic_items, "Aquatic", "Terrestrial"))

# View the result
print(food_df)
View(food_df)
write.csv (food_df, "food_df.csv")
library(readxl)
IAI <- read_excel("C:/Desktop/MY THESIS/IAI % IN 7 LOCATIONS.xlsx")
View(IAI)
IAI  <- IAI  %>%
  mutate(Habitat = ifelse(Food_Item %in% aquatic_items, "Aquatic", "Terrestrial")) %>%
  arrange(desc(Habitat)) 
view(IAI)
write.csv(IAI, "IAI.csv")


#3D GRAPH FOR RESOURCE RICHNESS
## Plot ##
library(lme4)
library(ggplot2)
library(ggfortify)
library(data.table)
library(metafor)
library(gridExtra)
library(tidyverse)
library(visreg)
library(ggpubr)

# Your Model:
Model_6 <- lm(Resource_richness ~ biomass_density * Precipitation, data = latest_biomass_dataaAA)

library(visreg)
library(ggplot2)

g1 <- visreg2d(
  fit = Model_6,
  xvar = "biomass_density",
  yvar = "Precipitation",
  plot.type = "gg"
) + 
  geom_point(
    data = Model_6$model, # Access the data used in the model
    aes(x = biomass_density, y = Precipitation),
    colour = "black", alpha = 0.6, size = 2
  ) +
  theme_bw() + 
  ylab("Precipitation") + 
  xlab("Biomass Density") + 
  theme(axis.title = element_text(colour = "black", size = 12, face = "bold")) + 
  theme(axis.text = element_text(colour = "black", size = 12)) + 
  guides(fill = guide_legend(title = "Resource Richness")) + 
  theme(legend.title = element_text(size = 12, colour = "black", face = "bold")) + 
  theme(legend.text = element_text(size = 10, colour = "black")) + 
  ggtitle("Resource Richness across Biomass and Precipitation") + 
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  scale_fill_gradient(low = "white", high = "darkorange")

g1

ggsave("Resource richness 3d.png", width = 12, height = 6, units = "in", dpi = 300)



#Amundsen 
#Calculate Pi(Prey specific abundance) and Foi for all samples (Individuals in a location at a time) (needed to plot amundsen graph)
library(readxl)

H_M_R_new <- read_excel("C:/Desktop/MY THESIS/H_M R_new.xlsx")
View(H_M_R_new)

library(dplyr)
library(tidyr)
all_prey <- names(H_M_R_new)[10:ncol(H_M_R_new)]

Diet_Summary <- H_M_R_new %>%
  # 1. Create a temporary column for Total Stomach Volume per individual
  mutate(Total_Stomach = rowSums(across(all_of(all_prey)), na.rm = TRUE)) %>%
  # 2. Pivot to long format
  pivot_longer(cols = all_of(all_prey), names_to = "Prey", values_to = "Volume") %>%
  # 3. Group by Site, Month, and Prey
  group_by(Local, Month, Prey) %>%
  summarise(
    # FOi: Number of fish that ate it / Total fish in that sample
    FOi = sum(Volume > 0) / n(),
    # Pi: Average contribution in fish that actually ate it
    Pi = mean(Volume[Volume > 0] / Total_Stomach[Volume > 0], na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  # 4. Clean up items that were never eaten in a specific sample
  mutate(Pi = replace_na(Pi, 0))

View(Diet_Summary)
write.csv(Diet_Summary, "Pi_Foi.csv")

#Pi_Foi = Pi_complete + FOi
#PLOT, each site
# Load data
library(readr)
Pi_Foi <- read_csv("C:/Desktop/MY THESIS/Pi_Foi.csv")
View(Pi_Foi)
# Choose one study site (e.g., "Amparo")
site_name <- "Gararu"
site_data <- subset(Pi_Foi, Local == site_name)

# --- Filter for dominant preys only ---
threshold_pi <- quantile(site_data$Pi, 0.95, na.rm = TRUE)
threshold_foi <- quantile(site_data$FOi, 0.95, na.rm = TRUE)

dominant_preys <- subset(site_data, Pi >= threshold_pi | FOi >= threshold_foi)

print(dominant_preys)

# PLOT
library(ggplot2)
library(ggrepel)

ggplot(dominant_preys, aes(x = FOi, y = Pi, label = Prey)) +
  
  # 1. Add Vertical Line (FOi = 0.5)
  geom_vline(xintercept = 0.50, linetype = "dashed", color = "red", linewidth = 0.8) +
  
  # 2. Add Horizontal Line (Pi = 50)
  # Changed from geom_abline (diagonal) to geom_hline (horizontal)
  geom_hline(yintercept = 50, linetype = "dashed", color = "blue", linewidth = 0.8) +
  
  # 3. Plot points and labels
  geom_point(color = "forestgreen", size = 3) +
  geom_text_repel(size = 3, max.overlaps = 30) +
  
  # 4. Define scales
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) + 
  
  # 5. Set Labels
  labs(
    title = paste("Dominant Preys - Amundsen Graph for", site_name, "(95th percentile)"),
    x = "Frequency of Occurrence (FOi)",
    y = "Prey Specific Abundance (Pi)"
  ) +
  
  # 6. Set Theme, including reduced title font size
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
  )

ggsave("Gararu_amundsen_new.png", width = 10, height = 6, dpi=300)


#FOR SUPPLEMENTARY INFORMATION (SAMPLE - SITE*MONTH)
library(ggplot2)
library(ggrepel)
library(dplyr)

# 1. Load data
library(readr)
Pi_Foi <- read_csv("C:/Desktop/MY THESIS/Pi_Foi.csv")
View(Pi_Foi)
# 2. Data Cleaning: Ensure 'Local' names are consistent
# (Fixing 'Ilha do ouro' vs 'Ilha do Ouro')
Pi_Foi <- Pi_Foi %>%
  mutate(Local = tools::toTitleCase(tolower(Local))) %>%
  filter(!is.na(Local))

# 3. Define Dominant Prey per Sample (Site x Month)
# Instead of a global threshold, we calculate it for each unique sample
Pi_filtered <- Pi_Foi %>%
  group_by(Local, Month) %>%
  mutate(
    thresh_pi = quantile(Pi, 0.95, na.rm = TRUE),
    thresh_foi = quantile(FOi, 0.95, na.rm = TRUE)
  ) %>%
  filter(Pi >= thresh_pi | FOi >= thresh_foi) %>%
  ungroup()

# 4. Generate the Faceted Amundsen Plot
g_samples <- ggplot(Pi_filtered, aes(x = FOi, y = Pi, label = Prey)) +
  
  # Reference Lines (Standard Amundsen interpretation)
  geom_vline(xintercept = 0.50, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "blue", linewidth = 0.5) +
  
  # Data Points
  geom_point(color = "forestgreen", size = 2, alpha = 0.7) +
  
  # Labels (using ggrepel to avoid overlap in small facets)
  geom_text_repel(size = 2, max.overlaps = 10, segment.color = "grey50") +
  
  # THE KEY PART: Facet by Site (Local) and Month (Season)
  facet_wrap(~Local + Month, ncol = 4) +
  
  # Scales and Labels
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 50)) + 
  
  labs(
    title = "Feeding Strategy of H. marginatus: Sample-Level Analysis (Site x Month)",
    subtitle = "Horizontal line (50% Pi) and Vertical line (50% FOi) delineate quadrants",
    x = "Frequency of Occurrence (FOi)",
    y = "Prey Specific Abundance (Pi)"
  ) +
  
  theme_bw() +
  theme(
    strip.text = element_text(size = 8, face = "bold"), # Facet headers
    axis.text = element_text(size = 7),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  )

ggsave("Amundsen_Faceted_Samples.pdf", g_samples, width = 12, height = 15)

getwd()


#FOR COMBINED
# Load necessary libraries
library(readr)
Pi_Foi <- read_csv("C:/Desktop/MY THESIS/Pi_Foi.csv")
View(Pi_Foi)
library(ggplot2)
library(ggrepel)

threshold_pi <- quantile(Pi_Foi$Pi, 0.99, na.rm = TRUE)
threshold_foi <- quantile(Pi_Foi$FOi, 0.99, na.rm = TRUE)

#Filter for Dominant Preys Across All Sites 
dominant_preys_all_sites <- subset(Pi_Foi, Pi >= threshold_pi | FOi >= threshold_foi)

#Generate the Combined Amundsen Graph
ggplot(dominant_preys_all_sites, aes(x = FOi, y = Pi, label = Prey, color = Local)) +
  
  # Plot the data points
  geom_point(size = 3) +
  
  # Labels for prey items
  geom_text_repel(aes(label = Prey), size = 3, max.overlaps = 15, show.legend = FALSE) +
  
  # Vertical line at FOi = 0.50 (50% Frequency)
  geom_vline(xintercept = 0.50, linetype = "dashed", color = "red", linewidth = 0.8) +
  
  # Horizontal line at Pi = 50 (50% Specific Abundance)
  geom_hline(yintercept = 50, linetype = "dashed", color = "blue", linewidth = 0.8) +
  
labs(
  title = "Amundsen Graph of Dominant Preys Across All 7 Sites 
           (99th Percentile)",
  x = "Frequency of Occurrence (FOi)",
  y = "Prey Specific Abundance (Pi)",
  color = "Site Name" 
) +
  
  # Set axes to standard Amundsen scales
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) + 
  theme_bw() +
  theme(legend.position = "right")


ggsave("Amundsen_Combined_Graph.png", width = 12, height = 6, units = "in", dpi = 300)

getwd()

#FOR COMBINED BUT GENERAL BEHAVIOUR
library(dplyr)
library(ggplot2)
library(ggrepel)

library(readr)
Pi_Foi <- read_csv("C:/Desktop/MY THESIS/Pi_Foi.csv")
View(Pi_Foi)
diet_data <- Pi_Foi

# 2. Calculate General Behavior metrics
# We calculate the mean FO and the mean Pi (excluding zeros) for each prey
general_amundsen <- diet_data %>%
  filter(!is.na(Prey)) %>%
  group_by(Prey) %>%
  summarise(
    # General FO: Average frequency across all samples
    General_FO = mean(FOi, na.rm = TRUE),
    # General Pi: Average abundance ONLY when the prey was present
    General_Pi = mean(Pi[Pi > 0], na.rm = TRUE)
  ) %>%
  # If your Pi is in 0-100 scale, convert to 0-1 for the graph
  mutate(General_Pi = ifelse(General_Pi > 1, General_Pi / 100, General_Pi)) %>%
  filter(!is.na(General_Pi))

# 3. Plot the General Amundsen Graph
General_feeding_amundsen <- ggplot(general_amundsen, aes(x = General_FO, y = General_Pi)) +
  # Quadrant lines
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") +
  
  # Points
  geom_point(size = 3, color = "darkorange", alpha = 0.7) +
  
  # Labels for important items (adjust thresholds as needed)
  geom_text_repel(data = subset(general_amundsen, General_FO > 0.3 | General_Pi > 0.4),
                  aes(label = Prey), size = 3, fontface = "italic") +
  
  # Formatting
  scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
  labs(x = "Frequency of Occurrence (FO)",
       y = "Prey-Specific Abundance (Pi)") +
  theme_bw()

ggsave(
  "General_feeding_amundsen.png",
  plot = General_feeding_amundsen,       
  width = 10,     
  height = 7,      
  dpi = 300
)


#FOR CORRELATION

library(readxl)
correlation <- read_excel("C:/Desktop/MY THESIS/correlation.xlsx")
View(correlation)

cor(correlation[, c("Levins_index", "Resource_richness", "Biomass_ratio")], use = "complete.obs", method = "pearson")


#TO CALCULATE THE IAI, FO, V% tables (grand total, combining all the values from all 24 samples (site-month) for all 68 food items)

library(dplyr)
#IAI
library(readxl)
HMR_IAI <- read_excel("C:/Desktop/MY THESIS/HMR_IAI.xlsx")
View(HMR_IAI)
HMR_IAI_df <- as.data.frame(HMR_IAI)
# 2. Calculate the General Behavior
# We take the mean of all columns (except the 'Prey' column)
general_iai <- HMR_IAI_df %>%
  mutate(Mean_IAI = rowMeans(select(., -Prey), na.rm = TRUE)) %>%
  # Standardize the result so it sums to 100%
  mutate(IAi = (Mean_IAI / sum(Mean_IAI)) * 100) %>%
  # Select the columns we want and sort by importance
  select(Prey, IAi) %>%
  arrange(desc(IAi))

# 3. View and Save
View(general_iai)
write.csv(general_iai, "General_IAI_Behavior.csv", row.names = FALSE)

#FOI
library(readxl)
foi_raw <- read_excel("C:/Desktop/MY THESIS/FOI.xlsx")
foi_df <- as.data.frame(foi_raw)

# 2. Calculate General Behavior for FOi
# Since your FOI table has Samples in rows, we take the mean of each COLUMN
general_foi <- foi_df %>%
  summarise(across(-Sample, ~ mean(. , na.rm = TRUE))) %>%
  # Flip the table so Prey are in rows (Long format)
  pivot_longer(cols = everything(), names_to = "Prey", values_to = "FOi") %>%
  # Sort from most frequent to least frequent
  arrange(desc(FOi))

# 3. View and Save
View(general_foi)
write.csv(general_foi, "General_FOi_Behavior.csv", row.names = FALSE)


#FP (VO%)
library(readxl)
library(dplyr)
library(tidyr)

# 1. Load your FP data
fp_raw <- read_excel("C:/Desktop/MY THESIS/FP.xlsx")
fp_df <- as.data.frame(fp_raw)

general_fp <- fp_df %>%
  summarise(across(-Sample, ~ mean(. , na.rm = TRUE))) %>%
  # Flip the table so Prey are in rows (Long format)
  pivot_longer(cols = everything(), names_to = "Prey", values_to = "General_FP") %>%
  # Re-standardize to 100% (or 1.0) to ensure the total is consistent
  mutate(General_FP_Percent = (General_FP / sum(General_FP)) * 100) %>%
  # Sort from most preferred to least preferred
  arrange(desc(General_FP_Percent))

# 3. View and Save
View(general_fp)
write.csv(general_fp, "General_FP_Behavior.csv", row.names = FALSE)


#PART 2, MODELLING WITH BETTER VARIABLE NUMBERS (RAINFALL)
#Philippe and some edits from me, Philippe did all of the Evi data and gave me a csv list

setwd(THESIS)
rm(list=ls())

library(ncdf4)
library(sf)
library(terra)
library(rnaturalearth)
library(viridis)

# 1. LOAD DATA ---------------------------------------------------------------
library(readr)
samples_list <- read_csv("raw_data/Dates_coordinates - Sheet1 (2).csv")
samples_list$date <- as.Date(samples_list$date)

all_results <- list()

# Load Brazil borders
r_brazil <- ne_countries(country = "Brazil", returnclass = "sf")
brazil_vect <- vect(r_brazil)

# --- START PDF EXPORT ---
# This creates a file in your folder. All plots after this will go into it.
pdf("Sampling_Site_Precipitation_map.pdf", width = 8, height = 10)

# 2. START THE LOOP ----------------------------------------------------------
for(i in 1:nrow(samples_list)) {
  
  target_date <- samples_list$date[i]
  lat_pt      <- samples_list$lat[i]
  lon_pt      <- samples_list$lon[i]
  n_days      <- 7
  
  pre_dates <- seq(target_date - (n_days-1), target_date, by = "day")
  nc_files  <- file.path(paste0("./data/GPCPDAY_L3_", format(pre_dates, "%Y%m%d"), "_V3.3.nc4"))
  
  if(!all(file.exists(nc_files))) {
    message(paste("Skipping", samples_list$site[i], "- files missing"))
    next
  }
  
  # Philippe's Core Extraction Logic
  nc0  <- nc_open(nc_files[1])
  lats <- ncvar_get(nc0, "lat")
  lons <- ncvar_get(nc0, "lon")
  lat_nc <- which.min(abs(lats - lat_pt))
  lon_nc <- which.min(abs(lons - lon_pt))
  nc_close(nc0)
  
  precip_df <- data.frame(
    date = pre_dates,
    precip_mm = sapply(nc_files, function(f) {
      nc <- nc_open(f)
      p <- ncvar_get(nc, "precip", start = c(lon_nc, lat_nc, 1), count = c(1, 1, 1))
      fill <- ncatt_get(nc, "precip", "_FillValue")$value
      nc_close(nc)
      ifelse(p == fill, NA, p)
    })
  )
  
  # Calculate frames and store results
  all_results[[i]] <- data.frame(
    site = samples_list$site[i], 
    date = target_date, 
    p1 = precip_df$precip_mm[7],
    p3 = sum(tail(precip_df$precip_mm, 3), na.rm = TRUE),
    p7 = sum(precip_df$precip_mm, na.rm = TRUE)
  )
  
  # Mapping Logic
  cum_precip <- array(0, dim = c(length(lons), length(lats)))
  for(f in nc_files){
    nc <- nc_open(f); p <- ncvar_get(nc, "precip")
    fill <- ncatt_get(nc, "precip", "_FillValue")$value; nc_close(nc)
    p[p == fill] <- NA
    cum_precip <- cum_precip + p
  }
  
  r <- rast(t(cum_precip), crs = "EPSG:4326")
  ext(r) <- ext(min(lons), max(lons), min(lats), max(lats))
  r <- flip(r, "vertical")
  rb_cropped <- crop(mask(r, brazil_vect), brazil_vect)
  
  # Plotting (Directly to PDF)
  target_loc <- vect(data.frame(x = lon_pt, y = lat_pt), geom = c("x","y"), crs = "EPSG:4326")
  
  plot(rb_cropped, main = paste("Site:", samples_list$site[i], "\nDate:", target_date, 
                                "\n7-day Total:", round(sum(precip_df$precip_mm, na.rm=T), 2), "mm"),
       col = viridis(20))
  plot(brazil_vect, add = TRUE, border = "white", lwd = 1)
  points(target_loc, col = "red", pch = 19, cex = 1.5)
}

# --- CLOSE PDF EXPORT ---
dev.off() 

# 3. SAVE FINAL DATA ---------------------------------------------------------
final_table <- do.call(rbind, all_results)
write.csv(final_table, "precipitation_all_sampling.csv", row.names = FALSE)
getwd()
message("Task complete! Check your folder for the PDF and CSV files.")


#lmer with latest precipitation and EVI data
# Niche width model -------------------------------------------------------
library(lme4)  
library(car)
library(lmerTest)

library(readr)
all_data <- read_csv("raw_data/Modelling_complete_list_EVI.csv")
View(all_data)
model_18 <- lmer(Levins_index ~ p7 + evi_mean + (1|site), data = all_data)
summary(model_18)
#Check for Multi-collinearity 
vif(model_18)

#Model diagnostics
library("DHARMa")
res_3 <- simulateResiduals(fittedModel = model_18)
plot(res_3)
testDispersion(res_3)
testOutliers(res_3)

#plot
# install.packages("patchwork")
library(patchwork)
library(visreg)
library(ggplot2)

# Create Plot A: EVI
p1 <- visreg(model_18, "evi_mean", gg = TRUE) + 
  theme_bw() + 
  labs(title = "A", 
       x = "Enhanced Vegetation Index (EVI)", 
       y = "Levin's Index (Niche Width)")

# Create Plot B: Rain
p2 <- visreg(model_18, "p7", gg = TRUE) + 
  theme_bw() + 
  labs(title = "B", 
       x = "7-day Precipitation (mm)", 
       y = "Levin's Index (Niche Width)")

# Display them side by side
combined_plot_1 <- p1 + p2

ggsave(
  filename = "Niche width_lmer.png",  # file name
  plot = combined_plot_1,                      # your ggplot object    # folder path (change to where you want it saved)
  width = 10,                               # width in inches
  height = 6,                               # height in inches
  dpi = 300                                 # resolution for print quality
)
#ctrl + shift + R to create new sessions
# Resource richness model -------------------------------------------------
View(all_data)
model_19 <- lmer(Resource_richness ~ p7 + evi_mean + (1|site), data = all_data)
summary(model_19)

#Model diagnostics
library("DHARMa")
res_4 <- simulateResiduals(fittedModel = model_19)
plot(res_4)
testDispersion(res_4)
testOutliers(res_4)

# Create Plot A: EVI
p3 <- visreg(model_19, "evi_mean", gg = TRUE) + 
  theme_bw() + 
  labs(title = "A", 
       x = "Enhanced Vegetation Index (EVI)", 
       y = "Resource_richness")

# Create Plot B: Rain
p4 <- visreg(model_19, "p7", gg = TRUE) + 
  theme_bw() + 
  labs(title = "B", 
       x = "7-day Precipitation (mm)", 
       y = "Resource_richness")

# Display them side by side
combined_plot_2 <- p3 + p4

ggsave(
  filename = "Resource richness_lmer.png",  # file name
  plot = combined_plot_2,                      # your ggplot object    # folder path (change to where you want it saved)
  width = 10,                               # width in inches
  height = 6,                               # height in inches
  dpi = 300                                 # resolution for print quality
)
# Biomass ratio model -----------------------------------------------------
View(all_data)
data_no_inf <- all_data[-1, ]
data_no_inf$ratioo <- log(data_no_inf$ratio)
model_20 <- lmer(ratioo ~ p7 + evi_mean + (1|site), data = data_no_inf)
summary(model_20)

#Model diagnostics
library("DHARMa")
res_5 <- simulateResiduals(fittedModel = model_20)
plot(res_5)
testDispersion(res_5)
testOutliers(res_5)

# Create Plot A: EVI
p5 <- visreg(model_20, "evi_mean", gg = TRUE) + 
  theme_bw() + 
  labs(title = "A", 
       x = "Enhanced Vegetation Index (EVI)", 
       y = "Biomass_ratio")

# Create Plot B: Rain
p6 <- visreg(model_20, "p7", gg = TRUE) + 
  theme_bw() + 
  labs(title = "B", 
       x = "7-day Precipitation (mm)", 
       y = "Biomass_ratio")

# Display them side by side
combined_plot_3 <- p5 + p6

ggsave(
  filename = "Biomass ratio_lmer.png",  # file name
  plot = combined_plot_3,                      # your ggplot object    # folder path (change to where you want it saved)
  width = 10,                               # width in inches
  height = 6,                               # height in inches
  dpi = 300                                 # resolution for print quality
)


plot(p3~evi_mean, data = all_data)

