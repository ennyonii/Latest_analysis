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

