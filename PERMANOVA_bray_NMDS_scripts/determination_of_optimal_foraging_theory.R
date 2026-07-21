# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
# 1. Read the dataset (preserving original column names)
df  <- read_csv("env_data_aligned.csv")
# 2. Define "high" rainfall threshold (using the median value)
threshold <- median(df$Precip_p15_MERGE)

population_summary <- df %>%
  filter(Precip_p15_MERGE >= threshold) %>%
  select(Location, Month, Precip_p15_MERGE, Acari:Vespidae) %>%
  # Pivot to long format to easily sort items
  pivot_longer(cols = Acari:Vespidae, names_to = "Item", values_to = "Abundance") %>%
  # Group by each distinct population row
  group_by(Location, Month, Precip_p15_MERGE) %>%
  # Arrange items in descending order of abundance for each population
  arrange(desc(Abundance)) %>%
  slice_head(n = 3) %>%
  summarise(
    Predominant_Items = paste0(Item, " (", round(Abundance * 100, 1), "%)", collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(Precip_p15_MERGE))

print(population_summary)
write.csv(population_summary,"optimal foraging theory.csv")
