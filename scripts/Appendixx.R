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