############INFLUENCE OF RAINFALL AND TERRESTRIAL BIOMASS ON PREY COMPOSITION#############
############ENIOLA OLU-AYORINDE###################
############2026-12-05##############################
#TO CALCULATE the 3 indexes I will be using for my thesis (IAI, Levin's index, resource biomass)

#T calculate IAI
library(readxl)
library(readr)
The_food_items <- read_excel("raw_data/H_M R_new.xlsx")
dim(The_food_items)
View(The_food_items)
columns_to_sum_CJ <- The_food_items[1:20, 9:73]  # Columns I to BU are columns 9 to 73, THIS OPERATION SUMS THE SPECIFIC FIGURES IN THE COLUMNS OF EACH FOOD ITEM UNDER THE SPECIFIED LOCATION-DATE, the result wiill be given in a table for each food item
CurrJan <- colSums(columns_to_sum_CJ, na.rm = TRUE)
columns_to_sum_CA <- The_food_items[29:48 , 9:73]  # Columns I to BU are columns 9 to 72
CurrApr <- colSums(columns_to_sum_CA, na.rm = TRUE)
print(CurrApr)
columns_to_sum_CJL <- The_food_items[54:73 , 9:73]  # Columns I to BU are columns 9 to 72
CurrJul <- colSums(columns_to_sum_CJL, na.rm = TRUE)
print(CurrJul)
columns_to_sum_CO <- The_food_items[79:98 , 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
CurrOct <- colSums(columns_to_sum_CO, na.rm = TRUE)
print(CurrOct)
columns_to_sum_IJ <- The_food_items[104:123 , 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
IihaJan <- colSums(columns_to_sum_IJ, na.rm = TRUE)
print(IihaJan)
columns_to_sum_IA <- The_food_items[129:148 , 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
IihaApr <- colSums(columns_to_sum_IA, na.rm = TRUE)
print(IihaApr)
columns_to_sum_IJL <- The_food_items[154:171 , 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
IihaJul <- colSums(columns_to_sum_IJL, na.rm = TRUE)
print(IihaJul)
columns_to_sum_IO <- The_food_items[177:195 , 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
IihaOct <- colSums(columns_to_sum_IO, na.rm = TRUE)
print(IihaOct)
columns_to_sum_NJ <- The_food_items[201:220 , 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
NiteJan <- colSums(columns_to_sum_NJ, na.rm = TRUE)
print(NiteJan)
columns_to_sum_NA <- The_food_items[226:245 , 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
NiteApr <- colSums(columns_to_sum_NA, na.rm = TRUE)
print(NiteApr)
columns_to_sum_NJL <- The_food_items[251:270 , 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
NiteJul <- colSums(columns_to_sum_NJL, na.rm = TRUE)
print(NiteJul)
columns_to_sum_NO <- The_food_items[276:295 , 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
NiteOct <- colSums(columns_to_sum_NO, na.rm = TRUE)
print(NiteOct)
columns_to_sum_UJ <- The_food_items[301:320, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
UHEJan <- colSums(columns_to_sum_UJ, na.rm = TRUE)
print(UHEJan)
columns_to_sum_UJL <- The_food_items[326:344, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
UHEJul <- colSums(columns_to_sum_UJL, na.rm = TRUE)
print(UHEJul)
columns_to_sum_UO <- The_food_items[350:361, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
UHEOct <- colSums(columns_to_sum_UO, na.rm = TRUE)
print(UHEOct)
columns_to_sum_AJ <- The_food_items[367:386, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
AmpJan <- colSums(columns_to_sum_AJ, na.rm = TRUE)
print(AmpJan)
columns_to_sum_AA <- The_food_items[417:435, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
AmpApr <- colSums(columns_to_sum_AA, na.rm = TRUE)
print(AmpApr)
columns_to_sum_AJL <- The_food_items[392:411, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
AmpJul <- colSums(columns_to_sum_AJL, na.rm = TRUE)
print(AmpJul)
columns_to_sum_GA <- The_food_items[441:460, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
GarApr <- colSums(columns_to_sum_GA, na.rm = TRUE)
print(GarApr)
columns_to_sum_GJl <- The_food_items[466:483, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
GarJul <- colSums(columns_to_sum_GJl, na.rm = TRUE)
print(GarJul)
columns_to_sum_GO <- The_food_items[489:508, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
GarOct <- colSums(columns_to_sum_GO, na.rm = TRUE)
print(GarOct)
columns_to_sum_PJ <- The_food_items[514:533, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
PropJan <- colSums(columns_to_sum_PJ, na.rm = TRUE)
print(PropJan)
columns_to_sum_PA<- The_food_items[539:554, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
PropApr <- colSums(columns_to_sum_PA, na.rm = TRUE)
print(PropApr)
columns_to_sum_PJL<- The_food_items[560:568, 9:73]  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
PropJul <- colSums(columns_to_sum_PJL, na.rm = TRUE)
print(PropJul)



total_sum_PJL<- sum(The_food_items[560:568, 9:73], na.rm = TRUE) #Calculate the total sum of each location-month presented.This particular line of code is 
#doe Propria in July, do for all Location month and don't forget to change row and column numbers



FP_Total_ratio_PJL<-  PropJul/ total_sum_PJL#Divide each food item number sum (sum of values e.g 0+0+0.01+0.03 in each food item column, not number of fish, e.g 20 in the curr january segment) by the total sum of all food items numbers (specific numbers), not how many fish are there in each location-month presented in rows demarcation, but because it is R, you can do for all of them at the same time, 9:72 will give the answers of all, but individually, AT THE SAME TIME)
View(columns_to_sum_CJ)
##FP is sum of one row/sum of all rows in the sheet, FO= sum of columns in a row that has numbers(not the numbers)/Sum of the whole columns in the specific row), IAI= FP*FO OF ONE ROW/FP*FO OF ALL ROWS

#to calculate foi:
List_columns_numeric_values_PJL <- The_food_items[560:568, 9:73]  
food_presence_PJL <- ifelse(List_columns_numeric_values_PJL > 0, 1, 0)  #it creates a binary matrix where each cell is 1 if the corresponding value in List_columns_numeric_values_CJ is greater than 0, and 0 otherwise.
food_occurrence_PJL <- colSums(food_presence_PJL, na.rm = TRUE) #tHESE(AND THE CODE ABOVE IT TOGETHER) COUNTS/ADD THE NUMBER OF COLUMNS THAT HAVE NUMERIC VALUES GREATER THAN 0 (THAT HAVE BEEN REPRESENTED BY "1" WITH THE CODE ABOVE)in each food item row(it does it in mass for the selected rows and columns, but gives individual answers for all), FOR CJ (FILAMENTOUS ALGAE row FOR INSTANCE, 5 OUT OF 20 OF THE columns HAD NUMBERS GREATER THAN 0)
total_samples_PJL <- nrow(List_columns_numeric_values_PJL)  #COUNTS THE NUMBER OF ROWS
#sO FREQUENCY OF OCCURRENCE IS THE NUMBER OF COLUMNS THAT HAVE NUMERIC VALUE GREATER THAN 0, WHICH IS 5 (FOOD OCCURRENCE)/THE NUMBER OF ROWS (NOT THE SUM OF VALUES IN THE ROWS), WHICH IS 20
FO_PJL <- food_occurrence_PJL / total_samples_PJL

total_FO <- zapsmall(sum(FO_PJL))
View(total_FO)

FPFO_PJL <- FP_Total_ratio_PJL * FO_PJL
sum(FPFO_PJL)
IAI_PJL <- FPFO_PJL / sum(FPFO_PJL)



#nOW, LEVIN'S index, lEVIN'S INDEX = 1/SUM OF SQUARES OF ALL THE FP
# List of all FP_Total_ratio variables for different location-months
# Add a small constant to avoid division by zero
fp_list <- list(
  CJ = FP_Total_ratio_CJ,
  CA = FP_Total_ratio_CA,
  CJL = FP_Total_ratio_CJL,
  CO = FP_Total_ratio_CO,
  IJ = FP_Total_ratio_IJ,
  IA = FP_Total_ratio_IA,
  IJL = FP_Total_ratio_IJL,
  IO = FP_Total_ratio_IO,
  NJ = FP_Total_ratio_NJ,
  NAPRIL = FP_Total_ratio_NA,
  NJL = FP_Total_ratio_NJL,
  NO = FP_Total_ratio_NO,
  UJ = FP_Total_ratio_UJ,
  UJL = FP_Total_ratio_UJL,
  UO = FP_Total_ratio_UO,
  AJ = FP_Total_ratio_AJ,
  AA = FP_Total_ratio_AA,
  AJL = FP_Total_ratio_AJL,
  GA = FP_Total_ratio_GA,
  GJL = FP_Total_ratio_GJL,
  GO = FP_Total_ratio_GO,
  PJ = FP_Total_ratio_PJ,
  PA = FP_Total_ratio_PA,
  PJL = FP_Total_ratio_PJL
)

# Apply Levin’s Index formula to all location-months
levins_indices <- sapply(fp_list, function(fp) {
  1 / sum(fp^2, na.rm = TRUE)
})
# Print Levin’s Index for all location-months
print(levins_indices)


#TO EXPORT NECESSARY RESULTS TO EXCEL
library(dplyr)
library(tidyr)
install.packages("writexlsx")

all_dfs <- Filter(is.data.frame, mget(ls())) #TO DOWNLOAD EVERYTHING IN THE GLOBAL ENVIRONMENT THAT IS IN DATAFRAME FORMAT TO EXCEL
install.packages("openxlsx") 
library(openxlsx)
openxlsx::write.xlsx(all_dfs, "all_results.xlsx")

getwd() 
library(writexl)


# Convert results into a dataframe
levins_results_df <- data.frame(Location_Month = names(levins_indices), Levin_Index = levins_indices)
library(writexl)
# Save to Excel
write_xlsx(levins_results_df, "Levin_Index_Results.xlsx")


install.packages("openxlsx")  
library(openxlsx)
data_frame <- data.frame(
  IAI_AA = IAI_AA,
  IAI_AJ = IAI_AJ,
  IAI_AJL = IAI_AJL,
  IAI_CA = IAI_CA,
  IAI_CJ = IAI_CJ,
  IAI_CJL = IAI_CJL,
  IAI_CO = IAI_CO,
  IAI_GA = IAI_GA,
  IAI_GJL = IAI_GJL,
  IAI_GO = IAI_GO,
  IAI_IA = IAI_IA,
  IAI_IJ = IAI_IJ,
  IAI_IJL = IAI_IJL,
  IAI_IO = IAI_IO,
  IAI_NA = IAI_NA,
  IAI_NJ = IAI_NJ,
  IAI_NJL = IAI_NJL,
  IAI_NO = IAI_NO,
  IAI_PA = IAI_PA,
  IAI_PJ = IAI_PJ,
  IAI_PJL = IAI_PJL,
  IAI_UJ = IAI_UJ,
  IAI_UJL = IAI_UJL,
  IAI_UO = IAI_UO
)

write.xlsx(data_frame, file = "output.xlsx")



library(writexl)
# Convert named numeric vectors to data frames
IAI_NJL_df <- data.frame(IAI_NJL = IAI_NJL)
IAI_NO_df <- data.frame(IAI_NO = IAI_NO)

# Combine them into a single data frame
combined_data <- cbind(IAI_NJL_df, IAI_NO_df)

write_xlsx(combined_data, "IAI_data.xlsx")


#Now, biomass
total_sum_PJL<- sum(The_food_items[560:568, 9:73], na.rm = TRUE) 
total_sum_CJ <- sum(The_food_items[1:20 , 9:73], na.rm = TRUE)
total_sum_CA <- sum(The_food_items[29:48 , 9:73], na.rm = TRUE) 
total_sum_CJL <- sum(The_food_items[54:73 , 9:73], na.rm = TRUE) 
total_sum_CO <- sum(The_food_items[79:98 , 9:73], na.rm = TRUE) 
total_sum_IJ <- sum(The_food_items[104:123 , 9:73], na.rm = TRUE) 
total_sum_IA <- sum(The_food_items[129:148 , 9:73], na.rm = TRUE)  
total_sum_IJL <- sum(The_food_items[154:171 , 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_IO <- sum(The_food_items[177:195 , 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_NJ <- sum(The_food_items[201:220 , 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_NA <-sum(The_food_items[226:245 , 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_NJL <-sum(The_food_items[251:270 , 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_NO <- sum(The_food_items[276:295 , 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_UJ <- sum(The_food_items[301:320, 9:73] , na.rm = TRUE)  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_UJL <- sum(The_food_items[326:344, 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_UO <-sum(The_food_items[350:361, 9:73] , na.rm = TRUE)  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_AJ <-sum(The_food_items[367:386, 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_AA <-sum(The_food_items[417:435, 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_AJL <-sum(The_food_items[392:411, 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_GA <-sum(The_food_items[441:460, 9:73] , na.rm = TRUE)  # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_GJl <-sum(The_food_items[466:483, 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_GO <- sum(The_food_items[489:508, 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_PJ <- sum(The_food_items[514:533, 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_PA<- sum(The_food_items[539:554, 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72
total_sum_PJL<-sum(The_food_items[560:568, 9:73], na.rm = TRUE)   # first set of numbers are for rows, second set for columns,Columns I to BU are columns 9 to 72

# Step 2: Define aquatic and terrestrial items (ensure they match column names)
aquatic_items <- c("Amphipoda", "Baetidae", "Caenidae", "Shrimp_L", "Calamoceratidae", 
                   "Ceratopogonidae", "Chaoboridae", "Chironomidae_L", "Chironomidae_pupa", 
                   "Copepoda", "Corixidae", "Cladocera", "Diptera_pupa", "Diptera_adult", 
                   "Dytiscidae", "Elmidae", "Empididae", "Gerrridae", "Ephemeroptera", 
                   "Helicopsychidae", "Hydropsychidae", "Leptoceridae", "Leptohyphes", 
                   "Libellulidae", "Leptophlebiidae", "Naucoridae", "Noteridae", 
                   "Odonata.N", "Ostracoda", "Invertebrate_eggs", "Philopotamidae", 
                   "Plecoptera", "Pleidae", "Psychodidae", "Simullidae", "Trichoptera.L", 
                   "Trichoptera", "Vellidae", "Filamentous_Algae", "Plant_matter", 
                   "Organic_matter")

terrestrial_items <- c("Acari", "Aranae", "Colembola", "Coleoptera", "Coleoptera.larva", 
                       "Formicidae", "Hemiptera", "Hymenoptera", "Hydracarina", "Isoptera", 
                       "Lampyridae", "Orthoptera", "Pseudoscorpionida", "Psocoptera", 
                       "Scales", "Staphynilidae", "Thysanura", "Tingidae", "Vespidae", 
                       "Seeds_and_Fruits")

# Step 3: Find matching columns
matching_aquatic <- intersect(colnames(The_food_items), aquatic_items)
matching_terrestrial <- intersect(colnames(The_food_items), terrestrial_items)

# Step 4: Update the items vectors
aquatic_items <- matching_aquatic
terrestrial_items <- matching_terrestrial

# Function to calculate the total biomass for a given set of columns and items
calculate_biomass <- function(data, items) {
  colSums(data[, items, drop = FALSE], na.rm = TRUE)
}

# Step 1: Define row ranges for each location-month
location_months <- list(
  CJ = 1:20,
  CA = 29:48,
  CJL = 54:72,
  CO = 79:98,
  IJ = 104:123,
  IA =129:148,
  IJL = 154:171,
  IO = 177:195,
  NJ = 201:220,
  NAPRIL = 226:245,
  NJL = 251:270,
  NO = 276:295,
  UJ = 301:320,
  UJL = 326:344,
  UO = 350:361,
  AJ = 367:386,
  AA = 392:411,
  AJL = 417:435,
  GA = 441:460,
  GJL = 466:483,
  GO = 489:508,
  PJ = 514:533,
  PA = 533:554,
  PJL = 560:568
)

# Step 2: Define item columns (e.g., columns 9 to 72 in your data)
biomass_columns <- 9:72

# Step 3: Define which column names are aquatic vs terrestrial
aquatic_items <- c("Amphipoda", "Baetidae", "Caenidae", "Shrimp_L", "Calamoceratidae", 
                   "Ceratopogonidae", "Chaoboridae", "Chironomidae_L", "Chironomidae_Pupa", 
                   "Copepoda", "Corixidae", "Cladocera", "Diptera_pupa", "Diptera_adult", 
                   "Dytiscidae", "Elmidae", "Empididae", "Gerrridae", "Ephemeroptera", 
                   "Helicopsychidae", "Hydropsychidae", "Leptoceridae", "Leptohyphes", 
                   "Libellulidae", "Leptophlebiidae", "Naucoridae", "Noteridae", 
                   "Odonata_N", "Ostracoda", "Invertebrate_eggs", "Philopotamidae", 
                   "Plecoptera", "Pleidae", "Psychodidae", "Simullidae", "Trichoptera_L", 
                   "Trichoptera", "Vellidae", "Filamentous_Algae", "Plant_matter", 
                   "Organic_matter")

terrestrial_items <- c("Acari", "Aranae", "Colembola", "Coleoptera", "Coleoptera_larva", 
                       "Formicidae", "Hemiptera", "Hymenoptera", "Hydracarina", "Isoptera", 
                       "Lampyridae", "Orthoptera", "Pseudoscorpionida", "Psocoptera", 
                       "Scales", "Staphynilidae", "Thysanura", "Tingidae", "Vespidae", 
                       "Seeds_and_Fruits")

# Step 4: Calculate biomass per type and location
results <- lapply(names(location_months), function(lm) {
  rows <- location_months[[lm]]
  data <- The_food_items[rows, biomass_columns]
  
  aquatic_biomass <- sum(data[, aquatic_items], na.rm = TRUE)
  terrestrial_biomass <- sum(data[, terrestrial_items], na.rm = TRUE)
  
  main_source <- ifelse(aquatic_biomass > terrestrial_biomass, "Aquatic", "Terrestrial")
  
  data.frame(
    Location_Month = lm,
    Aquatic_Biomass = aquatic_biomass,
    Terrestrial_Biomass = terrestrial_biomass,
    Main_Source = main_source
  )
})

# Step 5: Combine results
results_df <- do.call(rbind, results)
View(results_df)
write.csv(results_df, "results_df.csv", row.names = FALSE)
getwd()
write.csv(results_df, "C:/Users/ENIOLA OLU-AYORINDE/Desktop/results_df.csv", row.names = FALSE)