#*############################################################*#
## RETRIEVE MEAN EVI DATA FOR A GIVEN LOCATION AND TIME RANGE ##
#*############################################################*#

## Author: Philippe Fernandez-Fournier, 2026

# Download "MODIS/Terra Gap-Filled, Smoothed EVI 8-Day L4 250m SIN Grid" here: 
# https://www.earthdata.nasa.gov/data/catalog/laads-mod09q1g-evi-6
# Direct download: https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/6/MOD09Q1G_EVI/
# DOI https://doi.org/10.5067/MODIS/MOD09Q1G_EVI.006

## Each file is ONE tile only. Maybe only need a select few slides for Brazil?? 
# Yes! Only need h14v09 and h14v10

## Because terrestrial biomass in the previous week matters, I downloaded the 8 day composite data whose window ends just before the sampling date 
## (e.g. EVI data for Jan 9-16 for sampling date Jan 19)

# File name explanation:
# "MOD09Q1G_EVI" = Product name and data type
# "A2015017" = Acquisition date (AYYYYDDD)
# "h14v10" = MODIS tile (h = horizontal index; v = vertical index)
# "006" = Collection number
# "2018299133908" = Processing timestamp (ignore).

# Spatial Resolution = 500 meters
# Temporal Resolution = 8 days
## IMPORTANT: Date is every 8 days and counted as days of the year (1-365)
# Data is an 8-day composite starting at a given day of year. 
# EVI data represents the highest quality vegetation signal observed for each pixel during an 8 day window

# Things to figure out:
## What spatial buffer around sites? Currently 5000m
## For EVI data: mean, median, upstream catchment average?
## Filter out poor quality pixels? From MODIS_EVI_QC raster layer (1 = Good, 4 = Poor)

# What this script does:
# - Reads TIF files in folder (Downloaded then manually converted HDF to TIF)
# - Merges h14v09 and h14v10 for each sampling date
# - Extracts mean EVI of sites with buffer
# - Plot example map 

#setwd("D:/URegina/SOARES LAB/Eniola_fishdiet/")
rm(list=ls())

library(terra)
library(stringr)
library(dplyr)
library(ggplot2)


# LOAD FILES & MOSAIC TILES -----------------------------------------------
data_all <- read.csv("./data/Modelling_complete_list.csv")
data_all$lon <- -data_all$lon # Correct longitude
data_all$date_doy <- as.integer(format(as.Date(data_all$date), "%j")) # Convert calendar date to day of year (1-365)

# Which start day of composite data was downloaded?  
comp_starts <- seq(1, 361, 8)
data_all$MODIS_doy <- comp_starts[findInterval(data_all$date_doy - 7, comp_starts)] # floor function to assign the correct composite date

tif_files <- list.files(path = "./data/EVI_EarthData_LAADS/", # folder where the EVI data is located
                        pattern = "MOD09Q1G_EVI.*\\.tif$", full.names = TRUE) # I converted hdf files to tif manually  
tif_files

#plot(rast(tif_files[1])) # Plot the six layers (EVI, Smoothed EVI, Composed, etc.) of one tile to check

sites <- vect(data_all, geom = c("lon", "lat"), crs = "EPSG:4326") # Create raster object for sites (for plotting)
sites_proj <- project(sites, crs(rast(tif_files[1]))) # Reproject sites to EVI data CRS (Sinusoidal)


# EXTRACT MEAN EVI -------------------------------------------------------
# Set buffer around sites in meters (distance in every direction from the point)
buffer_m <- 500
data_all$evi_mean <- NA

for(d in unique(data_all$MODIS_doy)){
  # Select tif files for this composite date
  tif_subset <- tif_files[str_detect(tif_files, paste0("A2015", sprintf("%03d", d)))]
  
  # Read and extract EVI layer for both tiles
  evi_list <- lapply(tif_subset, function(f){
    r <- rast(f)
    e <- r[["MODIS_EVI"]]
    e[e < 0] <- NA
    return(e)
  })
  
  # Mosaic tiles
  evi_mosaic <- do.call(merge, evi_list)
  
  # Select sites corresponding to this composite date
  idx <- which(data_all$MODIS_doy == d)
  sites_sub <- sites_proj[idx]
  
  # Buffer
  buffer_sites <- buffer(sites_sub, width = buffer_m)
  
  # Extract
  evi_vals <- terra::extract(evi_mosaic, buffer_sites, fun = mean, na.rm = TRUE)
  
  data_all$evi_mean[idx] <- evi_vals[,2]
}

data_all$evi_mean

# Compare to biomass density
ggplot(data_all, aes(x = evi_mean, y = Biomass_density)) +
  geom_point(size = 3) +
  labs(x = "Mean EVI",
       y = "Biomass density") +
  theme_bw()

# Save final data
write.csv(data_all, "./data/Modelling_complete_list_EVI.csv", row.names = FALSE)


# PLOT MAP ----------------------------------------------------------------
# Which date as example plot?
date_plot <- unique(data_all$MODIS_doy)[1]
tif_plot <- tif_files[str_detect(tif_files, paste0("A2015", sprintf("%03d", date_plot)))]

# Read and extract EVI layer for both tiles
evi_list <- lapply(tif_plot, function(f){
  r <- rast(f)
  e <- r[["MODIS_EVI"]]
  e[e < 0] <- NA
  return(e)
})

# Mosaic tiles
evi_plot <- do.call(merge, evi_list)

# Crop raster for plotting
crop_ext <- ext(-4200000, -3950000, -1200000, -1000000) # (xmin, xmax, ymin, ymax)
evi_area <- crop(evi_plot, crop_ext)

# Plot the combined tiles (with sites) in Sinusoidal projection
jpeg("./graphs/EVI_sinusoidal.jpg", width = 7, height = 6, units = "in", res = 300)
plot(evi_area) 
points(sites_proj, col = "red", pch = 17, cex = 2)
dev.off()

# Plot with buffer
jpeg("./graphs/EVI_sinusoidal_buffer_5km.jpg", width = 7, height = 6, units = "in", res = 300)
plot(evi_area) 
points(sites_proj, col = "red", pch = 19, cex = 1)
plot(buffer_sites, add = TRUE, border = "RED", lwd = 2)
dev.off()

