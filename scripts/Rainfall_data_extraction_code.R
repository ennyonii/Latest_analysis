############RAINFALL DATA EXTRACTION FROM COPERNICUS#############
############ENIOLA OLU-AYORINDE & PHILIPPE ####################
############2026-04-02##############################
#Getting rainfall data (7 days sum, 3 days sum, 1 day) of the specific sampling sites and dates from the whole rainfall data present on the website


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