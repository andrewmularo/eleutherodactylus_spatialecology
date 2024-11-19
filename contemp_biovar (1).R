library(raster)
library(sf)
library(dismo)

setwd("D:/Research_Data_Backup/Eleutherodactylus_invasion")

# Read monthly data
for (year in 2010:2019) {
  for (month in 1:12) {
    # Convert the month number to a zero-padded string
    month_str <- sprintf("%02d", month)
    
    # Construct the tmin file name
    tmin_name <- paste0("wc2.1_2.5m_tmin_", year, "-", month_str, ".tif")
    
    # Read the tmin file
    tmin <- raster(paste0("wc2.1_cruts4.06_2.5m_tmin_2010-2019/",tmin_name))
    assign(tmin_name, tmin)
    
    # Construct the tmax file name
    tmax_name <- paste0("wc2.1_2.5m_tmax_", year, "-", month_str, ".tif")
    
    # Read the tmax file
    tmax <- raster(paste0("wc2.1_cruts4.06_2.5m_tmax_2010-2019/",tmax_name))
    assign(tmax_name, tmax)
    
    # Construct the prec file name
    prec_name <- paste0("wc2.1_2.5m_prec_", year, "-", month_str, ".tif")
    
    # Read the prec file
    prec <- raster(paste0("wc2.1_cruts4.06_2.5m_prec_2010-2019/",prec_name))
    assign(prec_name, prec)
  }
}

for (year in 2020:2021) {
  for (month in 1:12) {
    # Convert the month number to a zero-padded string
    month_str <- sprintf("%02d", month)
    
    # Construct the tmin file name
    tmin_name <- paste0("wc2.1_2.5m_tmin_", year, "-", month_str, ".tif")
    
    # Read the tmin file
    tmin <- raster(paste0("wc2.1_cruts4.06_2.5m_tmin_2020-2021/",tmin_name))
    assign(tmin_name, tmin)
    
    # Construct the tmax file name
    tmax_name <- paste0("wc2.1_2.5m_tmax_", year, "-", month_str, ".tif")
    
    # Read the tmax file
    tmax <- raster(paste0("wc2.1_cruts4.06_2.5m_tmax_2020-2021/",tmax_name))
    assign(tmax_name, tmax)
    
    # Construct the prec file name
    prec_name <- paste0("wc2.1_2.5m_prec_", year, "-", month_str, ".tif")
    
    # Read the prec file
    prec <- raster(paste0("wc2.1_cruts4.06_2.5m_prec_2020-2021/",prec_name))
    assign(prec_name, prec)
  }
}

# Average months by years
for (month in 1:12) {
  # Convert the month number to a zero-padded string
  month_str <- sprintf("%02d", month)
  
  # Construct the avg tmin file name
  tmin_avg_name <- paste0("wc2.1_2.5m_tmin", "-", month_str)
  
  # Calculate monthly average
  for (year in 2010:2021){
    assign(paste0("tmin_",year), get(paste0("wc2.1_2.5m_tmin_", year, "-", month_str, ".tif")))
  }
  tmin_list <- lapply(2010:2021, function(year) get(paste0("tmin_", year)))
  tmin_stack <- stack(tmin_list)
  tmin_avg <- calc(tmin_stack, fun = mean)
  assign(tmin_avg_name, tmin_avg)
  
  # Construct the avg tmax file name
  tmax_avg_name <- paste0("wc2.1_2.5m_tmax", "-", month_str)
  
  # Calculate monthly average
  for (year in 2010:2021){
    assign(paste0("tmax_",year), get(paste0("wc2.1_2.5m_tmax_", year, "-", month_str, ".tif")))
  }
  tmax_list <- lapply(2010:2021, function(year) get(paste0("tmax_", year)))
  tmax_stack <- stack(tmax_list)
  tmax_avg <- calc(tmax_stack, fun = mean)
  assign(tmax_avg_name, tmax_avg)
  
  # Construct the avg tmax file name
  prec_avg_name <- paste0("wc2.1_2.5m_prec", "-", month_str)
  
  # Calculate monthly average
  for (year in 2010:2021){
    assign(paste0("prec_",year), get(paste0("wc2.1_2.5m_prec_", year, "-", month_str, ".tif")))
  }
  prec_list <- lapply(2010:2021, function(year) get(paste0("prec_", year)))
  prec_stack <- stack(prec_list)
  prec_avg <- calc(prec_stack, fun = mean)
  assign(prec_avg_name, prec_avg)
}    

# Remove unnecessary files 
all_objects <- ls()
tif_objects <- all_objects[grep("\\.tif$", all_objects)]
rm(list = tif_objects)

# Make raster stacks # Resample raster stacks to 30s resolution
tmin_stack <- stack()
tmax_stack <- stack()
prec_stack <- stack()

for (month in 1:12) {
  # Convert the month number to a zero-padded string
  month_str <- sprintf("%02d", month)
  
  tmin_layer <- get(paste0("wc2.1_2.5m_tmin", "-", month_str))
  tmin_stack <- stack(tmin_stack, tmin_layer)
  
  tmax_layer <- get(paste0("wc2.1_2.5m_tmax", "-", month_str))
  tmax_stack <- stack(tmax_stack, tmax_layer)
  
  prec_layer <- get(paste0("wc2.1_2.5m_prec", "-", month_str))
  prec_stack <- stack(prec_stack, prec_layer)
}

# Clip raster stacks by a 100km-buffered polygon
#buffered1_Ecoqui <- st_read("Eleutherodactylus_invasion_arcgis/Ecoqui_GBIF_20202021_onLand_100km_onLand.shp")
#buffered1_Ejohnstonei <- st_read("Eleutherodactylus_invasion_arcgis/Ejohnstonei_GBIF_20202021_onLand_100km_onLand.shp")
#buffered1_Eplanirostris <- st_read("Eleutherodactylus_invasion_arcgis/Eplanirostris_GBIF_20202021_onLand_100km_onLand.shp")
buffered1_Ecoqui <- st_read("Eleutherodactylus_invasion_arcgis/coqui_establishedpops_onLand_100km_onLand.shp")
buffered1_Ejohnstonei <- st_read("Eleutherodactylus_invasion_arcgis/johnstonei_establishedpops_onLand_100km_onLand.shp")
buffered1_Eplanirostris <- st_read("Eleutherodactylus_invasion_arcgis/planirostris_establishedpops_onLand_100km_onLand.shp")

tmin_Ecoqui_100 <- raster::crop(tmin_stack, extent(buffered1_Ecoqui))
tmin_Ecoqui_100 <- raster::mask(tmin_Ecoqui_100, buffered1_Ecoqui)

tmax_Ecoqui_100 <- raster::crop(tmax_stack, extent(buffered1_Ecoqui))
tmax_Ecoqui_100 <- raster::mask(tmax_Ecoqui_100, buffered1_Ecoqui)

prec_Ecoqui_100 <- raster::crop(prec_stack, extent(buffered1_Ecoqui))
prec_Ecoqui_100 <- raster::mask(prec_Ecoqui_100, buffered1_Ecoqui)

plot(tmin_Ecoqui_100$layer.1.1.1)

tmin_Ejohnstonei_100 <- raster::crop(tmin_stack, extent(buffered1_Ejohnstonei))
tmin_Ejohnstonei_100 <- raster::mask(tmin_Ejohnstonei_100, buffered1_Ejohnstonei)

tmax_Ejohnstonei_100 <- raster::crop(tmax_stack, extent(buffered1_Ejohnstonei))
tmax_Ejohnstonei_100 <- raster::mask(tmax_Ejohnstonei_100, buffered1_Ejohnstonei)

prec_Ejohnstonei_100 <- raster::crop(prec_stack, extent(buffered1_Ejohnstonei))
prec_Ejohnstonei_100 <- raster::mask(prec_Ejohnstonei_100, buffered1_Ejohnstonei)

plot(tmin_Ejohnstonei_100$layer.1.1.1)

tmin_Eplanirostris_100 <- raster::crop(tmin_stack, extent(buffered1_Eplanirostris))
tmin_Eplanirostris_100 <- raster::mask(tmin_Eplanirostris_100, buffered1_Eplanirostris)

tmax_Eplanirostris_100 <- raster::crop(tmax_stack, extent(buffered1_Eplanirostris))
tmax_Eplanirostris_100 <- raster::mask(tmax_Eplanirostris_100, buffered1_Eplanirostris)

prec_Eplanirostris_100 <- raster::crop(prec_stack, extent(buffered1_Eplanirostris))
prec_Eplanirostris_100 <- raster::mask(prec_Eplanirostris_100, buffered1_Eplanirostris)

plot(tmin_Eplanirostris_100$layer.1.1.1)

writeRaster(tmin_Ecoqui_100, "tmin_Ecoqui_100.tif", format="GTiff", overwrite=TRUE)
writeRaster(tmax_Ecoqui_100, "tmax_Ecoqui_100.tif", format="GTiff", overwrite=TRUE)
writeRaster(prec_Ecoqui_100, "prec_Ecoqui_100.tif", format="GTiff", overwrite=TRUE)
writeRaster(tmin_Ejohnstonei_100, "tmin_Ejohnstonei_100.tif", format="GTiff", overwrite=TRUE)
writeRaster(tmax_Ejohnstonei_100, "tmax_Ejohnstonei_100.tif", format="GTiff", overwrite=TRUE)
writeRaster(prec_Ejohnstonei_100, "prec_Ejohnstonei_100.tif", format="GTiff", overwrite=TRUE)
writeRaster(tmin_Eplanirostris_100, "tmin_Eplanirostris_100.tif", format="GTiff", overwrite=TRUE)
writeRaster(tmax_Eplanirostris_100, "tmax_Eplanirostris_100.tif", format="GTiff", overwrite=TRUE)
writeRaster(prec_Eplanirostris_100, "prec_Eplanirostris_100.tif", format="GTiff", overwrite=TRUE)

# Remove unnecessary files 
all_objects <- ls()
wc2.1_objects <- all_objects[grep("^wc2.1", all_objects)]
rm(list = wc2.1_objects)

avg_objects <- all_objects[grep("avg$", all_objects)]
rm(list = avg_objects)

layer_objects <- all_objects[grep("layer$", all_objects)]
rm(list = layer_objects)

stack_objects <- all_objects[grep("stack$", all_objects)]
rm(list = stack_objects)

for (year in 2010:2021){
  year_objects <- all_objects[grep(paste0(year,"$"), all_objects)]
  rm(list = year_objects, envir = .GlobalEnv)
}

rm(tmin)
rm(tmax)
rm(prec)

# Resample to 30s-resolution -> this needs too large temporary files by R. Shift again to ArcGIS for resampling (export and import "*_100" files; use batch resampling
reference_raster <- raster("HadGEM3-GC31-LL/wc2.1_30s_bioc_HadGEM3-GC31-LL_ssp126_2021-2040.tif")

tmin_Ecoqui_100_30s <- raster::resample(tmin_Ecoqui_100, reference_raster, method = "bilinear")
tmax_Ecoqui_100_30s <- raster::resample(tmax_Ecoqui_100, reference_raster, method = "bilinear")
prec_Ecoqui_100_30s <- raster::resample(prec_Ecoqui_100, reference_raster, method = "bilinear")

tmin_Ejohnstonei_100_30s <- raster::resample(tmin_Ejohnstonei_100, reference_raster, method = "bilinear")
tmax_Ejohnstonei_100_30s <- raster::resample(tmax_Ejohnstonei_100, reference_raster, method = "bilinear")
prec_Ejohnstonei_100_30s <- raster::resample(prec_Ejohnstonei_100, reference_raster, method = "bilinear")

tmin_Eplanirostris_100_30s <- raster::resample(tmin_Eplanirostris_100, reference_raster, method = "bilinear")
tmax_Eplanirostris_100_30s <- raster::resample(tmax_Eplanirostris_100, reference_raster, method = "bilinear")
prec_Eplanirostris_100_30s <- raster::resample(prec_Eplanirostris_100, reference_raster, method = "bilinear")

# Clip raster stacks by a 80km-buffered polygon -> continue on ArcGIS
#buffered2_Ecoqui <- st_read("Eleutherodactylus_invasion_arcgis/Ecoqui_GBIF_20202021_onLand_80km_onLand.shp")
#buffered2_Ejohnstonei <- st_read("Eleutherodactylus_invasion_arcgis/Ejohnstonei_GBIF_20202021_onLand_80km_onLand.shp")
#buffered2_Eplanirostris <- st_read("Eleutherodactylus_invasion_arcgis/Eplanirostris_GBIF_20202021_onLand_80km_onLand.shp")
buffered2_Ecoqui <- st_read("Eleutherodactylus_invasion_arcgis/coqui_establishedpops_onLand_80km_onLand.shp")
buffered2_Ejohnstonei <- st_read("Eleutherodactylus_invasion_arcgis/johnstonei_establishedpops_onLand_80km_onLand.shp")
buffered2_Eplanirostris <- st_read("Eleutherodactylus_invasion_arcgis/planirostris_establishedpops_onLand_80km_onLand.shp")

tmin_Ecoqui_80_30s <- raster::crop(tmin_Ecoqui_100_30s, extent(buffered2_Ecoqui))
tmin_Ecoqui_80_30s <- raster::mask(tmin_Ecoqui_80_30s, buffered2_Ecoqui)

tmax_Ecoqui_80_30s <- raster::crop(tmax_Ecoqui_100_30s, extent(buffered2_Ecoqui))
tmax_Ecoqui_80_30s <- raster::mask(tmax_Ecoqui_80_30s, buffered2_Ecoqui)

prec_Ecoqui_80_30s <- raster::crop(prec_Ecoqui_100_30s, extent(buffered2_Ecoqui))
prec_Ecoqui_80_30s <- raster::mask(prec_Ecoqui_80_30s, buffered2_Ecoqui)

plot(prec_Ecoqui_80_30s$layer.1.1)

tmin_Ejohnstonei_80_30s <- raster::crop(tmin_Ejohnstonei_100_30s, extent(buffered2_Ejohnstonei))
tmin_Ejohnstonei_80_30s <- raster::mask(tmin_Ejohnstonei_80_30s, buffered2_Ejohnstonei)

tmax_Ejohnstonei_80_30s <- raster::crop(tmax_Ejohnstonei_100_30s, extent(buffered2_Ejohnstonei))
tmax_Ejohnstonei_80_30s <- raster::mask(tmax_Ejohnstonei_80_30s, buffered2_Ejohnstonei)

prec_Ejohnstonei_80_30s <- raster::crop(prec_Ejohnstonei_100_30s, extent(buffered2_Ejohnstonei))
prec_Ejohnstonei_80_30s <- raster::mask(prec_Ejohnstonei_80_30s, buffered2_Ejohnstonei)

plot(prec_Ejohnstonei_80_30s$layer.1.1)

tmin_Eplanirostris_80_30s <- raster::crop(tmin_Eplanirostris_100_30s, extent(buffered2_Eplanirostris))
tmin_Eplanirostris_80_30s <- raster::mask(tmin_Eplanirostris_80_30s, buffered2_Eplanirostris)

tmax_Eplanirostris_80_30s <- raster::crop(tmax_Eplanirostris_100_30s, extent(buffered2_Eplanirostris))
tmax_Eplanirostris_80_30s <- raster::mask(tmax_Eplanirostris_80_30s, buffered2_Eplanirostris)

prec_Eplanirostris_80_30s <- raster::crop(prec_Eplanirostris_100_30s, extent(buffered2_Eplanirostris))
prec_Eplanirostris_80_30s <- raster::mask(prec_Eplanirostris_80_30s, buffered2_Eplanirostris)

plot(prec_Eplanirostris_80_30s$layer.1.1)

# Calculate bioclimatic variables <- in R again
tmin_Ecoqui_80_30s <- stack("Eleutherodactylus_invasion_arcgis/tmin_Ecoqui_80_30s.tif")  
tmax_Ecoqui_80_30s <- stack("Eleutherodactylus_invasion_arcgis/tmax_Ecoqui_80_30s.tif")
prec_Ecoqui_80_30s <- stack("Eleutherodactylus_invasion_arcgis/prec_Ecoqui_80_30s.tif")

tmin_Ejohnstonei_80_30s <- stack("Eleutherodactylus_invasion_arcgis/tmin_Ejohnstonei_80_30s.tif")  
tmax_Ejohnstonei_80_30s <- stack("Eleutherodactylus_invasion_arcgis/tmax_Ejohnstonei_80_30s.tif")
prec_Ejohnstonei_80_30s <- stack("Eleutherodactylus_invasion_arcgis/prec_Ejohnstonei_80_30s.tif")

tmin_Eplanirostris_80_30s <- stack("Eleutherodactylus_invasion_arcgis/tmin_Eplanirostris_80_30s.tif")  
tmax_Eplanirostris_80_30s <- stack("Eleutherodactylus_invasion_arcgis/tmax_Eplanirostris_80_30s.tif")
prec_Eplanirostris_80_30s <- stack("Eleutherodactylus_invasion_arcgis/prec_Eplanirostris_80_30s.tif")
# Due to seldom disconnection to the external storage, I used below for biovar_Eplanirostris
#tmin_Eplanirostris_80_30s <- stack("C:/Users/jyj55/OneDrive - purdue.edu/DeWoody_Lab/Lab work/Temp/tmin_Eplanirostris_80_30s.tif")  
#tmax_Eplanirostris_80_30s <- stack("C:/Users/jyj55/OneDrive - purdue.edu/DeWoody_Lab/Lab work/Temp/tmax_Eplanirostris_80_30s.tif")
#prec_Eplanirostris_80_30s <- stack("C:/Users/jyj55/OneDrive - purdue.edu/DeWoody_Lab/Lab work/Temp/prec_Eplanirostris_80_30s.tif")

biovar_Ecoqui <- biovars(tmin=tmin_Ecoqui_80_30s, tmax=tmax_Ecoqui_80_30s, prec=prec_Ecoqui_80_30s)
biovar_Ejohnstonei <- biovars(tmin=tmin_Ejohnstonei_80_30s, tmax=tmax_Ejohnstonei_80_30s, prec=prec_Ejohnstonei_80_30s)
biovar_Eplanirostris <- biovars(tmin=tmin_Eplanirostris_80_30s, tmax=tmax_Eplanirostris_80_30s, prec=prec_Eplanirostris_80_30s)

writeRaster(biovar_Ecoqui, "biovar_Ecoqui.tif", format="GTiff", overwrite=TRUE)
writeRaster(biovar_Ejohnstonei, "biovar_Ejohnstonei.tif", format="GTiff", overwrite=TRUE)
writeRaster(biovar_Eplanirostris, "biovar_Eplanirostris.tif", format="GTiff", overwrite=TRUE)

# Test correlation -> continue on ArcGIS ("Make Raster Layer" + "Band Collection Statistics")
biovar_Ecoqui_corr <- layerStats(biovar_Ecoqui, 'pearson', na.rm=T)
print(biovar_Ecoqui_corr)

biovar_Ejohnstonei_corr <- layerStats(biovar_Ejohnstonei, 'pearson', na.rm=T)
print(biovar_Ejohnstonei_corr)

biovar_Eplanirostris_corr <- layerStats(biovar_Eplanirostris, 'pearson', na.rm=T)
print(biovar_Eplanirostris_corr)

# Export selected variables
#Ecoqui_vars <- c("bio1", "bio2", "bio3", "bio5", "bio14", "bio19")
Ecoqui_vars <- c("bio1", "bio2", "bio4", "bio12", "bio15", "bio19")
#Ejohnstonei_vars <- c("bio1", "bio2", "bio3", "bio12", "bio14", "bio18")
Ejohnstonei_vars <- c("bio1", "bio2", "bio3", "bio12", "bio14")
#Eplanirostris_vars <- c("bio1", "bio2", "bio3", "bio5", "bio8", "bio12", "bio14", "bio18")
Eplanirostris_vars <- c("bio1", "bio2", "bio3", "bio5", "bio8", "bio12", "bio14", "bio15", "bio18")
species_list <- c("Ecoqui", "Ejohnstonei","Eplanirostris")

#biovar_Ecoqui <- stack("biovar_Ecoqui.tif")
#biovar_Ejohnstonei <- stack("biovar_Ejohnstonei.tif")
#biovar_Eplanirostris <- stack("biovar_Eplanirostris.tif")

# Export -> continued on ArcGIS already by "Make Raster Layer"
for (species in species_list) {
  for (var in get(paste0(species,"_vars"))) {
    # Extract the raster layer corresponding to the variable
    raster_layer <- get(paste0("biovar_",species))
    raster_var <- raster_layer[[var]]
    
    # Construct the file name
    folder_name <- paste0(species,"_contemp/")
    file_name <- paste0(folder_name,var,".tif")
    
    # Export the raster layer to a GeoTIFF file
    writeRaster(raster_var, filename = file_name, format = "GTiff", overwrite = TRUE)
  }
}
