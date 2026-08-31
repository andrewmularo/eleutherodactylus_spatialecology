library(ecospat)
library(ade4)
library(raster)
library(CoordinateCleaner)
library(tidyverse)
library(geodata)
library(dismo)
library(vegan)
library(biomod2)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(ENMTools)
library(sf)
library(spThin)
library(remotes)
library(MinBAR)
library(ggpubr)


wd<- "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat"

setwd(wd)

###### Raw GBIF downloaded data for Eleutherodactylus ##################################################################################################################################
##################################################################################################################################################################

coqui<- read.csv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/RawData/Ecoqui.csv")
planirostris <- read.csv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/RawData/Eplanirostris.csv")
johnstonei<- read.csv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/RawData/Ejohnstonei.csv")
antillensis<- read.csv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/RawData/Eantillensis.csv")
martinicensis<- read.csv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/RawData/Emartinicensis.csv")

##### Data cleaning - tidyverse and coordinatecleaner###################################################################################################################
#####################################################################################################################################################################


eleuth<- rbind(coqui, planirostris, johnstonei, antillensis, martinicensis)
eleuth<- as.data.frame(eleuth)


### QC for historic records using CoordinateCleaner
eleuth_cleaned<- eleuth %>% cc_aohi(lon = "decimalLongitude", lat = "decimalLatitude") %>% 
  cc_cen(lon = "decimalLongitude", lat = "decimalLatitude", species = "species") %>% 
  cc_inst(lon = "decimalLongitude", lat = "decimalLatitude", species = "species", buffer = 100)


eleuth_cleaned_dates<- eleuth_cleaned %>% dplyr::select(c("gbifID", "species", "countryCode", "locality", "stateProvince",
                                                          "decimalLatitude", "decimalLongitude", "day", "month", "year", "license", "institutionCode", "rightsHolder")) %>% 
  filter(year != "NA")

#write.csv(eleuth_cleaned_dates, "eleutherocatylus_invasionhistory_manualcuration_v2_test.csv")




### Upload the file containing the manual curation of invasive status

eleuth_cleaned_mancuration<- read.csv("eleutherocatylus_invasionhistory_manualcuration_v2.csv")


#### Filter based on coordinate uncertainty of 5000 meters 
### IMPORTANT NOTE - Did not do a year based cutoff 
eleuth_cleaned_precise<- eleuth_cleaned %>% filter(!is.na(decimalLatitude)) %>% 
  filter(!is.na(year)) %>% 
  filter(coordinateUncertaintyInMeters <= 5000 )


  ## Filter to only include native and likely established


eleuth_gbif<- eleuth_cleaned_precise%>% dplyr::select(c("gbifID"))

eleuth_cleaned_climate<- eleuth_cleaned_mancuration %>% 
  inner_join(eleuth_gbif) %>% 
  dplyr::filter(Status %in% c("Native", "Likely established"))



coqui_final<- eleuth_cleaned_mancuration %>% filter(species == "Eleutherodactylus coqui") %>% filter(Status != "NA")
planirostris_final<- eleuth_cleaned_mancuration %>% filter(species == "Eleutherodactylus planirostris")%>% filter(Status != "NA")
johnstonei_final<- eleuth_cleaned_mancuration%>% filter(species == "Eleutherodactylus johnstonei")%>% filter(Status != "NA")
antillensis_final<- eleuth_cleaned_mancuration%>% filter(species == "Eleutherodactylus antillensis")%>% filter(Status != "NA")
martinicensis_final<- eleuth_cleaned_mancuration%>% filter(species == "Eleutherodactylus martinicensis")%>% filter(Status != "NA")

### Maps for introduction history 


world_coordinates <- map_data("world") 

## Coqui - world
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  theme_classic()+
  coord_sf(xlim = c(-170, -40), ylim = c(-10, 80), expand = FALSE)+
  geom_point(data = coqui_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status),  size = 6)+
  theme(legend.position = "none", 
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 60),
        axis.text.y = element_text(face = "bold", size = 60))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#6495ed", "#cd5c5c", "#fcc200", "black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/coqui_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(-68, -64), ylim = c(17, 19), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = coqui_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#6495ed", "#cd5c5c", "#fcc200","black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/coqui_VI.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

# Hawaii
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(-160, -154), ylim = c(18, 23), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = coqui_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#6495ed", "#cd5c5c", "#fcc200", "black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/coqui_hawaii.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')


# California
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(-120, -115), ylim = c(31, 35), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = coqui_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#6495ed", "#cd5c5c" , "#fcc200", "black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/coqui_california.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')



#### Planirostris History #############################################################################################################################

ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  theme_classic()+
  geom_point(data = planirostris_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status),  size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#6495ed", "#cd5c5c" , "#fcc200", "black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/planirostris_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

# USA
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(-110, -70), ylim = c(12, 50), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = planirostris_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#6495ed", "#cd5c5c" , "#fcc200", "black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/planirostris_usa.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')


## Asia
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(100, 150), ylim = c(0, 30), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = planirostris_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#6495ed", "#cd5c5c" , "#fcc200", "black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/planirostris_asia.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

# Hawaii
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(-160, -154), ylim = c(18, 23), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = planirostris_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#6495ed", "#cd5c5c" , "#fcc200", "black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/planirostris_hawaii.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')



### Johnstonei History #########################################################################################################################################

ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  theme_classic()+
  coord_sf(xlim = c(-120, -30), ylim = c(-40, 40), expand = FALSE)+
  geom_point(data = johnstonei_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status),  size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#cd5c5c", "#fcc200","black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/johnstonei_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

## Antilles
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(-65, -60), ylim = c(10, 19), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = johnstonei_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#cd5c5c", "#fcc200","black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/johnstonei_antilles.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

## N south america
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(-85, -65), ylim = c(2, 13), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = johnstonei_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#cd5c5c", "#fcc200","black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/johnstonei_samerica.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

#### Antillensis
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(-68, -64), ylim = c(17.5, 19), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = antillensis_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#cd5c5c", "#fcc200","black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/antillensis_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')


#### Martinicensis
ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  coord_sf(xlim = c(-65, -60), ylim = c(10, 19), expand = FALSE)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_point(data = martinicensis_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status), size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#cd5c5c", "#fcc200","black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures/Maps/martinicensis_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')







###### Get bioclimate rasters and mask them to the study area -amphibian biogeographic regions ###################################################################################
##################################################################################################################################################################################

### Upload worldclim data 1970-2000 
bio <- worldclim_global(var = "bio", 
                        lon = c(-180, 180),
                        lat = c(-90, 90), 
                        res = 2.5, # resolution: 2.5 minutes
                        path = "wd")

land <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
  filter(subregion == "Caribbean")
minor_islands<- rnaturalearth::ne_download(scale = 10,
                                           type = "minor_islands",
                                           category = "physical",
                                           returnclass = "sf") 
minor_islands<- st_transform(minor_islands, st_crs(land))

combined_land <- bind_rows(land, minor_islands) %>% vect()

caribbean<- combined_land %>% crop(ext(-90, -50, 10,30))
plot(caribbean)

caribbean_proj<- project(caribbean, crs(bio))
plot(caribbean_proj)

# upload amphibian bioregions polygon and clip the worldclim raster to these regions
amp_regions<- terra::vect("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Background_shapefiles/Bioregions/Bioregions/amp_regions.shp")
amp_regions_proj<- project(amp_regions, crs(caribbean_proj))
amp_regions_carib <- crop(amp_regions_proj, caribbean_proj)
plot(amp_regions_carib) 

# Here merges amp_regions with Caribbean, so we don't lose islands
combined_amp_land <- aggregate(rbind(caribbean_proj, amp_regions_carib))
cropped_bio<- crop(bio, combined_amp_land)
mask_bio_amphregions_carib<- mask(cropped_bio, combined_amp_land)
plot(mask_bio_amphregions_carib[[1]]) 





#### Get bioclimate variables for occurrence points ########################################################################################################
############################################################################################################################################################
############################################################################################################################################################


### Filter only native records for each species and conduct spatial thinning ###################################

coqui_native<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus coqui") %>% filter(Type == "Native") %>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10, max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/coqui/native")
planirostris_native<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus planirostris") %>% filter(Type == "Native")%>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10,max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/planirostris/native")
johnstonei_native<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus johnstonei") %>% filter(Type == "Native")%>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10,max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/johnstonei/native")
antillensis_native<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus antillensis") %>% filter(Type == "Native")%>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10,max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/antillensis/native")
martinicensis_native<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus martinicensis") %>% filter(Type == "Native")%>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10,max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/martinicensis/native")

### filter to only invasive datapoints and conduct spatial thinning#########################################################

coqui_invasive<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus coqui") %>% filter(Type == "Invasive")%>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10, max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/coqui/invasive")
planirostris_invasive<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus planirostris") %>% filter(Type == "Invasive")%>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10, max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/planirostris/invasive")
johnstonei_invasive<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus johnstonei") %>% filter(Type == "Invasive")%>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10, max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/johnstonei/invasive")
antillensis_invasive<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus antillensis") %>% filter(Type == "Invasive")%>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10, max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/antillensis/invasive")
martinicensis_invasive<- eleuth_cleaned_climate %>% filter(species == "Eleutherodactylus martinicensis") %>% filter(Type == "Invasive") %>% 
  thin(lat.col = "decimalLatitude", long.col  = "decimalLongitude", spec.col = "species", thin.par = 1, rep = 10, max.files = 1,
       write.files = T, out.dir = "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/martinicensis/invasive")


#### Read in all csv files of spatially thinned records ########################################################
coqui_native<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/coqui/native/thinned_data_thin1_new_new_new.csv")
planirostris_native<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/planirostris/native/thinned_data_thin1_new_new_new.csv")
johnstonei_native<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/johnstonei/native/thinned_data_thin1_new_new_new.csv")
antillensis_native<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/antillensis/native/thinned_data_thin1_new_new_new.csv")
martinicensis_native<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/martinicensis/native/thinned_data_thin1_new_new_new.csv")

coqui_invasive<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/coqui/invasive/thinned_data_thin1_new_new_new.csv")
planirostris_invasive<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/planirostris/invasive/thinned_data_thin1_new_new_new.csv")
johnstonei_invasive<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/johnstonei/invasive/thinned_data_thin1_new_new_new.csv")
antillensis_invasive<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/antillensis/invasive/thinned_data_thin1_new_new_new.csv")
martinicensis_invasive<- read.csv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/Ecospat/Spatial_Thinning_07092026/martinicensis/invasive/thinned_data_thin1_new_new_new.csv")



native_occ<- rbind(coqui_native, planirostris_native, johnstonei_native, antillensis_native, martinicensis_native)
invasive_occ<- rbind(coqui_invasive, planirostris_invasive, johnstonei_invasive, antillensis_invasive, martinicensis_invasive) 

occ<- rbind(native_occ, invasive_occ)

native_bio<- extract(bio, vect(native_occ)) %>% cbind(as.data.frame(native_occ)) %>% select(!c("species", "ID", "decimalLatitude", "decimalLongitude"))%>%  mutate(Type = "Native") %>% mutate(Background = "No")
invasive_bio<- extract(bio, vect(invasive_occ)) %>% cbind(as.data.frame(invasive_occ)) %>% select(!c("species", "ID", "decimalLatitude", "decimalLongitude")) %>% mutate(Type = "Invasive") %>% mutate(Background = "No")

occurence_bio<- rbind(native_bio, invasive_bio)

occurrence_final<- cbind(occ, occurence_bio)


### species specific data frame 
coqui_occ<- occurrence_final %>% dplyr::filter(species == "Eleutherodactylus coqui")
planirostris_occ<- occurrence_final %>% filter(species == "Eleutherodactylus planirostris")
johnstonei_occ<- occurrence_final %>% filter(species == "Eleutherodactylus johnstonei")
antillensis_occ<- occurrence_final %>% filter(species == "Eleutherodactylus antillensis")
martinicensis_occ<- occurrence_final %>% filter(species == "Eleutherodactylus martinicensis")


##### Generate the Background Area ###############################################################################################################################################
##################################################################################################################################################################################



### Generate native background area - sample raster cells cropped to the the 
## zoogeographic/caribbean polygon ##############################

### For ecospat niche comparisons, we  sample target group background points across the caribbean
## coqui native background

set.seed(123)

coqui_native_background_niche <- spatSample(x = mask_bio_amphregions_carib, 
                                                   size = 10000, 
                                                   ext = ext(mask_bio_amphregions_carib),
                                                   na.rm = T, 
                                                   method = "regular") %>% mutate(species = "Eleutherodactylus coqui") %>% 
  mutate(Type = "Native") %>% 
  mutate(Background = "Yes")




# planirostris native background
planirostris_native_background_niche <- spatSample(x = mask_bio_amphregions_carib, 
                                      size = 10000, 
                                      ext = ext(mask_bio_amphregions_carib),
                                      na.rm = T, 
                                      method = "regular") %>% mutate(species = "Eleutherodactylus planirostris") %>% 
  mutate(Type = "Native") %>% 
  mutate(Background = "Yes")

## johnstonei native background
johnstonei_native_background_niche <- spatSample(x = mask_bio_amphregions_carib, 
                                      size = 10000, 
                                      ext = ext(mask_bio_amphregions_carib),
                                      na.rm = T, 
                                      method = "regular") %>% mutate(species = "Eleutherodactylus johnstonei") %>% 
  mutate(Type = "Native") %>% 
  mutate(Background = "Yes")

# antillensis native background
antillensis_native_background_niche <- spatSample(x = mask_bio_amphregions_carib, 
                                      size = 10000, 
                                      ext = ext(mask_bio_amphregions_carib),
                                      na.rm = T, 
                                      method = "regular") %>% mutate(species = "Eleutherodactylus antillensis") %>% 
  mutate(Type = "Native") %>% 
  mutate(Background = "Yes")

## martinicensis native background
martinicensis_native_background_niche <- spatSample(x = mask_bio_amphregions_carib, 
                                      size = 10000, 
                                      ext = ext(mask_bio_amphregions_carib),
                                      na.rm = T, 
                                      method = "regular") %>% mutate(species = "Eleutherodactylus martinicensis") %>% 
  mutate(Type = "Native") %>% 
  mutate(Background = "Yes")



native_background_niche<- rbind(coqui_native_background_niche, planirostris_native_background_niche, johnstonei_native_background_niche, antillensis_native_background_niche, martinicensis_native_background_niche)


#### For ENM, use target based approach############################################################################
### Get target group amphibian data ###############################################################################
##################################################################################################################

### Using target group 1 for carribbean - western hemisphere

background1<- read_tsv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/RawData/0003739-260715120105164_targetbackground1/0003739-260715120105164.csv")

background1<- background1 %>% filter(decimalLatitude != "NA") %>% 
  filter(coordinateUncertaintyInMeters <= 5000 ) %>% 
  filter(species != "Eleutherodactylus planirostris")%>% 
  filter(species != "Eleutherodactylus coqui")%>% 
  filter(species != "Eleutherodactylus johnstonei")%>% 
  filter(species != "Eleutherodactylus antillensis")%>% 
  filter(species != "Eleutherodactylus martinicensis") %>% cc_aohi(lon = "decimalLongitude", lat = "decimalLatitude") %>% 
  cc_cen(lon = "decimalLongitude", lat = "decimalLatitude", species = "species") %>% 
  cc_inst(lon = "decimalLongitude", lat = "decimalLatitude", species = "species", buffer = 100) %>% 
  select(c("decimalLongitude", "decimalLatitude")) %>% vect()


native_target_background<- project(background1, crs(caribbean_proj))
native_target_background <- crop(native_target_background, caribbean_proj)

n<- 1299 # 5% of total number of points/rows in the native target background - 25981          


native_background_sample <- native_target_background[
  sample(nrow(native_target_background), n)]


coqui_target_native_background <- native_background_sample %>% as.data.frame(geom = 'XY') %>% mutate(Species = "Eleutherodactylus coqui")

planirostris_target_native_background <- native_background_sample %>% as.data.frame(geom = 'XY') %>% mutate(Species = "Eleutherodactylus planirostris")


johnstonei_target_native_background <- native_background_sample %>% as.data.frame(geom = 'XY') %>% mutate(Species = "Eleutherodactylus johnstonei")


antillensis_target_native_background <- native_background_sample %>% as.data.frame(geom = 'XY') %>% mutate(Species = "Eleutherodactylus antillensis")


martinicensis_target_native_background <- native_background_sample %>% as.data.frame(geom = 'XY') %>% mutate(Species = "Eleutherodactylus martinicensis")

native_background_enm<- rbind(coqui_target_native_background,planirostris_target_native_background,
                              johnstonei_target_native_background,
                              antillensis_target_native_background, 
                              martinicensis_target_native_background) %>% 
  mutate(Type = "Native") %>% 
  mutate(Background = "Yes")
  

##### Invasive Background - Different for each analysis ##########################################################################################################################################################

##### Generate invasive background area for ecospat niche modelling - can be broad biogeographic regions ############################

#coqui invasive area
coqui_countries<- rnaturalearth::ne_countries(continent = c('North America', scale = "medium", returnclass = "sf")) %>% vect() %>% project(crs(bio))
coqui_countries<- rbind(coqui_countries, caribbean_proj)

plot(coqui_countries)



## planirostris invasive area

planirostris_countries<- rnaturalearth::ne_countries(country = c('Philippines', 'Singapore', 'Taiwan'),
                                                     scale = "medium", returnclass = "sf") %>% vect() %>% project(crs(bio))
planirostris_countries<- rbind(planirostris_countries, vect(ne_countries(continent = c("North America", "South America", "Asia")))) %>% project(crs(bio))
planirostris_countries<- rbind(planirostris_countries, caribbean_proj)

plot(planirostris_countries)

#johnstonei_invasive_countries
johnstonei_countries<- rnaturalearth::ne_countries(continent = c("South America"),scale = "medium", returnclass = "sf") %>% vect() %>% project(crs(bio))
johnstonei_countries<- rbind(johnstonei_countries, caribbean_proj)


plot(johnstonei_countries)

#antillensis invasive area
antillensis_countries<- caribbean_proj
plot(antillensis_countries)

## martinicensis invasive area
martinicensis_countries<- caribbean_proj
plot(martinicensis_countries)

#### mask each species to the respective polygons 

coqui_invasive_rast<- crop(bio, coqui_countries)
coqui_invasive_rast<- mask(coqui_invasive_rast, coqui_countries)


planirostris_invasive_rast<- crop(bio, planirostris_countries)
planirostris_invasive_rast<- mask(planirostris_invasive_rast, planirostris_countries)


johnstonei_invasive_rast<- crop(bio, johnstonei_countries)
johnstonei_invasive_rast<- mask(johnstonei_invasive_rast, johnstonei_countries)


antillensis_invasive_rast<- mask_bio_amphregions_carib
martinicensis_invasive_rast<- mask_bio_amphregions_carib




## generate sampling of background area
coqui_invasive_background <- spatSample(x = coqui_invasive_rast, 
                                      size = 10000, 
                                      ext = ext(coqui_invasive_rast),
                                      na.rm = T, 
                                      method = "regular") %>% mutate(species = "Eleutherodactylus coqui") %>% 
  mutate(Type = "Invasive") %>% 
  mutate(Background = "Yes")

planirostris_invasive_background <- spatSample(x = planirostris_invasive_rast, 
                                             size = 10000, 
                                             ext = ext(planirostris_invasive_rast),
                                             na.rm = T, 
                                             method = "regular") %>% mutate(species = "Eleutherodactylus planirostris") %>% 
  mutate(Type = "Invasive") %>% 
  mutate(Background = "Yes")

johnstonei_invasive_background <- spatSample(x = johnstonei_invasive_rast, 
                                           size = 10000, 
                                           ext = ext(johnstonei_invasive_rast),
                                           na.rm = T, 
                                           method = "regular") %>% mutate(species = "Eleutherodactylus johnstonei") %>% 
  mutate(Type = "Invasive") %>% 
  mutate(Background = "Yes")

antillensis_invasive_background <- spatSample(x = antillensis_invasive_rast, 
                                            size = 10000, 
                                            ext = ext(antillensis_invasive_rast),
                                            na.rm = T, 
                                            method = "regular") %>% mutate(species = "Eleutherodactylus antillensis") %>% 
  mutate(Type = "Invasive") %>% 
  mutate(Background = "Yes")

martinicensis_invasive_background <- spatSample(x = martinicensis_invasive_rast, 
                                              size = 10000, 
                                              ext = ext(martinicensis_invasive_rast),
                                              na.rm = T, 
                                              method = "regular") %>% mutate(species = "Eleutherodactylus martinicensis") %>% 
  mutate(Type = "Invasive") %>% 
  mutate(Background = "Yes")


##### combine all background files for niche modelling

invasive_background_niche<- rbind(coqui_invasive_background, planirostris_invasive_background,
                                  johnstonei_invasive_background,
                                  antillensis_invasive_background,
                                  martinicensis_invasive_background)



#### Generate invasive background area for ENM - needs to be more constrained using 25 km background buffer

### target group approaches - get second target group dataset from the eastern hemisphere
background2<- read_tsv("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/RawData/0003842-260715120105164_targetbackground2/0003842-260715120105164.csv")

background2<- background2 %>% filter(decimalLatitude != "NA") %>% 
  filter(coordinateUncertaintyInMeters <= 5000 ) %>% 
  filter(species != "Eleutherodactylus planirostris")%>% 
  filter(species != "Eleutherodactylus coqui")%>% 
  filter(species != "Eleutherodactylus johnstonei")%>% 
  filter(species != "Eleutherodactylus antillensis")%>% 
  filter(species != "Eleutherodactylus martinicensis") %>% cc_aohi(lon = "decimalLongitude", lat = "decimalLatitude") %>% 
  cc_cen(lon = "decimalLongitude", lat = "decimalLatitude", species = "species") %>% 
  cc_inst(lon = "decimalLongitude", lat = "decimalLatitude", species = "species", buffer = 100) %>% 
  select(c("decimalLongitude", "decimalLatitude")) %>% vect()

### combine with background 1 

target_background<- rbind(background1, background2)

#### coqui background

coqui_points<- coqui_invasive[,2:3]  %>% terra::vect()
coqui_points_proj <- project(
  coqui_points,
  "ESRI:54009"
)
coqui_enm_background<- background.buffer(coqui_points_proj, buffer.width = 40000, buffer.type = 'circles',
                                         mask = bio, return.type = 'polygon') 
coqui_proj<- project(coqui_enm_background, crs(bio))
coqui_target_invasive_background<- crop(target_background, coqui_proj)
coqui_target_invasive_background<- as.data.frame(coqui_target_invasive_background, geom='XY') %>% 
  mutate(Species = "Eleutherodactylus coqui") # 17578  points 



### planirostris background
planirostris_points<- planirostris_invasive[,2:3]  %>% terra::vect()
planirostris_points_proj <- project(
  planirostris_points,
  "ESRI:54009"
)
planirostris_enm_background<- background.buffer(planirostris_points_proj, buffer.width = 40000, buffer.type = 'circles',
                                                mask = bio, return.type = 'polygon') 
planirostris_proj<- project(planirostris_enm_background, crs(bio))
planirostris_target_invasive_background<- crop(target_background, planirostris_proj)
planirostris_target_invasive_background<- as.data.frame(planirostris_target_invasive_background, geom='XY') %>% 
  mutate(Species = "Eleutherodactylus planirostris")#383666     points



## johnstonei background
johnstonei_points<- johnstonei_invasive[,2:3]  %>% terra::vect()
johnstonei_points_proj <- project(
  johnstonei_points,
  "ESRI:54009"
)
johnstonei_enm_background<- background.buffer(johnstonei_points_proj, buffer.width = 40000, buffer.type = 'circles',
                                              mask = bio, return.type = 'polygon') 
johnstonei_proj<- project(johnstonei_enm_background, crs(bio))
johnstonei_target_invasive_background<- crop(target_background, johnstonei_proj)
johnstonei_target_invasive_background<- as.data.frame(johnstonei_target_invasive_background, geom='XY') %>% 
  mutate(Species = "Eleutherodactylus johnstonei") #24604 points 


## antillensis background

antillensis_points<- antillensis_invasive[,2:3]  %>% terra::vect()
antillensis_points_proj <- project(
  antillensis_points,
  "ESRI:54009"
)
antillensis_enm_background<- background.buffer(antillensis_points_proj, buffer.width = 40000, buffer.type = 'circles',
                                               mask = bio, return.type = 'polygon') 
antillensis_proj<- project(antillensis_enm_background, crs(bio))
antillensis_target_invasive_background<- crop(target_background, antillensis_proj)
antillensis_target_invasive_background<- as.data.frame(antillensis_target_invasive_background, geom='XY') %>% 
  mutate(Species = "Eleutherodactylus antillensis")#178   points 

## martinicensis background

martinicensis_points<- martinicensis_invasive[,2:3]  %>% terra::vect()
martinicensis_points_proj <- project(
  martinicensis_points,
  "ESRI:54009"
)
martinicensis_enm_background<- background.buffer(martinicensis_points_proj, buffer.width = 40000, buffer.type = 'circles',
                                                 mask = bio, return.type = 'polygon') 
martinicensis_proj<- project(martinicensis_enm_background, crs(bio))
martinicensis_target_invasive_background<- crop(target_background, martinicensis_proj) 
martinicensis_target_invasive_background<- as.data.frame(martinicensis_target_invasive_background, geom='XY') %>% 
  mutate(Species = "Eleutherodactylus martinicensis")#618   points


##### Subset the target group to each species buffered background

### subset 5 % of the number of rows/points for each species


coqui_target_invasive_background <- coqui_target_invasive_background[sample(nrow(coqui_target_invasive_background), 879), ]
planirostris_target_invasive_background <- planirostris_target_invasive_background[sample(nrow(planirostris_target_invasive_background), 19183), ]
johnstonei_target_invasive_background<- johnstonei_target_invasive_background[sample(nrow(johnstonei_target_invasive_background), 1230), ]
antillensis_target_invasive_background<- antillensis_target_invasive_background[sample(nrow(antillensis_target_invasive_background), 9), ]
martinicensis_target_invasive_background <- martinicensis_target_invasive_background[sample(nrow(martinicensis_target_invasive_background), 31), ]

invasive_background_enm<- rbind(coqui_target_invasive_background,planirostris_target_invasive_background,
                              johnstonei_target_invasive_background,
                              antillensis_target_invasive_background, 
                              martinicensis_target_invasive_background) %>% 
  mutate(Type = "Invasive") %>% 
  mutate(Background = "Yes")



###### Combine all files

occurence_final_filtered<- occurrence_final %>%  select(c("decimalLongitude", "decimalLatitude", "species", "Type", "Background"))
colnames(occurence_final_filtered)<- c("x","y", "Species", "Type", "Background")

final_enm_datapoints<- rbind(occurence_final_filtered, native_background_enm, invasive_background_enm)

#write.csv(final_enm_datapoints, "final_enm_datapoints_07312026_v2.csv")



###### PCA explaining 95% variance for spatially thinned data points #################################################################
######################################################################################################################################


final_enm_datapoints<- read.csv("final_enm_datapoints_07312026_v2.csv")

## No background included
coqui_pca.env <- ade4::dudi.pca(na.omit(coqui_occ[,4:22]),scannf=F,nf=10)
write.csv(coqui_pca.env$c1, "PCA_ENV_07092026/coqui_princomp.csv")
write.csv(coqui_pca.env$eig, "PCA_ENV_07092026/coqui_eigenvalues.csv")
planirostris_pca.env <- ade4::dudi.pca(na.omit(planirostris_occ[,4:22]),scannf=F,nf=10)
write.csv(planirostris_pca.env$c1, "PCA_ENV_07092026/planirostris_princomp.csv")
write.csv(planirostris_pca.env$eig, "PCA_ENV_07092026/planirostris_eigenvalues.csv")
johnstonei_pca.env <- ade4::dudi.pca(na.omit(johnstonei_occ[,4:22]),scannf=F,nf=10)
write.csv(johnstonei_pca.env$c1, "PCA_ENV_07092026/johnstonei_princomp.csv")
write.csv(johnstonei_pca.env$eig, "PCA_ENV_07092026/johnstonei_eigenvalues.csv")
antillensis_pca.env <- ade4::dudi.pca(na.omit(antillensis_occ[,4:22]),scannf=F,nf=10)
write.csv(antillensis_pca.env$c1, "PCA_ENV_07092026/antillensis_princomp.csv")
write.csv(antillensis_pca.env$eig, "PCA_ENV_07092026/antillensis_eigenvalues.csv")
martinicensis_pca.env <- ade4::dudi.pca(na.omit(martinicensis_occ[,4:22]),scannf=F,nf=10)
write.csv(martinicensis_pca.env$c1, "PCA_ENV_07092026/martinicensis_princomp.csv")
write.csv(martinicensis_pca.env$eig, "PCA_ENV_07092026/martinicensis_eigenvalues.csv")






##### Coqui Niche Dynamics
coqui_background<- rbind(coqui_native_background_niche, coqui_invasive_background) %>% select(!"species")
coqui_occurrence<- coqui_occ %>% select(!c("species", "decimalLatitude", "decimalLongitude"))

coqui_niche<- rbind(coqui_occurrence, coqui_background) %>% na.omit()


coqui_pca.env_all <- ade4::dudi.pca(coqui_niche[,1:19],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=coqui_pca.env_all$co, eigen=coqui_pca.env_all$eig)
png("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures_Submission2/Ecospat_Niche_Quantification/coqui_PCA.png", units="in", width=25, height=15,  res = 300)

write.csv(coqui_pca.env_all$co, "coqui_pca_bioclimate_backgroundadjusted.csv")

#PCA scoresfor the wholestudyarea
coqui_scores.globclim <-coqui_pca.env_all$li

#PCA scoresfor the species nativedistribution 
coqui_scores.sp.nat<-ade4::suprow(coqui_pca.env_all,coqui_niche[which(coqui_niche[,21]=="No" & coqui_niche[,20]=="Native"),1:19])$li
#PCA scoresfor the species invasive distribution 
coqui_scores.sp.inv<-ade4::suprow(coqui_pca.env_all,coqui_niche[which(coqui_niche[,21]=="No" & coqui_niche[,20]=="Invasive"),1:19])$li

#PCA scoresfor the wholenativestudyarea - Double check this
coqui_scores.clim.nat <-ade4::suprow(coqui_pca.env_all,coqui_niche[which(coqui_niche[,21]=="Yes" & coqui_niche[,20]=="Native"),1:19])$li

#PCA scoresfor the wholeinvaded studyarea - Double check this
coqui_scores.clim.inv <-ade4::suprow(coqui_pca.env_all,coqui_niche[which(coqui_niche[,21]=="Yes" & coqui_niche[,20]=="Invasive"),1:19])$li

#gridding the nativeniche 
coqui_grid.clim.nat<-ecospat.grid.clim.dyn(glob=coqui_scores.globclim, glob1=coqui_scores.clim.nat, sp=coqui_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
coqui_grid.clim.inv <- ecospat.grid.clim.dyn(glob=coqui_scores.globclim, glob1=coqui_scores.clim.inv, sp=coqui_scores.sp.inv, R=100, th.sp=0)


coqui_D.overlap <- ecospat.niche.overlap (coqui_grid.clim.nat, coqui_grid.clim.inv, cor = TRUE)$D 
coqui_D.overlap

coqui_I.overlap <- ecospat.niche.overlap (coqui_grid.clim.nat, coqui_grid.clim.inv, cor = TRUE)$I
coqui_I.overlap


coqui_dynamics<- ecospat.niche.dyn.index(coqui_grid.clim.nat, coqui_grid.clim.inv, intersection = 0)

# For this z2_only_NA = Pioneering
# z2_only_A Expansion
# z2_z1 Stability
# z1_only_NA Abandonment
# z1_only_A Unfilling
# z1_z2 Non COUE proportion

coqui_COUE<- c(
  pioneering = coqui_dynamics$category_quantity["z2_only_NA"],
  expansion = coqui_dynamics$category_quantity["z2_only_A"],
  stability = coqui_dynamics$category_quantity["z2_z1"],
  abandonment = coqui_dynamics$category_quantity["z1_only_NA"],
  unfilling = coqui_dynamics$category_quantity["z1_only_A"]
)


coqui_COUE_prop <- coqui_COUE / sum(coqui_COUE)





#### Use 1000 replications for equivalency and similarity test

##### Equivalency test - niche divergence

coqui_eq.test1<- ecospat.niche.equivalency.test(coqui_grid.clim.nat, coqui_grid.clim.inv,rep=1000, intersection = 0.1, 
                                               overlap.alternative = "lower",
                                               expansion.alternative = "higher",
                                               stability.alternative = "lower",
                                               unfilling.alternative = "higher")

##### Equivalency test - niche conservatism

coqui_eq.test2<- ecospat.niche.equivalency.test(coqui_grid.clim.nat, coqui_grid.clim.inv,rep=1000, intersection = 0.1, 
                                               overlap.alternative = "higher",
                                               expansion.alternative = "lower",
                                               stability.alternative = "higher",
                                               unfilling.alternative = "lower")




### Similarity test - niche divergence
coqui_sim.test1<-  ecospat.niche.similarity.test(coqui_grid.clim.nat, coqui_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                overlap.alternative = "lower",
                                                expansion.alternative = "higher",
                                                stability.alternative = "lower",
                                                unfilling.alternative = "higher") 



### Similarity test - niche conservatism
coqui_sim.test2<-  ecospat.niche.similarity.test(coqui_grid.clim.nat, coqui_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                overlap.alternative = "higher",
                                                expansion.alternative = "lower",
                                                stability.alternative = "higher",
                                                unfilling.alternative = "lower") 





### make niche figures
ecospat.plot.niche.dyn(coqui_grid.clim.nat, coqui_grid.clim.inv, quant=0.25, interest=2, title= NULL, name.axis1="PC1 (64.67%)", name.axis2="PC2 (17.97%)")
ecospat.shift.centroids(coqui_scores.sp.nat, coqui_scores.sp.inv, coqui_scores.clim.nat, coqui_scores.clim.inv)
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures_Submission2/Ecospat_Niche_Quantification/coqui_niche.tiff", units="in", width=25, height=15, dpi=300, compression = 'lzw')




##### planirostris Niche Dynamics
planirostris_background<- rbind(planirostris_native_background_niche, planirostris_invasive_background) %>% select(!"species")
planirostris_occurrence<- planirostris_occ %>% select(!c("species", "decimalLatitude", "decimalLongitude"))

planirostris_niche<- rbind(planirostris_occurrence, planirostris_background) %>% na.omit()


planirostris_pca.env_all <- ade4::dudi.pca(planirostris_niche[,1:19],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=planirostris_pca.env_all$co, eigen=planirostris_pca.env_all$eig)
#ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures_Submission2/Ecospat_Niche_Quantification/planirostris_PCA.tiff", units="in", width=25, height=15, dpi=300, compression = 'lzw')

write.csv(planirostris_pca.env_all$co, "planirostris_pca_bioclimate_backgroundadjusted.csv")

#PCA scoresfor the wholestudyarea
planirostris_scores.globclim <-planirostris_pca.env_all$li

#PCA scoresfor the species nativedistribution 
planirostris_scores.sp.nat<-ade4::suprow(planirostris_pca.env_all,planirostris_niche[which(planirostris_niche[,21]=="No" & planirostris_niche[,20]=="Native"),1:19])$li
#PCA scoresfor the species invasive distribution 
planirostris_scores.sp.inv<-ade4::suprow(planirostris_pca.env_all,planirostris_niche[which(planirostris_niche[,21]=="No" & planirostris_niche[,20]=="Invasive"),1:19])$li

#PCA scoresfor the wholenativestudyarea - Double check this
planirostris_scores.clim.nat <-ade4::suprow(planirostris_pca.env_all,planirostris_niche[which(planirostris_niche[,21]=="Yes" & planirostris_niche[,20]=="Native"),1:19])$li

#PCA scoresfor the wholeinvaded studyarea - Double check this
planirostris_scores.clim.inv <-ade4::suprow(planirostris_pca.env_all,planirostris_niche[which(planirostris_niche[,21]=="Yes" & planirostris_niche[,20]=="Invasive"),1:19])$li

#gridding the nativeniche 
planirostris_grid.clim.nat<-ecospat.grid.clim.dyn(glob=planirostris_scores.globclim, glob1=planirostris_scores.clim.nat, sp=planirostris_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
planirostris_grid.clim.inv <- ecospat.grid.clim.dyn(glob=planirostris_scores.globclim, glob1=planirostris_scores.clim.inv, sp=planirostris_scores.sp.inv, R=100, th.sp=0)


planirostris_D.overlap <- ecospat.niche.overlap (planirostris_grid.clim.nat, planirostris_grid.clim.inv, cor = TRUE)$D 
planirostris_D.overlap

planirostris_I.overlap <- ecospat.niche.overlap (planirostris_grid.clim.nat, planirostris_grid.clim.inv, cor = TRUE)$I
planirostris_I.overlap



planirostris_dynamics<- ecospat.niche.dyn.index(planirostris_grid.clim.nat, planirostris_grid.clim.inv, intersection = 0)

# For this z2_only_NA = Pioneering
# z2_only_A Expansion
# z2_z1 Stability
# z1_only_NA Abandonment
# z1_only_A Unfilling
# z1_z2 Non COUE proportion

planirostris_COUE<- c(
  pioneering = planirostris_dynamics$category_quantity["z2_only_NA"],
  expansion = planirostris_dynamics$category_quantity["z2_only_A"],
  stability = planirostris_dynamics$category_quantity["z2_z1"],
  abandonment = planirostris_dynamics$category_quantity["z1_only_NA"],
  unfilling = planirostris_dynamics$category_quantity["z1_only_A"]
)


planirostris_COUE_prop <- planirostris_COUE / sum(planirostris_COUE)






#### Use 1000 replications for equivalency and similarity test


planirostris_eq.test1<- ecospat.niche.equivalency.test(planirostris_grid.clim.nat, planirostris_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                      overlap.alternative = "lower",
                                                      expansion.alternative = "higher",
                                                      stability.alternative = "lower",
                                                      unfilling.alternative = "higher")


##### Equivalency test - niche conservatism


planirostris_eq.test2<- ecospat.niche.equivalency.test(planirostris_grid.clim.nat, planirostris_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                      overlap.alternative = "higher",
                                                      expansion.alternative = "lower",
                                                      stability.alternative = "higher",
                                                      unfilling.alternative = "lower")


### Similarity test - niche divergence
planirostris_sim.test1<-  ecospat.niche.similarity.test(planirostris_grid.clim.nat, planirostris_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                       overlap.alternative = "lower",
                                                       expansion.alternative = "higher",
                                                       stability.alternative = "lower",
                                                       unfilling.alternative = "higher") 


### Similarity test - niche conservatism
planirostris_sim.test2<-  ecospat.niche.similarity.test(planirostris_grid.clim.nat, planirostris_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                       overlap.alternative = "higher",
                                                       expansion.alternative = "lower",
                                                       stability.alternative = "higher",
                                                       unfilling.alternative = "lower") 


##### MAKE SURE TO CHANGE PC AXES

### make niche figures
ecospat.plot.niche.dyn(planirostris_grid.clim.nat, planirostris_grid.clim.inv, quant=0.25, interest=2, title= NULL, name.axis1="PC1 (57.56%)", name.axis2="PC2 (17.16%)")
ecospat.shift.centroids(planirostris_scores.sp.nat, planirostris_scores.sp.inv, planirostris_scores.clim.nat, planirostris_scores.clim.inv)




##### johnstonei Niche Dynamics
johnstonei_background<- rbind(johnstonei_native_background_niche, johnstonei_invasive_background) %>% select(!"species")
johnstonei_occurrence<- johnstonei_occ %>% select(!c("species", "decimalLatitude", "decimalLongitude"))

johnstonei_niche<- rbind(johnstonei_occurrence, johnstonei_background) %>% na.omit()


johnstonei_pca.env_all <- ade4::dudi.pca(johnstonei_niche[,1:19],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=johnstonei_pca.env_all$co, eigen=johnstonei_pca.env_all$eig)
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures_Submission2/Ecospat_Niche_Quantification/johnstonei_PCA.tiff", units="in", width=25, height=15, dpi=300, compression = 'lzw')

write.csv(johnstonei_pca.env_all$co, "johnstonei_pca_bioclimate_backgroundadjusted.csv")

#PCA scoresfor the wholestudyarea
johnstonei_scores.globclim <-johnstonei_pca.env_all$li

#PCA scoresfor the species nativedistribution 
johnstonei_scores.sp.nat<-ade4::suprow(johnstonei_pca.env_all,johnstonei_niche[which(johnstonei_niche[,21]=="No" & johnstonei_niche[,20]=="Native"),1:19])$li
#PCA scoresfor the species invasive distribution 
johnstonei_scores.sp.inv<-ade4::suprow(johnstonei_pca.env_all,johnstonei_niche[which(johnstonei_niche[,21]=="No" & johnstonei_niche[,20]=="Invasive"),1:19])$li

#PCA scoresfor the wholenativestudyarea - Double check this
johnstonei_scores.clim.nat <-ade4::suprow(johnstonei_pca.env_all,johnstonei_niche[which(johnstonei_niche[,21]=="Yes" & johnstonei_niche[,20]=="Native"),1:19])$li

#PCA scoresfor the wholeinvaded studyarea - Double check this
johnstonei_scores.clim.inv <-ade4::suprow(johnstonei_pca.env_all,johnstonei_niche[which(johnstonei_niche[,21]=="Yes" & johnstonei_niche[,20]=="Invasive"),1:19])$li

#gridding the nativeniche 
johnstonei_grid.clim.nat<-ecospat.grid.clim.dyn(glob=johnstonei_scores.globclim, glob1=johnstonei_scores.clim.nat, sp=johnstonei_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
johnstonei_grid.clim.inv <- ecospat.grid.clim.dyn(glob=johnstonei_scores.globclim, glob1=johnstonei_scores.clim.inv, sp=johnstonei_scores.sp.inv, R=100, th.sp=0)


johnstonei_D.overlap <- ecospat.niche.overlap (johnstonei_grid.clim.nat, johnstonei_grid.clim.inv, cor = TRUE)$D 
johnstonei_D.overlap

johnstonei_I.overlap <- ecospat.niche.overlap (johnstonei_grid.clim.nat, johnstonei_grid.clim.inv, cor = TRUE)$I
johnstonei_I.overlap


johnstonei_dynamics<- ecospat.niche.dyn.index(johnstonei_grid.clim.nat, johnstonei_grid.clim.inv, intersection = 0)

# For this z2_only_NA = Pioneering
# z2_only_A Expansion
# z2_z1 Stability
# z1_only_NA Abandonment
# z1_only_A Unfilling
# z1_z2 Non COUE proportion

johnstonei_COUE<- c(
  pioneering = johnstonei_dynamics$category_quantity["z2_only_NA"],
  expansion = johnstonei_dynamics$category_quantity["z2_only_A"],
  stability = johnstonei_dynamics$category_quantity["z2_z1"],
  abandonment = johnstonei_dynamics$category_quantity["z1_only_NA"],
  unfilling = johnstonei_dynamics$category_quantity["z1_only_A"]
)


johnstonei_COUE_prop <- johnstonei_COUE / sum(johnstonei_COUE)








#### Use 1000 replications for equivalency and similarity test

johnstonei_eq.test1<- ecospat.niche.equivalency.test(johnstonei_grid.clim.nat, johnstonei_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                    overlap.alternative = "lower",
                                                    expansion.alternative = "higher",
                                                    stability.alternative = "lower",
                                                    unfilling.alternative = "higher")



##### Equivalency test - niche conservatism

johnstonei_eq.test2<- ecospat.niche.equivalency.test(johnstonei_grid.clim.nat, johnstonei_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                    overlap.alternative = "higher",
                                                    expansion.alternative = "lower",
                                                    stability.alternative = "higher",
                                                    unfilling.alternative = "lower")



### Similarity test - niche divergence
johnstonei_sim.test1<-  ecospat.niche.similarity.test(johnstonei_grid.clim.nat, johnstonei_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                     overlap.alternative = "lower",
                                                     expansion.alternative = "higher",
                                                     stability.alternative = "lower",
                                                     unfilling.alternative = "higher") 

### Similarity test - niche conservatism
johnstonei_sim.test2<-  ecospat.niche.similarity.test(johnstonei_grid.clim.nat, johnstonei_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                     overlap.alternative = "higher",
                                                     expansion.alternative = "lower",
                                                     stability.alternative = "higher",
                                                     unfilling.alternative = "lower") 

##### MAKE SURE TO CHANGE PC AXES

### make niche figures
ecospat.plot.niche.dyn(johnstonei_grid.clim.nat, johnstonei_grid.clim.inv, quant=0.25, interest=2, title= NULL, name.axis1="PC1 (54.46%)", name.axis2="PC2 (18.6%)")
ecospat.shift.centroids(johnstonei_scores.sp.nat, johnstonei_scores.sp.inv, johnstonei_scores.clim.nat, johnstonei_scores.clim.inv)




##### antillensis Niche Dynamics
antillensis_background<- rbind(antillensis_native_background_niche, antillensis_invasive_background) %>% select(!"species")
antillensis_occurrence<- antillensis_occ %>% select(!c("species", "decimalLatitude", "decimalLongitude"))

antillensis_niche<- rbind(antillensis_occurrence, antillensis_background) %>% na.omit()


antillensis_pca.env_all <- ade4::dudi.pca(antillensis_niche[,1:19],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=antillensis_pca.env_all$co, eigen=antillensis_pca.env_all$eig)
#ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures_Submission2/Ecospat_Niche_Quantification/antillensis_PCA.tiff", units="in", width=25, height=15, dpi=300, compression = 'lzw')

write.csv(antillensis_pca.env_all$co, "antillensis_pca_bioclimate_backgroundadjusted.csv")


#PCA scoresfor the wholestudyarea
antillensis_scores.globclim <-antillensis_pca.env_all$li

#PCA scoresfor the species nativedistribution 
antillensis_scores.sp.nat<-ade4::suprow(antillensis_pca.env_all,antillensis_niche[which(antillensis_niche[,21]=="No" & antillensis_niche[,20]=="Native"),1:19])$li
#PCA scoresfor the species invasive distribution 
antillensis_scores.sp.inv<-ade4::suprow(antillensis_pca.env_all,antillensis_niche[which(antillensis_niche[,21]=="No" & antillensis_niche[,20]=="Invasive"),1:19])$li

#PCA scoresfor the wholenativestudyarea - Double check this
antillensis_scores.clim.nat <-ade4::suprow(antillensis_pca.env_all,antillensis_niche[which(antillensis_niche[,21]=="Yes" & antillensis_niche[,20]=="Native"),1:19])$li

#PCA scoresfor the wholeinvaded studyarea - Double check this
antillensis_scores.clim.inv <-ade4::suprow(antillensis_pca.env_all,antillensis_niche[which(antillensis_niche[,21]=="Yes" & antillensis_niche[,20]=="Invasive"),1:19])$li

#gridding the nativeniche 
antillensis_grid.clim.nat<-ecospat.grid.clim.dyn(glob=antillensis_scores.globclim, glob1=antillensis_scores.clim.nat, sp=antillensis_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
antillensis_grid.clim.inv <- ecospat.grid.clim.dyn(glob=antillensis_scores.globclim, glob1=antillensis_scores.clim.inv, sp=antillensis_scores.sp.inv, R=100, th.sp=0)


antillensis_D.overlap <- ecospat.niche.overlap (antillensis_grid.clim.nat, antillensis_grid.clim.inv, cor = TRUE)$D 
antillensis_D.overlap

antillensis_I.overlap <- ecospat.niche.overlap (antillensis_grid.clim.nat, antillensis_grid.clim.inv, cor = TRUE)$I
antillensis_I.overlap



antillensis_dynamics<- ecospat.niche.dyn.index(antillensis_grid.clim.nat, antillensis_grid.clim.inv, intersection = 0)

# For this z2_only_NA = Pioneering
# z2_only_A Expansion
# z2_z1 Stability
# z1_only_NA Abandonment
# z1_only_A Unfilling
# z1_z2 Non COUE proportion

antillensis_COUE<- c(
  pioneering = antillensis_dynamics$category_quantity["z2_only_NA"],
  expansion = antillensis_dynamics$category_quantity["z2_only_A"],
  stability = antillensis_dynamics$category_quantity["z2_z1"],
  abandonment = antillensis_dynamics$category_quantity["z1_only_NA"],
  unfilling = antillensis_dynamics$category_quantity["z1_only_A"]
)


antillensis_COUE_prop <- antillensis_COUE / sum(antillensis_COUE)







#### Use 1000 replications for equivalency and similarity test

antillensis_eq.test1<- ecospat.niche.equivalency.test(antillensis_grid.clim.nat, antillensis_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                     overlap.alternative = "lower",
                                                     expansion.alternative = "higher",
                                                     stability.alternative = "lower",
                                                     unfilling.alternative = "higher")


##### Equivalency test - niche conservatism

antillensis_eq.test2<-ecospat.niche.equivalency.test(antillensis_grid.clim.nat, antillensis_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                     overlap.alternative = "higher",
                                                     expansion.alternative = "lower",
                                                     stability.alternative = "higher",
                                                     unfilling.alternative = "lower")




### Similarity test - niche divergence
antillensis_sim.test1<-  ecospat.niche.similarity.test(antillensis_grid.clim.nat, antillensis_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                      overlap.alternative = "lower",
                                                      expansion.alternative = "higher",
                                                      stability.alternative = "lower",
                                                      unfilling.alternative = "higher") 



### Similarity test - niche conservatism
antillensis_sim.test2<-  ecospat.niche.similarity.test(antillensis_grid.clim.nat, antillensis_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                      overlap.alternative = "higher",
                                                      expansion.alternative = "lower",
                                                      stability.alternative = "higher",
                                                      unfilling.alternative = "lower") 


##### MAKE SURE TO CHANGE PC AXES

### make niche figures
ecospat.plot.niche.dyn(antillensis_grid.clim.nat, antillensis_grid.clim.inv, quant=0.25, interest=2, title= NULL, name.axis1="PC1 (39.88%)", name.axis2="PC2 (26.33)")
ecospat.shift.centroids(antillensis_scores.sp.nat, antillensis_scores.sp.inv, antillensis_scores.clim.nat, antillensis_scores.clim.inv)





##### martinicensis Niche Dynamics
martinicensis_background<- rbind(martinicensis_native_background_niche, martinicensis_invasive_background) %>% select(!"species")
martinicensis_occurrence<- martinicensis_occ %>% select(!c("species", "decimalLatitude", "decimalLongitude"))

martinicensis_niche<- rbind(martinicensis_occurrence, martinicensis_background) %>% na.omit()


martinicensis_pca.env_all <- ade4::dudi.pca(martinicensis_niche[,1:19],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=martinicensis_pca.env_all$co, eigen=martinicensis_pca.env_all$eig)
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Invasions_Submission/Figures_Submission2/Ecospat_Niche_Quantification/martinicensis_PCA.tiff", units="in", width=25, height=15, dpi=300, compression = 'lzw')

write.csv(martinicensis_pca.env_all$co, "martinicensis_pca_bioclimate_backgroundadjusted.csv")

#PCA scoresfor the wholestudyarea
martinicensis_scores.globclim <-martinicensis_pca.env_all$li

#PCA scoresfor the species nativedistribution 
martinicensis_scores.sp.nat<-ade4::suprow(martinicensis_pca.env_all,martinicensis_niche[which(martinicensis_niche[,21]=="No" & martinicensis_niche[,20]=="Native"),1:19])$li
#PCA scoresfor the species invasive distribution 
martinicensis_scores.sp.inv<-ade4::suprow(martinicensis_pca.env_all,martinicensis_niche[which(martinicensis_niche[,21]=="No" & martinicensis_niche[,20]=="Invasive"),1:19])$li

#PCA scoresfor the wholenativestudyarea - Double check this
martinicensis_scores.clim.nat <-ade4::suprow(martinicensis_pca.env_all,martinicensis_niche[which(martinicensis_niche[,21]=="Yes" & martinicensis_niche[,20]=="Native"),1:19])$li

#PCA scoresfor the wholeinvaded studyarea - Double check this
martinicensis_scores.clim.inv <-ade4::suprow(martinicensis_pca.env_all,martinicensis_niche[which(martinicensis_niche[,21]=="Yes" & martinicensis_niche[,20]=="Invasive"),1:19])$li

#gridding the nativeniche 
martinicensis_grid.clim.nat<-ecospat.grid.clim.dyn(glob=martinicensis_scores.globclim, glob1=martinicensis_scores.clim.nat, sp=martinicensis_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
martinicensis_grid.clim.inv <- ecospat.grid.clim.dyn(glob=martinicensis_scores.globclim, glob1=martinicensis_scores.clim.inv, sp=martinicensis_scores.sp.inv, R=100, th.sp=0)


martinicensis_D.overlap <- ecospat.niche.overlap (martinicensis_grid.clim.nat, martinicensis_grid.clim.inv, cor = TRUE)$D 
martinicensis_D.overlap

martinicensis_I.overlap <- ecospat.niche.overlap (martinicensis_grid.clim.nat, martinicensis_grid.clim.inv, cor = TRUE)$I
martinicensis_I.overlap


martinicensis_dynamics<- ecospat.niche.dyn.index(martinicensis_grid.clim.nat, martinicensis_grid.clim.inv, intersection = 0)

# For this z2_only_NA = Pioneering
# z2_only_A Expansion
# z2_z1 Stability
# z1_only_NA Abandonment
# z1_only_A Unfilling
# z1_z2 Non COUE proportion

martinicensis_COUE<- c(
  pioneering = martinicensis_dynamics$category_quantity["z2_only_NA"],
  expansion = martinicensis_dynamics$category_quantity["z2_only_A"],
  stability = martinicensis_dynamics$category_quantity["z2_z1"],
  abandonment = martinicensis_dynamics$category_quantity["z1_only_NA"],
  unfilling = martinicensis_dynamics$category_quantity["z1_only_A"]
)


martinicensis_COUE_prop <- martinicensis_COUE / sum(martinicensis_COUE)







#### Use 1000 replications for equivalency and similarity test


martinicensis_eq.test1<- ecospat.niche.equivalency.test(martinicensis_grid.clim.nat, martinicensis_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                       overlap.alternative = "lower",
                                                       expansion.alternative = "higher",
                                                       stability.alternative = "lower",
                                                       unfilling.alternative = "higher")



##### Equivalency test - niche conservatism

martinicensis_eq.test2<- ecospat.niche.equivalency.test(martinicensis_grid.clim.nat, martinicensis_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                       overlap.alternative = "higher",
                                                       expansion.alternative = "lower",
                                                       stability.alternative = "higher",
                                                       unfilling.alternative = "lower")




### Similarity test - niche divergence
martinicensis_sim.test1<-  ecospat.niche.similarity.test(martinicensis_grid.clim.nat, martinicensis_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                        overlap.alternative = "lower",
                                                        expansion.alternative = "higher",
                                                        stability.alternative = "lower",
                                                        unfilling.alternative = "higher") 


### Similarity test - niche conservatism
martinicensis_sim.test2<-  ecospat.niche.similarity.test(martinicensis_grid.clim.nat, martinicensis_grid.clim.inv,rep=1000, intersection = 0.1, 
                                                        overlap.alternative = "higher",
                                                        expansion.alternative = "lower",
                                                        stability.alternative = "higher",
                                                        unfilling.alternative = "lower") 


##### MAKE SURE TO CHANGE PC AXES

### make niche figures
ecospat.plot.niche.dyn(martinicensis_grid.clim.nat, martinicensis_grid.clim.inv, quant=0.25, interest=2, title= NULL, name.axis1="PC1 (38.29%)", name.axis2="PC2 (28.45%)")
ecospat.shift.centroids(martinicensis_scores.sp.nat, martinicensis_scores.sp.inv, martinicensis_scores.clim.nat, martinicensis_scores.clim.inv)



### Highlight annual temperature and precipitation


coqui_niche<- coqui_niche %>% mutate(species = "Eleutherodactylus coqui")
planirostris_niche<- planirostris_niche %>% mutate(species = "Eleutherodactylus planirostris")
johnstonei_niche<- johnstonei_niche %>% mutate(species = "Eleutherodactylus johnstonei")
antillensis_niche<- antillensis_niche %>% mutate(species = "Eleutherodactylus antillensis")
martinicensis_niche<- martinicensis_niche %>% mutate(species = "Eleutherodactylus martinicensis")


niche<- rbind(coqui_niche, planirostris_niche, johnstonei_niche, antillensis_niche, martinicensis_niche)

niche_realized<- niche %>% filter(Background == "No")
niche_background<- niche %>% filter(Background == "Yes")


p1<- ggplot(niche_realized, aes(x=species, y = wc2.1_2.5m_bio_1,fill = Type))+
  geom_boxplot()+
  scale_fill_manual(values=c("#cd5c5c", "#fcc200"))+
  theme_classic()+
  xlab(element_blank())+
  ylab("Annual Mean Temperature (C)")+
  theme(axis.text.x = element_text(face = "italic", size = 30),
        axis.text.y = element_text(size = 25),
        axis.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 50),
        legend.text = element_text(size = 45))+
  scale_x_discrete(labels = c("Eleutherodactylus antillensis" = "E. antillensis",
                              "Eleutherodactylus coqui" = "E. coqui",
                              "Eleutherodactylus planirostris" = "E. planirostris",
                              "Eleutherodactylus johnstonei" = "E. johnstonei",
                              "Eleutherodactylus martinicensis"= "E. martinicensis"))


p2<- ggplot(niche_background, aes(x=species, y = wc2.1_2.5m_bio_1,fill = Type))+
  geom_boxplot()+
  scale_fill_manual(values=c("#cd5c5c", "#fcc200"))+
  theme_classic()+
  xlab(element_blank())+
  ylab("Annual Mean Temperature")+
  theme(axis.text.x = element_text(face = "italic", size = 30),
        axis.text.y = element_text(size = 25),
        axis.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 50),
        legend.text = element_text(size = 45))+
  scale_x_discrete(labels = c("Eleutherodactylus antillensis" = "E. antillensis",
                              "Eleutherodactylus coqui" = "E. coqui",
                              "Eleutherodactylus planirostris" = "E. planirostris",
                              "Eleutherodactylus johnstonei" = "E. johnstonei",
                              "Eleutherodactylus martinicensis"= "E. martinicensis"))


p3<- ggplot(niche_realized, aes(x=species, y = wc2.1_2.5m_bio_12,fill = Type))+
  geom_boxplot()+
  scale_fill_manual(values=c("#cd5c5c", "#fcc200"))+
  theme_classic()+
  xlab(element_blank())+
  ylab("Annual Precipitation (mm)")+
  theme(axis.text.x = element_text(face = "italic", size = 30),
        axis.text.y = element_text(size = 25),
        axis.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 50),
        legend.text = element_text(size = 45))+
  scale_x_discrete(labels = c("Eleutherodactylus antillensis" = "E. antillensis",
                              "Eleutherodactylus coqui" = "E. coqui",
                              "Eleutherodactylus planirostris" = "E. planirostris",
                              "Eleutherodactylus johnstonei" = "E. johnstonei",
                              "Eleutherodactylus martinicensis"= "E. martinicensis"))


p4<- ggplot(niche_background, aes(x=species, y = wc2.1_2.5m_bio_12,fill = Type))+
  geom_boxplot()+
  scale_fill_manual(values=c("#cd5c5c", "#fcc200"))+
  theme_classic()+
  xlab(element_blank())+
  ylab("Annual Precipitation (mm)")+
  theme(axis.text.x = element_text(face = "italic", size = 30),
        axis.text.y = element_text(size = 25),
        axis.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 50),
        legend.text = element_text(size = 45))+
  scale_x_discrete(labels = c("Eleutherodactylus antillensis" = "E. antillensis",
                              "Eleutherodactylus coqui" = "E. coqui",
                              "Eleutherodactylus planirostris" = "E. planirostris",
                              "Eleutherodactylus johnstonei" = "E. johnstonei",
                              "Eleutherodactylus martinicensis"= "E. martinicensis"))


ggarrange(p1,p2,p3,p4)




