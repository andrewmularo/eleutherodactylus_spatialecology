library(ecospat)
library(ade4)
library(raster)
library(CoordinateCleaner)
library(tidyverse)
library(geodata)
library(dismo)
library(vegan)


wd<- "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Historic_Reconstruction"

setwd(wd)

###### Raw GBIF downloaded data

coqui<- read_tsv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/RawData/Version2/Ecoqui.csv")
planirostris <- read_tsv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/RawData/Version2/Eplanirostris.csv")
johnstonei<- read_tsv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/RawData/Version2/Ejohnstonei.csv")
antillensis<- read_tsv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/RawData/Version2/Eantillensis.csv")
martinicensis<- read_tsv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/RawData/Version2/Emartinicensis.csv")
cystignathoides<- read_tsv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/RawData/Version2/Ecystignathoides.csv")

##### Data cleaning - tidyverse and coordinatecleaner

eleuth<- rbind(coqui, planirostris, johnstonei, antillensis, martinicensis, cystignathoides)
eleuth<- as.data.frame(eleuth)

### QC for historic records
eleuth_cleaned<- eleuth %>% cc_aohi(lon = "decimalLongitude", lat = "decimalLatitude") %>% 
  cc_cen(lon = "decimalLongitude", lat = "decimalLatitude", species = "species") %>% 
  cc_inst(lon = "decimalLongitude", lat = "decimalLatitude", species = "species", buffer = 100)


eleuth_cleaned_dates<- eleuth_cleaned %>% select(c("gbifID", "species", "countryCode", "locality", "stateProvince",
                                                   "decimalLatitude", "decimalLongitude", "day", "month", "year")) %>% 
  filter(year != "NA")

# write.csv(eleuth_cleaned_dates, "eleutherocatylus_invasionhistory.csv")

#write.csv(eleuth_cleaned, "eleuth_cleaned.csv")



# Check to see how many are flagged with each command
#cc_aohi(eleuth, lon = "decimalLongitude", lat = "decimalLatitude") # 232 marked records
#cc_dupl(eleuth, lon = "decimalLongitude", lat = "decimalLatitude", species = "species") #21360 marked records - will not use
#cc_cen(eleuth, lon = "decimalLongitude", lat = "decimalLatitude", species = "species") # 933 records
#cc_coun(eleuth, lon = "decimalLongitude", lat = "decimalLatitude", iso3 = "countryCode") ### this function isn't working properly, getting rid of all points
#cc_gbif(eleuth, lon = "decimalLongitude", lat = "decimalLatitude", species = "species", buffer = 1000)## No Records Removed
#cc_inst(eleuth, lon = "decimalLongitude", lat = "decimalLatitude", species = "species", buffer = 100) ## Removed 137 records


eleuth_cleaned_mancuration<- read.csv("eleutherocatylus_invasionhistory_manualcuration.csv")


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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/coqui_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/coqui_VI.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/coqui_hawaii.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')


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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/coqui_california.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')



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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/planirostris_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/planirostris_usa.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')


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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/planirostris_asia.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/planirostris_hawaii.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')



### Johnstonei History #########################################################################################################################################

ggplot() + geom_map( 
  data = world_coordinates, map = world_coordinates, 
  aes(long, lat, map_id = region), 
  color = "grey", fill= "grey")+
  geom_sf() +
  theme_classic()+
  coord_sf(xlim = c(-120, -40), ylim = c(-10, 40), expand = FALSE)+
  geom_point(data = johnstonei_final, aes(decimalLongitude, decimalLatitude, color = Status, fill = Status),  size = 6)+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 75),
        axis.title.y = element_text(face = "bold", size = 75),
        axis.text.x = element_text(face = "bold", size = 50),
        axis.text.y = element_text(face = "bold", size = 50))+
  labs(x="Longitude", y="Latitude")+
  scale_color_manual(values=c("#cd5c5c", "#fcc200","black"))
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/johnstonei_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/johnstonei_antilles.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/johnstonei_samerica.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')

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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/antillensis_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')


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
ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/martinicensis_world.tiff",  width = 25, height = 25, dpi = 300, compression = 'lzw')





## QC for biovars/niche comparison
eleuth_cleaned_climate<- eleuth_cleaned_mancuration %>% 
  full_join(eleuth_cleaned)%>% 
  filter(decimalLatitude != "NA") %>% 
  filter(Status %in% c("Likely established", "Native")) %>% 
  filter(species !="Eleutherodactylus cystignathoides")%>% 
  filter(coordinateUncertaintyInMeters <= 5000 ) %>% 
  filter(year >= 1951) 



#write.csv(eleuth_cleaned_climate, "eleutherodactylus_cleaned_forENM_final_2.csv")



rm(list = ls())





# split points into different years ---------------------------------------

# Changed the name of the file manually to send to collaborators, 
# eleutherodactylus_cleaned_for_ENM_final_2 and eleutherodactylus_cleaned_for _ENM_final_2revised
# are the same files
eleuth_cleaned_climate<- read.csv("eleutherodactylus_cleaned_forENM_final_2revised.csv")

### Split points into different

#1951
points_1951<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1951) %>% filter(decimalLatitude !="NA")
latlong_1951<- points_1951 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1952
points_1952<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1952) %>% filter(decimalLatitude !="NA")
latlong_1952<- points_1952 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1953
points_1953<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1953) %>% filter(decimalLatitude !="NA")
latlong_1953<- points_1953 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1954
points_1954<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1954) %>% filter(decimalLatitude !="NA")
latlong_1954<- points_1954 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1955
points_1955<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1955) %>% filter(decimalLatitude !="NA")
latlong_1955<- points_1955 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1956
points_1956<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1956) %>% filter(decimalLatitude !="NA")
latlong_1956<- points_1956 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1957
points_1957<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1957) %>% filter(decimalLatitude !="NA")
latlong_1957<- points_1957 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1958
points_1958<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1958) %>% filter(decimalLatitude !="NA")
latlong_1958<- points_1958 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1959
points_1959<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1959) %>% filter(decimalLatitude !="NA")
latlong_1959<- points_1959 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1960
points_1960<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1960) %>% filter(decimalLatitude !="NA")
latlong_1960<- points_1960 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1961
points_1961<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1961) %>% filter(decimalLatitude !="NA")
latlong_1961<- points_1961 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1962
points_1962<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1962) %>% filter(decimalLatitude !="NA")
latlong_1962<- points_1962 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1963
points_1963<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1963) %>% filter(decimalLatitude !="NA")
latlong_1963<- points_1963 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1964
points_1964<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1964) %>% filter(decimalLatitude !="NA")
latlong_1964<- points_1964 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1965

points_1965<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1965) %>% filter(decimalLatitude !="NA")
latlong_1965<- points_1965 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1966

points_1966<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1966) %>% filter(decimalLatitude !="NA")
latlong_1966<- points_1966 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1967

points_1967<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1967) %>% filter(decimalLatitude !="NA")
latlong_1967<- points_1967 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1968

points_1968<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1968) %>% filter(decimalLatitude !="NA")
latlong_1968<- points_1968 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1969
points_1969<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1969) %>% filter(decimalLatitude !="NA")
latlong_1969<- points_1969 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1970
points_1970<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1970) %>% filter(decimalLatitude !="NA")
latlong_1970<- points_1970 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1971
points_1971<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1971) %>% filter(decimalLatitude !="NA")
latlong_1971<- points_1971 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1972
points_1972<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1972) %>% filter(decimalLatitude !="NA")
latlong_1972<- points_1972 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1973
points_1973<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1973) %>% filter(decimalLatitude !="NA")
latlong_1973<- points_1973 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1974
points_1974<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1974) %>% filter(decimalLatitude !="NA")
latlong_1974<- points_1974 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1975

points_1975<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1975) %>% filter(decimalLatitude !="NA")
latlong_1975<- points_1975 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1976

points_1976<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1976) %>% filter(decimalLatitude !="NA")
latlong_1976<- points_1976 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1977

points_1977<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1977) %>% filter(decimalLatitude !="NA")
latlong_1977<- points_1977 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1978

points_1978<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1978) %>% filter(decimalLatitude !="NA")
latlong_1978<- points_1978 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1979
points_1979<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1979) %>% filter(decimalLatitude !="NA")
latlong_1979<- points_1979 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)
#1980
points_1980<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1980) %>% filter(decimalLatitude !="NA")
latlong_1980<- points_1980 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1981
points_1981<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1981) %>% filter(decimalLatitude !="NA")
latlong_1981<- points_1981 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1982
points_1982<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1982) %>% filter(decimalLatitude !="NA")
latlong_1982<- points_1982 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1983
points_1983<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1983) %>% filter(decimalLatitude !="NA")
latlong_1983<- points_1983 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1984
points_1984<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1984) %>% filter(decimalLatitude !="NA")
latlong_1984<- points_1984 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1985

points_1985<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1985) %>% filter(decimalLatitude !="NA")
latlong_1985<- points_1985 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1986

points_1986<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1986) %>% filter(decimalLatitude !="NA")
latlong_1986<- points_1986 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1987

points_1987<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1987) %>% filter(decimalLatitude !="NA")
latlong_1987<- points_1987 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1988

points_1988<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1988) %>% filter(decimalLatitude !="NA")
latlong_1988<- points_1988 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1989
points_1989<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1989) %>% filter(decimalLatitude !="NA")
latlong_1989<- points_1989 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1990
points_1990<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1990) %>% filter(decimalLatitude !="NA")
latlong_1990<- points_1990 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1991
points_1991<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1991) %>% filter(decimalLatitude !="NA")
latlong_1991<- points_1991 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1992
points_1992<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1992) %>% filter(decimalLatitude !="NA")
latlong_1992<- points_1992 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1993
points_1993<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1993) %>% filter(decimalLatitude !="NA")
latlong_1993<- points_1993 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1994
points_1994<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1994) %>% filter(decimalLatitude !="NA")
latlong_1994<- points_1994 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1995

points_1995<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1995) %>% filter(decimalLatitude !="NA")
latlong_1995<- points_1995 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1996

points_1996<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1996) %>% filter(decimalLatitude !="NA")
latlong_1996<- points_1996 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1997

points_1997<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1997) %>% filter(decimalLatitude !="NA")
latlong_1997<- points_1997 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1998

points_1998<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1998) %>% filter(decimalLatitude !="NA")
latlong_1998<- points_1998 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#1999
points_1999<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==1999) %>% filter(decimalLatitude !="NA")
latlong_1999<- points_1999 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2000
points_2000<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2000) %>% filter(decimalLatitude !="NA")
latlong_2000<- points_2000 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2001
points_2001<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2001) %>% filter(decimalLatitude !="NA")
latlong_2001<- points_2001 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2002
points_2002<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2002) %>% filter(decimalLatitude !="NA")
latlong_2002<- points_2002 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2003
points_2003<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2003) %>% filter(decimalLatitude !="NA")
latlong_2003<- points_2003 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2004
points_2004<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2004) %>% filter(decimalLatitude !="NA")
latlong_2004<- points_2004 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2005

points_2005<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2005) %>% filter(decimalLatitude !="NA")
latlong_2005<- points_2005 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2006

points_2006<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2006) %>% filter(decimalLatitude !="NA")
latlong_2006<- points_2006 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2007

points_2007<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2007) %>% filter(decimalLatitude !="NA")
latlong_2007<- points_2007 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2008

points_2008<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2008) %>% filter(decimalLatitude !="NA")
latlong_2008<- points_2008 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2009
points_2009<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2009) %>% filter(decimalLatitude !="NA")
latlong_2009<- points_2009 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2010
points_2010<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2010) %>% filter(decimalLatitude !="NA")
latlong_2010<- points_2010 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2011
points_2011<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2011) %>% filter(decimalLatitude !="NA")
latlong_2011<- points_2011 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2012
points_2012<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2012) %>% filter(decimalLatitude !="NA")
latlong_2012<- points_2012 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2013
points_2013<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2013) %>% filter(decimalLatitude !="NA")
latlong_2013<- points_2013 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2014
points_2014<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2014) %>% filter(decimalLatitude !="NA")
latlong_2014<- points_2014 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2015

points_2015<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2015) %>% filter(decimalLatitude !="NA")
latlong_2015<- points_2015 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2016

points_2016<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2016) %>% filter(decimalLatitude !="NA")
latlong_2016<- points_2016 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2017

points_2017<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2017) %>% filter(decimalLatitude !="NA")
latlong_2017<- points_2017 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2018

points_2018<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2018) %>% filter(decimalLatitude !="NA")
latlong_2018<- points_2018 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2019
points_2019<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2019) %>% filter(decimalLatitude !="NA")
latlong_2019<- points_2019 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2020
points_2020<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2020) %>% filter(decimalLatitude !="NA")
latlong_2020<- points_2020 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2021
points_2021<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2021) %>% filter(decimalLatitude !="NA")
latlong_2021<- points_2021 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2022
points_2022<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2022) %>% filter(decimalLatitude !="NA")
latlong_2022<- points_2022 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2023
points_2023<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2023) %>% filter(decimalLatitude !="NA")
latlong_2023<- points_2023 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)

#2024
points_2024<- eleuth_cleaned_climate %>% select(c("gbifID", "species", "decimalLatitude", "decimalLongitude", "countryCode", "year")) %>% filter(year ==2024) %>% filter(decimalLatitude !="NA")
latlong_2024<- points_2024 %>% select(c("decimalLatitude", "decimalLongitude"))%>% 
  relocate(decimalLatitude, .after = decimalLongitude)



# Extract Bioclimate Values for points ------------------------------------



list_1951_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1951/tmin",
                       pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1951_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1951/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1951_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1951/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1951_tmin<- lapply(list_1951_tmin, raster)
rasters_1951_tmax<- lapply(list_1951_tmax, raster)
rasters_1951_prec<- lapply(list_1951_prec, raster)

# Process 1951
a<- matrix(0,12,dim(latlong_1951)[1])
b<- matrix(0,12,dim(latlong_1951)[1])
c<- matrix(0,12,dim(latlong_1951)[1])
## Tmax
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1951_tmin[[i]], latlong_1951)
    b[i,]<- extract(rasters_1951_tmax[[i]], latlong_1951)
    c[i,]<- extract(rasters_1951_prec[[i]], latlong_1951)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
               "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
               "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1951<-cbind(points_1951,a,b,c)


list_1952_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1952/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1952_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1952/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1952_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1952/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1952_tmin<- lapply(list_1952_tmin, raster)
rasters_1952_tmax<- lapply(list_1952_tmax, raster)
rasters_1952_prec<- lapply(list_1952_prec, raster)

# Process 1952
a<- matrix(0,12,dim(latlong_1952)[1])
b<- matrix(0,12,dim(latlong_1952)[1])
c<- matrix(0,12,dim(latlong_1952)[1])
## Tmax
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1952_tmin[[i]], latlong_1952)
    b[i,]<- extract(rasters_1952_tmax[[i]], latlong_1952)
    c[i,]<- extract(rasters_1952_prec[[i]], latlong_1952)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1952<-cbind(points_1952,a,b,c)

list_1953_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1953/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1953_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1953/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1953_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1953/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1953_tmin<- lapply(list_1953_tmin, raster)
rasters_1953_tmax<- lapply(list_1953_tmax, raster)
rasters_1953_prec<- lapply(list_1953_prec, raster)

# Process 1953
a<- matrix(0,12,dim(latlong_1953)[1])
b<- matrix(0,12,dim(latlong_1953)[1])
c<- matrix(0,12,dim(latlong_1953)[1])
## Tmax
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1953_tmin[[i]], latlong_1953)
    b[i,]<- extract(rasters_1953_tmax[[i]], latlong_1953)
    c[i,]<- extract(rasters_1953_prec[[i]], latlong_1953)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1953<-cbind(points_1953,a,b,c)
list_1954_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1954/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1954_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1954/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1954_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1954/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1954_tmin<- lapply(list_1954_tmin, raster)
rasters_1954_tmax<- lapply(list_1954_tmax, raster)
rasters_1954_prec<- lapply(list_1954_prec, raster)

# Process 1954
a<- matrix(0,12,dim(latlong_1954)[1])
b<- matrix(0,12,dim(latlong_1954)[1])
c<- matrix(0,12,dim(latlong_1954)[1])
## Tmax
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1954_tmin[[i]], latlong_1954)
    b[i,]<- extract(rasters_1954_tmax[[i]], latlong_1954)
    c[i,]<- extract(rasters_1954_prec[[i]], latlong_1954)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1954<-cbind(points_1954,a,b,c)

list_1955_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1955/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1955_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1955/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1955_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1955/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1955_tmin<- lapply(list_1955_tmin, raster)
rasters_1955_tmax<- lapply(list_1955_tmax, raster)
rasters_1955_prec<- lapply(list_1955_prec, raster)

# Process 1955
a<- matrix(0,12,dim(latlong_1955)[1])
b<- matrix(0,12,dim(latlong_1955)[1])
c<- matrix(0,12,dim(latlong_1955)[1])
## Tmax
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1955_tmin[[i]], latlong_1955)
    b[i,]<- extract(rasters_1955_tmax[[i]], latlong_1955)
    c[i,]<- extract(rasters_1955_prec[[i]], latlong_1955)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1955<-cbind(points_1955,a,b,c)


list_1956_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1956/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1956_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1956/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1956_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1956/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1956_tmin<- lapply(list_1956_tmin, raster)
rasters_1956_tmax<- lapply(list_1956_tmax, raster)
rasters_1956_prec<- lapply(list_1956_prec, raster)

# Process 1956
a<- matrix(0,12,dim(latlong_1956)[1])
b<- matrix(0,12,dim(latlong_1956)[1])
c<- matrix(0,12,dim(latlong_1956)[1])
## Tmax
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1956_tmin[[i]], latlong_1956)
    b[i,]<- extract(rasters_1956_tmax[[i]], latlong_1956)
    c[i,]<- extract(rasters_1956_prec[[i]], latlong_1956)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1956<-cbind(points_1956,a,b,c)

list_1957_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1957/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1957_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1957/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1957_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1957/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1957_tmin<- lapply(list_1957_tmin, raster)
rasters_1957_tmax<- lapply(list_1957_tmax, raster)
rasters_1957_prec<- lapply(list_1957_prec, raster)

# Process 1957
a<- matrix(0,12,dim(latlong_1957)[1])
b<- matrix(0,12,dim(latlong_1957)[1])
c<- matrix(0,12,dim(latlong_1957)[1])
## Tmax
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1957_tmin[[i]], latlong_1957)
    b[i,]<- extract(rasters_1957_tmax[[i]], latlong_1957)
    c[i,]<- extract(rasters_1957_prec[[i]], latlong_1957)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1957<-cbind(points_1957,a,b,c)


list_1958_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1958/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1958_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1958/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1958_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1958/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1958_tmin<- lapply(list_1958_tmin, raster)
rasters_1958_tmax<- lapply(list_1958_tmax, raster)
rasters_1958_prec<- lapply(list_1958_prec, raster)

# Process 1958
a<- matrix(0,12,dim(latlong_1958)[1])
b<- matrix(0,12,dim(latlong_1958)[1])
c<- matrix(0,12,dim(latlong_1958)[1])
## Tmax
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1958_tmin[[i]], latlong_1958)
    b[i,]<- extract(rasters_1958_tmax[[i]], latlong_1958)
    c[i,]<- extract(rasters_1958_prec[[i]], latlong_1958)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1958<-cbind(points_1958,a,b,c)


list_1959_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1959/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1959_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1959/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1959_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1959/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1959_tmin<- lapply(list_1959_tmin, raster)
rasters_1959_tmax<- lapply(list_1959_tmax, raster)
rasters_1959_prec<- lapply(list_1959_prec, raster)

# Process 1959
a<- matrix(0,12,dim(latlong_1959)[1])
b<- matrix(0,12,dim(latlong_1959)[1])
c<- matrix(0,12,dim(latlong_1959)[1])
## Tmax
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1959_tmin[[i]], latlong_1959)
    b[i,]<- extract(rasters_1959_tmax[[i]], latlong_1959)
    c[i,]<- extract(rasters_1959_prec[[i]], latlong_1959)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1959<-cbind(points_1959,a,b,c)

list_1960_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1960/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1960_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1960/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1960_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1960/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1960_tmin<- lapply(list_1960_tmin, raster)
rasters_1960_tmax<- lapply(list_1960_tmax, raster)
rasters_1960_prec<- lapply(list_1960_prec, raster)

# Process 1960
a<- matrix(0,12,dim(latlong_1960)[1])
b<- matrix(0,12,dim(latlong_1960)[1])
c<- matrix(0,12,dim(latlong_1960)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1960_tmin[[i]], latlong_1960)
    b[i,]<- extract(rasters_1960_tmax[[i]], latlong_1960)
    c[i,]<- extract(rasters_1960_prec[[i]], latlong_1960)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1960<-cbind(points_1960,a,b,c)


list_1961_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1961/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1961_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1961/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1961_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1961/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1961_tmin<- lapply(list_1961_tmin, raster)
rasters_1961_tmax<- lapply(list_1961_tmax, raster)
rasters_1961_prec<- lapply(list_1961_prec, raster)

# Process 1961
a<- matrix(0,12,dim(latlong_1961)[1])
b<- matrix(0,12,dim(latlong_1961)[1])
c<- matrix(0,12,dim(latlong_1961)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1961_tmin[[i]], latlong_1961)
    b[i,]<- extract(rasters_1961_tmax[[i]], latlong_1961)
    c[i,]<- extract(rasters_1961_prec[[i]], latlong_1961)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1961<-cbind(points_1961,a,b,c)

list_1962_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1962/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1962_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1962/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1962_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1962/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1962_tmin<- lapply(list_1962_tmin, raster)
rasters_1962_tmax<- lapply(list_1962_tmax, raster)
rasters_1962_prec<- lapply(list_1962_prec, raster)

# Process 1962
a<- matrix(0,12,dim(latlong_1962)[1])
b<- matrix(0,12,dim(latlong_1962)[1])
c<- matrix(0,12,dim(latlong_1962)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1962_tmin[[i]], latlong_1962)
    b[i,]<- extract(rasters_1962_tmax[[i]], latlong_1962)
    c[i,]<- extract(rasters_1962_prec[[i]], latlong_1962)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1962<-cbind(points_1962,a,b,c)


list_1963_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1963/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1963_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1963/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1963_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1963/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1963_tmin<- lapply(list_1963_tmin, raster)
rasters_1963_tmax<- lapply(list_1963_tmax, raster)
rasters_1963_prec<- lapply(list_1963_prec, raster)

# Process 1963
a<- matrix(0,12,dim(latlong_1963)[1])
b<- matrix(0,12,dim(latlong_1963)[1])
c<- matrix(0,12,dim(latlong_1963)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1963_tmin[[i]], latlong_1963)
    b[i,]<- extract(rasters_1963_tmax[[i]], latlong_1963)
    c[i,]<- extract(rasters_1963_prec[[i]], latlong_1963)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1963<-cbind(points_1963,a,b,c)


list_1964_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1964/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1964_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1964/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1964_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1964/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1964_tmin<- lapply(list_1964_tmin, raster)
rasters_1964_tmax<- lapply(list_1964_tmax, raster)
rasters_1964_prec<- lapply(list_1964_prec, raster)

# Process 1964
a<- matrix(0,12,dim(latlong_1964)[1])
b<- matrix(0,12,dim(latlong_1964)[1])
c<- matrix(0,12,dim(latlong_1964)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1964_tmin[[i]], latlong_1964)
    b[i,]<- extract(rasters_1964_tmax[[i]], latlong_1964)
    c[i,]<- extract(rasters_1964_prec[[i]], latlong_1964)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1964<-cbind(points_1964,a,b,c)

list_1965_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1965/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1965_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1965/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1965_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1965/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1965_tmin<- lapply(list_1965_tmin, raster)
rasters_1965_tmax<- lapply(list_1965_tmax, raster)
rasters_1965_prec<- lapply(list_1965_prec, raster)

# Process 1965
a<- matrix(0,12,dim(latlong_1965)[1])
b<- matrix(0,12,dim(latlong_1965)[1])
c<- matrix(0,12,dim(latlong_1965)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1965_tmin[[i]], latlong_1965)
    b[i,]<- extract(rasters_1965_tmax[[i]], latlong_1965)
    c[i,]<- extract(rasters_1965_prec[[i]], latlong_1965)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1965<-cbind(points_1965,a,b,c)

list_1966_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1966/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1966_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1966/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1966_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1966/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1966_tmin<- lapply(list_1966_tmin, raster)
rasters_1966_tmax<- lapply(list_1966_tmax, raster)
rasters_1966_prec<- lapply(list_1966_prec, raster)

# Process 1966
a<- matrix(0,12,dim(latlong_1966)[1])
b<- matrix(0,12,dim(latlong_1966)[1])
c<- matrix(0,12,dim(latlong_1966)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1966_tmin[[i]], latlong_1966)
    b[i,]<- extract(rasters_1966_tmax[[i]], latlong_1966)
    c[i,]<- extract(rasters_1966_prec[[i]], latlong_1966)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1966<-cbind(points_1966,a,b,c)

list_1967_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1967/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1967_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1967/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1967_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1967/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1967_tmin<- lapply(list_1967_tmin, raster)
rasters_1967_tmax<- lapply(list_1967_tmax, raster)
rasters_1967_prec<- lapply(list_1967_prec, raster)

# Process 1967
a<- matrix(0,12,dim(latlong_1967)[1])
b<- matrix(0,12,dim(latlong_1967)[1])
c<- matrix(0,12,dim(latlong_1967)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1967_tmin[[i]], latlong_1967)
    b[i,]<- extract(rasters_1967_tmax[[i]], latlong_1967)
    c[i,]<- extract(rasters_1967_prec[[i]], latlong_1967)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1967<-cbind(points_1967,a,b,c)

list_1968_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1968/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1968_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1968/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1968_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1968/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1968_tmin<- lapply(list_1968_tmin, raster)
rasters_1968_tmax<- lapply(list_1968_tmax, raster)
rasters_1968_prec<- lapply(list_1968_prec, raster)

# Process 1968
a<- matrix(0,12,dim(latlong_1968)[1])
b<- matrix(0,12,dim(latlong_1968)[1])
c<- matrix(0,12,dim(latlong_1968)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1968_tmin[[i]], latlong_1968)
    b[i,]<- extract(rasters_1968_tmax[[i]], latlong_1968)
    c[i,]<- extract(rasters_1968_prec[[i]], latlong_1968)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1968<-cbind(points_1968,a,b,c)

list_1969_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1969/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1969_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1969/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1969_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1969/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1969_tmin<- lapply(list_1969_tmin, raster)
rasters_1969_tmax<- lapply(list_1969_tmax, raster)
rasters_1969_prec<- lapply(list_1969_prec, raster)

# Process 1969
a<- matrix(0,12,dim(latlong_1969)[1])
b<- matrix(0,12,dim(latlong_1969)[1])
c<- matrix(0,12,dim(latlong_1969)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1969_tmin[[i]], latlong_1969)
    b[i,]<- extract(rasters_1969_tmax[[i]], latlong_1969)
    c[i,]<- extract(rasters_1969_prec[[i]], latlong_1969)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1969<-cbind(points_1969,a,b,c)

list_1970_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1970/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1970_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1970/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1970_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1970/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1970_tmin<- lapply(list_1970_tmin, raster)
rasters_1970_tmax<- lapply(list_1970_tmax, raster)
rasters_1970_prec<- lapply(list_1970_prec, raster)

# Process 1970
a<- matrix(0,12,dim(latlong_1970)[1])
b<- matrix(0,12,dim(latlong_1970)[1])
c<- matrix(0,12,dim(latlong_1970)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1970_tmin[[i]], latlong_1970)
    b[i,]<- extract(rasters_1970_tmax[[i]], latlong_1970)
    c[i,]<- extract(rasters_1970_prec[[i]], latlong_1970)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1970<-cbind(points_1970,a,b,c)


list_1971_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1971/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1971_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1971/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1971_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1971/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1971_tmin<- lapply(list_1971_tmin, raster)
rasters_1971_tmax<- lapply(list_1971_tmax, raster)
rasters_1971_prec<- lapply(list_1971_prec, raster)

# Process 1971
a<- matrix(0,12,dim(latlong_1971)[1])
b<- matrix(0,12,dim(latlong_1971)[1])
c<- matrix(0,12,dim(latlong_1971)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1971_tmin[[i]], latlong_1971)
    b[i,]<- extract(rasters_1971_tmax[[i]], latlong_1971)
    c[i,]<- extract(rasters_1971_prec[[i]], latlong_1971)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1971<-cbind(points_1971,a,b,c)




list_1972_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1972/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1972_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1972/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1972_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1972/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1972_tmin<- lapply(list_1972_tmin, raster)
rasters_1972_tmax<- lapply(list_1972_tmax, raster)
rasters_1972_prec<- lapply(list_1972_prec, raster)

# Process 1972
a<- matrix(0,12,dim(latlong_1972)[1])
b<- matrix(0,12,dim(latlong_1972)[1])
c<- matrix(0,12,dim(latlong_1972)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1972_tmin[[i]], latlong_1972)
    b[i,]<- extract(rasters_1972_tmax[[i]], latlong_1972)
    c[i,]<- extract(rasters_1972_prec[[i]], latlong_1972)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1972<-cbind(points_1972,a,b,c)
list_1973_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1973/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1973_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1973/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1973_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1973/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1973_tmin<- lapply(list_1973_tmin, raster)
rasters_1973_tmax<- lapply(list_1973_tmax, raster)
rasters_1973_prec<- lapply(list_1973_prec, raster)

# Process 1973
a<- matrix(0,12,dim(latlong_1973)[1])
b<- matrix(0,12,dim(latlong_1973)[1])
c<- matrix(0,12,dim(latlong_1973)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1973_tmin[[i]], latlong_1973)
    b[i,]<- extract(rasters_1973_tmax[[i]], latlong_1973)
    c[i,]<- extract(rasters_1973_prec[[i]], latlong_1973)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1973<-cbind(points_1973,a,b,c)
list_1974_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1974/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1974_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1974/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1974_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1974/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1974_tmin<- lapply(list_1974_tmin, raster)
rasters_1974_tmax<- lapply(list_1974_tmax, raster)
rasters_1974_prec<- lapply(list_1974_prec, raster)

# Process 1974
a<- matrix(0,12,dim(latlong_1974)[1])
b<- matrix(0,12,dim(latlong_1974)[1])
c<- matrix(0,12,dim(latlong_1974)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1974_tmin[[i]], latlong_1974)
    b[i,]<- extract(rasters_1974_tmax[[i]], latlong_1974)
    c[i,]<- extract(rasters_1974_prec[[i]], latlong_1974)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1974<-cbind(points_1974,a,b,c)

list_1975_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1975/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1975_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1975/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1975_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1975/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1975_tmin<- lapply(list_1975_tmin, raster)
rasters_1975_tmax<- lapply(list_1975_tmax, raster)
rasters_1975_prec<- lapply(list_1975_prec, raster)

# Process 1975
a<- matrix(0,12,dim(latlong_1975)[1])
b<- matrix(0,12,dim(latlong_1975)[1])
c<- matrix(0,12,dim(latlong_1975)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1975_tmin[[i]], latlong_1975)
    b[i,]<- extract(rasters_1975_tmax[[i]], latlong_1975)
    c[i,]<- extract(rasters_1975_prec[[i]], latlong_1975)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1975<-cbind(points_1975,a,b,c)
list_1976_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1976/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1976_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1976/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1976_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1976/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1976_tmin<- lapply(list_1976_tmin, raster)
rasters_1976_tmax<- lapply(list_1976_tmax, raster)
rasters_1976_prec<- lapply(list_1976_prec, raster)

# Process 1976
a<- matrix(0,12,dim(latlong_1976)[1])
b<- matrix(0,12,dim(latlong_1976)[1])
c<- matrix(0,12,dim(latlong_1976)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1976_tmin[[i]], latlong_1976)
    b[i,]<- extract(rasters_1976_tmax[[i]], latlong_1976)
    c[i,]<- extract(rasters_1976_prec[[i]], latlong_1976)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1976<-cbind(points_1976,a,b,c)

list_1977_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1977/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1977_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1977/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1977_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1977/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1977_tmin<- lapply(list_1977_tmin, raster)
rasters_1977_tmax<- lapply(list_1977_tmax, raster)
rasters_1977_prec<- lapply(list_1977_prec, raster)

# Process 1977
a<- matrix(0,12,dim(latlong_1977)[1])
b<- matrix(0,12,dim(latlong_1977)[1])
c<- matrix(0,12,dim(latlong_1977)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1977_tmin[[i]], latlong_1977)
    b[i,]<- extract(rasters_1977_tmax[[i]], latlong_1977)
    c[i,]<- extract(rasters_1977_prec[[i]], latlong_1977)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1977<-cbind(points_1977,a,b,c)
list_1978_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1978/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1978_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1978/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1978_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1978/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1978_tmin<- lapply(list_1978_tmin, raster)
rasters_1978_tmax<- lapply(list_1978_tmax, raster)
rasters_1978_prec<- lapply(list_1978_prec, raster)

# Process 1978
a<- matrix(0,12,dim(latlong_1978)[1])
b<- matrix(0,12,dim(latlong_1978)[1])
c<- matrix(0,12,dim(latlong_1978)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1978_tmin[[i]], latlong_1978)
    b[i,]<- extract(rasters_1978_tmax[[i]], latlong_1978)
    c[i,]<- extract(rasters_1978_prec[[i]], latlong_1978)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1978<-cbind(points_1978,a,b,c)
list_1979_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1979/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1979_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1979/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1979_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1979/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1979_tmin<- lapply(list_1979_tmin, raster)
rasters_1979_tmax<- lapply(list_1979_tmax, raster)
rasters_1979_prec<- lapply(list_1979_prec, raster)

# Process 1979
a<- matrix(0,12,dim(latlong_1979)[1])
b<- matrix(0,12,dim(latlong_1979)[1])
c<- matrix(0,12,dim(latlong_1979)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1979_tmin[[i]], latlong_1979)
    b[i,]<- extract(rasters_1979_tmax[[i]], latlong_1979)
    c[i,]<- extract(rasters_1979_prec[[i]], latlong_1979)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1979<-cbind(points_1979,a,b,c)

list_1980_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1980/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1980_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1980/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1980_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1980/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1980_tmin<- lapply(list_1980_tmin, raster)
rasters_1980_tmax<- lapply(list_1980_tmax, raster)
rasters_1980_prec<- lapply(list_1980_prec, raster)

# Process 1980
a<- matrix(0,12,dim(latlong_1980)[1])
b<- matrix(0,12,dim(latlong_1980)[1])
c<- matrix(0,12,dim(latlong_1980)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1980_tmin[[i]], latlong_1980)
    b[i,]<- extract(rasters_1980_tmax[[i]], latlong_1980)
    c[i,]<- extract(rasters_1980_prec[[i]], latlong_1980)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1980<-cbind(points_1980,a,b,c)


list_1981_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1981/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1981_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1981/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1981_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1981/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1981_tmin<- lapply(list_1981_tmin, raster)
rasters_1981_tmax<- lapply(list_1981_tmax, raster)
rasters_1981_prec<- lapply(list_1981_prec, raster)

# Process 1981
a<- matrix(0,12,dim(latlong_1981)[1])
b<- matrix(0,12,dim(latlong_1981)[1])
c<- matrix(0,12,dim(latlong_1981)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1981_tmin[[i]], latlong_1981)
    b[i,]<- extract(rasters_1981_tmax[[i]], latlong_1981)
    c[i,]<- extract(rasters_1981_prec[[i]], latlong_1981)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1981<-cbind(points_1981,a,b,c)


list_1982_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1982/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1982_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1982/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1982_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1982/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1982_tmin<- lapply(list_1982_tmin, raster)
rasters_1982_tmax<- lapply(list_1982_tmax, raster)
rasters_1982_prec<- lapply(list_1982_prec, raster)

# Process 1982
a<- matrix(0,12,dim(latlong_1982)[1])
b<- matrix(0,12,dim(latlong_1982)[1])
c<- matrix(0,12,dim(latlong_1982)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1982_tmin[[i]], latlong_1982)
    b[i,]<- extract(rasters_1982_tmax[[i]], latlong_1982)
    c[i,]<- extract(rasters_1982_prec[[i]], latlong_1982)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1982<-cbind(points_1982,a,b,c)
list_1983_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1983/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1983_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1983/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1983_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1983/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1983_tmin<- lapply(list_1983_tmin, raster)
rasters_1983_tmax<- lapply(list_1983_tmax, raster)
rasters_1983_prec<- lapply(list_1983_prec, raster)

# Process 1983
a<- matrix(0,12,dim(latlong_1983)[1])
b<- matrix(0,12,dim(latlong_1983)[1])
c<- matrix(0,12,dim(latlong_1983)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1983_tmin[[i]], latlong_1983)
    b[i,]<- extract(rasters_1983_tmax[[i]], latlong_1983)
    c[i,]<- extract(rasters_1983_prec[[i]], latlong_1983)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1983<-cbind(points_1983,a,b,c)
list_1984_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1984/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1984_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1984/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1984_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1984/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1984_tmin<- lapply(list_1984_tmin, raster)
rasters_1984_tmax<- lapply(list_1984_tmax, raster)
rasters_1984_prec<- lapply(list_1984_prec, raster)

# Process 1984
a<- matrix(0,12,dim(latlong_1984)[1])
b<- matrix(0,12,dim(latlong_1984)[1])
c<- matrix(0,12,dim(latlong_1984)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1984_tmin[[i]], latlong_1984)
    b[i,]<- extract(rasters_1984_tmax[[i]], latlong_1984)
    c[i,]<- extract(rasters_1984_prec[[i]], latlong_1984)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1984<-cbind(points_1984,a,b,c)

list_1985_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1985/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1985_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1985/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1985_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1985/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1985_tmin<- lapply(list_1985_tmin, raster)
rasters_1985_tmax<- lapply(list_1985_tmax, raster)
rasters_1985_prec<- lapply(list_1985_prec, raster)

# Process 1985
a<- matrix(0,12,dim(latlong_1985)[1])
b<- matrix(0,12,dim(latlong_1985)[1])
c<- matrix(0,12,dim(latlong_1985)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1985_tmin[[i]], latlong_1985)
    b[i,]<- extract(rasters_1985_tmax[[i]], latlong_1985)
    c[i,]<- extract(rasters_1985_prec[[i]], latlong_1985)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1985<-cbind(points_1985,a,b,c)
list_1986_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1986/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1986_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1986/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1986_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1986/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1986_tmin<- lapply(list_1986_tmin, raster)
rasters_1986_tmax<- lapply(list_1986_tmax, raster)
rasters_1986_prec<- lapply(list_1986_prec, raster)

# Process 1986
a<- matrix(0,12,dim(latlong_1986)[1])
b<- matrix(0,12,dim(latlong_1986)[1])
c<- matrix(0,12,dim(latlong_1986)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1986_tmin[[i]], latlong_1986)
    b[i,]<- extract(rasters_1986_tmax[[i]], latlong_1986)
    c[i,]<- extract(rasters_1986_prec[[i]], latlong_1986)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1986<-cbind(points_1986,a,b,c)

list_1987_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1987/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1987_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1987/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1987_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1987/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1987_tmin<- lapply(list_1987_tmin, raster)
rasters_1987_tmax<- lapply(list_1987_tmax, raster)
rasters_1987_prec<- lapply(list_1987_prec, raster)

# Process 1987
a<- matrix(0,12,dim(latlong_1987)[1])
b<- matrix(0,12,dim(latlong_1987)[1])
c<- matrix(0,12,dim(latlong_1987)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1987_tmin[[i]], latlong_1987)
    b[i,]<- extract(rasters_1987_tmax[[i]], latlong_1987)
    c[i,]<- extract(rasters_1987_prec[[i]], latlong_1987)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1987<-cbind(points_1987,a,b,c)
list_1988_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1988/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1988_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1988/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1988_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1988/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1988_tmin<- lapply(list_1988_tmin, raster)
rasters_1988_tmax<- lapply(list_1988_tmax, raster)
rasters_1988_prec<- lapply(list_1988_prec, raster)

# Process 1988
a<- matrix(0,12,dim(latlong_1988)[1])
b<- matrix(0,12,dim(latlong_1988)[1])
c<- matrix(0,12,dim(latlong_1988)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1988_tmin[[i]], latlong_1988)
    b[i,]<- extract(rasters_1988_tmax[[i]], latlong_1988)
    c[i,]<- extract(rasters_1988_prec[[i]], latlong_1988)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1988<-cbind(points_1988,a,b,c)
list_1989_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1989/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1989_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1989/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1989_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1989/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1989_tmin<- lapply(list_1989_tmin, raster)
rasters_1989_tmax<- lapply(list_1989_tmax, raster)
rasters_1989_prec<- lapply(list_1989_prec, raster)

# Process 1989
a<- matrix(0,12,dim(latlong_1989)[1])
b<- matrix(0,12,dim(latlong_1989)[1])
c<- matrix(0,12,dim(latlong_1989)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1989_tmin[[i]], latlong_1989)
    b[i,]<- extract(rasters_1989_tmax[[i]], latlong_1989)
    c[i,]<- extract(rasters_1989_prec[[i]], latlong_1989)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1989<-cbind(points_1989,a,b,c)

list_1990_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1990/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1990_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1990/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1990_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1990/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1990_tmin<- lapply(list_1990_tmin, raster)
rasters_1990_tmax<- lapply(list_1990_tmax, raster)
rasters_1990_prec<- lapply(list_1990_prec, raster)

# Process 1990
a<- matrix(0,12,dim(latlong_1990)[1])
b<- matrix(0,12,dim(latlong_1990)[1])
c<- matrix(0,12,dim(latlong_1990)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1990_tmin[[i]], latlong_1990)
    b[i,]<- extract(rasters_1990_tmax[[i]], latlong_1990)
    c[i,]<- extract(rasters_1990_prec[[i]], latlong_1990)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1990<-cbind(points_1990,a,b,c)

list_1991_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1991/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1991_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1991/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1991_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1991/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1991_tmin<- lapply(list_1991_tmin, raster)
rasters_1991_tmax<- lapply(list_1991_tmax, raster)
rasters_1991_prec<- lapply(list_1991_prec, raster)

# Process 1991
a<- matrix(0,12,dim(latlong_1991)[1])
b<- matrix(0,12,dim(latlong_1991)[1])
c<- matrix(0,12,dim(latlong_1991)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1991_tmin[[i]], latlong_1991)
    b[i,]<- extract(rasters_1991_tmax[[i]], latlong_1991)
    c[i,]<- extract(rasters_1991_prec[[i]], latlong_1991)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1991<-cbind(points_1991,a,b,c)


list_1992_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1992/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1992_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1992/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1992_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1992/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1992_tmin<- lapply(list_1992_tmin, raster)
rasters_1992_tmax<- lapply(list_1992_tmax, raster)
rasters_1992_prec<- lapply(list_1992_prec, raster)

# Process 1992
a<- matrix(0,12,dim(latlong_1992)[1])
b<- matrix(0,12,dim(latlong_1992)[1])
c<- matrix(0,12,dim(latlong_1992)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1992_tmin[[i]], latlong_1992)
    b[i,]<- extract(rasters_1992_tmax[[i]], latlong_1992)
    c[i,]<- extract(rasters_1992_prec[[i]], latlong_1992)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1992<-cbind(points_1992,a,b,c)
list_1993_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1993/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1993_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1993/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1993_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1993/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1993_tmin<- lapply(list_1993_tmin, raster)
rasters_1993_tmax<- lapply(list_1993_tmax, raster)
rasters_1993_prec<- lapply(list_1993_prec, raster)

# Process 1993
a<- matrix(0,12,dim(latlong_1993)[1])
b<- matrix(0,12,dim(latlong_1993)[1])
c<- matrix(0,12,dim(latlong_1993)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1993_tmin[[i]], latlong_1993)
    b[i,]<- extract(rasters_1993_tmax[[i]], latlong_1993)
    c[i,]<- extract(rasters_1993_prec[[i]], latlong_1993)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1993<-cbind(points_1993,a,b,c)
list_1994_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1994/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1994_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1994/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1994_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1994/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1994_tmin<- lapply(list_1994_tmin, raster)
rasters_1994_tmax<- lapply(list_1994_tmax, raster)
rasters_1994_prec<- lapply(list_1994_prec, raster)

# Process 1994
a<- matrix(0,12,dim(latlong_1994)[1])
b<- matrix(0,12,dim(latlong_1994)[1])
c<- matrix(0,12,dim(latlong_1994)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1994_tmin[[i]], latlong_1994)
    b[i,]<- extract(rasters_1994_tmax[[i]], latlong_1994)
    c[i,]<- extract(rasters_1994_prec[[i]], latlong_1994)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1994<-cbind(points_1994,a,b,c)

list_1995_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1995/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1995_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1995/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1995_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1995/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1995_tmin<- lapply(list_1995_tmin, raster)
rasters_1995_tmax<- lapply(list_1995_tmax, raster)
rasters_1995_prec<- lapply(list_1995_prec, raster)

# Process 1995
a<- matrix(0,12,dim(latlong_1995)[1])
b<- matrix(0,12,dim(latlong_1995)[1])
c<- matrix(0,12,dim(latlong_1995)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1995_tmin[[i]], latlong_1995)
    b[i,]<- extract(rasters_1995_tmax[[i]], latlong_1995)
    c[i,]<- extract(rasters_1995_prec[[i]], latlong_1995)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1995<-cbind(points_1995,a,b,c)
list_1996_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1996/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1996_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1996/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1996_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1996/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1996_tmin<- lapply(list_1996_tmin, raster)
rasters_1996_tmax<- lapply(list_1996_tmax, raster)
rasters_1996_prec<- lapply(list_1996_prec, raster)

# Process 1996
a<- matrix(0,12,dim(latlong_1996)[1])
b<- matrix(0,12,dim(latlong_1996)[1])
c<- matrix(0,12,dim(latlong_1996)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1996_tmin[[i]], latlong_1996)
    b[i,]<- extract(rasters_1996_tmax[[i]], latlong_1996)
    c[i,]<- extract(rasters_1996_prec[[i]], latlong_1996)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1996<-cbind(points_1996,a,b,c)

list_1997_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1997/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1997_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1997/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1997_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1997/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1997_tmin<- lapply(list_1997_tmin, raster)
rasters_1997_tmax<- lapply(list_1997_tmax, raster)
rasters_1997_prec<- lapply(list_1997_prec, raster)

# Process 1997
a<- matrix(0,12,dim(latlong_1997)[1])
b<- matrix(0,12,dim(latlong_1997)[1])
c<- matrix(0,12,dim(latlong_1997)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1997_tmin[[i]], latlong_1997)
    b[i,]<- extract(rasters_1997_tmax[[i]], latlong_1997)
    c[i,]<- extract(rasters_1997_prec[[i]], latlong_1997)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1997<-cbind(points_1997,a,b,c)
list_1998_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1998/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1998_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1998/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1998_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1998/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1998_tmin<- lapply(list_1998_tmin, raster)
rasters_1998_tmax<- lapply(list_1998_tmax, raster)
rasters_1998_prec<- lapply(list_1998_prec, raster)

# Process 1998
a<- matrix(0,12,dim(latlong_1998)[1])
b<- matrix(0,12,dim(latlong_1998)[1])
c<- matrix(0,12,dim(latlong_1998)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1998_tmin[[i]], latlong_1998)
    b[i,]<- extract(rasters_1998_tmax[[i]], latlong_1998)
    c[i,]<- extract(rasters_1998_prec[[i]], latlong_1998)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1998<-cbind(points_1998,a,b,c)
list_1999_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1999/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1999_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1999/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_1999_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/1999/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_1999_tmin<- lapply(list_1999_tmin, raster)
rasters_1999_tmax<- lapply(list_1999_tmax, raster)
rasters_1999_prec<- lapply(list_1999_prec, raster)

# Process 1999
a<- matrix(0,12,dim(latlong_1999)[1])
b<- matrix(0,12,dim(latlong_1999)[1])
c<- matrix(0,12,dim(latlong_1999)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_1999_tmin[[i]], latlong_1999)
    b[i,]<- extract(rasters_1999_tmax[[i]], latlong_1999)
    c[i,]<- extract(rasters_1999_prec[[i]], latlong_1999)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_1999<-cbind(points_1999,a,b,c)

list_2000_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2000/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2000_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2000/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2000_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2000/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2000_tmin<- lapply(list_2000_tmin, raster)
rasters_2000_tmax<- lapply(list_2000_tmax, raster)
rasters_2000_prec<- lapply(list_2000_prec, raster)

# Process 2000
a<- matrix(0,12,dim(latlong_2000)[1])
b<- matrix(0,12,dim(latlong_2000)[1])
c<- matrix(0,12,dim(latlong_2000)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2000_tmin[[i]], latlong_2000)
    b[i,]<- extract(rasters_2000_tmax[[i]], latlong_2000)
    c[i,]<- extract(rasters_2000_prec[[i]], latlong_2000)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2000<-cbind(points_2000,a,b,c)
list_2001_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2001/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2001_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2001/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2001_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2001/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2001_tmin<- lapply(list_2001_tmin, raster)
rasters_2001_tmax<- lapply(list_2001_tmax, raster)
rasters_2001_prec<- lapply(list_2001_prec, raster)

# Process 2001
a<- matrix(0,12,dim(latlong_2001)[1])
b<- matrix(0,12,dim(latlong_2001)[1])
c<- matrix(0,12,dim(latlong_2001)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2001_tmin[[i]], latlong_2001)
    b[i,]<- extract(rasters_2001_tmax[[i]], latlong_2001)
    c[i,]<- extract(rasters_2001_prec[[i]], latlong_2001)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2001<-cbind(points_2001,a,b,c)


list_2002_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2002/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2002_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2002/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2002_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2002/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2002_tmin<- lapply(list_2002_tmin, raster)
rasters_2002_tmax<- lapply(list_2002_tmax, raster)
rasters_2002_prec<- lapply(list_2002_prec, raster)

# Process 2002
a<- matrix(0,12,dim(latlong_2002)[1])
b<- matrix(0,12,dim(latlong_2002)[1])
c<- matrix(0,12,dim(latlong_2002)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2002_tmin[[i]], latlong_2002)
    b[i,]<- extract(rasters_2002_tmax[[i]], latlong_2002)
    c[i,]<- extract(rasters_2002_prec[[i]], latlong_2002)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2002<-cbind(points_2002,a,b,c)
list_2003_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2003/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2003_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2003/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2003_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2003/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2003_tmin<- lapply(list_2003_tmin, raster)
rasters_2003_tmax<- lapply(list_2003_tmax, raster)
rasters_2003_prec<- lapply(list_2003_prec, raster)

# Process 2003
a<- matrix(0,12,dim(latlong_2003)[1])
b<- matrix(0,12,dim(latlong_2003)[1])
c<- matrix(0,12,dim(latlong_2003)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2003_tmin[[i]], latlong_2003)
    b[i,]<- extract(rasters_2003_tmax[[i]], latlong_2003)
    c[i,]<- extract(rasters_2003_prec[[i]], latlong_2003)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2003<-cbind(points_2003,a,b,c)
list_2004_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2004/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2004_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2004/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2004_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2004/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2004_tmin<- lapply(list_2004_tmin, raster)
rasters_2004_tmax<- lapply(list_2004_tmax, raster)
rasters_2004_prec<- lapply(list_2004_prec, raster)

# Process 2004
a<- matrix(0,12,dim(latlong_2004)[1])
b<- matrix(0,12,dim(latlong_2004)[1])
c<- matrix(0,12,dim(latlong_2004)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2004_tmin[[i]], latlong_2004)
    b[i,]<- extract(rasters_2004_tmax[[i]], latlong_2004)
    c[i,]<- extract(rasters_2004_prec[[i]], latlong_2004)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2004<-cbind(points_2004,a,b,c)

list_2005_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2005/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2005_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2005/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2005_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2005/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2005_tmin<- lapply(list_2005_tmin, raster)
rasters_2005_tmax<- lapply(list_2005_tmax, raster)
rasters_2005_prec<- lapply(list_2005_prec, raster)

# Process 2005
a<- matrix(0,12,dim(latlong_2005)[1])
b<- matrix(0,12,dim(latlong_2005)[1])
c<- matrix(0,12,dim(latlong_2005)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2005_tmin[[i]], latlong_2005)
    b[i,]<- extract(rasters_2005_tmax[[i]], latlong_2005)
    c[i,]<- extract(rasters_2005_prec[[i]], latlong_2005)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2005<-cbind(points_2005,a,b,c)
list_2006_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2006/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2006_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2006/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2006_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2006/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2006_tmin<- lapply(list_2006_tmin, raster)
rasters_2006_tmax<- lapply(list_2006_tmax, raster)
rasters_2006_prec<- lapply(list_2006_prec, raster)

# Process 2006
a<- matrix(0,12,dim(latlong_2006)[1])
b<- matrix(0,12,dim(latlong_2006)[1])
c<- matrix(0,12,dim(latlong_2006)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2006_tmin[[i]], latlong_2006)
    b[i,]<- extract(rasters_2006_tmax[[i]], latlong_2006)
    c[i,]<- extract(rasters_2006_prec[[i]], latlong_2006)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2006<-cbind(points_2006,a,b,c)

list_2007_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2007/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2007_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2007/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2007_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2007/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2007_tmin<- lapply(list_2007_tmin, raster)
rasters_2007_tmax<- lapply(list_2007_tmax, raster)
rasters_2007_prec<- lapply(list_2007_prec, raster)

# Process 2007
a<- matrix(0,12,dim(latlong_2007)[1])
b<- matrix(0,12,dim(latlong_2007)[1])
c<- matrix(0,12,dim(latlong_2007)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2007_tmin[[i]], latlong_2007)
    b[i,]<- extract(rasters_2007_tmax[[i]], latlong_2007)
    c[i,]<- extract(rasters_2007_prec[[i]], latlong_2007)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2007<-cbind(points_2007,a,b,c)
list_2008_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2008/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2008_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2008/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2008_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2008/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2008_tmin<- lapply(list_2008_tmin, raster)
rasters_2008_tmax<- lapply(list_2008_tmax, raster)
rasters_2008_prec<- lapply(list_2008_prec, raster)

# Process 2008
a<- matrix(0,12,dim(latlong_2008)[1])
b<- matrix(0,12,dim(latlong_2008)[1])
c<- matrix(0,12,dim(latlong_2008)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2008_tmin[[i]], latlong_2008)
    b[i,]<- extract(rasters_2008_tmax[[i]], latlong_2008)
    c[i,]<- extract(rasters_2008_prec[[i]], latlong_2008)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2008<-cbind(points_2008,a,b,c)
list_2009_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2009/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2009_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2009/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2009_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2009/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2009_tmin<- lapply(list_2009_tmin, raster)
rasters_2009_tmax<- lapply(list_2009_tmax, raster)
rasters_2009_prec<- lapply(list_2009_prec, raster)

# Process 2009
a<- matrix(0,12,dim(latlong_2009)[1])
b<- matrix(0,12,dim(latlong_2009)[1])
c<- matrix(0,12,dim(latlong_2009)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2009_tmin[[i]], latlong_2009)
    b[i,]<- extract(rasters_2009_tmax[[i]], latlong_2009)
    c[i,]<- extract(rasters_2009_prec[[i]], latlong_2009)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2009<-cbind(points_2009,a,b,c)

list_2010_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2010/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2010_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2010/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2010_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2010/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2010_tmin<- lapply(list_2010_tmin, raster)
rasters_2010_tmax<- lapply(list_2010_tmax, raster)
rasters_2010_prec<- lapply(list_2010_prec, raster)

# Process 2010
a<- matrix(0,12,dim(latlong_2010)[1])
b<- matrix(0,12,dim(latlong_2010)[1])
c<- matrix(0,12,dim(latlong_2010)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2010_tmin[[i]], latlong_2010)
    b[i,]<- extract(rasters_2010_tmax[[i]], latlong_2010)
    c[i,]<- extract(rasters_2010_prec[[i]], latlong_2010)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2010<-cbind(points_2010,a,b,c)

list_2011_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2011/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2011_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2011/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2011_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2011/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2011_tmin<- lapply(list_2011_tmin, raster)
rasters_2011_tmax<- lapply(list_2011_tmax, raster)
rasters_2011_prec<- lapply(list_2011_prec, raster)

# Process 2011
a<- matrix(0,12,dim(latlong_2011)[1])
b<- matrix(0,12,dim(latlong_2011)[1])
c<- matrix(0,12,dim(latlong_2011)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2011_tmin[[i]], latlong_2011)
    b[i,]<- extract(rasters_2011_tmax[[i]], latlong_2011)
    c[i,]<- extract(rasters_2011_prec[[i]], latlong_2011)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2011<-cbind(points_2011,a,b,c)


list_2012_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2012/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2012_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2012/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2012_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2012/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2012_tmin<- lapply(list_2012_tmin, raster)
rasters_2012_tmax<- lapply(list_2012_tmax, raster)
rasters_2012_prec<- lapply(list_2012_prec, raster)

# Process 2012
a<- matrix(0,12,dim(latlong_2012)[1])
b<- matrix(0,12,dim(latlong_2012)[1])
c<- matrix(0,12,dim(latlong_2012)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2012_tmin[[i]], latlong_2012)
    b[i,]<- extract(rasters_2012_tmax[[i]], latlong_2012)
    c[i,]<- extract(rasters_2012_prec[[i]], latlong_2012)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2012<-cbind(points_2012,a,b,c)
list_2013_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2013/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2013_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2013/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2013_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2013/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2013_tmin<- lapply(list_2013_tmin, raster)
rasters_2013_tmax<- lapply(list_2013_tmax, raster)
rasters_2013_prec<- lapply(list_2013_prec, raster)

# Process 2013
a<- matrix(0,12,dim(latlong_2013)[1])
b<- matrix(0,12,dim(latlong_2013)[1])
c<- matrix(0,12,dim(latlong_2013)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2013_tmin[[i]], latlong_2013)
    b[i,]<- extract(rasters_2013_tmax[[i]], latlong_2013)
    c[i,]<- extract(rasters_2013_prec[[i]], latlong_2013)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2013<-cbind(points_2013,a,b,c)
list_2014_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2014/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2014_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2014/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2014_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2014/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2014_tmin<- lapply(list_2014_tmin, raster)
rasters_2014_tmax<- lapply(list_2014_tmax, raster)
rasters_2014_prec<- lapply(list_2014_prec, raster)

# Process 2014
a<- matrix(0,12,dim(latlong_2014)[1])
b<- matrix(0,12,dim(latlong_2014)[1])
c<- matrix(0,12,dim(latlong_2014)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2014_tmin[[i]], latlong_2014)
    b[i,]<- extract(rasters_2014_tmax[[i]], latlong_2014)
    c[i,]<- extract(rasters_2014_prec[[i]], latlong_2014)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2014<-cbind(points_2014,a,b,c)

list_2015_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2015/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2015_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2015/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2015_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2015/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2015_tmin<- lapply(list_2015_tmin, raster)
rasters_2015_tmax<- lapply(list_2015_tmax, raster)
rasters_2015_prec<- lapply(list_2015_prec, raster)

# Process 2015
a<- matrix(0,12,dim(latlong_2015)[1])
b<- matrix(0,12,dim(latlong_2015)[1])
c<- matrix(0,12,dim(latlong_2015)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2015_tmin[[i]], latlong_2015)
    b[i,]<- extract(rasters_2015_tmax[[i]], latlong_2015)
    c[i,]<- extract(rasters_2015_prec[[i]], latlong_2015)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2015<-cbind(points_2015,a,b,c)
list_2016_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2016/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2016_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2016/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2016_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2016/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2016_tmin<- lapply(list_2016_tmin, raster)
rasters_2016_tmax<- lapply(list_2016_tmax, raster)
rasters_2016_prec<- lapply(list_2016_prec, raster)

# Process 2016
a<- matrix(0,12,dim(latlong_2016)[1])
b<- matrix(0,12,dim(latlong_2016)[1])
c<- matrix(0,12,dim(latlong_2016)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2016_tmin[[i]], latlong_2016)
    b[i,]<- extract(rasters_2016_tmax[[i]], latlong_2016)
    c[i,]<- extract(rasters_2016_prec[[i]], latlong_2016)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2016<-cbind(points_2016,a,b,c)

list_2017_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2017/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2017_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2017/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2017_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2017/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2017_tmin<- lapply(list_2017_tmin, raster)
rasters_2017_tmax<- lapply(list_2017_tmax, raster)
rasters_2017_prec<- lapply(list_2017_prec, raster)

# Process 2017
a<- matrix(0,12,dim(latlong_2017)[1])
b<- matrix(0,12,dim(latlong_2017)[1])
c<- matrix(0,12,dim(latlong_2017)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2017_tmin[[i]], latlong_2017)
    b[i,]<- extract(rasters_2017_tmax[[i]], latlong_2017)
    c[i,]<- extract(rasters_2017_prec[[i]], latlong_2017)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2017<-cbind(points_2017,a,b,c)
list_2018_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2018/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2018_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2018/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2018_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2018/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2018_tmin<- lapply(list_2018_tmin, raster)
rasters_2018_tmax<- lapply(list_2018_tmax, raster)
rasters_2018_prec<- lapply(list_2018_prec, raster)

# Process 2018
a<- matrix(0,12,dim(latlong_2018)[1])
b<- matrix(0,12,dim(latlong_2018)[1])
c<- matrix(0,12,dim(latlong_2018)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2018_tmin[[i]], latlong_2018)
    b[i,]<- extract(rasters_2018_tmax[[i]], latlong_2018)
    c[i,]<- extract(rasters_2018_prec[[i]], latlong_2018)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2018<-cbind(points_2018,a,b,c)
list_2019_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2019/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2019_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2019/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2019_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2019/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2019_tmin<- lapply(list_2019_tmin, raster)
rasters_2019_tmax<- lapply(list_2019_tmax, raster)
rasters_2019_prec<- lapply(list_2019_prec, raster)

# Process 2019
a<- matrix(0,12,dim(latlong_2019)[1])
b<- matrix(0,12,dim(latlong_2019)[1])
c<- matrix(0,12,dim(latlong_2019)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2019_tmin[[i]], latlong_2019)
    b[i,]<- extract(rasters_2019_tmax[[i]], latlong_2019)
    c[i,]<- extract(rasters_2019_prec[[i]], latlong_2019)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2019<-cbind(points_2019,a,b,c)


list_2020_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2020/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2020_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2020/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2020_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2020/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2020_tmin<- lapply(list_2020_tmin, raster)
rasters_2020_tmax<- lapply(list_2020_tmax, raster)
rasters_2020_prec<- lapply(list_2020_prec, raster)

# Process 2020
a<- matrix(0,12,dim(latlong_2020)[1])
b<- matrix(0,12,dim(latlong_2020)[1])
c<- matrix(0,12,dim(latlong_2020)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2020_tmin[[i]], latlong_2020)
    b[i,]<- extract(rasters_2020_tmax[[i]], latlong_2020)
    c[i,]<- extract(rasters_2020_prec[[i]], latlong_2020)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2020<-cbind(points_2020,a,b,c)



list_2021_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2021/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2021_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2021/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2021_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2021/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2021_tmin<- lapply(list_2021_tmin, raster)
rasters_2021_tmax<- lapply(list_2021_tmax, raster)
rasters_2021_prec<- lapply(list_2021_prec, raster)

# Process 2021
a<- matrix(0,12,dim(latlong_2021)[1])
b<- matrix(0,12,dim(latlong_2021)[1])
c<- matrix(0,12,dim(latlong_2021)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2021_tmin[[i]], latlong_2021)
    b[i,]<- extract(rasters_2021_tmax[[i]], latlong_2021)
    c[i,]<- extract(rasters_2021_prec[[i]], latlong_2021)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2021<-cbind(points_2021,a,b,c)


list_2022_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2022/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2022_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2022/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2022_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2022/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2022_tmin<- lapply(list_2022_tmin, raster)
rasters_2022_tmax<- lapply(list_2022_tmax, raster)
rasters_2022_prec<- lapply(list_2022_prec, raster)

# Process 2022
a<- matrix(0,12,dim(latlong_2022)[1])
b<- matrix(0,12,dim(latlong_2022)[1])
c<- matrix(0,12,dim(latlong_2022)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2022_tmin[[i]], latlong_2022)
    b[i,]<- extract(rasters_2022_tmax[[i]], latlong_2022)
    c[i,]<- extract(rasters_2022_prec[[i]], latlong_2022)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2022<-cbind(points_2022,a,b,c)



list_2023_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2023/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2023_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2023/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2023_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2023/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2023_tmin<- lapply(list_2023_tmin, raster)
rasters_2023_tmax<- lapply(list_2023_tmax, raster)
rasters_2023_prec<- lapply(list_2023_prec, raster)

# Process 2023
a<- matrix(0,12,dim(latlong_2023)[1])
b<- matrix(0,12,dim(latlong_2023)[1])
c<- matrix(0,12,dim(latlong_2023)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2023_tmin[[i]], latlong_2023)
    b[i,]<- extract(rasters_2023_tmax[[i]], latlong_2023)
    c[i,]<- extract(rasters_2023_prec[[i]], latlong_2023)
  }
}

a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2023<-cbind(points_2023,a,b,c)

list_2024_tmin<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2024/tmin",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2024_tmax<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2024/tmax",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)
list_2024_prec<- list.files(path = "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/Version2/Worldclim/2024/prec",
                            pattern=".tif$", all.files=TRUE, full.names=TRUE)

rasters_2024_tmin<- lapply(list_2024_tmin, raster)
rasters_2024_tmax<- lapply(list_2024_tmax, raster)
rasters_2024_prec<- lapply(list_2024_prec, raster)

# Process 2024
a<- matrix(0,12,dim(latlong_2024)[1])
b<- matrix(0,12,dim(latlong_2024)[1])
c<- matrix(0,12,dim(latlong_2024)[1])
for (i in 1:12) {
  for (j in dim(a)[1]) {
    a[i,]<- extract(rasters_2024_tmin[[i]], latlong_2024)
    b[i,]<- extract(rasters_2024_tmax[[i]], latlong_2024)
    c[i,]<- extract(rasters_2024_prec[[i]], latlong_2024)
  }
}
a<- t(a)
b<- t(b)
c<- t(c)
colnames(a)<- c("tmin_1", "tmin_2", "tmin_3", "tmin_4", "tmin_5",
                "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
                "tmin_11", "tmin_12")
colnames(b)<- c("tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
                "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10",
                "tmax_11", "tmax_12")
colnames(c)<- c("prec_1", "prec_2", "prec_3", "prec_4", "prec_5",
                "prec_6", "prec_7", "prec_8", "prec_9", "prec_10",
                "prec_11", "prec_12")

final_2024<-cbind(points_2024,a,b,c)


climate_data<- rbind(final_1951,final_1952,final_1953,final_1954,final_1955, final_1956,final_1957,final_1958,final_1959,
                     final_1960,final_1961,final_1962,final_1963,final_1964,final_1965,final_1966,final_1967,final_1968,final_1969,
                     final_1970,final_1971,final_1972,final_1973,final_1974,final_1975,final_1976,final_1977,final_1978,final_1979,
                     final_1980,final_1981,final_1982,final_1983,final_1984,final_1985,final_1986,final_1987,final_1988,final_1989,
                     final_1990,final_1991,final_1992,final_1993,final_1994,final_1995,final_1996,final_1997,final_1998,final_1999,
                     final_2000,final_2001,final_2002,final_2003,final_2004,final_2005,final_2006,final_2007,final_2008,final_2009,
                     final_2010,final_2011,final_2012,final_2013,final_2014,final_2015,final_2016,final_2017,final_2018,final_2019,
                     final_2020,final_2021,final_2022,final_2023,final_2024)

climate_data<- left_join(climate_data, eleuth_cleaned_climate)

climate_function <- function(a){
  tmin = as.numeric(c(a[7:18]))
  tmax = as.numeric(c(a[19:30]))
  prec = as.numeric(c(a[31:42]))
  return(biovars(prec=prec, tmin=tmin, tmax=tmax))
}


biovar<- apply(climate_data, 1, climate_function)

cols <- c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19")

## transpose matrix and add names for each bioclimate variable 
climate_final <- setNames(as.data.frame(t(biovar)), cols)
## bind the GBIFID, Object ID, specis and region to the dataset
climate_final_binded <- cbind(c(climate_data[1:2], climate_data[5:6], climate_data[,44:50]), climate_final) #adds back the orginals Ids to the front of the dataframe



#write.csv(climate_final_binded, "climate_final_binded2.csv")


# EcoSpat Analysis --------------------------------------------------------

rm(list = ls())

climate_final_binded<- read.csv("climate_final_binded2.csv")



 
coqui_final<- climate_final_binded %>% filter(species == "Eleutherodactylus coqui")%>% filter(bio1 != "NA")
planirostris_final<- climate_final_binded %>% filter(species == "Eleutherodactylus planirostris") %>% filter(bio1 != "NA")
johnstonei_final<- climate_final_binded%>% filter(species == "Eleutherodactylus johnstonei") %>% filter(bio1 != "NA")
cystignathoides_final<- climate_final_binded%>% filter(species == "Eleutherodactylus cystignathoides")%>% filter(bio1 != "NA")
antillensis_final<- climate_final_binded%>% filter(species == "Eleutherodactylus antillensis")%>% filter(bio1 != "NA")
martinicensis_final<- climate_final_binded%>% filter(species == "Eleutherodactylus martinicensis")%>% filter(bio1 != "NA")


## E. coqui #########################################################################################################
coqui_pca.env <- ade4::dudi.pca(coqui_final[,13:31],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=coqui_pca.env$co, eigen=coqui_pca.env$eig)
## eigenvectors for each variable
write.csv(coqui_pca.env$c1, "coqui_princomp.csv")


#PCA scoresfor the wholestudyarea
coqui_scores.globclim <-coqui_pca.env$li

#PCA scoresfor the species nativedistribution 
coqui_scores.sp.nat<-ade4::suprow(coqui_pca.env,coqui_final[which(coqui_final[,12]=="Native"),13:31])$li

#PCA scoresfor the species invasive distribution 
coqui_scores.sp.inv<-ade4::suprow(coqui_pca.env,coqui_final[which(coqui_final[,12]=="Likely established"),13:31])$li

#PCA scoresfor the wholenativestudyarea - Double check this
coqui_scores.clim.nat <-ade4::suprow(coqui_pca.env,coqui_final[which(coqui_final[,12]=="Native"),13:31])$li

#PCA scoresfor the wholeinvaded studyarea - Double check this
coqui_scores.clim.inv <-ade4::suprow(coqui_pca.env,coqui_final[which(coqui_final[,12]=="Likely established"),13:31])$li

#gridding the nativeniche 
coqui_grid.clim.nat<-ecospat.grid.clim.dyn(glob=coqui_scores.globclim, glob1=coqui_scores.clim.nat, sp=coqui_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
coqui_grid.clim.inv <- ecospat.grid.clim.dyn(glob=coqui_scores.globclim, glob1=coqui_scores.clim.inv, sp=coqui_scores.sp.inv, R=100, th.sp=0)


coqui_D.overlap <- ecospat.niche.overlap (coqui_grid.clim.nat, coqui_grid.clim.inv, cor = TRUE)$D 
coqui_D.overlap

#### Use 1000 replications for equivalency and similarity test
coqui_eq.test <- ecospat.niche.equivalency.test(coqui_grid.clim.nat, coqui_grid.clim.inv,rep=1000, intersection = 0.1, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower")
ecospat.plot.overlap.test(coqui_eq.test, "D", "Equivalency")

coqui_sim.test <- ecospat.niche.similarity.test(coqui_grid.clim.nat, coqui_grid.clim.inv,rep=1000, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower", intersection = 0.1, rand.type=1)
ecospat.plot.overlap.test(coqui_sim.test, "D", "Similarity")


ecospat.plot.niche.dyn(coqui_grid.clim.nat, coqui_grid.clim.inv, quant=0.25, interest=2, title= "Niche Overlap", name.axis1="PC1 (34.86%)", name.axis2="PC2 (26.54%)")
ecospat.shift.centroids(coqui_scores.sp.nat, coqui_scores.sp.inv, coqui_scores.clim.nat, coqui_scores.clim.inv)
ggsave("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/coqui_niche.tiff", units="in", width=25, height=15, dpi=150, compression = 'lzw')

## E. planirostris #########################################################################################################
planirostris_pca.env <- ade4::dudi.pca(planirostris_final[,13:31],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=planirostris_pca.env$co, eigen=planirostris_pca.env$eig)
## eigenvectors for each variable
write.csv(planirostris_pca.env$c1, "planirostris_princomp.csv")


#PCA scoresfor the wholestudyarea
planirostris_scores.globclim <-planirostris_pca.env$li

#PCA scoresfor the species nativedistribution 
planirostris_scores.sp.nat<-ade4::suprow(planirostris_pca.env,planirostris_final[which(planirostris_final[,12]=="Native"),13:31])$li

#PCA scoresfor the species invasive distribution 
planirostris_scores.sp.inv<-ade4::suprow(planirostris_pca.env,planirostris_final[which(planirostris_final[,12]=="Likely established"),13:31])$li

#PCA scoresfor the wholenativestudyarea 
planirostris_scores.clim.nat <-ade4::suprow(planirostris_pca.env,planirostris_final[which(planirostris_final[,12]=="Native"),13:31])$li

#PCA scoresfor the wholeinvaded studyarea 
planirostris_scores.clim.inv <-ade4::suprow(planirostris_pca.env,planirostris_final[which(planirostris_final[,12]=="Likely established"),13:31])$li

#gridding the nativeniche 
planirostris_grid.clim.nat<-ecospat.grid.clim.dyn(glob=planirostris_scores.globclim, glob1=planirostris_scores.clim.nat, sp=planirostris_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
planirostris_grid.clim.inv <- ecospat.grid.clim.dyn(glob=planirostris_scores.globclim, glob1=planirostris_scores.clim.inv, sp=planirostris_scores.sp.inv, R=100, th.sp=0)


planirostris_D.overlap <- ecospat.niche.overlap (planirostris_grid.clim.nat, planirostris_grid.clim.inv, cor = TRUE)$D 
planirostris_D.overlap

#### Use 1000 replications for equivalency and similarity test
planirostris_eq.test <- ecospat.niche.equivalency.test(planirostris_grid.clim.nat, planirostris_grid.clim.inv,rep=1000, intersection = 0.1, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower")
ecospat.plot.overlap.test(planirostris_eq.test, "D", "Equivalency")

planirostris_sim.test <- ecospat.niche.similarity.test(planirostris_grid.clim.nat, planirostris_grid.clim.inv,rep=1000, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower", intersection = 0.1, rand.type=1)
ecospat.plot.overlap.test(planirostris_sim.test, "D", "Similarity")


ecospat.plot.niche.dyn(planirostris_grid.clim.nat, planirostris_grid.clim.inv, quant=0.25, interest=2, title= "Niche Overlap", name.axis1="PC1 (34.66%)", name.axis2="PC2 (21.04%)") 
ecospat.shift.centroids(planirostris_scores.sp.nat, planirostris_scores.sp.inv, planirostris_scores.clim.nat, planirostris_scores.clim.inv)
ggsave("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/planirostris_niche.tiff", units="in", width=25, height=15, dpi=150, compression = 'lzw')


## E. johnstonei #########################################################################################################
johnstonei_pca.env <- ade4::dudi.pca(johnstonei_final[,13:31],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=johnstonei_pca.env$co, eigen=johnstonei_pca.env$eig)
## eigenvectors for each variable
write.csv(johnstonei_pca.env$c1, "johnstonei_princomp.csv")


#PCA scoresfor the wholestudyarea
johnstonei_scores.globclim <-johnstonei_pca.env$li

#PCA scoresfor the species nativedistribution 
johnstonei_scores.sp.nat<-ade4::suprow(johnstonei_pca.env,johnstonei_final[which(johnstonei_final[,12]=="Native"),13:31])$li

#PCA scoresfor the species invasive distribution 
johnstonei_scores.sp.inv<-ade4::suprow(johnstonei_pca.env,johnstonei_final[which(johnstonei_final[,12]=="Likely established"),13:31])$li

#PCA scoresfor the wholenativestudyarea - Double check this
johnstonei_scores.clim.nat <-ade4::suprow(johnstonei_pca.env,johnstonei_final[which(johnstonei_final[,12]=="Native"),13:31])$li

#PCA scoresfor the wholeinvaded studyarea - Double check this
johnstonei_scores.clim.inv <-ade4::suprow(johnstonei_pca.env,johnstonei_final[which(johnstonei_final[,12]=="Likely established"),13:31])$li

#gridding the nativeniche 
johnstonei_grid.clim.nat<-ecospat.grid.clim.dyn(glob=johnstonei_scores.globclim, glob1=johnstonei_scores.clim.nat, sp=johnstonei_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
johnstonei_grid.clim.inv <- ecospat.grid.clim.dyn(glob=johnstonei_scores.globclim, glob1=johnstonei_scores.clim.inv, sp=johnstonei_scores.sp.inv, R=100, th.sp=0)


johnstonei_D.overlap <- ecospat.niche.overlap (johnstonei_grid.clim.nat, johnstonei_grid.clim.inv, cor = TRUE)$D 
johnstonei_D.overlap

#### Use 1000 replications for equivalency and similarity test
johnstonei_eq.test <- ecospat.niche.equivalency.test(johnstonei_grid.clim.nat, johnstonei_grid.clim.inv,rep=1000, intersection = 0.1, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower")
ecospat.plot.overlap.test(johnstonei_eq.test, "D", "Equivalency")

johnstonei_sim.test <- ecospat.niche.similarity.test(johnstonei_grid.clim.nat, johnstonei_grid.clim.inv,rep=1000, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower", intersection = 0.1, rand.type=1)
ecospat.plot.overlap.test(johnstonei_sim.test, "D", "Similarity")


ecospat.plot.niche.dyn(johnstonei_grid.clim.nat, johnstonei_grid.clim.inv, quant=0.25, interest=2, title= "Niche Overlap", name.axis1="PC1 (43.5%)", name.axis2="PC2 (18.99%)") 
ecospat.shift.centroids(johnstonei_scores.sp.nat, johnstonei_scores.sp.inv, johnstonei_scores.clim.nat, johnstonei_scores.clim.inv)
ggsave("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/johnstonei_niche.tiff", units="in", width=25, height=15, dpi=150, compression = 'lzw')


## E. antillensis #########################################################################################################
antillensis_pca.env <- ade4::dudi.pca(antillensis_final[,13:31],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=antillensis_pca.env$co, eigen=antillensis_pca.env$eig)
## eigenvectors for each variable
write.csv(antillensis_pca.env$c1, "antillensis_princomp.csv")


my.pca.antillensis <- prcomp(antillensis_final[,13:31], center=T, scale=TRUE)
prin_comp_antillensis <-rda(antillensis_final[,13:31], center = T, scale=TRUE)

pca_antillensis<- autoplot(my.pca.antillensis, x=1, y = 2, data = antillensis_final, colour = "countryCode", 
                           loadings = F, loadings.label = F, pch = 4, size = 3)+labs(title = "E. antillensis")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 5, face = "bold"),
        legend.title = element_text(size = 5, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))



#PCA scoresfor the wholestudyarea
antillensis_scores.globclim <-antillensis_pca.env$li

#PCA scoresfor the species nativedistribution 
antillensis_scores.sp.nat<-ade4::suprow(antillensis_pca.env,antillensis_final[which(antillensis_final[,12]=="Native"),13:31])$li

#PCA scoresfor the species invasive distribution 
antillensis_scores.sp.inv<-ade4::suprow(antillensis_pca.env,antillensis_final[which(antillensis_final[,12]=="Likely established"),13:31])$li

#PCA scoresfor the wholenativestudyarea - Double check this
antillensis_scores.clim.nat <-ade4::suprow(antillensis_pca.env,antillensis_final[which(antillensis_final[,12]=="Native"),13:31])$li

#PCA scoresfor the wholeinvaded studyarea - Double check this
antillensis_scores.clim.inv <-ade4::suprow(antillensis_pca.env,antillensis_final[which(antillensis_final[,12]=="Likely established"),13:31])$li

#gridding the nativeniche 
antillensis_grid.clim.nat<-ecospat.grid.clim.dyn(glob=antillensis_scores.globclim, glob1=antillensis_scores.clim.nat, sp=antillensis_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
antillensis_grid.clim.inv <- ecospat.grid.clim.dyn(glob=antillensis_scores.globclim, glob1=antillensis_scores.clim.inv, sp=antillensis_scores.sp.inv, R=100, th.sp=0)


antillensis_D.overlap <- ecospat.niche.overlap (antillensis_grid.clim.nat, antillensis_grid.clim.inv, cor = TRUE)$D 
antillensis_D.overlap

#### Use 1000 replications for equivalency and similarity test
antillensis_eq.test <- ecospat.niche.equivalency.test(antillensis_grid.clim.nat, antillensis_grid.clim.inv,rep=1000, intersection = 0.1, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower")
ecospat.plot.overlap.test(antillensis_eq.test, "D", "Equivalency")

antillensis_sim.test <- ecospat.niche.similarity.test(antillensis_grid.clim.nat, antillensis_grid.clim.inv,rep=1000, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower", intersection = 0.1, rand.type=1)
ecospat.plot.overlap.test(antillensis_sim.test, "D", "Similarity")


ecospat.plot.niche.dyn(antillensis_grid.clim.nat, antillensis_grid.clim.inv, quant=0.25, interest=2, title= "Niche Overlap", name.axis1="PC1 (45.27%)", name.axis2="PC2 (23.86%)") 
ecospat.shift.centroids(antillensis_scores.sp.nat, antillensis_scores.sp.inv, antillensis_scores.clim.nat, antillensis_scores.clim.inv)
ggsave("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/antillensis_niche.tiff", units="in", width=25, height=15, dpi=150, compression = 'lzw')



## E. martinicensis #########################################################################################################
martinicensis_pca.env <- ade4::dudi.pca(martinicensis_final[,13:31],scannf=F,nf=2) 
ecospat.plot.contrib(contrib=martinicensis_pca.env$co, eigen=martinicensis_pca.env$eig)
## eigenvectors for each variable
write.csv(martinicensis_pca.env$c1, "martinicensis_princomp.csv")


#PCA scoresfor the wholestudyarea
martinicensis_scores.globclim <-martinicensis_pca.env$li

#PCA scoresfor the species nativedistribution 
martinicensis_scores.sp.nat<-ade4::suprow(martinicensis_pca.env,martinicensis_final[which(martinicensis_final[,12]=="Native"),13:31])$li

#PCA scoresfor the species invasive distribution 
martinicensis_scores.sp.inv<-ade4::suprow(martinicensis_pca.env,martinicensis_final[which(martinicensis_final[,12]=="Likely established"),13:31])$li

#PCA scoresfor the wholenativestudyarea - Double check this
martinicensis_scores.clim.nat <-ade4::suprow(martinicensis_pca.env,martinicensis_final[which(martinicensis_final[,12]=="Native"),13:31])$li

#PCA scoresfor the wholeinvaded studyarea - Double check this
martinicensis_scores.clim.inv <-ade4::suprow(martinicensis_pca.env,martinicensis_final[which(martinicensis_final[,12]=="Likely established"),13:31])$li

#gridding the nativeniche 
martinicensis_grid.clim.nat<-ecospat.grid.clim.dyn(glob=martinicensis_scores.globclim, glob1=martinicensis_scores.clim.nat, sp=martinicensis_scores.sp.nat,R=100, th.sp=0)

# gridding the invasive niche 
martinicensis_grid.clim.inv <- ecospat.grid.clim.dyn(glob=martinicensis_scores.globclim, glob1=martinicensis_scores.clim.inv, sp=martinicensis_scores.sp.inv, R=100, th.sp=0)


martinicensis_D.overlap <- ecospat.niche.overlap (martinicensis_grid.clim.nat, martinicensis_grid.clim.inv, cor = TRUE)$D 
martinicensis_D.overlap

#### Use 1000 replications for equivalency and similarity test
martinicensis_eq.test <- ecospat.niche.equivalency.test(martinicensis_grid.clim.nat, martinicensis_grid.clim.inv,rep=1000, intersection = 0.1, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower")
ecospat.plot.overlap.test(martinicensis_eq.test, "D", "Equivalency")

martinicensis_sim.test <- ecospat.niche.similarity.test(martinicensis_grid.clim.nat, martinicensis_grid.clim.inv,rep=1000, overlap.alternative = "higher", expansion.alternative = "lower", stability.alternative = "higher", unfilling.alternative = "lower", intersection = 0.1, rand.type=1)
ecospat.plot.overlap.test(martinicensis_sim.test, "D", "Similarity")


ecospat.plot.niche.dyn(martinicensis_grid.clim.nat, martinicensis_grid.clim.inv, quant=0.25, interest=2, name.axis1="PC1 (38.19%)", name.axis2="PC2 (21.14%)", transparency = 80) 
ecospat.shift.centroids(martinicensis_scores.sp.nat, martinicensis_scores.sp.inv, martinicensis_scores.clim.nat, martinicensis_scores.clim.inv)
ggsave("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Biological_Diversity_Submission/Submission2/Figures/martinicensis_niche.jpeg", width=25, height=25, dpi=300)


