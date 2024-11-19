### Eleutherodactylus data clean - refine 

## Load Libraries 

library(tidyverse)
library(dismo)
library(vegan)
library(ggplot2)
library(plotly) ## DOUBLE CHECK IF NEEDED
library(devtools)
library(ggpubr)
library(ggfortify)
library(psych)

wd<- "C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Historic_Reconstruction"
setwd(wd)

coqui<- read.csv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/RawData/Ecoqui_GBIF/Ecoqui_GBIF.CSV")
planirostris<-  read.csv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/RawData/Eplanirostris_GBIF/Eplanirostris_GBIF.CSV")
johnstonei<-  read.csv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/RawData/Ejohnstonei_GBIF/Ejohnstonei_GBIF.CSV")


coqui_filter<- coqui %>% dplyr::select("gbifID", "species", "countryCode", "locality", "stateProvince", "occurrenceStatus", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "day", "month", "year", "collectionCode", "catalogNumber")
planirostris_filter<- planirostris %>% dplyr::select("gbifID", "species", "countryCode", "locality", "stateProvince", "occurrenceStatus", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "day", "month", "year", "collectionCode", "catalogNumber")
johnstonei_filter<- johnstonei %>% dplyr::select("gbifID", "species", "countryCode", "locality", "stateProvince", "occurrenceStatus", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "day", "month", "year", "collectionCode", "catalogNumber")


coqui_filter<- coqui_filter %>% filter(occurrenceStatus != "ABSENT" )
planirostris_filter<- planirostris_filter %>% filter(occurrenceStatus != "ABSENT" )
johnstonei_filter<- johnstonei_filter %>%  filter(occurrenceStatus != "ABSENT" )


write.csv(coqui_filter, "coqui_history.csv")
write.csv(planirostris_filter, "planirostris_history.csv")
write.csv(johnstonei_filter, "johnstonei_history.csv")

### Filter Coqui 

coqui_filter_climate_native<- coqui_filter %>% 
  filter(countryCode == "PR") %>% 
  filter(year >= 2010) %>% 
  mutate(status = "native")
  
coqui_filter_climate_US<- coqui_filter %>% 
  filter(countryCode == "US" & stateProvince == "Hi" | stateProvince == "Hawaii" | stateProvince == "" & locality == "Hawaii, Kukuihaele") %>% 
  filter(year >= 2010) %>% 
  mutate(status = "invasive")

coqui_filter_climate_VI<- coqui_filter %>% 
  filter(countryCode == "VI" & year >= 2016)%>% 
  mutate(status = "invasive")

coqui_filter_climate<- rbind(coqui_filter_climate_native, coqui_filter_climate_US, coqui_filter_climate_VI)
coqui_filter_climate<- coqui_filter_climate %>% 
  filter(coordinateUncertaintyInMeters <= 1000) %>% 
  filter(year <= 2021)


write.csv(coqui_filter_climate, "coqui_establishedpops.csv")

## Filter planirostris 

planirostris_filter_native<- planirostris_filter %>% 
  filter(countryCode %in% c("KY", "BS", "CU")) %>% 
  filter(year >= 2010) %>% 
  mutate(status = "native")

planirostris_filter_climate_hawaii<- planirostris_filter %>% 
  filter(countryCode == "US" & stateProvince == "Hi" | countryCode == "US" & stateProvince == "Hawaii" | countryCode == "US" & stateProvince == "29") %>% 
  filter(year >= 2013)%>% 
  mutate(status = "invasive")

planirostris_filter_climate_us <- planirostris_filter %>% 
  filter(countryCode == "US" & stateProvince != "Hawaii") %>%
  filter(countryCode == "US" & stateProvince != "29") %>% 
  filter(year >= 2010)%>% 
  mutate(status = "invasive")



planirostris_filter_climate_china<- planirostris_filter %>% 
  filter(countryCode == "CN" | countryCode == "HK") %>% 
  filter(year >= 2012)%>% 
  mutate(status = "invasive")

planirostris_filter_climate_guam<- planirostris_filter %>% 
  filter(countryCode == "GU") %>% 
  filter(year >= 2017)%>% 
  mutate(status = "invasive")

planirostris_filter_climate_honduras<- planirostris_filter %>% 
  filter(countryCode == "HN") %>% 
  filter(year >= 2017)%>% 
  mutate(status = "invasive")

planirostris_filter_climate_mexico<- planirostris_filter %>% 
  filter(countryCode == "MX") %>% 
  filter(year >= 2010)%>% 
  mutate(status = "invasive")

planirostris_filter_climate_phillipines<- planirostris_filter %>%
  filter(countryCode == "PH") %>% 
  filter(year >= 2013)%>% 
  mutate(status = "invasive")

planirostris_filter_climate<- rbind(planirostris_filter_native,
                                    planirostris_filter_climate_china,
                                    planirostris_filter_climate_guam,
                                    planirostris_filter_climate_honduras,
                                    planirostris_filter_climate_mexico,
                                    planirostris_filter_climate_phillipines,
                                    planirostris_filter_climate_hawaii,
                                    planirostris_filter_climate_us)

planirostris_filter_climate<- planirostris_filter_climate %>% 
  filter(coordinateUncertaintyInMeters <= 1000) %>% 
  filter(year <= 2021)

write.csv(planirostris_filter_climate, "planirostris_establishedpops.csv")


### Filter Johnstonei 
johnstonei_filter_native<- johnstonei_filter %>% 
  filter(countryCode %in% c("LC", "AG", "BQ", "MF", "KN", "VC", "MS", "SX")) %>% 
  filter(year >= 2010) %>% 
  mutate(status = "native")

johnstonei_filter_climate_barbados<- johnstonei_filter %>% 
  filter(countryCode == "BB") %>% 
  filter(year >= 2015)%>% 
  mutate(status = "invasive")

johnstonei_filter_climate_brazil<- johnstonei_filter %>% 
  filter(countryCode == "BR") %>% 
  filter(year >= 2012)%>% 
  mutate(status = "invasive")

johnstonei_filter_climate_colombia<- johnstonei_filter %>% 
  filter(countryCode == "CO") %>% 
  filter(year >= 2015)%>% 
  mutate(status = "invasive")

johnstonei_filter_climate_guadeloupe<- johnstonei_filter %>% 
  filter(countryCode == "GP") %>% 
  filter(year >= 2010)%>% 
  mutate(status = "invasive")

johnstonei_filter_climate_martinique<- johnstonei_filter %>% 
  filter(countryCode == "MQ") %>% 
  filter(year >= 2011)%>% 
  mutate(status = "invasive")

johnstonei_filter_climate_venezuela<- johnstonei_filter %>% 
  filter(countryCode == "VE") %>% 
  filter(year >= 2015)%>% 
  mutate(status = "invasive")


johnstonei_filter_climate<- rbind(johnstonei_filter_native,
                                  johnstonei_filter_climate_barbados,
                                  johnstonei_filter_climate_brazil,
                                  johnstonei_filter_climate_colombia,
                                  johnstonei_filter_climate_guadeloupe,
                                  johnstonei_filter_climate_martinique,
                                  johnstonei_filter_climate_venezuela)


johnstonei_filter_climate<- johnstonei_filter_climate %>% 
  filter(coordinateUncertaintyInMeters <= 1000) %>% 
  filter(year <= 2021)

write.csv(johnstonei_filter_climate, "johnstonei_establishedpops.csv")

#### Load in and process bioclimate data 

climate_data_final<- read.csv("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/concatenated_climatedata.csv")

climate_function <- function(a){
  tmin = as.numeric(c(a[3:14]))
  tmax = as.numeric(c(a[15:26]))
  prec = as.numeric(c(a[27:38]))
  return(biovars(prec=prec, tmin=tmin, tmax=tmax))
}

## biovars function applied to all 4167 datapoints
biovar<- apply(climate_data_final, 1, climate_function)

cols <- c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19")

## transpose matrix and add names for each bioclimate variable 
climate_final <- setNames(as.data.frame(t(biovar)), cols)
## bind the GBIFID, Object ID, specis and region to the dataset
climate_final_binded <- cbind(c(climate_data_final[1:2], climate_data_final[,39:42]), climate_final) #adds back the orginals Ids to the front of the dataframe

### separate out each species

coqui_bioclim<- climate_final_binded %>% filter(species == "Eleutherodactylus coqui")
planirostris_bioclim<- climate_final_binded %>% filter(species == "Eleutherodactylus planirostris")
johnstonei_bioclim<- climate_final_binded %>% filter(species == "Eleutherodactylus johnstonei")


coqui_final<- inner_join(coqui_bioclim, coqui_filter_climate)
planirostris_final<- inner_join(planirostris_bioclim, planirostris_filter_climate)
johnstonei_final<- inner_join(johnstonei_bioclim, johnstonei_filter_climate)

##Autocorrelation for each species 
corr_coqui <- coqui_final[c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")] # grouping all independent continuous factors into a single object to facilitate running the correlations
coqui_corr <- corr.test(corr_coqui, use = "pairwise", method = "pearson", adjust = "none") # runs the pairwise correlations between the independent continuous factors.
write.csv(coqui_corr$r, "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/autocorrelation_coqui_bioclimate.csv")

corr_planirostris <- planirostris_final[c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")] # grouping all independent continuous factors into a single object to facilitate running the correlations
planirostris_corr <- corr.test(corr_planirostris, use = "pairwise", method = "pearson", adjust = "none") # runs the pairwise correlations between the independent continuous factors.
write.csv(planirostris_corr$r, "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/autocorrelation_planirostris_bioclimate.csv")

corr_johnstonei <- johnstonei_final[c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")] # grouping all independent continuous factors into a single object to facilitate running the correlations
johnstonei_corr <- corr.test(corr_johnstonei, use = "pairwise", method = "pearson", adjust = "none") # runs the pairwise correlations between the independent continuous factors.
write.csv(johnstonei_corr$r, "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Climate_Analysis_EstablishmentOnly/autocorrelation_johnstonei_bioclimate.csv")



### Create "status" variable - native or non-native, for each species
unique(coqui_final$countryCode)
coqui_native<- coqui_final %>% filter(countryCode == "PR") %>% mutate(status = "Native")
coqui_invasive<- coqui_final %>% filter(countryCode %in% c("US", "CR", "VI")) %>% mutate(status = "Non-Native")
coqui_compare<- rbind(coqui_native, coqui_invasive)


unique(planirostris_final$countryCode)
planirostris_native<- planirostris_final %>% filter(countryCode %in% c("KY", "BS", "CU")) %>% mutate(status = "Native")
planirostris_invasive<- planirostris_final %>% filter(countryCode %in% c("BR", "US", "MX","PA","HK","GU","JM","TC","PH","CN","HN","SV","SG","MO","SX","CO","TW")) %>% mutate(status = "Non-Native")
planirostris_compare<- rbind(planirostris_native, planirostris_invasive)



unique(johnstonei_bioclim$countryCode)
johnstonei_native<- johnstonei_final %>% filter(countryCode %in% c("LC",  "AG", "BQ", "MF", "KN", "VC", "MS", "SX")) %>% mutate(status = "Native")
## Note, all AG samples from 2010-2021 are native Antigua
## Some Saint Vincent and Grenadines were native, some were non-native 

johnstonei_invasive<- johnstonei_final %>% filter(countryCode %in% c("ZZ","MQ", "JM", "TT", "CW", "GP", "CO","BB", "BR","GD", "DE", "GY", "BM", "VE", "GF", "DM")) %>%  mutate(status = "Non-Native")
## All ZZ labels were from Curacao, so they are designated as non-native

johnstonei_compare<- rbind(johnstonei_native, johnstonei_invasive)


# Coqui PCA
my.pca.coqui <- prcomp(coqui_compare[c(7:8,10,18:21,23:25)], center=T, scale=TRUE)
summary(my.pca.coqui) 
str(my.pca.coqui)
my.pca.coqui$rotation
my.pca.coqui$rotation[,1]

# Plot PCA #
prin_comp_coqui <-rda(coqui_compare[c(7:8,10,18:21,23:25)], center = T, scale=TRUE)
pca_scores<-scores(prin_comp_coqui)

plot(prin_comp_coqui)


coqui_scree<- screeplot(my.pca.coqui, bstick=T, col=colours()[574])
summary(prin_comp_coqui)



pca_coqui<- autoplot(my.pca.coqui, x=1, y = 2, data = coqui_compare, colour = "status", 
                     loadings = F, loadings.label = F, pch = 4, size = 3)+labs(title = "E. coqui")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))

pca_coqui3<- autoplot(my.pca.coqui, x=1, y = 3, data = coqui_compare, colour = "status", 
                      loadings = F, loadings.label = F, pch = 4, size = 3)+labs(title = "E. coqui")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))

pca_coqui4<- autoplot(my.pca.coqui, x=1, y = 4, data = coqui_compare, colour = "status", 
                      loadings = F, loadings.label = F, pch = 4, size = 3)+labs(title = "E. coqui")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))

# Planirostris PCA
my.pca.planirostris <- prcomp(planirostris_compare[c(7:9,11,14,16,18,20,21,24,25)], center=T, scale=TRUE)
summary(my.pca.planirostris) 
str(my.pca.planirostris)
my.pca.planirostris$rotation
my.pca.planirostris$rotation[,1]

# Plot PCA #
prin_comp_planirostris <-rda(planirostris_compare[c(7:9,11,14,16,18,20,21,24,25)], scale=TRUE)
pca_scores<-scores(prin_comp_planirostris)

plot(prin_comp_planirostris)					
planirostris_scree<- screeplot(prin_comp_planirostris, bstick=T, col=colours()[574])	
summary(prin_comp_planirostris)

pca_planirostris<- autoplot(my.pca.planirostris, x=1, y=2, data = planirostris_compare, colour = 'status', 
                            loadings = F, loadings.label = F, pch = 4, size=3)+labs(title = "E. planirostris")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))
  

pca_planirostris3<- autoplot(my.pca.planirostris, x=1, y=3, data = planirostris_compare, colour = 'status', 
                             loadings = F, loadings.label = F, pch = 4, size=3)+labs(title = "E. planirostris")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))

pca_planirostris4<- autoplot(my.pca.planirostris, x=1, y=4, data = planirostris_compare, colour = 'status', 
                             loadings = F, loadings.label = F, pch = 4, size=3)+labs(title = "E. planirostris")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))

# johnstonei PCA
my.pca.johnstonei <- prcomp(johnstonei_compare[c(7,10,13,18,20,21,24)], center=T, scale=TRUE)
summary(my.pca.johnstonei) 
str(my.pca.johnstonei)
my.pca.johnstonei$rotation
my.pca.johnstonei$rotation[,1]

# Plot PCA #
prin_comp_johnstonei <-rda(johnstonei_compare[c(7,10,13,18,20,21,24)], scale=TRUE)
pca_scores<-scores(prin_comp_johnstonei)

plot(prin_comp_johnstonei)					
johnstonei_scree<- screeplot(prin_comp_johnstonei, bstick=T, col=colours()[574])	
summary(prin_comp_johnstonei)

colors<- c("skyblue2", "darkseagreen3")
pca_johnstonei<- autoplot(my.pca.johnstonei, x=1, y=2, data = johnstonei_compare, colour = 'status', fill = "status",
                          loadings = F, loadings.label = F, pch = 4, size = 3)+labs(title = "E. johnstonei")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))





pca_johnstonei3 <- autoplot(my.pca.johnstonei, x=1, y=2, data = johnstonei_compare, colour = 'status', fill = "status",
                            loadings = F, loadings.label = F, pch = 4, size = 3)+labs(title = "E. johnstonei")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))

pca_johnstonei4<- autoplot(my.pca.johnstonei, x=1, y=2, data = johnstonei_compare, colour = 'status', fill = "status",
                           loadings = F, loadings.label = F, pch = 4, size = 3)+labs(title = "E. johnstonei")+theme_classic()+
  theme(plot.title = element_text(size = 35, face = "italic"), 
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"))+
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1"))

ggarrange(pca_coqui, pca_planirostris, pca_johnstonei, nrow=1, ncol =3, common.legend = T, legend = "right")
ggsave("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Figures/Figure2.tiff", units="in", width=25, height=15, dpi=300, compression = 'lzw')

ggarrange(pca_coqui3, pca_planirostris3, pca_johnstonei3, nrow=1, ncol =3, common.legend = T, legend = "right")
ggsave("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Figures/FigureS1.tiff", units="in", width=25, height=15, dpi=300, compression = 'lzw')

ggarrange(pca_coqui4, pca_planirostris4, pca_johnstonei4,nrow=1, ncol =3, common.legend = T, legend = "right")
ggsave("C://Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Figures/FigureS2.tiff", units="in", width=25, height=15, dpi=300, compression = 'lzw')


#### Comparisons of Native and Non-native occurence data 

## wilcox test for coqui
wilcox.test(bio1~status, coqui_compare)
wilcox.test(bio12~status, coqui_compare)
wilcox.test(bio15~status, coqui_compare)
wilcox.test(bio18~status, coqui_compare)

## wilcox test for planirostris
wilcox.test(bio1~status, planirostris_compare)
wilcox.test(bio12~status, planirostris_compare)
wilcox.test(bio15~status, planirostris_compare)
wilcox.test(bio18~status, planirostris_compare)

## wilcox test for johnstonei
wilcox.test(bio1~status, johnstonei_compare)
wilcox.test(bio12~status, johnstonei_compare)
wilcox.test(bio15~status, johnstonei_compare)
wilcox.test(bio18~status, johnstonei_compare)



### merge all together
eleutherodactylus_comparisons<- rbind(coqui_compare, planirostris_compare, johnstonei_compare)

names<- c("E. coqui",  "E. planirostris", "E. johnstonei")

bio1<- amt <- eleutherodactylus_comparisons %>% 
  ggplot(aes(x=species, y=bio1, fill=factor(status))) +
  geom_boxplot()+
  labs(x = NULL, y = "C") + 
  theme(axis.text.x = element_text(size = 25, face = "italic"),
        axis.text.y = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        plot.title = element_text(size = 35, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold")) +
  ggtitle("Annual Mean Temperature") +
  labs(fill="Status")+
  scale_x_discrete(labels = names)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))




bio12<- amt <- eleutherodactylus_comparisons %>% 
  ggplot(aes(x=species, y=bio12, fill=factor(status))) +
  geom_boxplot() +
  labs(x = NULL, y = "mm") + 
  theme(axis.text.x = element_text(size = 25, face = "italic"),
        axis.text.y = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        plot.title = element_text(size = 35, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold")) +
  ggtitle("Annual Precipitation") +
  scale_x_discrete(labels = names)+
  labs(fill="Status") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


bio15<- amt <- eleutherodactylus_comparisons %>% 
  ggplot(aes(x=species, y=bio15, fill=factor(status))) +
  geom_boxplot()+
  labs(x = NULL, y = "mm") + 
  theme(axis.text.x = element_text(size = 25, face = "italic"),
        axis.text.y = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        plot.title = element_text(size = 35, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold")) +
  ggtitle("Precipitation Seasonality") +
  labs(fill="Status") +
  scale_x_discrete(labels = names)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))




bio18<- amt <- eleutherodactylus_comparisons %>% 
  ggplot(aes(x=species, y=bio18, fill=factor(status))) +
  geom_boxplot() +
  labs(x = NULL, y = "mm") +
  scale_x_discrete(labels = names)+ 
  theme(axis.text.x = element_text(size = 25, face = "italic"),
        axis.text.y = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        plot.title = element_text(size = 35, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        legend.title = element_text(size = 30, face = "bold")) +
  ggtitle("Precipitation of the Warmest Quarter") +
  labs(fill="Status") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))




ggarrange(bio1, bio12,bio15, bio18, nrow = 2, ncol=2, common.legend = T, legend="right")

ggsave("C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Manuscript/Figures/Figure3.tiff", width = 25, height = 25, dpi = 300, compression = 'lzw')
dev.off()





