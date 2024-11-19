library(dismo)
library(tidyverse)
library(vegan)
library(ggplot2)
library(plotly) ## DOUBLE CHECK IF NEEDED
library(devtools)
library(ggpubr)
library(ggfortify)
library(psych)


wd<- "C://Users/andre/OneDrive/Documents/ArcGIS/Projects/Eleutherodactylus_REDO"

setwd(wd)


data_final<- read.csv("concatenated_climatedata.csv")

climate_function <- function(a){
  tmin = as.numeric(c(a[3:14]))
  tmax = as.numeric(c(a[15:26]))
  prec = as.numeric(c(a[27:38]))
  return(biovars(prec=prec, tmin=tmin, tmax=tmax))
}

## biovars function applied to all 4167 datapoints
biovar<- apply(data_final, 1, climate_function)

cols <- c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19")

## transpose matrix and add names for each bioclimate variable 
final <- setNames(as.data.frame(t(biovar)), cols)
## bind the GBIFID, Object ID, specis and region to the dataset
final_binded <- cbind(c(data_final[1:2], data_final[,39:42]), final) #adds back the orginals Ids to the front of the dataframe

### separate out each species

coqui_bioclim<- final_binded %>% filter(species == "Eleutherodactylus coqui")
planirostris_bioclim<- final_binded %>% filter(species == "Eleutherodactylus planirostris")
johnstonei_bioclim<- final_binded %>% filter(species == "Eleutherodactylus johnstonei")



## Autocorrelation and PCA for each species 

## Autocorrelation for all aggregated data

corr_data <- final_binded[c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")] # grouping all independent continuous factors into a single object to facilitate running the correlations
corr <- corr.test(corr_data, use = "pairwise", method = "pearson", adjust = "none") # runs the pairwise correlations between the independent continuous factors.
write.csv(corr$r, "autocorrelation_bioclimate.csv")


##Autocorrelation for each species 
corr_coqui <- coqui_bioclim[c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")] # grouping all independent continuous factors into a single object to facilitate running the correlations
coqui_corr <- corr.test(corr_coqui, use = "pairwise", method = "pearson", adjust = "none") # runs the pairwise correlations between the independent continuous factors.
write.csv(coqui_corr$r, "autocorrelation_coqui_bioclimate.csv")

corr_planirostris <- planirostris_bioclim[c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")] # grouping all independent continuous factors into a single object to facilitate running the correlations
planirostris_corr <- corr.test(corr_planirostris, use = "pairwise", method = "pearson", adjust = "none") # runs the pairwise correlations between the independent continuous factors.
write.csv(planirostris_corr$r, "autocorrelation_planirostris_bioclimate.csv")

corr_johnstonei <- johnstonei_bioclim[c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")] # grouping all independent continuous factors into a single object to facilitate running the correlations
johnstonei_corr <- corr.test(corr_johnstonei, use = "pairwise", method = "pearson", adjust = "none") # runs the pairwise correlations between the independent continuous factors.
write.csv(johnstonei_corr$r, "autocorrelation_johnstonei_bioclimate.csv")



### Create "status" variable - native or non-native, for each species
unique(coqui_bioclim$countryCode)
coqui_native<- coqui_bioclim %>% filter(countryCode == "PR") %>% mutate(status = "Native")
coqui_invasive<- coqui_bioclim %>% filter(countryCode %in% c("US", "CR", "VI")) %>% mutate(status = "Non-Native")
coqui_compare<- rbind(coqui_native, coqui_invasive)


unique(planirostris_bioclim$countryCode)
planirostris_native<- planirostris_bioclim %>% filter(countryCode %in% c("KY", "BS", "CU")) %>% mutate(status = "Native")
planirostris_invasive<- planirostris_bioclim %>% filter(countryCode %in% c("BR", "US", "MX","PA","HK","GU","JM","TC","PH","CN","HN","SV","SG","MO","SX","CO","TW")) %>% mutate(status = "Non-Native")
planirostris_compare<- rbind(planirostris_native, planirostris_invasive)



unique(johnstonei_bioclim$countryCode)
johnstonei_native<- johnstonei_bioclim %>% filter(countryCode %in% c("LC", "MQ", "AG", "BQ", "MF", "KN", "VC", "MS", "SX")) %>% mutate(status = "Native")
## Note, all AG samples from 2010-2021 are native Antigua
## Some Saint Vincent and Grenadines were native, some were non-native 

johnstonei_invasive<- johnstonei_bioclim %>% filter(countryCode %in% c("ZZ", "JM", "TT", "CW", "GP", "CO","BB", "BR","GD", "DE", "GY", "BM", "VE", "GF", "DM")) %>%  mutate(status = "Non-Native")
## All ZZ labels were from Curacao, so they are designated as non-native

johnstonei_compare<- rbind(johnstonei_native, johnstonei_invasive)



### set working directory to export figures
wd<- "C://Users/andre/OneDrive/Documents/ArcGIS/Projects/Eleutherodactylus_REDO/figures"

setwd(wd)



# Coqui PCA
my.pca.coqui <- prcomp(coqui_compare[c(7:10,18:21,24,25)], center=T, scale=TRUE)
summary(my.pca.coqui) 
str(my.pca.coqui)
my.pca.coqui$rotation
my.pca.coqui$rotation[,1]

# Plot PCA #
prin_comp_coqui <-rda(coqui_compare[c(7:10,18:21,24,25)], center = T, scale=TRUE)
pca_scores<-scores(prin_comp_coqui)

plot(prin_comp_coqui)


coqui_scree<- screeplot(my.pca.coqui, bstick=T, col=colours()[574])
summary(prin_comp_coqui)



pca_coqui<- autoplot(my.pca.coqui, x=1, y = 2, data = coqui_compare, colour = "status", 
         loadings = T, loadings.label = T, pch = 4)+labs(title = "Eleutherodactylus coqui")

pca_coqui3<- autoplot(my.pca.coqui, x=1, y = 3, data = coqui_compare, colour = "status", 
                     loadings = T, loadings.label = T, pch = 4)+labs(title = "Eleutherodactylus coqui")

pca_coqui4<- autoplot(my.pca.coqui, x=1, y = 4, data = coqui_compare, colour = "status", 
                     loadings = T, loadings.label = T, pch = 4)+labs(title = "Eleutherodactylus coqui")



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
         loadings = T, loadings.label = T, pch = 4, title="Eleutherodactylus planirostris")+labs(title = "Eleutherodactylus planirostris")

pca_planirostris3<- autoplot(my.pca.planirostris, x=1, y=3, data = planirostris_compare, colour = 'status', 
                            loadings = T, loadings.label = T, pch = 4, title="Eleutherodactylus planirostris")+labs(title = "Eleutherodactylus planirostris")

pca_planirostris4<- autoplot(my.pca.planirostris, x=1, y=4, data = planirostris_compare, colour = 'status', 
                            loadings = T, loadings.label = T, pch = 4, title="Eleutherodactylus planirostris")+labs(title = "Eleutherodactylus planirostris")


# johnstonei PCA
my.pca.johnstonei <- prcomp(johnstonei_compare[c(7,9,18,21,24)], center=T, scale=TRUE)
summary(my.pca.johnstonei) 
str(my.pca.johnstonei)
my.pca.johnstonei$rotation
my.pca.johnstonei$rotation[,1]

# Plot PCA #
prin_comp_johnstonei <-rda(johnstonei_compare[c(7,9,18,21,24)], scale=TRUE)
pca_scores<-scores(prin_comp_johnstonei)

plot(prin_comp_johnstonei)					
johnstonei_scree<- screeplot(prin_comp_johnstonei, bstick=T, col=colours()[574])	
summary(prin_comp_johnstonei)

pca_johnstonei<- autoplot(my.pca.johnstonei, x=1, y=2, data = johnstonei_compare, colour = 'status', 
         loadings = T, loadings.label = T, pch = 4)+labs(title = "Eleutherodactylus johnstonei")

pca_johnstonei3<- autoplot(my.pca.johnstonei, x=1,y=3, data = johnstonei_compare, colour = 'status', 
                          loadings = T, loadings.label = T, pch = 4)+labs(title = "Eleutherodactylus johnstonei")

pca_johnstonei4<- autoplot(my.pca.johnstonei, x=1, y=4, data = johnstonei_compare, colour = 'status', 
                          loadings = T, loadings.label = T, pch = 4)+labs(title = "Eleutherodactylus johnstonei")


ggarrange(pca_coqui, pca_planirostris, pca_johnstonei)

ggarrange(pca_coqui3, pca_planirostris3, pca_johnstonei3)

ggarrange(pca_coqui4, pca_planirostris4, pca_johnstonei4)

#### Comparisons of Native and Non-native occurence data 

## wilcox test for coqui
wilcox.test(bio1~status, coqui_compare)
wilcox.test(bio3~status, coqui_compare)
wilcox.test(bio12~status, coqui_compare)
wilcox.test(bio18~status, coqui_compare)

## wilcox test for planirostris
wilcox.test(bio1~status, planirostris_compare)
wilcox.test(bio3~status, planirostris_compare)
wilcox.test(bio12~status, planirostris_compare)
wilcox.test(bio18~status, planirostris_compare)

## wilcox test for johnstonei
wilcox.test(bio1~status, johnstonei_compare)
wilcox.test(bio3~status, johnstonei_compare)
wilcox.test(bio12~status, johnstonei_compare)
wilcox.test(bio18~status, johnstonei_compare)


### merge all together
eleutherodactylus_comparisons<- rbind(coqui_compare, planirostris_compare, johnstonei_compare)


bio1<- amt <- eleutherodactylus_comparisons %>% 
  ggplot(aes(x=species, y=bio1, fill=factor(status))) +
  geom_boxplot() +
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1")) +
  labs(x = NULL, y = "Annual Mean Temperature") + 
  theme(axis.text.x = element_text(size = 8, family = "Times New Roman", face = "italic")) +
  ggtitle("Annual Mean Temperature") +
  labs(fill="Status") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bio1


bio3<- amt <- eleutherodactylus_comparisons %>% 
  ggplot(aes(x=species, y=bio3, fill=factor(status))) +
  geom_boxplot() +
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1")) +
  labs(x = NULL, y = "Isothermality") + 
  theme(axis.text.x = element_text(size = 8, family = "Times New Roman", face = "italic")) +
  ggtitle("Isothermality") +
  labs(fill="Status") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

bio3

bio12<- amt <- eleutherodactylus_comparisons %>% 
  ggplot(aes(x=species, y=bio12, fill=factor(status))) +
  geom_boxplot() +
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1")) +
  labs(x = NULL, y = "Annual Precipitation") + 
  theme(axis.text.x = element_text(size = 8, family = "Times New Roman", face = "italic")) +
  ggtitle("Annual Precipitation") +
  labs(fill="Status") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

bio12

bio18<- amt <- eleutherodactylus_comparisons %>% 
  ggplot(aes(x=species, y=bio18, fill=factor(status))) +
  geom_boxplot() +
  scale_fill_manual(values=c("skyblue2", "darkseagreen3", "khaki1")) +
  labs(x = NULL, y = "Precipitation of the Warmest Quarter") + 
  theme(axis.text.x = element_text(size = 8, family = "Times New Roman", face = "italic")) +
  ggtitle("Precipitation of the Warmest Quarter") +
  labs(fill="Status") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

bio18

png("boxplot_comparisons.png", width = 100, height = 200)
ggarrange(bio1, bio3, bio12, bio18)
dev.off()






#### Separate out to only established individuals

coqui_compare<- coqui_compare %>% filter(countryCode==c("PR", "VI", "US"))

my.pca.coqui <- prcomp(coqui_compare[c(7:10,18:21,24,25)], center=T, scale=TRUE)
summary(my.pca.coqui) 
str(my.pca.coqui)
my.pca.coqui$rotation
my.pca.coqui$rotation[,1]

# Plot PCA #
prin_comp_coqui <-rda(coqui_compare[c(7:10,18:21,24,25)], center = T, scale=TRUE)
pca_scores<-scores(prin_comp_coqui)

plot(prin_comp_coqui)


coqui_scree<- screeplot(my.pca.coqui, bstick=T, col=colours()[574])
summary(prin_comp_coqui)



pca_coqui<- autoplot(my.pca.coqui, x=1, y = 2, data = coqui_compare, colour = "status", 
                     loadings = T, loadings.label = T, pch = 4)+labs(title = "Eleutherodactylus coqui")



### Planirostris 

planirostris_compare<- planirostris_compare %>% filter(countryCode==c("HK", "CN", "GU","HN","JM","MX","PA","PH", "US","KY", "BS", "CU"))

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
                            loadings = T, loadings.label = T, pch = 4, title="Eleutherodactylus planirostris")+labs(title = "Eleutherodactylus planirostris")


### JOHNSTONEI

johnstonei_compare<- johnstonei_compare %>% filter(countryCode==c("BR","BM","CO","CW","GF","GY","JM","TT","VE","LC", "MQ", "AG", "BQ", "MF", "KN", "VC", "MS", "SX"))


my.pca.johnstonei <- prcomp(johnstonei_compare[c(7,9,18,21,24)], center=T, scale=TRUE)
summary(my.pca.johnstonei) 
str(my.pca.johnstonei)
my.pca.johnstonei$rotation
my.pca.johnstonei$rotation[,1]

# Plot PCA #
prin_comp_johnstonei <-rda(johnstonei_compare[c(7,9,18,21,24)], scale=TRUE)
pca_scores<-scores(prin_comp_johnstonei)

plot(prin_comp_johnstonei)					
johnstonei_scree<- screeplot(prin_comp_johnstonei, bstick=T, col=colours()[574])	
summary(prin_comp_johnstonei)

pca_johnstonei<- autoplot(my.pca.johnstonei, x=1, y=2, data = johnstonei_compare, colour = 'status', 
                          loadings = T, loadings.label = T, pch = 4)+labs(title = "Eleutherodactylus johnstonei")

ggarrange(pca_coqui, pca_planirostris, pca_johnstonei)
