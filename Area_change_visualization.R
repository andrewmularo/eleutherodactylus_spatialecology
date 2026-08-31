library(tidyverse)
library(ggpubr)

wd<- "C:/Users/andre/OneDrive/Documents/Bernal Lab/Jack_Kirkwood/Eleutherodactylus_Redo_2023_2024/Version2/ENM/Results/ToMularo"

setwd(wd)


data<- read.csv("all_species_area_change_revised.csv")

data_coqui<- data %>% filter(species == "E.coqui")
data_planristris<- data %>% filter(species == "E.planirostris")
data_johnstonei<- data %>% filter(species == "E.johnstonei")
data_antillensis<- data %>% filter(species == "E.antillensis")
data_martinicensis<- data %>% filter(species == "E.martinicensis")


p1<- ggplot(data_coqui, aes(x=buffer, y=area_km2, group= scenario, color = scenario))+geom_point()+
  geom_line()+theme_classic()+ylim(0,135000)+
  ylab("Area (km2)")+xlab("Buffer Size")+ggtitle("Eleutherodactylus coqui")+theme(plot.title = element_text(face = "italic", size = 10))
p2<- ggplot(data_planristris, aes(x=buffer, y=area_km2, group= scenario, color = scenario))+geom_point()+
  geom_line()+theme_classic()+ylim(0,790000)+
  ylab("Area (km2)")+xlab("Buffer Size")+ggtitle("Eleutherodactylus planirostris")+theme(plot.title = element_text(face = "italic", size = 10))
p3<- ggplot(data_johnstonei, aes(x=buffer, y=area_km2, group= scenario, color = scenario))+geom_point()+
  geom_line()+theme_classic()+ylim(0,135000)+
  ylab("Area (km2)")+xlab("Buffer Size")+ggtitle("Eleutherodactylus johnstonei")+theme(plot.title = element_text(face = "italic", size = 10))
p4<- ggplot(data_antillensis, aes(x=buffer, y=area_km2, group= scenario, color = scenario))+geom_point()+
  geom_line()+theme_classic()+ylim(0,135000)+
  ylab("Area (km2)")+xlab("Buffer Size")+ggtitle("Eleutherodactylus antillensis")+theme(plot.title = element_text(face = "italic", size = 10))
p5<- ggplot(data_martinicensis, aes(x=buffer, y=area_km2, group= scenario, color = scenario))+geom_point()+
  geom_line()+theme_classic()+ylim(0,135000)+
  ylab("Area (km2)")+xlab("Buffer Size")+ggtitle("Eleutherodactylus martinicensis")+theme(plot.title = element_text(face = "italic", size = 10))

ggarrange(p1,p2,p3,p4,p5, common.legend = T, legend = "right")



