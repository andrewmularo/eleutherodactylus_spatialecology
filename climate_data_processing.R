
library(tidyverse)
library(readxl)
library(dismo)

wd<- "C://Users/andre/OneDrive/Documents/ArcGIS/Projects/Eleutherodactylus_REDO"

setwd(wd)

#### Load in all data 

buffer2010<- read_xls("eleutherodactylus_gbif_2010_NARemoved.xls")
buffer2011<- read_xls("eleutherodactylus_gbif_2011_NARemoved.xls")
buffer2012<- read_xls("eleutherodactylus_gbif_2012_NARemoved.xls")
buffer2013<- read_xls("eleutherodactylus_gbif_2013_NARemoved.xls")
buffer2014<- read_xls("eleutherodactylus_gbif_2014_NARemoved.xls")
buffer2015<- read_xls("eleutherodactylus_gbif_2015_NARemoved.xls")
buffer2016<- read_xls("eleutherodactylus_gbif_2016_NARemoved.xls")
buffer2017<- read_xls("eleutherodactylus_gbif_2017_NARemoved.xls")
buffer2018<- read_xls("eleutherodactylus_gbif_2018_NARemoved.xls")
buffer2019<- read_xls("eleutherodactylus_gbif_2019_NARemoved.xls")
buffer2020<- read_xls("eleutherodactylus_gbif_2020_NARemoved.xls")
buffer2021<- read_xls("eleutherodactylus_gbif_2021_NARemoved.xls")


buffer2010_ID<- buffer2010 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2011_ID<- buffer2011 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2012_ID<- buffer2012 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2013_ID<- buffer2013 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2014_ID<- buffer2014 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2015_ID<- buffer2015 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2016_ID<- buffer2016 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2017_ID<- buffer2017 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2018_ID<- buffer2018 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2019_ID<- buffer2019 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2020_ID<- buffer2020 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))
buffer2021_ID<- buffer2021 %>% dplyr::select(c("OBJECTID", "gbifID", "countryCode", "species", "year"))

#### 2010 data

tmax2010_01<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_01.xls")
tmax2010_02<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_02.xls")
tmax2010_03<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_03.xls")
tmax2010_04<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_04.xls")
tmax2010_05<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_05.xls")
tmax2010_06<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_06.xls")
tmax2010_07<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_07.xls")
tmax2010_08<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_08.xls")
tmax2010_09<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_09.xls")
tmax2010_10<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_10.xls")
tmax2010_11<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_11.xls")
tmax2010_12<- read_xls("2010/Maximum_Temperature_Resample_Resample_OutRaster_Maximum_Temperature_Clip_Clip_OutRaster_Maximum_Temperature_wc2_1_10m_tmax_2010_12.xls")
tmin2010_01<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_01.xls")
tmin2010_02<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_02.xls")
tmin2010_03<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_03.xls")
tmin2010_04<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_04.xls")
tmin2010_05<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_05.xls")
tmin2010_06<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_06.xls")
tmin2010_07<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_07.xls")
tmin2010_08<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_08.xls")
tmin2010_09<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_09.xls")
tmin2010_10<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_10.xls")
tmin2010_11<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_11.xls")
tmin2010_12<- read_xls("2010/Minimum_Temperature_Resample_Resample_OutRaster_Minimum_Temperature_Clip_Clip_OutRaster_Minimum_Temperature_wc2_1_10m_tmin_2010_12.xls")
prec2010_01<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_01.xls")
prec2010_02<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_02.xls")
prec2010_03<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_03.xls")
prec2010_04<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_04.xls")
prec2010_05<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_05.xls")
prec2010_06<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_06.xls")
prec2010_07<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_07.xls")
prec2010_08<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_08.xls")
prec2010_09<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_09.xls")
prec2010_10<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_10.xls")
prec2010_11<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_11.xls")
prec2010_12<- read_xls("2010/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2010_12.xls")


## 2011
prec2011_01<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_01.xls")
prec2011_02<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_02.xls")
prec2011_03<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_03.xls")
prec2011_04<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_04.xls")
prec2011_05<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_05.xls")
prec2011_06<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_06.xls")
prec2011_07<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_07.xls")
prec2011_08<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_08.xls")
prec2011_09<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_09.xls")
prec2011_10<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_10.xls")
prec2011_11<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_11.xls")
prec2011_12<- read_xls("2011/Precipitation_Resample_Resample_OutRaster_Precipitation_Clip_Clip_OutRaster_Precipitation_wc2_1_10m_prec_2011_12.xls")

tmax2011_01<- read_xls("2011/tmax_2011_01.xls")
tmax2011_02<- read_xls("2011/tmax_2011_02.xls")
tmax2011_03<- read_xls("2011/tmax_2011_03.xls")
tmax2011_04<- read_xls("2011/tmax_2011_04.xls")
tmax2011_05<- read_xls("2011/tmax_2011_05.xls")
tmax2011_06<- read_xls("2011/tmax_2011_06.xls")
tmax2011_07<- read_xls("2011/tmax_2011_07.xls")
tmax2011_08<- read_xls("2011/tmax_2011_08.xls")
tmax2011_09<- read_xls("2011/tmax_2011_09.xls")
tmax2011_10<- read_xls("2011/tmax_2011_10.xls")
tmax2011_11<- read_xls("2011/tmax_2011_11.xls")
tmax2011_12<- read_xls("2011/tmax_2011_12.xls")
tmin2011_01<- read_xls("2011/tmin_2011_01.xls")
tmin2011_02<- read_xls("2011/tmin_2011_02.xls")
tmin2011_03<- read_xls("2011/tmin_2011_03.xls")
tmin2011_04<- read_xls("2011/tmin_2011_04.xls")
tmin2011_05<- read_xls("2011/tmin_2011_05.xls")
tmin2011_06<- read_xls("2011/tmin_2011_06.xls")
tmin2011_07<- read_xls("2011/tmin_2011_07.xls")
tmin2011_08<- read_xls("2011/tmin_2011_08.xls")
tmin2011_09<- read_xls("2011/tmin_2011_09.xls")
tmin2011_10<- read_xls("2011/tmin_2011_10.xls")
tmin2011_11<- read_xls("2011/tmin_2011_11.xls")
tmin2011_12<- read_xls("2011/tmin_2011_12.xls")

## 2012
prec2012_01<- read_xls("2012/prec_2012_01.xls")
prec2012_02<- read_xls("2012/prec_2012_02.xls")
prec2012_03<- read_xls("2012/prec_2012_03.xls")
prec2012_04<- read_xls("2012/prec_2012_04.xls")
prec2012_05<- read_xls("2012/prec_2012_05.xls")
prec2012_06<- read_xls("2012/prec_2012_06.xls")
prec2012_07<- read_xls("2012/prec_2012_07.xls")
prec2012_08<- read_xls("2012/prec_2012_08.xls")
prec2012_09<- read_xls("2012/prec_2012_09.xls")
prec2012_10<- read_xls("2012/prec_2012_10.xls")
prec2012_11<- read_xls("2012/prec_2012_11.xls")
prec2012_12<- read_xls("2012/prec_2012_12.xls")
tmax2012_01<- read_xls("2012/tmax_2012_01.xls")
tmax2012_02<- read_xls("2012/tmax_2012_02.xls")
tmax2012_03<- read_xls("2012/tmax_2012_03.xls")
tmax2012_04<- read_xls("2012/tmax_2012_04.xls")
tmax2012_05<- read_xls("2012/tmax_2012_05.xls")
tmax2012_06<- read_xls("2012/tmax_2012_06.xls")
tmax2012_07<- read_xls("2012/tmax_2012_07.xls")
tmax2012_08<- read_xls("2012/tmax_2012_08.xls")
tmax2012_09<- read_xls("2012/tmax_2012_09.xls")
tmax2012_10<- read_xls("2012/tmax_2012_10.xls")
tmax2012_11<- read_xls("2012/tmax_2012_11.xls")
tmax2012_12<- read_xls("2012/tmax_2012_12.xls")
tmin2012_01<- read_xls("2012/tmin_2012_01.xls")
tmin2012_02<- read_xls("2012/tmin_2012_02.xls")
tmin2012_03<- read_xls("2012/tmin_2012_03.xls")
tmin2012_04<- read_xls("2012/tmin_2012_04.xls")
tmin2012_05<- read_xls("2012/tmin_2012_05.xls")
tmin2012_06<- read_xls("2012/tmin_2012_06.xls")
tmin2012_07<- read_xls("2012/tmin_2012_07.xls")
tmin2012_08<- read_xls("2012/tmin_2012_08.xls")
tmin2012_09<- read_xls("2012/tmin_2012_09.xls")
tmin2012_10<- read_xls("2012/tmin_2012_10.xls")
tmin2012_11<- read_xls("2012/tmin_2012_11.xls")
tmin2012_12<- read_xls("2012/tmin_2012_12.xls")

## 2013
prec2013_01<- read_xls("2013/prec_2013_01.xls")
prec2013_02<- read_xls("2013/prec_2013_02.xls")
prec2013_03<- read_xls("2013/prec_2013_03.xls")
prec2013_04<- read_xls("2013/prec_2013_04.xls")
prec2013_05<- read_xls("2013/prec_2013_05.xls")
prec2013_06<- read_xls("2013/prec_2013_06.xls")
prec2013_07<- read_xls("2013/prec_2013_07.xls")
prec2013_08<- read_xls("2013/prec_2013_08.xls")
prec2013_09<- read_xls("2013/prec_2013_09.xls")
prec2013_10<- read_xls("2013/prec_2013_10.xls")
prec2013_11<- read_xls("2013/prec_2013_11.xls")
prec2013_12<- read_xls("2013/prec_2013_12.xls")
tmax2013_01<- read_xls("2013/tmax_2013_01.xls")
tmax2013_02<- read_xls("2013/tmax_2013_02.xls")
tmax2013_03<- read_xls("2013/tmax_2013_03.xls")
tmax2013_04<- read_xls("2013/tmax_2013_04.xls")
tmax2013_05<- read_xls("2013/tmax_2013_05.xls")
tmax2013_06<- read_xls("2013/tmax_2013_06.xls")
tmax2013_07<- read_xls("2013/tmax_2013_07.xls")
tmax2013_08<- read_xls("2013/tmax_2013_08.xls")
tmax2013_09<- read_xls("2013/tmax_2013_09.xls")
tmax2013_10<- read_xls("2013/tmax_2013_10.xls")
tmax2013_11<- read_xls("2013/tmax_2013_11.xls")
tmax2013_12<- read_xls("2013/tmax_2013_12.xls")
tmin2013_01<- read_xls("2013/tmin_2013_01.xls")
tmin2013_02<- read_xls("2013/tmin_2013_02.xls")
tmin2013_03<- read_xls("2013/tmin_2013_03.xls")
tmin2013_04<- read_xls("2013/tmin_2013_04.xls")
tmin2013_05<- read_xls("2013/tmin_2013_05.xls")
tmin2013_06<- read_xls("2013/tmin_2013_06.xls")
tmin2013_07<- read_xls("2013/tmin_2013_07.xls")
tmin2013_08<- read_xls("2013/tmin_2013_08.xls")
tmin2013_09<- read_xls("2013/tmin_2013_09.xls")
tmin2013_10<- read_xls("2013/tmin_2013_10.xls")
tmin2013_11<- read_xls("2013/tmin_2013_11.xls")
tmin2013_12<- read_xls("2013/tmin_2013_12.xls")

#2014 
prec2014_01<- read_xls("2014/prec_2014_01.xls")
prec2014_02<- read_xls("2014/prec_2014_02.xls")
prec2014_03<- read_xls("2014/prec_2014_03.xls")
prec2014_04<- read_xls("2014/prec_2014_04.xls")
prec2014_05<- read_xls("2014/prec_2014_05.xls")
prec2014_06<- read_xls("2014/prec_2014_06.xls")
prec2014_07<- read_xls("2014/prec_2014_07.xls")
prec2014_08<- read_xls("2014/prec_2014_08.xls")
prec2014_09<- read_xls("2014/prec_2014_09.xls")
prec2014_10<- read_xls("2014/prec_2014_10.xls")
prec2014_11<- read_xls("2014/prec_2014_11.xls")
prec2014_12<- read_xls("2014/prec_2014_12.xls")
tmax2014_01<- read_xls("2014/tmax_2014_01.xls")
tmax2014_02<- read_xls("2014/tmax_2014_02.xls")
tmax2014_03<- read_xls("2014/tmax_2014_03.xls")
tmax2014_04<- read_xls("2014/tmax_2014_04.xls")
tmax2014_05<- read_xls("2014/tmax_2014_05.xls")
tmax2014_06<- read_xls("2014/tmax_2014_06.xls")
tmax2014_07<- read_xls("2014/tmax_2014_07.xls")
tmax2014_08<- read_xls("2014/tmax_2014_08.xls")
tmax2014_09<- read_xls("2014/tmax_2014_09.xls")
tmax2014_10<- read_xls("2014/tmax_2014_10.xls")
tmax2014_11<- read_xls("2014/tmax_2014_11.xls")
tmax2014_12<- read_xls("2014/tmax_2014_12.xls")
tmin2014_01<- read_xls("2014/tmin_2014_01.xls")
tmin2014_02<- read_xls("2014/tmin_2014_02.xls")
tmin2014_03<- read_xls("2014/tmin_2014_03.xls")
tmin2014_04<- read_xls("2014/tmin_2014_04.xls")
tmin2014_05<- read_xls("2014/tmin_2014_05.xls")
tmin2014_06<- read_xls("2014/tmin_2014_06.xls")
tmin2014_07<- read_xls("2014/tmin_2014_07.xls")
tmin2014_08<- read_xls("2014/tmin_2014_08.xls")
tmin2014_09<- read_xls("2014/tmin_2014_09.xls")
tmin2014_10<- read_xls("2014/tmin_2014_10.xls")
tmin2014_11<- read_xls("2014/tmin_2014_11.xls")
tmin2014_12<- read_xls("2014/tmin_2014_12.xls")


## 2015 
prec2015_01<- read_xlsx("2015/prec_2015_01.xlsx")
prec2015_02<- read_xlsx("2015/prec_2015_02.xlsx")
prec2015_03<- read_xlsx("2015/prec_2015_03.xlsx")
prec2015_04<- read_xlsx("2015/prec_2015_04.xlsx")
prec2015_05<- read_xlsx("2015/prec_2015_05.xlsx")
prec2015_06<- read_xlsx("2015/prec_2015_06.xlsx")
prec2015_07<- read_xlsx("2015/prec_2015_07.xlsx")
prec2015_08<- read_xlsx("2015/prec_2015_08.xlsx")
prec2015_09<- read_xlsx("2015/prec_2015_09.xlsx")
prec2015_10<- read_xlsx("2015/prec_2015_10.xlsx")
prec2015_11<- read_xlsx("2015/prec_2015_11.xlsx")
prec2015_12<- read_xlsx("2015/prec_2015_12.xlsx")
tmax2015_01<- read_xlsx("2015/tmax_2015_01.xlsx")
tmax2015_02<- read_xlsx("2015/tmax_2015_02.xlsx")
tmax2015_03<- read_xlsx("2015/tmax_2015_03.xlsx")
tmax2015_04<- read_xlsx("2015/tmax_2015_04.xlsx")
tmax2015_05<- read_xlsx("2015/tmax_2015_05.xlsx")
tmax2015_06<- read_xlsx("2015/tmax_2015_06.xlsx")
tmax2015_07<- read_xlsx("2015/tmax_2015_07.xlsx")
tmax2015_08<- read_xlsx("2015/tmax_2015_08.xlsx")
tmax2015_09<- read_xlsx("2015/tmax_2015_09.xlsx")
tmax2015_10<- read_xlsx("2015/tmax_2015_10.xlsx")
tmax2015_11<- read_xlsx("2015/tmax_2015_11.xlsx")
tmax2015_12<- read_xlsx("2015/tmax_2015_12.xlsx")
tmin2015_01<- read_xlsx("2015/tmin_2015_01.xlsx")
tmin2015_02<- read_xlsx("2015/tmin_2015_02.xlsx")
tmin2015_03<- read_xlsx("2015/tmin_2015_03.xlsx")
tmin2015_04<- read_xlsx("2015/tmin_2015_04.xlsx")
tmin2015_05<- read_xlsx("2015/tmin_2015_05.xlsx")
tmin2015_06<- read_xlsx("2015/tmin_2015_06.xlsx")
tmin2015_07<- read_xlsx("2015/tmin_2015_07.xlsx")
tmin2015_08<- read_xlsx("2015/tmin_2015_08.xlsx")
tmin2015_09<- read_xlsx("2015/tmin_2015_09.xlsx")
tmin2015_10<- read_xlsx("2015/tmin_2015_10.xlsx")
tmin2015_11<- read_xlsx("2015/tmin_2015_11.xlsx")
tmin2015_12<- read_xlsx("2015/tmin_2015_12.xlsx")

## 2016
prec2016_01<- read_xls("2016/prec_2016_01.xls")
prec2016_02<- read_xls("2016/prec_2016_02.xls")
prec2016_03<- read_xls("2016/prec_2016_03.xls")
prec2016_04<- read_xls("2016/prec_2016_04.xls")
prec2016_05<- read_xls("2016/prec_2016_05.xls")
prec2016_06<- read_xls("2016/prec_2016_06.xls")
prec2016_07<- read_xls("2016/prec_2016_07.xls")
prec2016_08<- read_xls("2016/prec_2016_08.xls")
prec2016_09<- read_xls("2016/prec_2016_09.xls")
prec2016_10<- read_xls("2016/prec_2016_10.xls")
prec2016_11<- read_xls("2016/prec_2016_11.xls")
prec2016_12<- read_xls("2016/prec_2016_12.xls")
tmax2016_01<- read_xls("2016/tmax_2016_01.xls")
tmax2016_02<- read_xls("2016/tmax_2016_02.xls")
tmax2016_03<- read_xls("2016/tmax_2016_03.xls")
tmax2016_04<- read_xls("2016/tmax_2016_04.xls")
tmax2016_05<- read_xls("2016/tmax_2016_05.xls")
tmax2016_06<- read_xls("2016/tmax_2016_06.xls")
tmax2016_07<- read_xls("2016/tmax_2016_07.xls")
tmax2016_08<- read_xls("2016/tmax_2016_08.xls")
tmax2016_09<- read_xls("2016/tmax_2016_09.xls")
tmax2016_10<- read_xls("2016/tmax_2016_10.xls")
tmax2016_11<- read_xls("2016/tmax_2016_11.xls")
tmax2016_12<- read_xls("2016/tmax_2016_12.xls")
tmin2016_01<- read_xls("2016/tmin_2016_01.xls")
tmin2016_02<- read_xls("2016/tmin_2016_02.xls")
tmin2016_03<- read_xls("2016/tmin_2016_03.xls")
tmin2016_04<- read_xls("2016/tmin_2016_04.xls")
tmin2016_05<- read_xls("2016/tmin_2016_05.xls")
tmin2016_06<- read_xls("2016/tmin_2016_06.xls")
tmin2016_07<- read_xls("2016/tmin_2016_07.xls")
tmin2016_08<- read_xls("2016/tmin_2016_08.xls")
tmin2016_09<- read_xls("2016/tmin_2016_09.xls")
tmin2016_10<- read_xls("2016/tmin_2016_10.xls")
tmin2016_11<- read_xls("2016/tmin_2016_11.xls")
tmin2016_12<- read_xls("2016/tmin_2016_12.xls")

#2017 
prec2017_01<- read_xls("2017/prec_2017_01.xls")
prec2017_02<- read_xls("2017/prec_2017_02.xls")
prec2017_03<- read_xls("2017/prec_2017_03.xls")
prec2017_04<- read_xls("2017/prec_2017_04.xls")
prec2017_05<- read_xls("2017/prec_2017_05.xls")
prec2017_06<- read_xls("2017/prec_2017_06.xls")
prec2017_07<- read_xls("2017/prec_2017_07.xls")
prec2017_08<- read_xls("2017/prec_2017_08.xls")
prec2017_09<- read_xls("2017/prec_2017_09.xls")
prec2017_10<- read_xls("2017/prec_2017_10.xls")
prec2017_11<- read_xls("2017/prec_2017_11.xls")
prec2017_12<- read_xls("2017/prec_2017_12.xls")
tmax2017_01<- read_xls("2017/tmax_2017_01.xls")
tmax2017_02<- read_xls("2017/tmax_2017_02.xls")
tmax2017_03<- read_xls("2017/tmax_2017_03.xls")
tmax2017_04<- read_xls("2017/tmax_2017_04.xls")
tmax2017_05<- read_xls("2017/tmax_2017_05.xls")
tmax2017_06<- read_xls("2017/tmax_2017_06.xls")
tmax2017_07<- read_xls("2017/tmax_2017_07.xls")
tmax2017_08<- read_xls("2017/tmax_2017_08.xls")
tmax2017_09<- read_xls("2017/tmax_2017_09.xls")
tmax2017_10<- read_xls("2017/tmax_2017_10.xls")
tmax2017_11<- read_xls("2017/tmax_2017_11.xls")
tmax2017_12<- read_xls("2017/tmax_2017_12.xls")
tmin2017_01<- read_xls("2017/tmin_2017_01.xls")
tmin2017_02<- read_xls("2017/tmin_2017_02.xls")
tmin2017_03<- read_xls("2017/tmin_2017_03.xls")
tmin2017_04<- read_xls("2017/tmin_2017_04.xls")
tmin2017_05<- read_xls("2017/tmin_2017_05.xls")
tmin2017_06<- read_xls("2017/tmin_2017_06.xls")
tmin2017_07<- read_xls("2017/tmin_2017_07.xls")
tmin2017_08<- read_xls("2017/tmin_2017_08.xls")
tmin2017_09<- read_xls("2017/tmin_2017_09.xls")
tmin2017_10<- read_xls("2017/tmin_2017_10.xls")
tmin2017_11<- read_xls("2017/tmin_2017_11.xls")
tmin2017_12<- read_xls("2017/tmin_2017_12.xls")

## 2018
prec2018_01<- read_xls("2018/prec_2018_01.xls")
prec2018_02<- read_xls("2018/prec_2018_02.xls")
prec2018_03<- read_xls("2018/prec_2018_03.xls")
prec2018_04<- read_xls("2018/prec_2018_04.xls")
prec2018_05<- read_xls("2018/prec_2018_05.xls")
prec2018_06<- read_xls("2018/prec_2018_06.xls")
prec2018_07<- read_xls("2018/prec_2018_07.xls")
prec2018_08<- read_xls("2018/prec_2018_08.xls")
prec2018_09<- read_xls("2018/prec_2018_09.xls")
prec2018_10<- read_xls("2018/prec_2018_10.xls")
prec2018_11<- read_xls("2018/prec_2018_11.xls")
prec2018_12<- read_xls("2018/prec_2018_12.xls")
tmax2018_01<- read_xls("2018/tmax_2018_01.xls")
tmax2018_02<- read_xls("2018/tmax_2018_02.xls")
tmax2018_03<- read_xls("2018/tmax_2018_03.xls")
tmax2018_04<- read_xls("2018/tmax_2018_04.xls")
tmax2018_05<- read_xls("2018/tmax_2018_05.xls")
tmax2018_06<- read_xls("2018/tmax_2018_06.xls")
tmax2018_07<- read_xls("2018/tmax_2018_07.xls")
tmax2018_08<- read_xls("2018/tmax_2018_08.xls")
tmax2018_09<- read_xls("2018/tmax_2018_09.xls")
tmax2018_10<- read_xls("2018/tmax_2018_10.xls")
tmax2018_11<- read_xls("2018/tmax_2018_11.xls")
tmax2018_12<- read_xls("2018/tmax_2018_12.xls")
tmin2018_01<- read_xls("2018/tmin_2018_01.xls")
tmin2018_02<- read_xls("2018/tmin_2018_02.xls")
tmin2018_03<- read_xls("2018/tmin_2018_03.xls")
tmin2018_04<- read_xls("2018/tmin_2018_04.xls")
tmin2018_05<- read_xls("2018/tmin_2018_05.xls")
tmin2018_06<- read_xls("2018/tmin_2018_06.xls")
tmin2018_07<- read_xls("2018/tmin_2018_07.xls")
tmin2018_08<- read_xls("2018/tmin_2018_08.xls")
tmin2018_09<- read_xls("2018/tmin_2018_09.xls")
tmin2018_10<- read_xls("2018/tmin_2018_10.xls")
tmin2018_11<- read_xls("2018/tmin_2018_11.xls")
tmin2018_12<- read_xls("2018/tmin_2018_12.xls")

## 2019
prec2019_01<- read_xls("2019/prec_2019_01.xls")
prec2019_02<- read_xls("2019/prec_2019_02.xls")
prec2019_03<- read_xls("2019/prec_2019_03.xls")
prec2019_04<- read_xls("2019/prec_2019_04.xls")
prec2019_05<- read_xls("2019/prec_2019_05.xls")
prec2019_06<- read_xls("2019/prec_2019_06.xls")
prec2019_07<- read_xls("2019/prec_2019_07.xls")
prec2019_08<- read_xls("2019/prec_2019_08.xls")
prec2019_09<- read_xls("2019/prec_2019_09.xls")
prec2019_10<- read_xls("2019/prec_2019_10.xls")
prec2019_11<- read_xls("2019/prec_2019_11.xls")
prec2019_12<- read_xls("2019/prec_2019_12.xls")
tmax2019_01<- read_xls("2019/tmax_2019_01.xls")
tmax2019_02<- read_xls("2019/tmax_2019_02.xls")
tmax2019_03<- read_xls("2019/tmax_2019_03.xls")
tmax2019_04<- read_xls("2019/tmax_2019_04.xls")
tmax2019_05<- read_xls("2019/tmax_2019_05.xls")
tmax2019_06<- read_xls("2019/tmax_2019_06.xls")
tmax2019_07<- read_xls("2019/tmax_2019_07.xls")
tmax2019_08<- read_xls("2019/tmax_2019_08.xls")
tmax2019_09<- read_xls("2019/tmax_2019_09.xls")
tmax2019_10<- read_xls("2019/tmax_2019_10.xls")
tmax2019_11<- read_xls("2019/tmax_2019_11.xls")
tmax2019_12<- read_xls("2019/tmax_2019_12.xls")
tmin2019_01<- read_xls("2019/tmin_2019_01.xls")
tmin2019_02<- read_xls("2019/tmin_2019_02.xls")
tmin2019_03<- read_xls("2019/tmin_2019_03.xls")
tmin2019_04<- read_xls("2019/tmin_2019_04.xls")
tmin2019_05<- read_xls("2019/tmin_2019_05.xls")
tmin2019_06<- read_xls("2019/tmin_2019_06.xls")
tmin2019_07<- read_xls("2019/tmin_2019_07.xls")
tmin2019_08<- read_xls("2019/tmin_2019_08.xls")
tmin2019_09<- read_xls("2019/tmin_2019_09.xls")
tmin2019_10<- read_xls("2019/tmin_2019_10.xls")
tmin2019_11<- read_xls("2019/tmin_2019_11.xls")
tmin2019_12<- read_xls("2019/tmin_2019_12.xls")

## 2020
prec2020_01<- read_xls("2020/prec_2020_01.xls")
prec2020_02<- read_xls("2020/prec_2020_02.xls")
prec2020_03<- read_xls("2020/prec_2020_03.xls")
prec2020_04<- read_xls("2020/prec_2020_04.xls")
prec2020_05<- read_xls("2020/prec_2020_05.xls")
prec2020_06<- read_xls("2020/prec_2020_06.xls")
prec2020_07<- read_xls("2020/prec_2020_07.xls")
prec2020_08<- read_xls("2020/prec_2020_08.xls")
prec2020_09<- read_xls("2020/prec_2020_09.xls")
prec2020_10<- read_xls("2020/prec_2020_10.xls")
prec2020_11<- read_xls("2020/prec_2020_11.xls")
prec2020_12<- read_xls("2020/prec_2020_12.xls")
tmax2020_01<- read_xls("2020/tmax_2020_01.xls")
tmax2020_02<- read_xls("2020/tmax_2020_02.xls")
tmax2020_03<- read_xls("2020/tmax_2020_03.xls")
tmax2020_04<- read_xls("2020/tmax_2020_04.xls")
tmax2020_05<- read_xls("2020/tmax_2020_05.xls")
tmax2020_06<- read_xls("2020/tmax_2020_06.xls")
tmax2020_07<- read_xls("2020/tmax_2020_07.xls")
tmax2020_08<- read_xls("2020/tmax_2020_08.xls")
tmax2020_09<- read_xls("2020/tmax_2020_09.xls")
tmax2020_10<- read_xls("2020/tmax_2020_10.xls")
tmax2020_11<- read_xls("2020/tmax_2020_11.xls")
tmax2020_12<- read_xls("2020/tmax_2020_12.xls")
tmin2020_01<- read_xls("2020/tmin_2020_01.xls")
tmin2020_02<- read_xls("2020/tmin_2020_02.xls")
tmin2020_03<- read_xls("2020/tmin_2020_03.xls")
tmin2020_04<- read_xls("2020/tmin_2020_04.xls")
tmin2020_05<- read_xls("2020/tmin_2020_05.xls")
tmin2020_06<- read_xls("2020/tmin_2020_06.xls")
tmin2020_07<- read_xls("2020/tmin_2020_07.xls")
tmin2020_08<- read_xls("2020/tmin_2020_08.xls")
tmin2020_09<- read_xls("2020/tmin_2020_09.xls")
tmin2020_10<- read_xls("2020/tmin_2020_10.xls")
tmin2020_11<- read_xls("2020/tmin_2020_11.xls")
tmin2020_12<- read_xls("2020/tmin_2020_12.xls")

## 2021
prec2021_01<- read_xls("2021/prec_2021_01.xls")
prec2021_02<- read_xls("2021/prec_2021_02.xls")
prec2021_03<- read_xls("2021/prec_2021_03.xls")
prec2021_04<- read_xls("2021/prec_2021_04.xls")
prec2021_05<- read_xls("2021/prec_2021_05.xls")
prec2021_06<- read_xls("2021/prec_2021_06.xls")
prec2021_07<- read_xls("2021/prec_2021_07.xls")
prec2021_08<- read_xls("2021/prec_2021_08.xls")
prec2021_09<- read_xls("2021/prec_2021_09.xls")
prec2021_10<- read_xls("2021/prec_2021_10.xls")
prec2021_11<- read_xls("2021/prec_2021_11.xls")
prec2021_12<- read_xls("2021/prec_2021_12.xls")
tmax2021_01<- read_xls("2021/tmax_2021_01.xls")
tmax2021_02<- read_xls("2021/tmax_2021_02.xls")
tmax2021_03<- read_xls("2021/tmax_2021_03.xls")
tmax2021_04<- read_xls("2021/tmax_2021_04.xls")
tmax2021_05<- read_xls("2021/tmax_2021_05.xls")
tmax2021_06<- read_xls("2021/tmax_2021_06.xls")
tmax2021_07<- read_xls("2021/tmax_2021_07.xls")
tmax2021_08<- read_xls("2021/tmax_2021_08.xls")
tmax2021_09<- read_xls("2021/tmax_2021_09.xls")
tmax2021_10<- read_xls("2021/tmax_2021_10.xls")
tmax2021_11<- read_xls("2021/tmax_2021_11.xls")
tmax2021_12<- read_xls("2021/tmax_2021_12.xls")
tmin2021_01<- read_xls("2021/tmin_2021_01.xls")
tmin2021_02<- read_xls("2021/tmin_2021_02.xls")
tmin2021_03<- read_xls("2021/tmin_2021_03.xls")
tmin2021_04<- read_xls("2021/tmin_2021_04.xls")
tmin2021_05<- read_xls("2021/tmin_2021_05.xls")
tmin2021_06<- read_xls("2021/tmin_2021_06.xls")
tmin2021_07<- read_xls("2021/tmin_2021_07.xls")
tmin2021_08<- read_xls("2021/tmin_2021_08.xls")
tmin2021_09<- read_xls("2021/tmin_2021_09.xls")
tmin2021_10<- read_xls("2021/tmin_2021_10.xls")
tmin2021_11<- read_xls("2021/tmin_2021_11.xls")
tmin2021_12<- read_xls("2021/tmin_2021_12.xls")

## Generate data frames for each year

## 2010
tmin2010_01<- tmin2010_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_01)<- c("OBJECTID", "tmin_01")
tmin2010_02<- tmin2010_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_02)<- c("OBJECTID", "tmin_02")
tmin2010_03<- tmin2010_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_03)<- c("OBJECTID", "tmin_03")
tmin2010_04<- tmin2010_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_04)<- c("OBJECTID", "tmin_04")
tmin2010_05<- tmin2010_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_05)<- c("OBJECTID", "tmin_05")
tmin2010_06<- tmin2010_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_06)<- c("OBJECTID", "tmin_06")
tmin2010_07<- tmin2010_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_07)<- c("OBJECTID", "tmin_07")
tmin2010_08<- tmin2010_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_08)<- c("OBJECTID", "tmin_08")
tmin2010_09<- tmin2010_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_09)<- c("OBJECTID", "tmin_09")
tmin2010_10<- tmin2010_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_10)<- c("OBJECTID", "tmin_10")
tmin2010_11<- tmin2010_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_11)<- c("OBJECTID", "tmin_11")
tmin2010_12<- tmin2010_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2010_12)<- c("OBJECTID", "tmin_12")

tmax2010_01<- tmax2010_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_01)<- c("OBJECTID", "tmax_01")
tmax2010_02<- tmax2010_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_02)<- c("OBJECTID", "tmax_02")
tmax2010_03<- tmax2010_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_03)<- c("OBJECTID", "tmax_03")
tmax2010_04<- tmax2010_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_04)<- c("OBJECTID", "tmax_04")
tmax2010_05<- tmax2010_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_05)<- c("OBJECTID", "tmax_05")
tmax2010_06<- tmax2010_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_06)<- c("OBJECTID", "tmax_06")
tmax2010_07<- tmax2010_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_07)<- c("OBJECTID", "tmax_07")
tmax2010_08<- tmax2010_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_08)<- c("OBJECTID", "tmax_08")
tmax2010_09<- tmax2010_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_09)<- c("OBJECTID", "tmax_09")
tmax2010_10<- tmax2010_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_10)<- c("OBJECTID", "tmax_10")
tmax2010_11<- tmax2010_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_11)<- c("OBJECTID", "tmax_11")
tmax2010_12<- tmax2010_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2010_12)<- c("OBJECTID", "tmax_12")

prec2010_01<- prec2010_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_01)<- c("OBJECTID", "prec_01")
prec2010_02<- prec2010_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_02)<- c("OBJECTID", "prec_02")
prec2010_03<- prec2010_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_03)<- c("OBJECTID", "prec_03")
prec2010_04<- prec2010_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_04)<- c("OBJECTID", "prec_04")
prec2010_05<- prec2010_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_05)<- c("OBJECTID", "prec_05")
prec2010_06<- prec2010_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_06)<- c("OBJECTID", "prec_06")
prec2010_07<- prec2010_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_07)<- c("OBJECTID", "prec_07")
prec2010_08<- prec2010_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_08)<- c("OBJECTID", "prec_08")
prec2010_09<- prec2010_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_09)<- c("OBJECTID", "prec_09")
prec2010_10<- prec2010_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_10)<- c("OBJECTID", "prec_10")
prec2010_11<- prec2010_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_11)<- c("OBJECTID", "prec_11")
prec2010_12<- prec2010_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2010_12)<- c("OBJECTID", "prec_12")


data2010<- inner_join(tmin2010_01, tmin2010_02) %>%
  inner_join(., tmin2010_03) %>% 
  inner_join(., tmin2010_04) %>% 
  inner_join(., tmin2010_05) %>% 
  inner_join(., tmin2010_06) %>% 
  inner_join(., tmin2010_07) %>% 
  inner_join(., tmin2010_08) %>% 
  inner_join(., tmin2010_09) %>% 
  inner_join(., tmin2010_10) %>% 
  inner_join(., tmin2010_11) %>% 
  inner_join(., tmin2010_12) %>% 
  inner_join(., tmax2010_01) %>%
  inner_join(., tmax2010_02) %>%
  inner_join(., tmax2010_03) %>% 
  inner_join(., tmax2010_04) %>% 
  inner_join(., tmax2010_05) %>% 
  inner_join(., tmax2010_06) %>% 
  inner_join(., tmax2010_07) %>% 
  inner_join(., tmax2010_08) %>% 
  inner_join(., tmax2010_09) %>% 
  inner_join(., tmax2010_10) %>% 
  inner_join(., tmax2010_11) %>% 
  inner_join(., tmax2010_12) %>% 
  inner_join(., prec2010_01) %>%
  inner_join(., prec2010_02) %>%
  inner_join(., prec2010_03) %>% 
  inner_join(., prec2010_04) %>% 
  inner_join(., prec2010_05) %>% 
  inner_join(., prec2010_06) %>% 
  inner_join(., prec2010_07) %>% 
  inner_join(., prec2010_08) %>% 
  inner_join(., prec2010_09) %>% 
  inner_join(., prec2010_10) %>% 
  inner_join(., prec2010_11) %>% 
  inner_join(., prec2010_12)

data2010<- inner_join(data2010, buffer2010_ID)  
  
# 2011



tmin2011_01<- tmin2011_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_01)<- c("OBJECTID", "tmin_01")
tmin2011_02<- tmin2011_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_02)<- c("OBJECTID", "tmin_02")
tmin2011_03<- tmin2011_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_03)<- c("OBJECTID", "tmin_03")
tmin2011_04<- tmin2011_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_04)<- c("OBJECTID", "tmin_04")
tmin2011_05<- tmin2011_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_05)<- c("OBJECTID", "tmin_05")
tmin2011_06<- tmin2011_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_06)<- c("OBJECTID", "tmin_06")
tmin2011_07<- tmin2011_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_07)<- c("OBJECTID", "tmin_07")
tmin2011_08<- tmin2011_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_08)<- c("OBJECTID", "tmin_08")
tmin2011_09<- tmin2011_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_09)<- c("OBJECTID", "tmin_09")
tmin2011_10<- tmin2011_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_10)<- c("OBJECTID", "tmin_10")
tmin2011_11<- tmin2011_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_11)<- c("OBJECTID", "tmin_11")
tmin2011_12<- tmin2011_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2011_12)<- c("OBJECTID", "tmin_12")

tmax2011_01<- tmax2011_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_01)<- c("OBJECTID", "tmax_01")
tmax2011_02<- tmax2011_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_02)<- c("OBJECTID", "tmax_02")
tmax2011_03<- tmax2011_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_03)<- c("OBJECTID", "tmax_03")
tmax2011_04<- tmax2011_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_04)<- c("OBJECTID", "tmax_04")
tmax2011_05<- tmax2011_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_05)<- c("OBJECTID", "tmax_05")
tmax2011_06<- tmax2011_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_06)<- c("OBJECTID", "tmax_06")
tmax2011_07<- tmax2011_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_07)<- c("OBJECTID", "tmax_07")
tmax2011_08<- tmax2011_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_08)<- c("OBJECTID", "tmax_08")
tmax2011_09<- tmax2011_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_09)<- c("OBJECTID", "tmax_09")
tmax2011_10<- tmax2011_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_10)<- c("OBJECTID", "tmax_10")
tmax2011_11<- tmax2011_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_11)<- c("OBJECTID", "tmax_11")
tmax2011_12<- tmax2011_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2011_12)<- c("OBJECTID", "tmax_12")

prec2011_01<- prec2011_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_01)<- c("OBJECTID", "prec_01")
prec2011_02<- prec2011_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_02)<- c("OBJECTID", "prec_02")
prec2011_03<- prec2011_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_03)<- c("OBJECTID", "prec_03")
prec2011_04<- prec2011_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_04)<- c("OBJECTID", "prec_04")
prec2011_05<- prec2011_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_05)<- c("OBJECTID", "prec_05")
prec2011_06<- prec2011_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_06)<- c("OBJECTID", "prec_06")
prec2011_07<- prec2011_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_07)<- c("OBJECTID", "prec_07")
prec2011_08<- prec2011_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_08)<- c("OBJECTID", "prec_08")
prec2011_09<- prec2011_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_09)<- c("OBJECTID", "prec_09")
prec2011_10<- prec2011_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_10)<- c("OBJECTID", "prec_10")
prec2011_11<- prec2011_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_11)<- c("OBJECTID", "prec_11")
prec2011_12<- prec2011_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2011_12)<- c("OBJECTID", "prec_12")


data2011<- inner_join(tmin2011_01, tmin2011_02) %>%
  inner_join(., tmin2011_03) %>% 
  inner_join(., tmin2011_04) %>% 
  inner_join(., tmin2011_05) %>% 
  inner_join(., tmin2011_06) %>% 
  inner_join(., tmin2011_07) %>% 
  inner_join(., tmin2011_08) %>% 
  inner_join(., tmin2011_09) %>% 
  inner_join(., tmin2011_10) %>% 
  inner_join(., tmin2011_11) %>% 
  inner_join(., tmin2011_12) %>% 
  inner_join(., tmax2011_01) %>%
  inner_join(., tmax2011_02) %>%
  inner_join(., tmax2011_03) %>% 
  inner_join(., tmax2011_04) %>% 
  inner_join(., tmax2011_05) %>% 
  inner_join(., tmax2011_06) %>% 
  inner_join(., tmax2011_07) %>% 
  inner_join(., tmax2011_08) %>% 
  inner_join(., tmax2011_09) %>% 
  inner_join(., tmax2011_10) %>% 
  inner_join(., tmax2011_11) %>% 
  inner_join(., tmax2011_12) %>% 
  inner_join(., prec2011_01) %>%
  inner_join(., prec2011_02) %>%
  inner_join(., prec2011_03) %>% 
  inner_join(., prec2011_04) %>% 
  inner_join(., prec2011_05) %>% 
  inner_join(., prec2011_06) %>% 
  inner_join(., prec2011_07) %>% 
  inner_join(., prec2011_08) %>% 
  inner_join(., prec2011_09) %>% 
  inner_join(., prec2011_10) %>% 
  inner_join(., prec2011_11) %>% 
  inner_join(., prec2011_12)

data2011<- inner_join(data2011, buffer2011_ID)  

# 2012



tmin2012_01<- tmin2012_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_01)<- c("OBJECTID", "tmin_01")
tmin2012_02<- tmin2012_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_02)<- c("OBJECTID", "tmin_02")
tmin2012_03<- tmin2012_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_03)<- c("OBJECTID", "tmin_03")
tmin2012_04<- tmin2012_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_04)<- c("OBJECTID", "tmin_04")
tmin2012_05<- tmin2012_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_05)<- c("OBJECTID", "tmin_05")
tmin2012_06<- tmin2012_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_06)<- c("OBJECTID", "tmin_06")
tmin2012_07<- tmin2012_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_07)<- c("OBJECTID", "tmin_07")
tmin2012_08<- tmin2012_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_08)<- c("OBJECTID", "tmin_08")
tmin2012_09<- tmin2012_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_09)<- c("OBJECTID", "tmin_09")
tmin2012_10<- tmin2012_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_10)<- c("OBJECTID", "tmin_10")
tmin2012_11<- tmin2012_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_11)<- c("OBJECTID", "tmin_11")
tmin2012_12<- tmin2012_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2012_12)<- c("OBJECTID", "tmin_12")

tmax2012_01<- tmax2012_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_01)<- c("OBJECTID", "tmax_01")
tmax2012_02<- tmax2012_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_02)<- c("OBJECTID", "tmax_02")
tmax2012_03<- tmax2012_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_03)<- c("OBJECTID", "tmax_03")
tmax2012_04<- tmax2012_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_04)<- c("OBJECTID", "tmax_04")
tmax2012_05<- tmax2012_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_05)<- c("OBJECTID", "tmax_05")
tmax2012_06<- tmax2012_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_06)<- c("OBJECTID", "tmax_06")
tmax2012_07<- tmax2012_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_07)<- c("OBJECTID", "tmax_07")
tmax2012_08<- tmax2012_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_08)<- c("OBJECTID", "tmax_08")
tmax2012_09<- tmax2012_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_09)<- c("OBJECTID", "tmax_09")
tmax2012_10<- tmax2012_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_10)<- c("OBJECTID", "tmax_10")
tmax2012_11<- tmax2012_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_11)<- c("OBJECTID", "tmax_11")
tmax2012_12<- tmax2012_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2012_12)<- c("OBJECTID", "tmax_12")

prec2012_01<- prec2012_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_01)<- c("OBJECTID", "prec_01")
prec2012_02<- prec2012_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_02)<- c("OBJECTID", "prec_02")
prec2012_03<- prec2012_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_03)<- c("OBJECTID", "prec_03")
prec2012_04<- prec2012_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_04)<- c("OBJECTID", "prec_04")
prec2012_05<- prec2012_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_05)<- c("OBJECTID", "prec_05")
prec2012_06<- prec2012_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_06)<- c("OBJECTID", "prec_06")
prec2012_07<- prec2012_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_07)<- c("OBJECTID", "prec_07")
prec2012_08<- prec2012_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_08)<- c("OBJECTID", "prec_08")
prec2012_09<- prec2012_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_09)<- c("OBJECTID", "prec_09")
prec2012_10<- prec2012_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_10)<- c("OBJECTID", "prec_10")
prec2012_11<- prec2012_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_11)<- c("OBJECTID", "prec_11")
prec2012_12<- prec2012_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2012_12)<- c("OBJECTID", "prec_12")


data2012<- inner_join(tmin2012_01, tmin2012_02) %>%
  inner_join(., tmin2012_03) %>% 
  inner_join(., tmin2012_04) %>% 
  inner_join(., tmin2012_05) %>% 
  inner_join(., tmin2012_06) %>% 
  inner_join(., tmin2012_07) %>% 
  inner_join(., tmin2012_08) %>% 
  inner_join(., tmin2012_09) %>% 
  inner_join(., tmin2012_10) %>% 
  inner_join(., tmin2012_11) %>% 
  inner_join(., tmin2012_12) %>% 
  inner_join(., tmax2012_01) %>%
  inner_join(., tmax2012_02) %>%
  inner_join(., tmax2012_03) %>% 
  inner_join(., tmax2012_04) %>% 
  inner_join(., tmax2012_05) %>% 
  inner_join(., tmax2012_06) %>% 
  inner_join(., tmax2012_07) %>% 
  inner_join(., tmax2012_08) %>% 
  inner_join(., tmax2012_09) %>% 
  inner_join(., tmax2012_10) %>% 
  inner_join(., tmax2012_11) %>% 
  inner_join(., tmax2012_12) %>% 
  inner_join(., prec2012_01) %>%
  inner_join(., prec2012_02) %>%
  inner_join(., prec2012_03) %>% 
  inner_join(., prec2012_04) %>% 
  inner_join(., prec2012_05) %>% 
  inner_join(., prec2012_06) %>% 
  inner_join(., prec2012_07) %>% 
  inner_join(., prec2012_08) %>% 
  inner_join(., prec2012_09) %>% 
  inner_join(., prec2012_10) %>% 
  inner_join(., prec2012_11) %>% 
  inner_join(., prec2012_12)

data2012<- inner_join(data2012, buffer2012_ID)  

# 2013



tmin2013_01<- tmin2013_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_01)<- c("OBJECTID", "tmin_01")
tmin2013_02<- tmin2013_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_02)<- c("OBJECTID", "tmin_02")
tmin2013_03<- tmin2013_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_03)<- c("OBJECTID", "tmin_03")
tmin2013_04<- tmin2013_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_04)<- c("OBJECTID", "tmin_04")
tmin2013_05<- tmin2013_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_05)<- c("OBJECTID", "tmin_05")
tmin2013_06<- tmin2013_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_06)<- c("OBJECTID", "tmin_06")
tmin2013_07<- tmin2013_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_07)<- c("OBJECTID", "tmin_07")
tmin2013_08<- tmin2013_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_08)<- c("OBJECTID", "tmin_08")
tmin2013_09<- tmin2013_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_09)<- c("OBJECTID", "tmin_09")
tmin2013_10<- tmin2013_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_10)<- c("OBJECTID", "tmin_10")
tmin2013_11<- tmin2013_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_11)<- c("OBJECTID", "tmin_11")
tmin2013_12<- tmin2013_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2013_12)<- c("OBJECTID", "tmin_12")

tmax2013_01<- tmax2013_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_01)<- c("OBJECTID", "tmax_01")
tmax2013_02<- tmax2013_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_02)<- c("OBJECTID", "tmax_02")
tmax2013_03<- tmax2013_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_03)<- c("OBJECTID", "tmax_03")
tmax2013_04<- tmax2013_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_04)<- c("OBJECTID", "tmax_04")
tmax2013_05<- tmax2013_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_05)<- c("OBJECTID", "tmax_05")
tmax2013_06<- tmax2013_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_06)<- c("OBJECTID", "tmax_06")
tmax2013_07<- tmax2013_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_07)<- c("OBJECTID", "tmax_07")
tmax2013_08<- tmax2013_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_08)<- c("OBJECTID", "tmax_08")
tmax2013_09<- tmax2013_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_09)<- c("OBJECTID", "tmax_09")
tmax2013_10<- tmax2013_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_10)<- c("OBJECTID", "tmax_10")
tmax2013_11<- tmax2013_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_11)<- c("OBJECTID", "tmax_11")
tmax2013_12<- tmax2013_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2013_12)<- c("OBJECTID", "tmax_12")

prec2013_01<- prec2013_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_01)<- c("OBJECTID", "prec_01")
prec2013_02<- prec2013_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_02)<- c("OBJECTID", "prec_02")
prec2013_03<- prec2013_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_03)<- c("OBJECTID", "prec_03")
prec2013_04<- prec2013_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_04)<- c("OBJECTID", "prec_04")
prec2013_05<- prec2013_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_05)<- c("OBJECTID", "prec_05")
prec2013_06<- prec2013_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_06)<- c("OBJECTID", "prec_06")
prec2013_07<- prec2013_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_07)<- c("OBJECTID", "prec_07")
prec2013_08<- prec2013_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_08)<- c("OBJECTID", "prec_08")
prec2013_09<- prec2013_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_09)<- c("OBJECTID", "prec_09")
prec2013_10<- prec2013_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_10)<- c("OBJECTID", "prec_10")
prec2013_11<- prec2013_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_11)<- c("OBJECTID", "prec_11")
prec2013_12<- prec2013_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2013_12)<- c("OBJECTID", "prec_12")


data2013<- inner_join(tmin2013_01, tmin2013_02) %>%
  inner_join(., tmin2013_03) %>% 
  inner_join(., tmin2013_04) %>% 
  inner_join(., tmin2013_05) %>% 
  inner_join(., tmin2013_06) %>% 
  inner_join(., tmin2013_07) %>% 
  inner_join(., tmin2013_08) %>% 
  inner_join(., tmin2013_09) %>% 
  inner_join(., tmin2013_10) %>% 
  inner_join(., tmin2013_11) %>% 
  inner_join(., tmin2013_12) %>% 
  inner_join(., tmax2013_01) %>%
  inner_join(., tmax2013_02) %>%
  inner_join(., tmax2013_03) %>% 
  inner_join(., tmax2013_04) %>% 
  inner_join(., tmax2013_05) %>% 
  inner_join(., tmax2013_06) %>% 
  inner_join(., tmax2013_07) %>% 
  inner_join(., tmax2013_08) %>% 
  inner_join(., tmax2013_09) %>% 
  inner_join(., tmax2013_10) %>% 
  inner_join(., tmax2013_11) %>% 
  inner_join(., tmax2013_12) %>% 
  inner_join(., prec2013_01) %>%
  inner_join(., prec2013_02) %>%
  inner_join(., prec2013_03) %>% 
  inner_join(., prec2013_04) %>% 
  inner_join(., prec2013_05) %>% 
  inner_join(., prec2013_06) %>% 
  inner_join(., prec2013_07) %>% 
  inner_join(., prec2013_08) %>% 
  inner_join(., prec2013_09) %>% 
  inner_join(., prec2013_10) %>% 
  inner_join(., prec2013_11) %>% 
  inner_join(., prec2013_12)

data2013<- inner_join(data2013, buffer2013_ID)  

## 2014



tmin2014_01<- tmin2014_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_01)<- c("OBJECTID", "tmin_01")
tmin2014_02<- tmin2014_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_02)<- c("OBJECTID", "tmin_02")
tmin2014_03<- tmin2014_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_03)<- c("OBJECTID", "tmin_03")
tmin2014_04<- tmin2014_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_04)<- c("OBJECTID", "tmin_04")
tmin2014_05<- tmin2014_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_05)<- c("OBJECTID", "tmin_05")
tmin2014_06<- tmin2014_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_06)<- c("OBJECTID", "tmin_06")
tmin2014_07<- tmin2014_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_07)<- c("OBJECTID", "tmin_07")
tmin2014_08<- tmin2014_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_08)<- c("OBJECTID", "tmin_08")
tmin2014_09<- tmin2014_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_09)<- c("OBJECTID", "tmin_09")
tmin2014_10<- tmin2014_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_10)<- c("OBJECTID", "tmin_10")
tmin2014_11<- tmin2014_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_11)<- c("OBJECTID", "tmin_11")
tmin2014_12<- tmin2014_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2014_12)<- c("OBJECTID", "tmin_12")

tmax2014_01<- tmax2014_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_01)<- c("OBJECTID", "tmax_01")
tmax2014_02<- tmax2014_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_02)<- c("OBJECTID", "tmax_02")
tmax2014_03<- tmax2014_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_03)<- c("OBJECTID", "tmax_03")
tmax2014_04<- tmax2014_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_04)<- c("OBJECTID", "tmax_04")
tmax2014_05<- tmax2014_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_05)<- c("OBJECTID", "tmax_05")
tmax2014_06<- tmax2014_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_06)<- c("OBJECTID", "tmax_06")
tmax2014_07<- tmax2014_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_07)<- c("OBJECTID", "tmax_07")
tmax2014_08<- tmax2014_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_08)<- c("OBJECTID", "tmax_08")
tmax2014_09<- tmax2014_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_09)<- c("OBJECTID", "tmax_09")
tmax2014_10<- tmax2014_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_10)<- c("OBJECTID", "tmax_10")
tmax2014_11<- tmax2014_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_11)<- c("OBJECTID", "tmax_11")
tmax2014_12<- tmax2014_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2014_12)<- c("OBJECTID", "tmax_12")

prec2014_01<- prec2014_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_01)<- c("OBJECTID", "prec_01")
prec2014_02<- prec2014_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_02)<- c("OBJECTID", "prec_02")
prec2014_03<- prec2014_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_03)<- c("OBJECTID", "prec_03")
prec2014_04<- prec2014_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_04)<- c("OBJECTID", "prec_04")
prec2014_05<- prec2014_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_05)<- c("OBJECTID", "prec_05")
prec2014_06<- prec2014_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_06)<- c("OBJECTID", "prec_06")
prec2014_07<- prec2014_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_07)<- c("OBJECTID", "prec_07")
prec2014_08<- prec2014_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_08)<- c("OBJECTID", "prec_08")
prec2014_09<- prec2014_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_09)<- c("OBJECTID", "prec_09")
prec2014_10<- prec2014_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_10)<- c("OBJECTID", "prec_10")
prec2014_11<- prec2014_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_11)<- c("OBJECTID", "prec_11")
prec2014_12<- prec2014_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2014_12)<- c("OBJECTID", "prec_12")


data2014<- inner_join(tmin2014_01, tmin2014_02) %>%
  inner_join(., tmin2014_03) %>% 
  inner_join(., tmin2014_04) %>% 
  inner_join(., tmin2014_05) %>% 
  inner_join(., tmin2014_06) %>% 
  inner_join(., tmin2014_07) %>% 
  inner_join(., tmin2014_08) %>% 
  inner_join(., tmin2014_09) %>% 
  inner_join(., tmin2014_10) %>% 
  inner_join(., tmin2014_11) %>% 
  inner_join(., tmin2014_12) %>% 
  inner_join(., tmax2014_01) %>%
  inner_join(., tmax2014_02) %>%
  inner_join(., tmax2014_03) %>% 
  inner_join(., tmax2014_04) %>% 
  inner_join(., tmax2014_05) %>% 
  inner_join(., tmax2014_06) %>% 
  inner_join(., tmax2014_07) %>% 
  inner_join(., tmax2014_08) %>% 
  inner_join(., tmax2014_09) %>% 
  inner_join(., tmax2014_10) %>% 
  inner_join(., tmax2014_11) %>% 
  inner_join(., tmax2014_12) %>% 
  inner_join(., prec2014_01) %>%
  inner_join(., prec2014_02) %>%
  inner_join(., prec2014_03) %>% 
  inner_join(., prec2014_04) %>% 
  inner_join(., prec2014_05) %>% 
  inner_join(., prec2014_06) %>% 
  inner_join(., prec2014_07) %>% 
  inner_join(., prec2014_08) %>% 
  inner_join(., prec2014_09) %>% 
  inner_join(., prec2014_10) %>% 
  inner_join(., prec2014_11) %>% 
  inner_join(., prec2014_12)

data2014<- inner_join(data2014, buffer2014_ID)

# 2015



tmin2015_01<- tmin2015_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_01)<- c("OBJECTID", "tmin_01")
tmin2015_02<- tmin2015_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_02)<- c("OBJECTID", "tmin_02")
tmin2015_03<- tmin2015_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_03)<- c("OBJECTID", "tmin_03")
tmin2015_04<- tmin2015_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_04)<- c("OBJECTID", "tmin_04")
tmin2015_05<- tmin2015_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_05)<- c("OBJECTID", "tmin_05")
tmin2015_06<- tmin2015_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_06)<- c("OBJECTID", "tmin_06")
tmin2015_07<- tmin2015_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_07)<- c("OBJECTID", "tmin_07")
tmin2015_08<- tmin2015_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_08)<- c("OBJECTID", "tmin_08")
tmin2015_09<- tmin2015_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_09)<- c("OBJECTID", "tmin_09")
tmin2015_10<- tmin2015_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_10)<- c("OBJECTID", "tmin_10")
tmin2015_11<- tmin2015_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_11)<- c("OBJECTID", "tmin_11")
tmin2015_12<- tmin2015_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2015_12)<- c("OBJECTID", "tmin_12")

tmax2015_01<- tmax2015_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_01)<- c("OBJECTID", "tmax_01")
tmax2015_02<- tmax2015_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_02)<- c("OBJECTID", "tmax_02")
tmax2015_03<- tmax2015_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_03)<- c("OBJECTID", "tmax_03")
tmax2015_04<- tmax2015_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_04)<- c("OBJECTID", "tmax_04")
tmax2015_05<- tmax2015_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_05)<- c("OBJECTID", "tmax_05")
tmax2015_06<- tmax2015_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_06)<- c("OBJECTID", "tmax_06")
tmax2015_07<- tmax2015_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_07)<- c("OBJECTID", "tmax_07")
tmax2015_08<- tmax2015_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_08)<- c("OBJECTID", "tmax_08")
tmax2015_09<- tmax2015_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_09)<- c("OBJECTID", "tmax_09")
tmax2015_10<- tmax2015_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_10)<- c("OBJECTID", "tmax_10")
tmax2015_11<- tmax2015_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_11)<- c("OBJECTID", "tmax_11")
tmax2015_12<- tmax2015_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2015_12)<- c("OBJECTID", "tmax_12")

prec2015_01<- prec2015_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_01)<- c("OBJECTID", "prec_01")
prec2015_02<- prec2015_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_02)<- c("OBJECTID", "prec_02")
prec2015_03<- prec2015_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_03)<- c("OBJECTID", "prec_03")
prec2015_04<- prec2015_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_04)<- c("OBJECTID", "prec_04")
prec2015_05<- prec2015_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_05)<- c("OBJECTID", "prec_05")
prec2015_06<- prec2015_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_06)<- c("OBJECTID", "prec_06")
prec2015_07<- prec2015_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_07)<- c("OBJECTID", "prec_07")
prec2015_08<- prec2015_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_08)<- c("OBJECTID", "prec_08")
prec2015_09<- prec2015_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_09)<- c("OBJECTID", "prec_09")
prec2015_10<- prec2015_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_10)<- c("OBJECTID", "prec_10")
prec2015_11<- prec2015_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_11)<- c("OBJECTID", "prec_11")
prec2015_12<- prec2015_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2015_12)<- c("OBJECTID", "prec_12")


data2015<- inner_join(tmin2015_01, tmin2015_02) %>%
  inner_join(., tmin2015_03) %>% 
  inner_join(., tmin2015_04) %>% 
  inner_join(., tmin2015_05) %>% 
  inner_join(., tmin2015_06) %>% 
  inner_join(., tmin2015_07) %>% 
  inner_join(., tmin2015_08) %>% 
  inner_join(., tmin2015_09) %>% 
  inner_join(., tmin2015_10) %>% 
  inner_join(., tmin2015_11) %>% 
  inner_join(., tmin2015_12) %>% 
  inner_join(., tmax2015_01) %>%
  inner_join(., tmax2015_02) %>%
  inner_join(., tmax2015_03) %>% 
  inner_join(., tmax2015_04) %>% 
  inner_join(., tmax2015_05) %>% 
  inner_join(., tmax2015_06) %>% 
  inner_join(., tmax2015_07) %>% 
  inner_join(., tmax2015_08) %>% 
  inner_join(., tmax2015_09) %>% 
  inner_join(., tmax2015_10) %>% 
  inner_join(., tmax2015_11) %>% 
  inner_join(., tmax2015_12) %>% 
  inner_join(., prec2015_01) %>%
  inner_join(., prec2015_02) %>%
  inner_join(., prec2015_03) %>% 
  inner_join(., prec2015_04) %>% 
  inner_join(., prec2015_05) %>% 
  inner_join(., prec2015_06) %>% 
  inner_join(., prec2015_07) %>% 
  inner_join(., prec2015_08) %>% 
  inner_join(., prec2015_09) %>% 
  inner_join(., prec2015_10) %>% 
  inner_join(., prec2015_11) %>% 
  inner_join(., prec2015_12)

data2015<- inner_join(data2015, buffer2015_ID)

# 2016



tmin2016_01<- tmin2016_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_01)<- c("OBJECTID", "tmin_01")
tmin2016_02<- tmin2016_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_02)<- c("OBJECTID", "tmin_02")
tmin2016_03<- tmin2016_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_03)<- c("OBJECTID", "tmin_03")
tmin2016_04<- tmin2016_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_04)<- c("OBJECTID", "tmin_04")
tmin2016_05<- tmin2016_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_05)<- c("OBJECTID", "tmin_05")
tmin2016_06<- tmin2016_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_06)<- c("OBJECTID", "tmin_06")
tmin2016_07<- tmin2016_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_07)<- c("OBJECTID", "tmin_07")
tmin2016_08<- tmin2016_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_08)<- c("OBJECTID", "tmin_08")
tmin2016_09<- tmin2016_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_09)<- c("OBJECTID", "tmin_09")
tmin2016_10<- tmin2016_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_10)<- c("OBJECTID", "tmin_10")
tmin2016_11<- tmin2016_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_11)<- c("OBJECTID", "tmin_11")
tmin2016_12<- tmin2016_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2016_12)<- c("OBJECTID", "tmin_12")

tmax2016_01<- tmax2016_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_01)<- c("OBJECTID", "tmax_01")
tmax2016_02<- tmax2016_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_02)<- c("OBJECTID", "tmax_02")
tmax2016_03<- tmax2016_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_03)<- c("OBJECTID", "tmax_03")
tmax2016_04<- tmax2016_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_04)<- c("OBJECTID", "tmax_04")
tmax2016_05<- tmax2016_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_05)<- c("OBJECTID", "tmax_05")
tmax2016_06<- tmax2016_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_06)<- c("OBJECTID", "tmax_06")
tmax2016_07<- tmax2016_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_07)<- c("OBJECTID", "tmax_07")
tmax2016_08<- tmax2016_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_08)<- c("OBJECTID", "tmax_08")
tmax2016_09<- tmax2016_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_09)<- c("OBJECTID", "tmax_09")
tmax2016_10<- tmax2016_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_10)<- c("OBJECTID", "tmax_10")
tmax2016_11<- tmax2016_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_11)<- c("OBJECTID", "tmax_11")
tmax2016_12<- tmax2016_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2016_12)<- c("OBJECTID", "tmax_12")

prec2016_01<- prec2016_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_01)<- c("OBJECTID", "prec_01")
prec2016_02<- prec2016_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_02)<- c("OBJECTID", "prec_02")
prec2016_03<- prec2016_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_03)<- c("OBJECTID", "prec_03")
prec2016_04<- prec2016_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_04)<- c("OBJECTID", "prec_04")
prec2016_05<- prec2016_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_05)<- c("OBJECTID", "prec_05")
prec2016_06<- prec2016_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_06)<- c("OBJECTID", "prec_06")
prec2016_07<- prec2016_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_07)<- c("OBJECTID", "prec_07")
prec2016_08<- prec2016_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_08)<- c("OBJECTID", "prec_08")
prec2016_09<- prec2016_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_09)<- c("OBJECTID", "prec_09")
prec2016_10<- prec2016_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_10)<- c("OBJECTID", "prec_10")
prec2016_11<- prec2016_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_11)<- c("OBJECTID", "prec_11")
prec2016_12<- prec2016_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2016_12)<- c("OBJECTID", "prec_12")


data2016<- inner_join(tmin2016_01, tmin2016_02) %>%
  inner_join(., tmin2016_03) %>% 
  inner_join(., tmin2016_04) %>% 
  inner_join(., tmin2016_05) %>% 
  inner_join(., tmin2016_06) %>% 
  inner_join(., tmin2016_07) %>% 
  inner_join(., tmin2016_08) %>% 
  inner_join(., tmin2016_09) %>% 
  inner_join(., tmin2016_10) %>% 
  inner_join(., tmin2016_11) %>% 
  inner_join(., tmin2016_12) %>% 
  inner_join(., tmax2016_01) %>%
  inner_join(., tmax2016_02) %>%
  inner_join(., tmax2016_03) %>% 
  inner_join(., tmax2016_04) %>% 
  inner_join(., tmax2016_05) %>% 
  inner_join(., tmax2016_06) %>% 
  inner_join(., tmax2016_07) %>% 
  inner_join(., tmax2016_08) %>% 
  inner_join(., tmax2016_09) %>% 
  inner_join(., tmax2016_10) %>% 
  inner_join(., tmax2016_11) %>% 
  inner_join(., tmax2016_12) %>% 
  inner_join(., prec2016_01) %>%
  inner_join(., prec2016_02) %>%
  inner_join(., prec2016_03) %>% 
  inner_join(., prec2016_04) %>% 
  inner_join(., prec2016_05) %>% 
  inner_join(., prec2016_06) %>% 
  inner_join(., prec2016_07) %>% 
  inner_join(., prec2016_08) %>% 
  inner_join(., prec2016_09) %>% 
  inner_join(., prec2016_10) %>% 
  inner_join(., prec2016_11) %>% 
  inner_join(., prec2016_12)

data2016<- inner_join(data2016, buffer2016_ID) 

## 2017



tmin2017_01<- tmin2017_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_01)<- c("OBJECTID", "tmin_01")
tmin2017_02<- tmin2017_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_02)<- c("OBJECTID", "tmin_02")
tmin2017_03<- tmin2017_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_03)<- c("OBJECTID", "tmin_03")
tmin2017_04<- tmin2017_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_04)<- c("OBJECTID", "tmin_04")
tmin2017_05<- tmin2017_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_05)<- c("OBJECTID", "tmin_05")
tmin2017_06<- tmin2017_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_06)<- c("OBJECTID", "tmin_06")
tmin2017_07<- tmin2017_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_07)<- c("OBJECTID", "tmin_07")
tmin2017_08<- tmin2017_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_08)<- c("OBJECTID", "tmin_08")
tmin2017_09<- tmin2017_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_09)<- c("OBJECTID", "tmin_09")
tmin2017_10<- tmin2017_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_10)<- c("OBJECTID", "tmin_10")
tmin2017_11<- tmin2017_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_11)<- c("OBJECTID", "tmin_11")
tmin2017_12<- tmin2017_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2017_12)<- c("OBJECTID", "tmin_12")

tmax2017_01<- tmax2017_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_01)<- c("OBJECTID", "tmax_01")
tmax2017_02<- tmax2017_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_02)<- c("OBJECTID", "tmax_02")
tmax2017_03<- tmax2017_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_03)<- c("OBJECTID", "tmax_03")
tmax2017_04<- tmax2017_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_04)<- c("OBJECTID", "tmax_04")
tmax2017_05<- tmax2017_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_05)<- c("OBJECTID", "tmax_05")
tmax2017_06<- tmax2017_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_06)<- c("OBJECTID", "tmax_06")
tmax2017_07<- tmax2017_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_07)<- c("OBJECTID", "tmax_07")
tmax2017_08<- tmax2017_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_08)<- c("OBJECTID", "tmax_08")
tmax2017_09<- tmax2017_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_09)<- c("OBJECTID", "tmax_09")
tmax2017_10<- tmax2017_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_10)<- c("OBJECTID", "tmax_10")
tmax2017_11<- tmax2017_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_11)<- c("OBJECTID", "tmax_11")
tmax2017_12<- tmax2017_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2017_12)<- c("OBJECTID", "tmax_12")

prec2017_01<- prec2017_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_01)<- c("OBJECTID", "prec_01")
prec2017_02<- prec2017_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_02)<- c("OBJECTID", "prec_02")
prec2017_03<- prec2017_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_03)<- c("OBJECTID", "prec_03")
prec2017_04<- prec2017_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_04)<- c("OBJECTID", "prec_04")
prec2017_05<- prec2017_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_05)<- c("OBJECTID", "prec_05")
prec2017_06<- prec2017_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_06)<- c("OBJECTID", "prec_06")
prec2017_07<- prec2017_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_07)<- c("OBJECTID", "prec_07")
prec2017_08<- prec2017_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_08)<- c("OBJECTID", "prec_08")
prec2017_09<- prec2017_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_09)<- c("OBJECTID", "prec_09")
prec2017_10<- prec2017_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_10)<- c("OBJECTID", "prec_10")
prec2017_11<- prec2017_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_11)<- c("OBJECTID", "prec_11")
prec2017_12<- prec2017_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2017_12)<- c("OBJECTID", "prec_12")


data2017<- inner_join(tmin2017_01, tmin2017_02) %>%
  inner_join(., tmin2017_03) %>% 
  inner_join(., tmin2017_04) %>% 
  inner_join(., tmin2017_05) %>% 
  inner_join(., tmin2017_06) %>% 
  inner_join(., tmin2017_07) %>% 
  inner_join(., tmin2017_08) %>% 
  inner_join(., tmin2017_09) %>% 
  inner_join(., tmin2017_10) %>% 
  inner_join(., tmin2017_11) %>% 
  inner_join(., tmin2017_12) %>% 
  inner_join(., tmax2017_01) %>%
  inner_join(., tmax2017_02) %>%
  inner_join(., tmax2017_03) %>% 
  inner_join(., tmax2017_04) %>% 
  inner_join(., tmax2017_05) %>% 
  inner_join(., tmax2017_06) %>% 
  inner_join(., tmax2017_07) %>% 
  inner_join(., tmax2017_08) %>% 
  inner_join(., tmax2017_09) %>% 
  inner_join(., tmax2017_10) %>% 
  inner_join(., tmax2017_11) %>% 
  inner_join(., tmax2017_12) %>% 
  inner_join(., prec2017_01) %>%
  inner_join(., prec2017_02) %>%
  inner_join(., prec2017_03) %>% 
  inner_join(., prec2017_04) %>% 
  inner_join(., prec2017_05) %>% 
  inner_join(., prec2017_06) %>% 
  inner_join(., prec2017_07) %>% 
  inner_join(., prec2017_08) %>% 
  inner_join(., prec2017_09) %>% 
  inner_join(., prec2017_10) %>% 
  inner_join(., prec2017_11) %>% 
  inner_join(., prec2017_12)

data2017<- inner_join(data2017, buffer2017_ID)


## 2018



tmin2018_01<- tmin2018_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_01)<- c("OBJECTID", "tmin_01")
tmin2018_02<- tmin2018_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_02)<- c("OBJECTID", "tmin_02")
tmin2018_03<- tmin2018_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_03)<- c("OBJECTID", "tmin_03")
tmin2018_04<- tmin2018_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_04)<- c("OBJECTID", "tmin_04")
tmin2018_05<- tmin2018_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_05)<- c("OBJECTID", "tmin_05")
tmin2018_06<- tmin2018_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_06)<- c("OBJECTID", "tmin_06")
tmin2018_07<- tmin2018_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_07)<- c("OBJECTID", "tmin_07")
tmin2018_08<- tmin2018_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_08)<- c("OBJECTID", "tmin_08")
tmin2018_09<- tmin2018_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_09)<- c("OBJECTID", "tmin_09")
tmin2018_10<- tmin2018_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_10)<- c("OBJECTID", "tmin_10")
tmin2018_11<- tmin2018_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_11)<- c("OBJECTID", "tmin_11")
tmin2018_12<- tmin2018_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2018_12)<- c("OBJECTID", "tmin_12")

tmax2018_01<- tmax2018_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_01)<- c("OBJECTID", "tmax_01")
tmax2018_02<- tmax2018_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_02)<- c("OBJECTID", "tmax_02")
tmax2018_03<- tmax2018_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_03)<- c("OBJECTID", "tmax_03")
tmax2018_04<- tmax2018_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_04)<- c("OBJECTID", "tmax_04")
tmax2018_05<- tmax2018_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_05)<- c("OBJECTID", "tmax_05")
tmax2018_06<- tmax2018_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_06)<- c("OBJECTID", "tmax_06")
tmax2018_07<- tmax2018_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_07)<- c("OBJECTID", "tmax_07")
tmax2018_08<- tmax2018_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_08)<- c("OBJECTID", "tmax_08")
tmax2018_09<- tmax2018_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_09)<- c("OBJECTID", "tmax_09")
tmax2018_10<- tmax2018_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_10)<- c("OBJECTID", "tmax_10")
tmax2018_11<- tmax2018_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_11)<- c("OBJECTID", "tmax_11")
tmax2018_12<- tmax2018_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2018_12)<- c("OBJECTID", "tmax_12")

prec2018_01<- prec2018_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_01)<- c("OBJECTID", "prec_01")
prec2018_02<- prec2018_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_02)<- c("OBJECTID", "prec_02")
prec2018_03<- prec2018_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_03)<- c("OBJECTID", "prec_03")
prec2018_04<- prec2018_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_04)<- c("OBJECTID", "prec_04")
prec2018_05<- prec2018_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_05)<- c("OBJECTID", "prec_05")
prec2018_06<- prec2018_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_06)<- c("OBJECTID", "prec_06")
prec2018_07<- prec2018_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_07)<- c("OBJECTID", "prec_07")
prec2018_08<- prec2018_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_08)<- c("OBJECTID", "prec_08")
prec2018_09<- prec2018_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_09)<- c("OBJECTID", "prec_09")
prec2018_10<- prec2018_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_10)<- c("OBJECTID", "prec_10")
prec2018_11<- prec2018_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_11)<- c("OBJECTID", "prec_11")
prec2018_12<- prec2018_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2018_12)<- c("OBJECTID", "prec_12")


data2018<- inner_join(tmin2018_01, tmin2018_02) %>%
  inner_join(., tmin2018_03) %>% 
  inner_join(., tmin2018_04) %>% 
  inner_join(., tmin2018_05) %>% 
  inner_join(., tmin2018_06) %>% 
  inner_join(., tmin2018_07) %>% 
  inner_join(., tmin2018_08) %>% 
  inner_join(., tmin2018_09) %>% 
  inner_join(., tmin2018_10) %>% 
  inner_join(., tmin2018_11) %>% 
  inner_join(., tmin2018_12) %>% 
  inner_join(., tmax2018_01) %>%
  inner_join(., tmax2018_02) %>%
  inner_join(., tmax2018_03) %>% 
  inner_join(., tmax2018_04) %>% 
  inner_join(., tmax2018_05) %>% 
  inner_join(., tmax2018_06) %>% 
  inner_join(., tmax2018_07) %>% 
  inner_join(., tmax2018_08) %>% 
  inner_join(., tmax2018_09) %>% 
  inner_join(., tmax2018_10) %>% 
  inner_join(., tmax2018_11) %>% 
  inner_join(., tmax2018_12) %>% 
  inner_join(., prec2018_01) %>%
  inner_join(., prec2018_02) %>%
  inner_join(., prec2018_03) %>% 
  inner_join(., prec2018_04) %>% 
  inner_join(., prec2018_05) %>% 
  inner_join(., prec2018_06) %>% 
  inner_join(., prec2018_07) %>% 
  inner_join(., prec2018_08) %>% 
  inner_join(., prec2018_09) %>% 
  inner_join(., prec2018_10) %>% 
  inner_join(., prec2018_11) %>% 
  inner_join(., prec2018_12)

data2018<- inner_join(data2018, buffer2018_ID) 

## 2019




tmin2019_01<- tmin2019_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_01)<- c("OBJECTID", "tmin_01")
tmin2019_02<- tmin2019_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_02)<- c("OBJECTID", "tmin_02")
tmin2019_03<- tmin2019_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_03)<- c("OBJECTID", "tmin_03")
tmin2019_04<- tmin2019_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_04)<- c("OBJECTID", "tmin_04")
tmin2019_05<- tmin2019_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_05)<- c("OBJECTID", "tmin_05")
tmin2019_06<- tmin2019_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_06)<- c("OBJECTID", "tmin_06")
tmin2019_07<- tmin2019_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_07)<- c("OBJECTID", "tmin_07")
tmin2019_08<- tmin2019_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_08)<- c("OBJECTID", "tmin_08")
tmin2019_09<- tmin2019_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_09)<- c("OBJECTID", "tmin_09")
tmin2019_10<- tmin2019_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_10)<- c("OBJECTID", "tmin_10")
tmin2019_11<- tmin2019_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_11)<- c("OBJECTID", "tmin_11")
tmin2019_12<- tmin2019_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2019_12)<- c("OBJECTID", "tmin_12")

tmax2019_01<- tmax2019_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_01)<- c("OBJECTID", "tmax_01")
tmax2019_02<- tmax2019_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_02)<- c("OBJECTID", "tmax_02")
tmax2019_03<- tmax2019_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_03)<- c("OBJECTID", "tmax_03")
tmax2019_04<- tmax2019_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_04)<- c("OBJECTID", "tmax_04")
tmax2019_05<- tmax2019_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_05)<- c("OBJECTID", "tmax_05")
tmax2019_06<- tmax2019_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_06)<- c("OBJECTID", "tmax_06")
tmax2019_07<- tmax2019_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_07)<- c("OBJECTID", "tmax_07")
tmax2019_08<- tmax2019_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_08)<- c("OBJECTID", "tmax_08")
tmax2019_09<- tmax2019_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_09)<- c("OBJECTID", "tmax_09")
tmax2019_10<- tmax2019_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_10)<- c("OBJECTID", "tmax_10")
tmax2019_11<- tmax2019_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_11)<- c("OBJECTID", "tmax_11")
tmax2019_12<- tmax2019_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2019_12)<- c("OBJECTID", "tmax_12")

prec2019_01<- prec2019_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_01)<- c("OBJECTID", "prec_01")
prec2019_02<- prec2019_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_02)<- c("OBJECTID", "prec_02")
prec2019_03<- prec2019_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_03)<- c("OBJECTID", "prec_03")
prec2019_04<- prec2019_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_04)<- c("OBJECTID", "prec_04")
prec2019_05<- prec2019_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_05)<- c("OBJECTID", "prec_05")
prec2019_06<- prec2019_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_06)<- c("OBJECTID", "prec_06")
prec2019_07<- prec2019_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_07)<- c("OBJECTID", "prec_07")
prec2019_08<- prec2019_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_08)<- c("OBJECTID", "prec_08")
prec2019_09<- prec2019_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_09)<- c("OBJECTID", "prec_09")
prec2019_10<- prec2019_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_10)<- c("OBJECTID", "prec_10")
prec2019_11<- prec2019_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_11)<- c("OBJECTID", "prec_11")
prec2019_12<- prec2019_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2019_12)<- c("OBJECTID", "prec_12")


data2019<- inner_join(tmin2019_01, tmin2019_02) %>%
  inner_join(., tmin2019_03) %>% 
  inner_join(., tmin2019_04) %>% 
  inner_join(., tmin2019_05) %>% 
  inner_join(., tmin2019_06) %>% 
  inner_join(., tmin2019_07) %>% 
  inner_join(., tmin2019_08) %>% 
  inner_join(., tmin2019_09) %>% 
  inner_join(., tmin2019_10) %>% 
  inner_join(., tmin2019_11) %>% 
  inner_join(., tmin2019_12) %>% 
  inner_join(., tmax2019_01) %>%
  inner_join(., tmax2019_02) %>%
  inner_join(., tmax2019_03) %>% 
  inner_join(., tmax2019_04) %>% 
  inner_join(., tmax2019_05) %>% 
  inner_join(., tmax2019_06) %>% 
  inner_join(., tmax2019_07) %>% 
  inner_join(., tmax2019_08) %>% 
  inner_join(., tmax2019_09) %>% 
  inner_join(., tmax2019_10) %>% 
  inner_join(., tmax2019_11) %>% 
  inner_join(., tmax2019_12) %>% 
  inner_join(., prec2019_01) %>%
  inner_join(., prec2019_02) %>%
  inner_join(., prec2019_03) %>% 
  inner_join(., prec2019_04) %>% 
  inner_join(., prec2019_05) %>% 
  inner_join(., prec2019_06) %>% 
  inner_join(., prec2019_07) %>% 
  inner_join(., prec2019_08) %>% 
  inner_join(., prec2019_09) %>% 
  inner_join(., prec2019_10) %>% 
  inner_join(., prec2019_11) %>% 
  inner_join(., prec2019_12)

data2019<- inner_join(data2019, buffer2019_ID)

## 2020



tmin2020_01<- tmin2020_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_01)<- c("OBJECTID", "tmin_01")
tmin2020_02<- tmin2020_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_02)<- c("OBJECTID", "tmin_02")
tmin2020_03<- tmin2020_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_03)<- c("OBJECTID", "tmin_03")
tmin2020_04<- tmin2020_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_04)<- c("OBJECTID", "tmin_04")
tmin2020_05<- tmin2020_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_05)<- c("OBJECTID", "tmin_05")
tmin2020_06<- tmin2020_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_06)<- c("OBJECTID", "tmin_06")
tmin2020_07<- tmin2020_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_07)<- c("OBJECTID", "tmin_07")
tmin2020_08<- tmin2020_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_08)<- c("OBJECTID", "tmin_08")
tmin2020_09<- tmin2020_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_09)<- c("OBJECTID", "tmin_09")
tmin2020_10<- tmin2020_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_10)<- c("OBJECTID", "tmin_10")
tmin2020_11<- tmin2020_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_11)<- c("OBJECTID", "tmin_11")
tmin2020_12<- tmin2020_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2020_12)<- c("OBJECTID", "tmin_12")

tmax2020_01<- tmax2020_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_01)<- c("OBJECTID", "tmax_01")
tmax2020_02<- tmax2020_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_02)<- c("OBJECTID", "tmax_02")
tmax2020_03<- tmax2020_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_03)<- c("OBJECTID", "tmax_03")
tmax2020_04<- tmax2020_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_04)<- c("OBJECTID", "tmax_04")
tmax2020_05<- tmax2020_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_05)<- c("OBJECTID", "tmax_05")
tmax2020_06<- tmax2020_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_06)<- c("OBJECTID", "tmax_06")
tmax2020_07<- tmax2020_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_07)<- c("OBJECTID", "tmax_07")
tmax2020_08<- tmax2020_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_08)<- c("OBJECTID", "tmax_08")
tmax2020_09<- tmax2020_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_09)<- c("OBJECTID", "tmax_09")
tmax2020_10<- tmax2020_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_10)<- c("OBJECTID", "tmax_10")
tmax2020_11<- tmax2020_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_11)<- c("OBJECTID", "tmax_11")
tmax2020_12<- tmax2020_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2020_12)<- c("OBJECTID", "tmax_12")

prec2020_01<- prec2020_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_01)<- c("OBJECTID", "prec_01")
prec2020_02<- prec2020_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_02)<- c("OBJECTID", "prec_02")
prec2020_03<- prec2020_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_03)<- c("OBJECTID", "prec_03")
prec2020_04<- prec2020_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_04)<- c("OBJECTID", "prec_04")
prec2020_05<- prec2020_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_05)<- c("OBJECTID", "prec_05")
prec2020_06<- prec2020_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_06)<- c("OBJECTID", "prec_06")
prec2020_07<- prec2020_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_07)<- c("OBJECTID", "prec_07")
prec2020_08<- prec2020_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_08)<- c("OBJECTID", "prec_08")
prec2020_09<- prec2020_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_09)<- c("OBJECTID", "prec_09")
prec2020_10<- prec2020_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_10)<- c("OBJECTID", "prec_10")
prec2020_11<- prec2020_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_11)<- c("OBJECTID", "prec_11")
prec2020_12<- prec2020_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2020_12)<- c("OBJECTID", "prec_12")


data2020<- inner_join(tmin2020_01, tmin2020_02) %>%
  inner_join(., tmin2020_03) %>% 
  inner_join(., tmin2020_04) %>% 
  inner_join(., tmin2020_05) %>% 
  inner_join(., tmin2020_06) %>% 
  inner_join(., tmin2020_07) %>% 
  inner_join(., tmin2020_08) %>% 
  inner_join(., tmin2020_09) %>% 
  inner_join(., tmin2020_10) %>% 
  inner_join(., tmin2020_11) %>% 
  inner_join(., tmin2020_12) %>% 
  inner_join(., tmax2020_01) %>%
  inner_join(., tmax2020_02) %>%
  inner_join(., tmax2020_03) %>% 
  inner_join(., tmax2020_04) %>% 
  inner_join(., tmax2020_05) %>% 
  inner_join(., tmax2020_06) %>% 
  inner_join(., tmax2020_07) %>% 
  inner_join(., tmax2020_08) %>% 
  inner_join(., tmax2020_09) %>% 
  inner_join(., tmax2020_10) %>% 
  inner_join(., tmax2020_11) %>% 
  inner_join(., tmax2020_12) %>% 
  inner_join(., prec2020_01) %>%
  inner_join(., prec2020_02) %>%
  inner_join(., prec2020_03) %>% 
  inner_join(., prec2020_04) %>% 
  inner_join(., prec2020_05) %>% 
  inner_join(., prec2020_06) %>% 
  inner_join(., prec2020_07) %>% 
  inner_join(., prec2020_08) %>% 
  inner_join(., prec2020_09) %>% 
  inner_join(., prec2020_10) %>% 
  inner_join(., prec2020_11) %>% 
  inner_join(., prec2020_12)

data2020<- inner_join(data2020, buffer2020_ID)

## 2021 



tmin2021_01<- tmin2021_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_01)<- c("OBJECTID", "tmin_01")
tmin2021_02<- tmin2021_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_02)<- c("OBJECTID", "tmin_02")
tmin2021_03<- tmin2021_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_03)<- c("OBJECTID", "tmin_03")
tmin2021_04<- tmin2021_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_04)<- c("OBJECTID", "tmin_04")
tmin2021_05<- tmin2021_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_05)<- c("OBJECTID", "tmin_05")
tmin2021_06<- tmin2021_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_06)<- c("OBJECTID", "tmin_06")
tmin2021_07<- tmin2021_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_07)<- c("OBJECTID", "tmin_07")
tmin2021_08<- tmin2021_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_08)<- c("OBJECTID", "tmin_08")
tmin2021_09<- tmin2021_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_09)<- c("OBJECTID", "tmin_09")
tmin2021_10<- tmin2021_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_10)<- c("OBJECTID", "tmin_10")
tmin2021_11<- tmin2021_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_11)<- c("OBJECTID", "tmin_11")
tmin2021_12<- tmin2021_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmin2021_12)<- c("OBJECTID", "tmin_12")

tmax2021_01<- tmax2021_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_01)<- c("OBJECTID", "tmax_01")
tmax2021_02<- tmax2021_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_02)<- c("OBJECTID", "tmax_02")
tmax2021_03<- tmax2021_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_03)<- c("OBJECTID", "tmax_03")
tmax2021_04<- tmax2021_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_04)<- c("OBJECTID", "tmax_04")
tmax2021_05<- tmax2021_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_05)<- c("OBJECTID", "tmax_05")
tmax2021_06<- tmax2021_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_06)<- c("OBJECTID", "tmax_06")
tmax2021_07<- tmax2021_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_07)<- c("OBJECTID", "tmax_07")
tmax2021_08<- tmax2021_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_08)<- c("OBJECTID", "tmax_08")
tmax2021_09<- tmax2021_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_09)<- c("OBJECTID", "tmax_09")
tmax2021_10<- tmax2021_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_10)<- c("OBJECTID", "tmax_10")
tmax2021_11<- tmax2021_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_11)<- c("OBJECTID", "tmax_11")
tmax2021_12<- tmax2021_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(tmax2021_12)<- c("OBJECTID", "tmax_12")

prec2021_01<- prec2021_01 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_01)<- c("OBJECTID", "prec_01")
prec2021_02<- prec2021_02 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_02)<- c("OBJECTID", "prec_02")
prec2021_03<- prec2021_03 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_03)<- c("OBJECTID", "prec_03")
prec2021_04<- prec2021_04 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_04)<- c("OBJECTID", "prec_04")
prec2021_05<- prec2021_05 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_05)<- c("OBJECTID", "prec_05")
prec2021_06<- prec2021_06 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_06)<- c("OBJECTID", "prec_06")
prec2021_07<- prec2021_07 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_07)<- c("OBJECTID", "prec_07")
prec2021_08<- prec2021_08 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_08)<- c("OBJECTID", "prec_08")
prec2021_09<- prec2021_09 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_09)<- c("OBJECTID", "prec_09")
prec2021_10<- prec2021_10 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_10)<- c("OBJECTID", "prec_10")
prec2021_11<- prec2021_11 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_11)<- c("OBJECTID", "prec_11")
prec2021_12<- prec2021_12 %>% dplyr::select(c(OBJECTID_1, MEAN))
colnames(prec2021_12)<- c("OBJECTID", "prec_12")


data2021<- inner_join(tmin2021_01, tmin2021_02) %>%
  inner_join(., tmin2021_03) %>% 
  inner_join(., tmin2021_04) %>% 
  inner_join(., tmin2021_05) %>% 
  inner_join(., tmin2021_06) %>% 
  inner_join(., tmin2021_07) %>% 
  inner_join(., tmin2021_08) %>% 
  inner_join(., tmin2021_09) %>% 
  inner_join(., tmin2021_10) %>% 
  inner_join(., tmin2021_11) %>% 
  inner_join(., tmin2021_12) %>% 
  inner_join(., tmax2021_01) %>%
  inner_join(., tmax2021_02) %>%
  inner_join(., tmax2021_03) %>% 
  inner_join(., tmax2021_04) %>% 
  inner_join(., tmax2021_05) %>% 
  inner_join(., tmax2021_06) %>% 
  inner_join(., tmax2021_07) %>% 
  inner_join(., tmax2021_08) %>% 
  inner_join(., tmax2021_09) %>% 
  inner_join(., tmax2021_10) %>% 
  inner_join(., tmax2021_11) %>% 
  inner_join(., tmax2021_12) %>% 
  inner_join(., prec2021_01) %>%
  inner_join(., prec2021_02) %>%
  inner_join(., prec2021_03) %>% 
  inner_join(., prec2021_04) %>% 
  inner_join(., prec2021_05) %>% 
  inner_join(., prec2021_06) %>% 
  inner_join(., prec2021_07) %>% 
  inner_join(., prec2021_08) %>% 
  inner_join(., prec2021_09) %>% 
  inner_join(., prec2021_10) %>% 
  inner_join(., prec2021_11) %>% 
  inner_join(., prec2021_12)

data2021<- inner_join(data2021, buffer2021_ID)


data_final<- rbind(data2010, data2011, data2012, data2013, data2014,
                   data2015, data2016, data2017, data2018, data2019,
                   data2020,
                   data2021)


write.csv(data_final, "concatenated_climatedata.csv")

