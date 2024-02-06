# script to make some generic maps for the creel SFR project
# CJD 1.3.2024

library(tidyverse)
library(ggpubr)
library(wdnr.fmdb)
library(wdnr.gis)

## loading in shape files for map making ##
hdro=st_read("C:/Users/dassocju/OneDrive - State of Wisconsin/replacementCosts/24k_Hydro_Waterbodies_(Open_Water).shp")
ctyBounds=st_read("C:/Users/dassocju/OneDrive - State of Wisconsin/replacementCosts/cnty_bnd_ar.shp")
stBound=st_read("C:/Users/dassocju/OneDrive - State of Wisconsin/replacementCosts/state_bnd_ar.shp")
ctBounds=st_read("C:/Users/dassocju/OneDrive - State of Wisconsin/replacementCosts/TRIBAL_LANDS/ceded_territory_ar.shp")
tribalBounds=st_read("C:/Users/dassocju/OneDrive - State of Wisconsin/replacementCosts/TRIBAL_LANDS/tribal_land_ar.shp")


# first make a map of WI with county boundaries drawn on it

wisc=stBound

wiscCty=ggplot(data = wisc)+theme_void()+
  geom_sf(fill=NA)+
  geom_sf(data = ctyBounds, fill=NA)
wiscCty

ggplot()+theme_void()+
  geom_sf(data=wisc,fill=NA)+
  geom_sf(data=ctBounds,fill="grey")


ggplot()+theme_void()+
  geom_sf(data=wisc,fill=NA)+
  geom_sf(data=ctBounds,fill=NA)+
  geom_sf(data=hdro[hdro$WATERBOD_2%in%lchar$wbic[lchar$trtystat!=0],],fill=NA)+
  geom_sf(data=tribalBounds, fill="orange")+
  coord_sf(ylim = c(44.5,47),crs = "+proj=longlat")


