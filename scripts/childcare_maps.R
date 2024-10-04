#Childcare index maps
library(tidyverse)
library(sf)
library(tmap)

library("PerformanceAnalytics")


#GET DATA----

#data from here
#https://www.ons.gov.uk/peoplepopulationandcommunity/educationandchildcare/articles/childcareaccessibilitybyneighbourhood/latest
ch <- read_csv("data/childcare_places_accessible_per100children_LSOA.csv")

#Get local authority / LSOA2021 lookup
#And shapefile

#LSOA has local authorities already flagged, huzzah! With codes though, use grepl
lsoa <- st_read("../../MapPolygons/England/2021/England_lsoa_2021_bgc/england_lsoa_2021_bgc.shp")

#Bradford, Calderdale, Kirklees, Leeds and Wakefield
#And non-voting York leaving out for now
wy.lsoa <- lsoa %>% 
  filter(
    grepl('Bradford|Calderdale|Kirklees|Leeds|Wakefield',lsoa21nm,ignore.case = T)
  )

#Ah, has each LSOA code in also... but got em all
unique(sub(" .*", "", wy.lsoa$lsoa21nm))

#But but want to label the whole things, thusly (to overlay LAs...)
wy.lsoa$localauthname <- sub(" .*", "", wy.lsoa$lsoa21nm)



#local authority shapefiles
la <- st_read('../../MapPolygons/UK/LocalAuthorityDistricts/Local_Authority_Districts_December_2023_Boundaries_UK_BFC_9042356933902664268/LAD_DEC_2023_UK_BFC.shp')

wy.la <- la %>% 
  filter(
    grepl('Bradford|Calderdale|Kirklees|Leeds|Wakefield',LAD23NM,ignore.case = T)
  )


#LINK MAP AND CHILDCARE DATA----

#check link... TICK
table(wy.lsoa$lsoa21cd %in% ch$LSOA21CD)

wy.ch <- wy.lsoa %>% 
  left_join(
    ch,
    by = c('lsoa21cd' = 'LSOA21CD')
  )


#MAP!----

tmap_mode('view')

tmap_options(check.and.fix = TRUE)

tm_shape(wy.ch) + 
  # tm_polygons('Childcare accessibility', palette = 'viridis', style = 'fisher', alpha = 0.4, id = 'localauthname') +
  tm_polygons('Childcare accessibility', palette = 'RdYlGn', style = 'fisher', alpha = 0.4, id = 'localauthname') +
  # tm_polygons('Childcare accessibility', palette = '-BrBG', style = 'fisher', alpha = 0.5)
  tm_shape(wy.la) +
  tm_borders(lwd = 4)


#Driving vs public transport is interesting
chart.Correlation(wy.ch %>% st_set_geometry(NULL) %>% select(`Childcare accessibility`:`Childcare accessibility - good or outstanding places (overall)`), histogram=TRUE, pch=19)









