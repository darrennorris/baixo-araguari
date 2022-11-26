# Make animation


library(tidyverse)
library(sf)
library(raster)
library(terra)
library(tmap)
library(gridExtra)
library(kableExtra)
library(leafpop)
library(mapview)
library(mgcv)
library(readxl)
library(landscapemetrics)

# Load files
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1985.tif"
mapbiomas_1985 <- rast(rin)
mapbiomas_1985[mapbiomas_1985==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1988.tif"
mapbiomas_1988 <- rast(rin)
mapbiomas_1988[mapbiomas_1988==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1991.tif"
mapbiomas_1991 <- rast(rin)
mapbiomas_1991[mapbiomas_1991==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1994.tif"
mapbiomas_1994 <- rast(rin)
mapbiomas_1994[mapbiomas_1994==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1997.tif"
mapbiomas_1997 <- rast(rin)
mapbiomas_1997[mapbiomas_1997==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2000.tif"
mapbiomas_2000 <- rast(rin)
mapbiomas_2000[mapbiomas_2000==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2003.tif"
mapbiomas_2003 <- rast(rin)
mapbiomas_2003[mapbiomas_2003==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2006.tif"
mapbiomas_2006 <- rast(rin)
mapbiomas_2006[mapbiomas_2006==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2009.tif"
mapbiomas_2009 <- rast(rin)
mapbiomas_2009[mapbiomas_2009==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2012.tif"
mapbiomas_2012 <- rast(rin)
mapbiomas_2012[mapbiomas_2012==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2015.tif"
mapbiomas_2015 <- rast(rin)
mapbiomas_2015[mapbiomas_2015==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2018.tif"
mapbiomas_2018 <- rast(rin)
mapbiomas_2018[mapbiomas_2018==0] <- NA
rin2020 <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2020.tif"
mapbiomas_2020 <- rast(rin2020)
mapbiomas_2020[mapbiomas_2020==0] <- NA

# reclassify 
class_nomes <- read_excel("data/raster/AP_utm_muni_cutias/mapbiomas_6_legend.xlsx")
class_antropic <- class_nomes %>% 
  filter(type_class == "antropic") %>% pull(aid)

mapbiomas_1985_reclass <- mapbiomas_1985
mapbiomas_1985_reclass[mapbiomas_1985 %in% class_antropic] <- 18
mapbiomas_1988_reclass <- mapbiomas_1988
mapbiomas_1988_reclass[mapbiomas_1988 %in% class_antropic] <- 18
mapbiomas_1991_reclass <- mapbiomas_1991
mapbiomas_1991_reclass[mapbiomas_1991 %in% class_antropic] <- 18
mapbiomas_1994_reclass <- mapbiomas_1994
mapbiomas_1994_reclass[mapbiomas_1994 %in% class_antropic] <- 18
mapbiomas_1997_reclass <- mapbiomas_1997
mapbiomas_1997_reclass[mapbiomas_1997 %in% class_antropic] <- 18
mapbiomas_2000_reclass <- mapbiomas_2000
mapbiomas_2000_reclass[mapbiomas_2000 %in% class_antropic] <- 18
mapbiomas_2003_reclass <- mapbiomas_2003
mapbiomas_2003_reclass[mapbiomas_2003 %in% class_antropic] <- 18
mapbiomas_2006_reclass <- mapbiomas_2006
mapbiomas_2006_reclass[mapbiomas_2006 %in% class_antropic] <- 18
mapbiomas_2009_reclass <- mapbiomas_2009
mapbiomas_2009_reclass[mapbiomas_2009 %in% class_antropic] <- 18
mapbiomas_2012_reclass <- mapbiomas_2012
mapbiomas_2012_reclass[mapbiomas_2012 %in% class_antropic] <- 18
mapbiomas_2015_reclass <- mapbiomas_2015
mapbiomas_2015_reclass[mapbiomas_2015 %in% class_antropic] <- 18
mapbiomas_2018_reclass <- mapbiomas_2018
mapbiomas_2018_reclass[mapbiomas_2018 %in% class_antropic] <- 18
mapbiomas_2020_reclass <- mapbiomas_2020
mapbiomas_2020_reclass[mapbiomas_2020 %in% class_antropic] <- 18

# plot 
mapbiomas_1985_2020 <- c(mapbiomas_1985, mapbiomas_1988, mapbiomas_1991, 
                         mapbiomas_1994, mapbiomas_1997, mapbiomas_2000,
                         mapbiomas_2003,mapbiomas_2006, mapbiomas_2009, 
                         mapbiomas_2012, mapbiomas_2015, mapbiomas_2018,
                         mapbiomas_2020)
mapbiomas_1985_2020 <- classify(mapbiomas_1985_2020, cbind(0, NA))
reclass_m <- as.matrix(data.frame(human = class_antropic, new_value = 18))
mapbiomas_1985_2020 <- classify(mapbiomas_1985_2020, reclass_m)

mapbiomas_1985_2020_ag <- aggregate(mapbiomas_1985_2020, fact=5, fun="modal")
# Plot
vals85a20 <- c(unique(values(mapbiomas_1985_reclass)), 
               unique(values(mapbiomas_1991_reclass)), 
               unique(values(mapbiomas_1997_reclass)), 
               unique(values(mapbiomas_2003_reclass)), 
               unique(values(mapbiomas_2009_reclass)), 
               unique(values(mapbiomas_2015_reclass)), 
               unique(values(mapbiomas_2020_reclass)))
class_vals <- unique(vals85a20[is.finite(vals85a20)])
class_color <- class_nomes %>% 
  dplyr::filter(aid %in% class_vals) %>% dplyr::pull(hexadecimal_code)
class_color <- paste("#", class_color, sep="")
names(class_color) <-  class_nomes %>% filter(aid %in% class_vals) %>% pull(aid)
#labelas
my_label <- class_nomes %>% 
  dplyr::filter(aid %in% class_vals) %>% dplyr::pull(classe_descricao)
my_label <- ifelse(my_label=="Agricultura", "Antropico", my_label)
names(my_label) <- class_nomes %>% filter(aid %in% class_vals) %>% pull(aid)
tm_shape(mapbiomas_1985_2020_ag) + 
  tm_raster(style = "cat", 
            palette = class_color, labels = my_label, title="")  +
  tm_facets(ncol=2) + 
  #tm_scale_bar(breaks = c(0, 20, 40), text.size = 1, 
  #             position=c("right", "bottom")) +
  tm_layout(legend.bg.color="white", 
            legend.outside = TRUE,
            legend.outside.position = "right")

# animation
# zoom in
my_bb <- st_read("data/vector/AP_muni_Macapa_Cutias_clipbuff2km.shp") %>% 
  st_bbox()
my_bb[1] <- 553378
# animation
tm_ani <- tm_shape(mapbiomas_1985_2020_ag, bbox = my_bb) + 
  tm_raster(style = "cat", 
            palette = class_color, legend.show =FALSE)  +
  tm_facets(ncol=1, nrow=1) 

tmap_animation(tm_ani, delay=100, filename = "www/baixo_araguari.gif")
