# calculate metrics before running tutorial

library(landscapemetrics)
library(tidyverse)
library(sf)
library(raster)
library(terra)
library(readxl)

# Dados Mapbiomas
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1985.tif"
mapbiomas_1985 <- rast(rin)
mapbiomas_1985[mapbiomas_1985==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1986.tif"
mapbiomas_1986 <- rast(rin)
mapbiomas_1986[mapbiomas_1986==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1987.tif"
mapbiomas_1987 <- rast(rin)
mapbiomas_1987[mapbiomas_1987==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1988.tif"
mapbiomas_1988 <- rast(rin)
mapbiomas_1988[mapbiomas_1988==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1989.tif"
mapbiomas_1989 <- rast(rin)
mapbiomas_1989[mapbiomas_1989==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1992.tif"
mapbiomas_1992 <- rast(rin)
mapbiomas_1992[mapbiomas_1992==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1993.tif"
mapbiomas_1993 <- rast(rin)
mapbiomas_1993[mapbiomas_1993==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1994.tif"
mapbiomas_1994 <- rast(rin)
mapbiomas_1994[mapbiomas_1994==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1995.tif"
mapbiomas_1995 <- rast(rin)
mapbiomas_1995[mapbiomas_1995==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1996.tif"
mapbiomas_1996 <- rast(rin)
mapbiomas_1996[mapbiomas_1996==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_1999.tif"
mapbiomas_1999 <- rast(rin)
mapbiomas_1999[mapbiomas_1999==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2000.tif"
mapbiomas_2000 <- rast(rin)
mapbiomas_2000[mapbiomas_2000==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2001.tif"
mapbiomas_2001 <- rast(rin)
mapbiomas_2001[mapbiomas_2001==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2002.tif"
mapbiomas_2002 <- rast(rin)
mapbiomas_2002[mapbiomas_2002==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2003.tif"
mapbiomas_2003 <- rast(rin)
mapbiomas_2003[mapbiomas_2003==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2008.tif"
mapbiomas_2008 <- rast(rin)
mapbiomas_2008[mapbiomas_2008==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2009.tif"
mapbiomas_2009 <- rast(rin)
mapbiomas_2009[mapbiomas_2009==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2010.tif"
mapbiomas_2010 <- rast(rin)
mapbiomas_2010[mapbiomas_2010==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2011.tif"
mapbiomas_2011 <- rast(rin)
mapbiomas_2011[mapbiomas_2011==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2012.tif"
mapbiomas_2012 <- rast(rin)
mapbiomas_2012[mapbiomas_2012==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2016.tif"
mapbiomas_2016 <- rast(rin)
mapbiomas_2016[mapbiomas_2016==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2017.tif"
mapbiomas_2017 <- rast(rin)
mapbiomas_2017[mapbiomas_2017==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2018.tif"
mapbiomas_2018 <- rast(rin)
mapbiomas_2018[mapbiomas_2018==0] <- NA
rin <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2019.tif"
mapbiomas_2019 <- rast(rin)
mapbiomas_2019[mapbiomas_2019==0] <- NA
rin2020 <- "data/raster/AP_utm_muni_cutias/utm_cover_AP_muni_cutias_2020.tif"
mapbiomas_2020 <- rast(rin2020)
mapbiomas_2020[mapbiomas_2020==0] <- NA

mapbiomas_1985_2020 <- c(mapbiomas_1985, mapbiomas_1986, mapbiomas_1987, 
                         mapbiomas_1988, mapbiomas_1989, 
                         mapbiomas_1992, mapbiomas_1993, mapbiomas_1994, 
                         mapbiomas_1995, mapbiomas_1996,
                         mapbiomas_1999, mapbiomas_2000, mapbiomas_2001, 
                         mapbiomas_2002, mapbiomas_2003, 
                         mapbiomas_2008, mapbiomas_2009, mapbiomas_2010,
                         mapbiomas_2011, mapbiomas_2012,
                         mapbiomas_2016, mapbiomas_2017, mapbiomas_2018, 
                         mapbiomas_2019, mapbiomas_2020)
plot(subset(mapbiomas_1985_2020,1), type="classes")
# Crop
my_bb <- st_read("data/vector/AP_muni_Macapa_Cutias_clipbuff2km.shp") %>% 
  st_bbox()
my_bb[1] <- 553378
myextent <- ext(my_bb)
mapbiomas_1985_2020 <- crop(mapbiomas_1985_2020, myextent)
plot(subset(mapbiomas_1985_2020,1))

# Reclassify
# Human all together
class_nomes <- read_excel("data/raster/AP_utm_muni_cutias/mapbiomas_6_legend.xlsx")
class_antropic <- class_nomes %>% 
  filter(type_class == "antropic") %>% pull(aid)
reclass_m <- as.matrix(data.frame(human = class_antropic, new_value = 18))
mapbiomas_1985_2020 <- classify(mapbiomas_1985_2020, reclass_m)
plot(subset(mapbiomas_1985_2020,1), type="classes")


# Dados: Vector (rios, pontos de amostragem, buffers)
meuSIG <- "data/vector/baixo_araguari.GPKG"
# pontos cada 0.75 km
rio_pontos <- sf::st_read(meuSIG, layer = "rio_pontos")
# linha central
rio_gurijuba <- sf::st_read(meuSIG, layer = "rio_gurijuba") 
rio_urucurituba <- sf::st_read(meuSIG, layer = "rio_urucurituba") 
# municipios
municipios_Macapa_Cutias <- sf::st_read(meuSIG, layer = "muni_poly") 
# buffers
rios_points_buffers <- sf::st_read(meuSIG, layer = "rios_points_buffers")

# Buffers
mybuffers <- rios_points_buffers %>% 
  filter(buff_dist %in% c(125, 250, 500, 1000, 2000, 4000), 
         nome_rio == "Rio Urucurituba")

# Save objects for later use
saveRDS(mybuffers, "data/mybuffers.RDS")
saveRDS(mapbiomas_1985_2020, "data/mapbiomas_1985_2020.RDS")
writeRaster(mapbiomas_1985_2020, "data/raster/mapbiomas_1985_2020.tif", 
            datatype = "INT2U", overwrite = TRUE)

#clear temporary files
tmpFiles(current =TRUE, remove = TRUE) 

# Metricás 
minhas_metricas <- c("lsm_c_cpland", 
                     "lsm_c_np",
                     "lsm_c_area_mn", "lsm_c_area_sd", "lsm_c_area_cv", 
                     "lsm_c_enn_mn", "lsm_c_enn_sd", "lsm_c_enn_cv", 
                     "lsm_c_pd", "lsm_c_cohesion")

# Calcular as metricás (50 minutos mais ou menos)
# 20 anos X 9 metricas x 50 pontos X 3 buffers
# Numero de classes mude conforme ano, ponto e buffer
res_metricas <- sample_lsm(mapbiomas_1985_2020, 
                           y = mybuffers, 
                           what = minhas_metricas, 
                           plot_id = data.frame(mybuffers)[, 'aid_buff'], 
                           edge_depth=1)
saveRDS(res_metricas, "data/res_metricas.RDS")
