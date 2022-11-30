# calculate metrics before running tutorial

library(landscapemetrics)
library(tidyverse)
library(sf)
library(raster)
library(terra)
library(readxl)

#works one folder at a time
get_files <- function(folder_name = NA) {
  library(tidyverse)
  folder_location <- folder_name
  in_files <- list.files(folder_location, 
                         pattern = ".tif", full.names = TRUE)
  data.frame(folder_id = folder_location, file_id = in_files) %>%  
    group_by(folder_id, file_id) %>% 
    summarise(file_count = n()) %>% 
    ungroup() -> df_muni_tif
  return(df_muni_tif)
}
infolder <- "data/raster/AP_utm_muni_cutias"
df_muni_tif <- get_files(folder_name = infolder)
df_muni_tif %>% 
  mutate(ano = str_sub(file_id, -8, -5), 
  ) -> df_muni_tif
# Dados Mapbiomas
mytifs <- list.files("data/raster/AP_utm_muni_cutias", 
                  pattern='\\.tif$', full.names = TRUE)
mapbiomas_1985_2020 <- rast(mytifs)
# Crop
my_bb <- st_read("data/vector/AP_muni_Macapa_Cutias_clipbuff2km.shp") %>% 
  st_bbox()
my_bb[1] <- 553378
myextent <- ext(my_bb)
mapbiomas_1985_2020 <- terra::crop(mapbiomas_1985_2020, myextent)

# Reclassify
# Human all together
mapbiomas_1985_2020 <- classify(mapbiomas_1985_2020, cbind(0, NA))
plot(subset(mapbiomas_1985_2020,1), type="classes")
class_nomes <- read_excel("data/raster/AP_utm_muni_cutias/mapbiomas_6_legend.xlsx")
class_antropic <- class_nomes %>% 
  filter(type_class == "antropic") %>% pull(aid)
reclass_m <- as.matrix(data.frame(human = class_antropic, new_value = 18))
mapbiomas_1985_2020 <- classify(mapbiomas_1985_2020, reclass_m)
plot(subset(mapbiomas_1985_2020,1), type="classes")

mapbiomas_1985_2020_ag <- aggregate(mapbiomas_1985_2020, fact=5, fun="modal")
saveRDS(mapbiomas_1985_2020_ag,"data/mapbiomas_1985_2020_ag.RDS")

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

# 36 anos X 9 metricas x 50 pontos X 3 buffers
# Numero de classes mude conforme ano, ponto e buffer
res_metricas <- sample_lsm(mapbiomas_1985_2020, 
                           y = mybuffers, 
                           what = minhas_metricas, 
                           plot_id = data.frame(mybuffers)[, 'aid_buff'], 
                           edge_depth=1)
saveRDS(res_metricas, "data/res_metricas.RDS")
