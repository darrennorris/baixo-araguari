
# https://www.car.gov.br/publico/imoveis/index
# https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/censo-agropecuario-2017#caracteristicas-estabelecimentos

library(tidyverse)
library(sf)
# Extent
myext <- st_read("data/vector/AP_muni_Macapa_Cutias_clipbuff2km.shp")
# CAR 1600212 = Cutias 
unzip("data/vector/CAR/SHAPE_1600212/APP.zip", 
      exdir="data/vector/CAR/SHAPE_1600212/APP")
unzip("data/vector/CAR/SHAPE_1600212/AREA_CONSOLIDADA.zip", 
      exdir="data/vector/CAR/SHAPE_1600212/AREA_CONSOLIDADA")
unzip("data/vector/CAR/SHAPE_1600212/AREA_IMOVEL.zip", 
      exdir="data/vector/CAR/SHAPE_1600212/AREA_IMOVEL")
unzip("data/vector/CAR/SHAPE_1600212/HIDROGRAFIA.zip", 
      exdir="data/vector/CAR/SHAPE_1600212/HIDROGRAFIA")
unzip("data/vector/CAR/SHAPE_1600212/AREA_IMOVEL.zip", 
      exdir="data/vector/CAR/SHAPE_1600212/AREA_IMOVEL")
# CAR / 1600303 = Macapá
unzip("data/vector/CAR/SHAPE_1600303/AREA_CONSOLIDADA.zip", 
      exdir="data/vector/CAR/SHAPE_1600303/AREA_CONSOLIDADA")

# Join CAR for different municipalites
area_con_cutias <- read_sf("data/vector/CAR/SHAPE_1600212/AREA_CONSOLIDADA") %>% 
  st_make_valid() %>% st_transform(31976) %>% st_buffer(dist = 0.001) %>% 
  st_crop(st_bbox(myext)) %>% mutate(muni = "Cutias")
area_con_macapa <- read_sf("data/vector/CAR/SHAPE_1600303/AREA_CONSOLIDADA")  %>% 
  st_make_valid() %>% st_transform(31976) %>% st_buffer(dist = 0.001) %>% 
  st_crop(st_bbox(myext)) %>% mutate(muni = "Macapá")
CAR_area_consolidada <- bind_rows(area_con_cutias, area_con_macapa)

# Rivers
rio_urucurituba <- read_sf("data/vector/urucurituba.shp")
rio_gurijuba <- read_sf("data/vector/gurijuba.shp")
