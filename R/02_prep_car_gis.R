
# https://www.car.gov.br/publico/imoveis/index
# https://sidra.ibge.gov.br/pesquisa/censo-agropecuario/censo-agropecuario-2017#caracteristicas-estabelecimentos

library(tidyverse)
library(sf)
library(riverdist)
# Extent
myext <- st_read("data/vector/AP_muni_Macapa_Cutias_clipbuff2km.shp")
# Muni poly
muni_poly <- st_read("data/vector/AP_muni_Macapa_Cutias.shp") %>% 
  st_transform(31976)

# Clipped muni poly
muni_poly_clip <- st_read("data/vector/AP_muni_Macapa_Cutias_clip.shp") %>% 
  st_union()
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

# Sample points at regular intervals
(st_length(rio_urucurituba)/1000)/50 # total = 37.8 km 0.757 km
(st_length(rio_gurijuba)/1000)/65 # total = 48.8 km 0.752 km

rio_urucurituba_points  <- st_cast(rio_urucurituba, "LINESTRING") %>% st_union() %>%  
  st_sample(size = 50, type = "regular") %>% st_cast("POINT") %>% data.frame() %>% st_as_sf()
rio_gurijuba_points  <- st_cast(rio_gurijuba, "LINESTRING") %>% st_union() %>%  
  st_sample(size = 65, type = "regular") %>% st_cast("POINT") %>% data.frame() %>% st_as_sf()

# Distance along river from Amazon. Include vertices at 30 m
# Make riverdist object
# 1273 vertices
rd_urucurituba <- cleanup(line2network(as(rio_urucurituba, "Spatial")))
# 1672 vertices
rd_gurijuba <- cleanup(line2network(as(rio_gurijuba, "Spatial")))

# Add sample points
points_riv_urucurituba <- xy2segvert(x=st_coordinates(rio_urucurituba_points)[,1], 
                                     y=st_coordinates(rio_urucurituba_points)[,2], 
                                     rivers=rd_urucurituba)
plot(rd_urucurituba)
riverpoints(seg=points_riv_urucurituba$seg, 
            vert=points_riv_urucurituba$vert, rivers=rd_urucurituba, pch=5, 
            col="blue")
#
points_riv_gurijuba <- xy2segvert(x=st_coordinates(rio_gurijuba_points)[,1], 
                                     y=st_coordinates(rio_gurijuba_points)[,2], 
                                     rivers=rd_gurijuba)
plot(rd_gurijuba)
riverpoints(seg=points_riv_gurijuba$seg, 
            vert=points_riv_gurijuba$vert, rivers=rd_gurijuba, pch=5, 
            col="blue")

# Add distances to points
rio_urucurituba_points$dist_amazonas_km <-  round( 
  riverdistancetofrom(seg1=points_riv_urucurituba$seg, 
                    vert1=points_riv_urucurituba$vert, 
                    seg2=1, vert2=1273, 
                    rivers=rd_urucurituba)/1000,3)
rio_urucurituba_points$aid <- paste("urucurituba", 
                                    rank(rio_urucurituba_points$dist_amazonas_km), 
                                    sep="_")
rio_urucurituba_points$nome_rio <- "Rio Urucurituba"
rio_gurijuba_points$dist_amazonas_km <- round( 
  riverdistancetofrom(seg1=points_riv_gurijuba$seg, 
                    vert1=points_riv_gurijuba$vert, 
                    seg2=1, vert2=1672, 
                    rivers=rd_gurijuba)/1000,3)
rio_gurijuba_points$aid <- paste("gurijuba", 
                                 rank(rio_gurijuba_points$dist_amazonas_km), 
                                 sep="_")
rio_gurijuba_points$nome_rio <- "Rio Gurijuba"

rio_pontos <- bind_rows(rio_urucurituba_points, rio_gurijuba_points)
# Buffers
bind_rows(
st_buffer(rio_pontos, dist=125) %>% 
    mutate(buff_dist = 125),
st_buffer(rio_pontos, dist=250) %>% 
  mutate(buff_dist = 250),
st_buffer(rio_pontos, dist=500) %>% 
  mutate(buff_dist = 500),
st_buffer(rio_pontos, dist=1000) %>% 
  mutate(buff_dist = 1000),
st_buffer(rio_pontos, dist=2000) %>% 
  mutate(buff_dist = 2000),
st_buffer(rio_pontos, dist=4000) %>% 
  mutate(buff_dist = 4000),
st_buffer(rio_pontos, dist=8000) %>% 
  mutate(buff_dist = 8000)
) %>% st_intersection(muni_poly_clip) -> rios_points_buffers
rios_points_buffers$buff_area_km2 <- round(as.numeric(units::set_units(st_area(rios_points_buffers),km^2)), 3)
mapview::mapview(rios_points_buffers)

# Export as gpkg
outfile <- "C:/Users/user/Documents/CA/baixo-araguari/data/vector/baixo_araguari.GPKG"
st_write(rio_pontos, dsn = outfile, 
         layer = "rio_pontos", delete_layer = TRUE, append = TRUE)
st_write(rios_points_buffers, dsn = outfile, 
         layer = "rios_points_buffers", delete_layer = TRUE, append = TRUE)
st_write(rio_gurijuba, dsn = outfile, 
         layer = "rio_gurijuba", delete_layer = TRUE, append = TRUE)
st_write(rio_urucurituba, dsn = outfile, 
         layer = "rio_urucurituba", delete_layer = TRUE, append = TRUE)
st_write(CAR_area_consolidada, dsn = outfile, 
         layer = "CAR_area_consolidada", delete_layer = TRUE, append = TRUE)
st_write(muni_poly, dsn = outfile, 
         layer = "muni_poly", delete_layer = TRUE, append = TRUE)

st_layers(outfile)
