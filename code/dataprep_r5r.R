# data prep r5r

library(tidyverse)
library(sf)

# Censos pontos
BGRI = st_read("/media/rosa/Dados/GIS/IMOB/BGRI21_170.gpkg")
PONTOS = BGRI |>
  filter(DTMN21 == "1106") |>
  st_centroid() |>
  st_transform(4326) |> 
  select(BGRI2021, N_EDIFICIOS_CLASSICOS, N_NUCLEOS_FAMILIARES, N_INDIVIDUOS)
PONTOS$id = rownames(PONTOS)
PONTOS = PONTOS |> select(id, BGRI2021, N_EDIFICIOS_CLASSICOS, N_NUCLEOS_FAMILIARES, N_INDIVIDUOS) |> 
  rename(buildings = N_EDIFICIOS_CLASSICOS, families = N_NUCLEOS_FAMILIARES, residents = N_INDIVIDUOS)
sum(PONTOS$residents) # 545.796 ok

write_sf(PONTOS, "data/r5r/Censos_Lx.gpkg", overwrite = TRUE)
piggyback::pb_upload("data/r5r/Censos_Lx.gpkg", "Censos_Lx.gpkg", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")

# Rede viaria principal
REDEbase = st_read("/media/rosa/Dados/GIS/Declives-RedeViaria/shapefiles/RedeViariaLisboa_osm.shp")
REDEbase = REDEbase |>
  filter(highway %in% c("primary","primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link")) |> 
  select(osm_id, name, highway, geometry)

write_sf(REDEbase, "data/r5r/REDEbase_Lx.gpkg", overwrite = TRUE)
piggyback::pb_upload("data/r5r/REDEbase_Lx.gpkg", "REDEbase_Lx.gpkg", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")


# r5r Lisboa com Metro e Carris
# preciso de Lisboa.osm.pbf

# preciso de GTFS de Metro e Carris
# os mesmos da aula anterior
# https://github.com/rosamfelix/PGmob360/releases/download/2024.11/metro_gtfs.zip
# https://github.com/rosamfelix/PGmob360/releases/download/2024.11/carris_gtfs.zip

# preciso de declives
# copia de /media/rosa/Dados/GIS/R5R/data/LisboaIST_clip_r1.tif

# coloquei tudo em original/r5r
r5r_lisboa = setup_r5(data_path = "original/r5r/")
# fiz um r5r_lisboa.zip só com os ficheiros network
piggyback::pb_upload("original/r5r/r5r_lisboa.zip", "r5r_lisboa.zip", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")



