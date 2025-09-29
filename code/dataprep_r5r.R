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

write_sf(PONTOS, "data/Lisbon/Censos_Lx.gpkg", overwrite = TRUE)
piggyback::pb_upload("data/Lisbon/Censos_Lx.gpkg", "Censos_Lx.gpkg", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")

# Censos areas
BGRI = st_read("/media/rosa/Dados/GIS/IMOB/BGRI21_170.gpkg")
AREAS = BGRI |>
  filter(DTMN21 == "1106") |>
  st_transform(4326) |>
  select(BGRI2021, N_EDIFICIOS_CLASSICOS, N_NUCLEOS_FAMILIARES, N_INDIVIDUOS) |> 
  rename(id = BGRI2021, buildings = N_EDIFICIOS_CLASSICOS, families = N_NUCLEOS_FAMILIARES, residents = N_INDIVIDUOS) |> 
  mutate(lon = st_coordinates(PONTOS)[,1],
         lat = st_coordinates(PONTOS)[,2])

# 



# Rede viaria principal
REDEbase_complete = st_read("data/Lisbon/Lisbon_road_network.gpkg")
REDEbase = REDEbase_complete |>
  filter(highway %in% c("primary","primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", "trunk", "trunk_link", "motorway", "motorway_link")) |> 
  select(osm_id, name, highway)
mapview::mapview(REDEbase)
REDEbase_redux = REDEbase_complete |>
  filter(highway %in% c("primary","primary_link", "secondary", "tertiary", "trunk", "motorway")) |> 
  select(osm_id, name, highway)
mapview::mapview(REDEbase_redux, zcol = "highway")

write_sf(REDEbase_redux, "data/Lisbon/REDEbase_Lx.gpkg", overwrite = TRUE)
piggyback::pb_upload("data/Lisbon/REDEbase_Lx.gpkg", "REDEbase_Lx.gpkg", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")


# r5r Lisboa com Metro e Carris
# preciso de Lisboa.osm.pbf

# preciso de GTFS de Metro e Carris
# os mesmos da aula anterior
# https://github.com/rosamfelix/PGmob360/releases/download/2024.11/metro_gtfs.zip
# https://github.com/rosamfelix/PGmob360/releases/download/2024.11/carris_gtfs.zip

# preciso de declives
# copia de /media/rosa/Dados/GIS/R5R/data/LisboaIST_clip_r1.tif

# coloquei tudo em data/r5r
r5r_lisboa = build_network(data_path = "data/Lisbon/r5r/", elevation = "TOBLER")
# fiz um r5r_lisboa.zip só com os ficheiros network
# Rede modelada com r5r
piggyback::pb_upload("data/Lisbon/r5r/r5r_Lisboa.zip", "r5r_lisboa.zip", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")



# od data and zones -------------------------------------------------------

zones = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/FREGUESIASgeo.Rds"))
lisbon_zones = zones |> filter(Concelho == "Lisboa")
st_write(lisbon_zones, "data/Lisbon/Freguesias_Lx.gpkg", overwrite = TRUE)
piggyback::pb_upload("data/Lisbon/Freguesias_Lx.gpkg", "Freguesias_Lx.gpkg", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")
# OD data
od_all = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/TRIPSmode_freguesias.Rds"))
od_lisbon = od_all |> 
  st_drop_geometry() |>
  filter(DICOFREor11 %in% lisbon_zones$Dicofre &
           DICOFREde11 %in% lisbon_zones$Dicofre) |> 
  mutate(DICOFREor11 = as.character(DICOFREor11), DICOFREde11 = as.character(DICOFREde11))
saveRDS(od_lisbon, "data/Lisbon/ODtrips_Freguesias_Lx.rds")
piggyback::pb_upload("data/Lisbon/ODtrips_Freguesias_Lx.rds", "ODtrips_Freguesias_Lx.rds", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")



# dem ---------------------------------------------------------------------

# using elevatr
devtools::install_github("usepa/elevatr")
library(elevatr)
dem_test = get_elev_raster(city_limit, z = 10)
terra::plot(dem_test)
terra::res(dem_test)
# not very useful, low res
