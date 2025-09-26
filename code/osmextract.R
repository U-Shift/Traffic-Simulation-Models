library(tidyverse)
# library(osmextract)
library(osmdata)
library(sf)

bbox = c(-9.285164,38.706142,-9.199076,38.752611)

Oeiras = opq(bbox) |> 
  add_osm_feature(key = 'highway') |> 
  osmdata_xml()
class(Oeiras$node)

q1 <- opq('Sevilla') %>%
  add_osm_feature(key = 'highway', value = 'cycleway')
cway_sev <- osmdata_sp(q1)
sp::plot(cway_sev$osm_lines)

q <- opq(bbox = c(51.1, 0.1, 51.2, 0.2))



# Lisbon road network -----------------------------------------------------

# use Hot Export tool to export from OpenStreetMap
Lisbon_road_network = st_read("data/Lisbon/Lisbon_road_network.gpkg")


# POIs --------------------------------------------------------------------

POIs = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/osm_poi_lisbon.geojson")
st_write(POIs, "data/Lisbon/POIs_Lx.gpkg", overwrite = TRUE)
piggyback::pb_upload("data/Lisbon/POIs_Lx.gpkg", "POIs_Lx.gpkg", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")


# count points by type in areas
POIs_in_AREAS <- st_join(POIs, AREAS, join = st_within)

counts_group <- POIs_in_AREAS %>%
  st_drop_geometry() %>%         # drop geometry for counting
  group_by(id, group) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = group, values_from = n, values_fill = 0)

counts_type <- POIs_in_AREAS %>%
  st_drop_geometry() %>%
  group_by(id, type) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0)

AREAS_counts <- AREAS %>%
  left_join(counts_group, by = "id") %>%
  left_join(counts_type,  by = "id") |> 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

mapview(AREAS_counts, zcol = "healthcare")
st_write(AREAS_counts, "data/Lisbon/CENSUS_POIs_Lx.gpkg", delete_dsn = TRUE)
piggyback::pb_upload("data/Lisbon/CENSUS_POIs_Lx.gpkg", "CENSUS_POIs_Lx.gpkg", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")
