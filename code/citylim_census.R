library(tidyverse)
library(sf)
census = st_read("data/Lisbon/BGRI2021_1106.gpkg") #polygons
plot(census$geom)

# points
census_poitns = census |> 
  st_transform(4326) |> 
  st_centroid()
plot(census_poitns)
names(census_poitns)

census_poitns = census_poitns |>
  select(BGRI2021, N_INDIVIDUOS) |>
  rename(id = BGRI2021,
         residents = N_INDIVIDUOS)
st_write(census_poitns, "data/census_poitns.gpkg")

#dissolve 
city_limit = census |> st_union() |> st_as_sf()
plot(city_limit)
