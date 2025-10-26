library(sf)
library(tidyverse)
library(tidytransit)

# city limit 
geneve_limit = st_read("original/Ville_de_Genève.shp")
geneve_limit = st_transform(geneve_limit, 4326)

# very large swiss gtfs
swiss_gtfs = read_gtfs("original/gtfs_fp2025_20251002.zip")
summary(swiss_gtfs)
geneve_gtfs = swiss_gtfs |> filter_feed_by_area(geneve_limit) # crop in geneve


# Method 1 - with GTFSwizard ----------------------------------------------


# create shape_id variable when is missing
geneve_gtfs$trips = geneve_gtfs$trips |> mutate(shape_id = NA) # this is the trick!
write_gtfs(geneve_gtfs, "original/geneve_gtfs.zip")

geneve_gtfs_wshapes = GTFSwizard::read_gtfs("original/geneve_gtfs.zip") # create the shapes
summary(geneve_gtfs_wshapes)
GTFSwizard::write_gtfs(geneve_gtfs_wshapes, "original/geneve_gtfs_shapes.zip") # saved GTFS with shapes

# verify shapes
geneve_shapes = tidytransit::shapes_as_sf(geneve_gtfs_wshapes$shapes)
mapview::mapview(geneve_shapes, zcol = "mode")



# luxemburg ---------------------------------------------------------------
lux_url = "https://download.data.public.lu/resources/horaires-et-arrets-des-transport-publics-gtfs/20251002-061636/gtfs-20251001-20251031.zip"

# library(gtfstools)
gtfs_lux = gtfstools::read_gtfs(lux_url)
summary(gtfs_lux)
table(gtfs_lux$routes$route_type)
gtfs_lux_modes = gtfstools::filter_by_route_type(gtfs_lux, route_type = 0) # tram only
write_gtfs(gtfs_lux_modes, "data/gtfs_lux_tram.zip")

