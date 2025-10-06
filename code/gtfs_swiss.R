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



# Method 2 - from OSM -----------------------------------------------------
library(osmdata)

geneve_transit_osm = opq(bbox = st_bbox(geneve_limit)) |> 
  add_osm_feature(key = "route", value = "bus")
  # add_osm_feature(key = "network") |> 

trips_geometry_osm = GTFShift::osm_trips_to_routes(geneve_gtfs_wshapes, geneve_transit_osm)
# not working