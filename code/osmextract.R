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
