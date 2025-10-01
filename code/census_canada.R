library(sf)
library(tidyverse)
library(readr)

census_blocks_url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/ldb_000b21a_e.zip"
download.file(census_blocks_url, destfile = "data/canada_census_blocks.zip")
unzip("canada_census_blocks.zip", exdir = "data/")

# canada_areas = st_read("/home/rosa/Downloads/canada/gda_000a11a_e.shp")
canada_blocks = st_read("/home/rosa/Downloads/canada/ldb_000b21a_e.shp")
canada_stats = read_csv("/home/rosa/Downloads/canada/2021_92-151_X.csv")

names(canada_blocks)
names(canada_stats)

# check if are ~the same
length(unique(canada_blocks$DBUID))
length(unique(canada_stats$DBUID_IDIDU))

canada_pop = canada_blocks |>
  left_join(canada_stats |>
              mutate(DBUID_IDIDU = as.character(DBUID_IDIDU)) |>
              select(DBUID_IDIDU, CMAUID_RMRIDU, DBPOP2021_IDPOP2021, DBTDWELL2021_IDTLOG2021), # do you need to bring more info?
         by = c("DBUID" = "DBUID_IDIDU")) |> 
  rename(population = DBPOP2021_IDPOP2021,
         dwellings = DBTDWELL2021_IDTLOG2021)

# For Victoria CMA, the official CMAUID is 935  
victoria_pop = canada_pop |> filter(CMAUID_RMRIDU == 935)

mapview::mapview(victoria_pop, zcol = "population")
