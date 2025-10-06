# --- Generate synthetic census-like points for a City ---
library(sf)
library(dplyr)


# 1. Read city polygon (replace with your file path)
city_limit <- st_read("data/city_limit.gpkg") |> st_transform(4326)

# 2. Set parameters
set.seed(123) # for reproducibility
total_pop <- 545000
avg_residents <- 300 # a census block average
n_points <- round(total_pop / avg_residents)

# 3. Generate random points within the city boundary
census_sym <- st_sample(city_limit, size = n_points, type = "random") |>
  st_as_sf() |>
  mutate(id = 1:n_points)

# # 4. Compute distance from city center (for density weighting)
# center <- st_centroid(city_limit)
# dist_center <- as.numeric(st_distance(census_sym, center))  # in meters
# 
# # 5. Create weights so closer points get higher population
# # Inverse distance weighting (add +1 to avoid division by zero)
# weights <- 1 / (dist_center + 1)

# assign random populations that sum to total_pop
weights <- runif(n_points)

# Normalize weights to sum to 1 and assign residents
residents <- round(weights / sum(weights) * total_pop)

# 6. Adjust rounding so the total sums exactly to total_pop
diff <- total_pop - sum(residents)
if (diff != 0) {
  residents[1:abs(diff)] <- residents[1:abs(diff)] + sign(diff)
}

# 7. Add residents to points
census_sym$residents <- residents
sum(census_sym$residents) # should be total_pop

# 8. Quick visualization
mapview::mapview(census_sym, zcol = "residents")

# 9. (Optional) Save to GeoPackage
st_write(census_sym, "data/synthetic_census_points.gpkg", delete_dsn = TRUE)
