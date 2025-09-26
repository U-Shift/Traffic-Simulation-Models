# Vamos calcular isócronas com base em GTFS
# https://ipeagit.github.io/r5r/articles/isochrones.html
# https://ipeagit.github.io/r5r/articles/accessibility.html


# carregar pacotes
library(tidyverse)
library(sf)
options(java.parameters = '-Xmx8G') # alocar memória para 8GB
library(r5r)
library(interp)


# Dados -------------------------------------------------------------------
# ver code/dataprep_r5r.R para ver como foram preparados

# Censos pontos
PONTOS = st_read("https://github.com/U-Shift/Traffic-Simulation-Models/releases/download/2025/Censos_Lx.gpkg")

# Ponto de origem na Baixa (Lisboa)
BAIXA = data.frame(id = "1", lat = 38.711884, lon = -9.137313) |> #casa rosa
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)
BAIXA$lon = st_coordinates(BAIXA)[,1]
BAIXA$lat = st_coordinates(BAIXA)[,2]

# Rede viária base (from OSM)
REDEbase = st_read("https://github.com/U-Shift/Traffic-Simulation-Models/releases/download/2025/REDEbase_Lx.gpkg")

# Limite município Lisboa e bounding box
# MunicipiosGEO = sf::st_read("https://github.com/U-Shift/MQAT/releases/download/2025/MUNICIPIOSgeo.gpkg")
# LisboaGEO = MunicipiosGEO |> filter(Concelho == "Lisboa")
# st_write(LisboaGEO, "data/r5r/Lisboa_lim.gpkg", delete_dsn = TRUE)
st_write(Lisboa_lim, "data/Lisbon/Lisboa_lim.geojson", delete_dsn = TRUE)
# piggyback::pb_upload("data/r5r/Lisboa_lim.gpkg", "Lisboa_lim.gpkg", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")
Lisboa_lim = sf::st_read("https://github.com/U-Shift/Traffic-Simulation-Models/releases/download/2025/Lisboa_lim.gpkg")
bb_lx = st_bbox(Lisboa_lim)
bb_x = c(bb_lx[[1]], bb_lx[[3]])
bb_y = c(bb_lx[[2]], bb_lx[[4]])

# Rede modelada com r5r
r5r_url = "https://github.com/U-Shift/Traffic-Simulation-Models/releases/download/2025/r5r_lisboa.zip" # 57MB
download.file(r5r_url, destfile = "data/r5r_lisboa.zip")
unzip("data/r5r_lisboa.zip", exdir = "data/r5r/")

r5r_lisboa = build_network(data_path = "data/r5r/") # ler rede já modelada


# Hora Ponta, 2h TP + walk, 1 transferência --------------------------------------------------------------------

# PT + Walk
mode = c("SUBWAY", "BUS") # TRANSIT, BUS, SUBWAY, RAIL, CAR, FERRY, WALK, BIKE, TRAM
mode_egress = "WALK" # ligações em falta (pode ser BIKE)
max_walk_time = 10 # in minutes
max_trip_duration = 90 # in minutes
departure_datetime_HP = as.POSIXct("20-11-2024 7:30:00", format = "%d-%m-%Y %H:%M:%S") # quarta-feira
time_window = 60 # in minutes

# calculate travel time matrix
ttm_zer_HP_1 = travel_time_matrix(r5r_network = r5r_lisboa,
                              origins = BAIXA,
                              destinations = PONTOS,
                              mode = mode,
                              mode_egress = mode_egress,
                              departure_datetime = departure_datetime_HP,
                              max_walk_time = max_walk_time,
                              max_trip_duration = max_trip_duration,
                              time_window = time_window,
                              max_rides = 2, #apenas uma transferência
                              verbose = FALSE)


# add coordinates of destinations to travel time matrix
ttm_zer_HP_1 = ttm_zer_HP_1 |>
  left_join(PONTOS, by = c("to_id" = "id"))  |> 
  st_as_sf()
ttm_zer_HP_1 = ttm_zer_HP_1 |>
  mutate(lon = st_coordinates(ttm_zer_HP_1)[,1],
         lat = st_coordinates(ttm_zer_HP_1)[,2]) |>
  st_drop_geometry()

# interpolate estimates to get spatially smooth result
travel_times.interp <- with(na.omit(ttm_zer_HP_1), interp(lon, lat, travel_time_p50)) |>
  with(cbind(travel_time=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) |>
  as.data.frame() |> na.omit()


# plot
plotHP1 = ggplot(travel_times.interp) +
  geom_contour_filled(aes(x = x, y = y, z = travel_time), alpha = .7) +
  geom_sf(data = REDEbase, color = "gray55", lwd = 0.5, alpha = 0.4) +
  geom_sf(data = LisboaGEO, fill = "transparent", color = "grey30") +
  geom_point(aes(x = lon, y = lat, color = 'Baixa'), data = BAIXA) +
  scale_fill_viridis_d(direction = -1, option = 'B') +
  scale_color_manual(values = c('Baixa' = 'black')) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) +
  labs(
    title = "Alcance desde a Baixa (Carris + Metro)",
    subtitle = "às 7h30 de Quarta, 20 Nov 2024 - máx 1 transf",
    fill = "Tempo viagem \n[min]",
    color = ''
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())
plotHP1

# Hora Ponta 2h TP + walk, 0 transferência --------------------------------------------------------------------

# calculate travel time matrix
ttm_zer_HP_0 = travel_time_matrix(r5r_core = r5r_lisboa,
                                  origins = BAIXA,
                                  destinations = PONTOS,
                                  mode = mode,
                                  mode_egress = mode_egress,
                                  departure_datetime = departure_datetime_HP,
                                  max_walk_time = max_walk_time,
                                  max_trip_duration = max_trip_duration,
                                  time_window = time_window,
                                  max_rides = 1, # direto - sem transferência
                                  verbose = FALSE)


# add coordinates of destinations to travel time matrix
ttm_zer_HP_0 = ttm_zer_HP_0 |>
  left_join(PONTOS, by = c("to_id" = "id"))  |> 
  st_as_sf()
ttm_zer_HP_0 = ttm_zer_HP_0 |>
  mutate(lon = st_coordinates(ttm_zer_HP_0)[,1],
         lat = st_coordinates(ttm_zer_HP_0)[,2]) |>
  st_drop_geometry()

# interpolate estimates to get spatially smooth result
travel_times.interp <- with(na.omit(ttm_zer_HP_0), interp(lon, lat, travel_time_p50)) |>
  with(cbind(travel_time=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) |>
  as.data.frame() |> na.omit()

# plot
plotHP0 = ggplot(travel_times.interp) +
  geom_contour_filled(aes(x = x, y = y, z = travel_time), alpha = .7) +
  geom_sf(data = REDEbase, color = "gray55", lwd = 0.5, alpha = 0.4) +
  geom_sf(data = LisboaGEO, fill = "transparent", color = "grey30") +
  geom_point(aes(x = lon, y = lat, color = 'Baixa'), data = BAIXA) +
  scale_fill_viridis_d(direction = -1, option = 'B') +
  scale_color_manual(values = c('Baixa' = 'black')) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) +
  labs(
    title = "Alcance desde a Baixa (Carris + Metro)",
    subtitle = "às 7h30 de Quarta, 20 Nov 2024 - sem transf",
    fill = "Tempo viagem \n[min]",
    color = ''
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())
plotHP0

# Domingo, 2h TP + walk, 1 transferência --------------------------------------------------------------------

departure_datetime_Dom = as.POSIXct("24-11-2024 22:00:00", format = "%d-%m-%Y %H:%M:%S") # quarta-feira


# calculate travel time matrix
ttm_zer_DOM_1 = travel_time_matrix(r5r_core = r5r_lisboa,
                                  origins = BAIXA,
                                  destinations = PONTOS,
                                  mode = mode,
                                  mode_egress = mode_egress,
                                  departure_datetime = departure_datetime_Dom,
                                  max_walk_time = max_walk_time,
                                  max_trip_duration = max_trip_duration,
                                  time_window = time_window,
                                  max_rides = 2, #apenas uma transferência
                                  verbose = FALSE)


# add coordinates of destinations to travel time matrix
ttm_zer_DOM_1 = ttm_zer_DOM_1 |>
  left_join(PONTOS, by = c("to_id" = "id"))  |> 
  st_as_sf()
ttm_zer_DOM_1 = ttm_zer_DOM_1 |>
  mutate(lon = st_coordinates(ttm_zer_DOM_1)[,1],
         lat = st_coordinates(ttm_zer_DOM_1)[,2]) |>
  st_drop_geometry()

# interpolate estimates to get spatially smooth result
travel_times.interp <- with(na.omit(ttm_zer_DOM_1), interp(lon, lat, travel_time_p50)) |>
  with(cbind(travel_time=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) |>
  as.data.frame() |> na.omit()


# plot
plotDOM1 = ggplot(travel_times.interp) +
  geom_contour_filled(aes(x = x, y = y, z = travel_time), alpha = .7) +
  geom_sf(data = REDEbase, color = "gray55", lwd = 0.5, alpha = 0.4) +
  geom_sf(data = LisboaGEO, fill = "transparent", color = "grey30") +
  geom_point(aes(x = lon, y = lat, color = 'Baixa'), data = BAIXA) +
  scale_fill_viridis_d(direction = -1, option = 'F') +
  scale_color_manual(values = c('Baixa' = 'black')) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) +
  labs(
    title = "Alcance desde a Baixa (Carris + Metro)",
    subtitle = "às 22h00 de Domingo, 24 Nov 2024 - máx 1 transf",
    fill = "Tempo viagem \n[min]",
    color = ''
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())
plotDOM1

# Domingo, 2h TP + walk, 0 transferência --------------------------------------------------------------------

# calculate travel time matrix
ttm_zer_DOM_0 = travel_time_matrix(r5r_core = r5r_lisboa,
                                  origins = BAIXA,
                                  destinations = PONTOS,
                                  mode = mode,
                                  mode_egress = mode_egress,
                                  departure_datetime = departure_datetime_Dom,
                                  max_walk_time = max_walk_time,
                                  max_trip_duration = max_trip_duration,
                                  time_window = time_window,
                                  max_rides = 1, # direto - sem transferência
                                  verbose = FALSE)


# add coordinates of destinations to travel time matrix
ttm_zer_DOM_0 = ttm_zer_DOM_0 |>
  left_join(PONTOS, by = c("to_id" = "id"))  |> 
  st_as_sf()
ttm_zer_DOM_0 = ttm_zer_DOM_0 |>
  mutate(lon = st_coordinates(ttm_zer_DOM_0)[,1],
         lat = st_coordinates(ttm_zer_DOM_0)[,2]) |>
  st_drop_geometry()

# interpolate estimates to get spatially smooth result
travel_times.interp <- with(na.omit(ttm_zer_DOM_0), interp(lon, lat, travel_time_p50)) |>
  with(cbind(travel_time=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) |>
  as.data.frame() |> na.omit()

# plot
plotDOM0 = ggplot(travel_times.interp) +
  geom_contour_filled(aes(x = x, y = y, z = travel_time), alpha = .7) +
  geom_sf(data = REDEbase, color = "gray55", lwd = 0.5, alpha = 0.4) +
  geom_sf(data = LisboaGEO, fill = "transparent", color = "grey30") +
  geom_point(aes(x = lon, y = lat, color = 'Baixa'), data = BAIXA) +
  scale_fill_viridis_d(direction = -1, option = 'F') +
  scale_color_manual(values = c('Baixa' = 'black')) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) +
  labs(
    title = "Alcance desde a Baixa (Carris + Metro)",
    subtitle = "às 22h00 de Domingo, 24 Nov 2024 - sem transf",
    fill = "Tempo viagem \n[min]",
    color = ''
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())
plotDOM0







# Acessibilidade a População -----------------------------------------------------------------------------------
## calcular quantas pessoas estão a menos de 15min, 30 min, 60 min, 
# quarta-feira 1 transfer
poplisboa = sum(PONTOS$N_INDIVIDUOS) #
100*sum(ttm_zer_HP_1$N_INDIVIDUOS[ttm_zer_HP_1$travel_time <= 15]) / poplisboa # 1.79%
100*sum(ttm_zer_HP_1$N_INDIVIDUOS[ttm_zer_HP_1$travel_time <= 30]) / poplisboa # 35.94%
100*sum(ttm_zer_HP_1$N_INDIVIDUOS[ttm_zer_HP_1$travel_time <= 45]) / poplisboa # 83.64%
100*sum(ttm_zer_HP_1$N_INDIVIDUOS[ttm_zer_HP_1$travel_time <= 60]) / poplisboa # 97.47%
# quarta-feira directo sem transf
100*sum(ttm_zer_HP_0$N_INDIVIDUOS[ttm_zer_HP_0$travel_time <= 15]) / poplisboa # 1.79%
100*sum(ttm_zer_HP_0$N_INDIVIDUOS[ttm_zer_HP_0$travel_time <= 30]) / poplisboa # 20.99%
100*sum(ttm_zer_HP_0$N_INDIVIDUOS[ttm_zer_HP_0$travel_time <= 45]) / poplisboa # 43.73%
100*sum(ttm_zer_HP_0$N_INDIVIDUOS[ttm_zer_HP_0$travel_time <= 60]) / poplisboa # 61.61%
# domingo 1 transfer
100*sum(ttm_zer_DOM_1$N_INDIVIDUOS[ttm_zer_DOM_1$travel_time <= 15]) / poplisboa # 1.13%
100*sum(ttm_zer_DOM_1$N_INDIVIDUOS[ttm_zer_DOM_1$travel_time <= 30]) / poplisboa # 22.76%
100*sum(ttm_zer_DOM_1$N_INDIVIDUOS[ttm_zer_DOM_1$travel_time <= 45]) / poplisboa # 68.19%
100*sum(ttm_zer_DOM_1$N_INDIVIDUOS[ttm_zer_DOM_1$travel_time <= 60]) / poplisboa # 89.71%
# domingo directo sem transf
100*sum(ttm_zer_DOM_0$N_INDIVIDUOS[ttm_zer_DOM_0$travel_time <= 15]) / poplisboa # 1.13%
100*sum(ttm_zer_DOM_0$N_INDIVIDUOS[ttm_zer_DOM_0$travel_time <= 30]) / poplisboa # 17.58%
100*sum(ttm_zer_DOM_0$N_INDIVIDUOS[ttm_zer_DOM_0$travel_time <= 45]) / poplisboa # 38.25%
100*sum(ttm_zer_DOM_0$N_INDIVIDUOS[ttm_zer_DOM_0$travel_time <= 60]) / poplisboa # 51.70%

# calculate population accessible
access = ttm_zer_HP_1[travel_time_p50 <= 60, .(acc = sum(N_INDIVIDUOS)), by=to_id] # 60 minutos
access = left_join(access, ttm_zer_HP_1)

# interpolate estimates to get spatially smooth result
access.interp = access |>
  inner_join(PONTOS, by=c('to_id'='id')) |>
  with(interp(lon, lat, acc)) |>
  with(cbind(acc=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) |> as.data.frame() |> na.omit()


# plot
ggplot(na.omit(access.interp)) +
  geom_contour_filled(aes(x=x, y=y, z=acc), alpha=.8) +
  geom_sf(data = REDEbase, color = "gray55", lwd=0.5, alpha = 0.5) +
  geom_sf(data = LisboaGEO, fill = "transparent", color = "grey30") +
  scale_fill_viridis_d(direction = -1, option = 'B') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) + 
  labs(
    title = "População com acesso à Baixa (Carris + Metro)",
    subtitle = "às 7h30 de Quarta, 20 Nov 2024 - máx 1 transf",
    fill = "População em\n60 minutos") +
  theme_minimal() +
  theme(axis.title = element_blank())





# END java ------------------------------------------------------------------------------------

stop_r5(r5r_lisboa)
rJava::.jgc(R.gc = TRUE)

