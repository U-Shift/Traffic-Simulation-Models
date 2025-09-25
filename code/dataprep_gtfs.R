# objetivo: ler dados de um arquivo GTFS e visualizar as paragens e rotas

# Pacotes
# install.packages("tidytransit")
library(tidyverse)
library(tidytransit)
library(mapview)

# Ler dados
METRO = read_gtfs("original/metro_gtfs.zip")
validate_gtfs(METRO) # validar

# Ver os dados

# Paragens
View(METRO$stops)

ESTACOES_metro = stops_as_sf(METRO$stops) # converter texto para geometria
# ESTACOES_metro = ESTACOES_metro |> filter(parent_station == "") # remover aquelas subsidiarias
mapview(ESTACOES_metro) # visualizar

## Desafio ##
# Ver que estações de metro se consegue alcançar em 15, 30 e 45 minutos a partir da Baixa
# 1. num dia útil em hora de ponta
# 2. num domingo de tarde


# 1. num dia útil em hora de ponta

# filtramos os dados para o dia de hoje entre as 7h30 e as 9h00
stop_times_metro = filter_stop_times(METRO, "2024-11-20", "07:30:00", "09:00:00")
duracao_viagem = travel_times(stop_times_metro, "Baixa / Chiado") # Baixa / Chiado é a estação de metro da Baixa

summary(duracao_viagem$travel_time/60) # resumo em minutos

duracao_viagem_BC_HP = ESTACOES_metro |> 
  left_join(duracao_viagem, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(!is.na(travel_time)) # remover as plataformas não usadas

# mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
ggplot(duracao_viagem_BC_HP) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde a estação Baixa-Chiado",
          subtitle = "às 7h30 de Quarta, 20 Nov 2024") +
  labs(color = "Tempo de viagem [min]") +
  theme_bw()

# 2. num domingo de tarde
# filtramos os dados para o próximo Domingo entre as 21h30 e as 22h30
stop_times_metro = filter_stop_times(METRO, "2024-11-24", "21:30:00", "22:30:00")
duracao_viagem = travel_times(stop_times_metro, "Baixa / Chiado") # Baixa / Chiado é a estação de metro da Baixa

summary(duracao_viagem$travel_time/60) # resumo em minutos

duracao_viagem_BC_Dom = ESTACOES_metro |> 
  left_join(duracao_viagem, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(!is.na(travel_time)) # remover as plataformas não usadas

# mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
ggplot(duracao_viagem_BC_Dom) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde a estação Baixa-Chiado",
          subtitle = "às 21h30 de Domingo, 24 Nov 2024") +
  labs(color = "Tempo de viagem [min]") +
  theme_bw()



# Carris Lisboa -----------------------------------------------------------

# Donwload e ler dados
carris_url = "https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS" # 14.3MB
download.file(carris_url, destfile = "original/carris_gtfs.zip")
CARRIS = read_gtfs("original/carris_gtfs.zip")
## Outro pacote
CARRIS_outro = gtfsrouter::extract_gtfs("original/carris_gtfs.zip")
carris_transfers = gtfsrouter::gtfs_transfer_table(CARRIS_outro)
write.csv(carris_transfers[["transfers"]], "original/transfers.txt", row.names = F, quote = F)
# incluir este ficheiro novamente no zip original

CARRIScomtransfers = read_gtfs("original/carris_gtfs.zip")
validate_gtfs(CARRIScomtransfers) # validar
CARRIS = CARRIScomtransfers

# Paragens
View(CARRIS$stops)
ESTACOES_carris = stops_as_sf(CARRIS$stops) # converter texto para geometria
mapview(ESTACOES_carris) # visualizar


# Percursos
ROTAS_carris = tidytransit::shapes_as_sf(CARRIS$shapes) # converter texto para geometria
plot(ROTAS_carris) # ver as 1739 rotas
mapview(ROTAS_carris, zcol = "shape_id") # visualizar


# 1. num dia útil em hora de ponta

# filtramos os dados para o dia de hoje entre as 7h30 e as 9h00
stop_times_carris1 = filter_stop_times(CARRIS, "2024-11-20", "07:30:00", "09:00:00")
duracao_viagem1 = travel_times(stop_times_carris1, "Cais Sodré", stop_dist_check = FALSE)

# estatísticas
summary(duracao_viagem1$travel_time/60) # resumo em minutos
hist(duracao_viagem1$travel_time/60, breaks = 20) # histograma
abline(v = mean(duracao_viagem1$travel_time/60), col = "red") # linha de média
abline(v = median(duracao_viagem1$travel_time/60), col = "blue") # linha de mediana
round(prop.table(table(duracao_viagem1$transfers))*100, 1) # percentagem de transferências

duracao_viagem_CS_HP = ESTACOES_carris |> 
  left_join(duracao_viagem1, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(transfers >= 1) |> # remover viagens com mais de 1 transferência
  filter(!is.na(travel_time)) |>  # remover as plataformas não usadas
  filter(travel_time < 60*60) |> # remover viagens com mais de 60 minutos
  filter(!stop_id %in% c(19001,14601,14602)) # remover as pagarens em Almada
  
# mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
ggplot(duracao_viagem_CS_HP) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde o Cais do Sodré (Carris)",
          subtitle = "às 7h30 de Quarta, 20 Nov 2024 - máx 1 transf") +
  labs(color = "Tempo de viagem [min]") +
  theme_bw()

# 2. num domingo de tarde
# filtramos os dados para o próximo Domingo entre as 21h30 e as 22h30
stop_times_carris2 = filter_stop_times(CARRIS, "2024-11-24", "22:00:00", "23:30:00")
duracao_viagem2 = travel_times(stop_times_carris2, "Cais Sodré", stop_dist_check = FALSE)

summary(duracao_viagem2$travel_time/60) # resumo em minutos

duracao_viagem_CS_Dom = ESTACOES_carris |> 
  left_join(duracao_viagem2, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(transfers >= 1) |> # remover viagens com mais de 1 transferência
  filter(!is.na(travel_time)) |>  # remover as plataformas não usadas
  filter(travel_time < 60*60) # remover viagens com mais de 60 minutos

# mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
ggplot(duracao_viagem_CS_Dom) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde o Cais do Sodré (Carris)",
          subtitle = "às 22h00 de Domingo, 24 Nov 2024 - máx 1 transf") +
  labs(color = "Tempo de viagem [min]") +
  theme_bw()









# Carris Metropolitana ------------------------------------------------------------------

# Ler dados
CARRISm = read_gtfs("original/carris_metropolitana.zip")
validate_gtfs(CARRISm) # validar

## Outro pacote
CARRISm_outro = gtfsrouter::extract_gtfs("original/carris_metropolitana.zip")
carris_transfers = gtfsrouter::gtfs_transfer_table(CARRISm_outro)
write.csv(carris_transfers[["transfers"]], "original/transfers.txt", row.names = F, quote = F)
# incluir este ficheiro novamente no zip original

CARRISmcomtransfers = read_gtfs("original/carris_metropolitana.zip")
validate_gtfs(CARRISmcomtransfers) # validar
CARRISm = CARRISmcomtransfers

View(CARRISm$stops)

# Paragens
ESTACOES_carrism = stops_as_sf(CARRISm$stops) # converter texto para geometria
mapview(ESTACOES_carrism) # visualizar
mapview(ESTACOES_carris, zcol = "has_shelter") # por tipo de abrigo
mapview(ESTACOES_carris, zcol = "municipality_name") # por concelho

# Percursos
ROTAS_carrism = tidytransit::shapes_as_sf(CARRISm$shapes) # converter texto para geometria
plot(ROTAS_carrism) # ver as 1739 rotas
mapview(ROTAS_carrism, zcol = "shape_id") # visualizar




# TUB Braga -----------------------------------------------------------

# Donwload e ler dados
tub_url = "https://tub.pt/developer/gtfs/feed/tub.zip" # 1.7MB
download.file(tub_url, destfile = "original/tub_gtfs.zip")
TUB = read_gtfs("original/tub_gtfs.zip")
## Outro pacote
TUB_outro = gtfsrouter::extract_gtfs("original/tub_gtfs.zip")
tub_transfers = gtfsrouter::gtfs_transfer_table(TUB_outro)
write.csv(tub_transfers[["transfers"]], "original/transfers.txt", row.names = F, quote = F)
# incluir este ficheiro novamente no zip original

TUBcomtransfers = read_gtfs("original/tub_gtfs.zip")
validate_gtfs(TUBcomtransfers) # validar
TUB = TUBcomtransfers

# Paragens
View(TUB$stops)
ESTACOES_tub = stops_as_sf(TUB$stops) # converter texto para geometria
mapview(ESTACOES_tub) # visualizar


# Percursos
ROTAS_tub = tidytransit::shapes_as_sf(TUB$shapes) # converter texto para geometria
plot(ROTAS_tub) # ver as 1739 rotas
mapview(ROTAS_tub, zcol = "shape_id") # visualizar


# 1. num dia útil em hora de ponta

# filtramos os dados para o dia de hoje entre as 7h30 e as 9h00
stop_times_tub1 = filter_stop_times(TUB, "2025-05-27", "07:30:00", "09:00:00")
duracao_viagem1 = travel_times(stop_times_tub1, "MONSENHOR AIROSA", stop_dist_check = FALSE)

# estatísticas
summary(duracao_viagem1$travel_time/60) # resumo em minutos
hist(duracao_viagem1$travel_time/60, breaks = 20) # histograma
abline(v = mean(duracao_viagem1$travel_time/60), col = "red") # linha de média
abline(v = median(duracao_viagem1$travel_time/60), col = "blue") # linha de mediana
round(prop.table(table(duracao_viagem1$transfers))*100, 1) # percentagem de transferências

duracao_viagem_Braga_HP = ESTACOES_tub |> 
  left_join(duracao_viagem1, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(transfers >= 1) |> # remover viagens com mais de 1 transferência
  filter(!is.na(travel_time)) |>  # remover as plataformas não usadas
  filter(travel_time < 60*60) |> # remover viagens com mais de 60 minutos
  filter(!stop_id %in% c(19001,14601,14602)) # remover as pagarens em Almada

# mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
ggplot(duracao_viagem_Braga_HP) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde o IPCA (TUB)",
          subtitle = "às 7h30 de Terça, 25 Maio 2025 - máx 1 transf") +
  labs(color = "Tempo de viagem [min]") +
  theme_bw()

# 2. num domingo de tarde
# filtramos os dados para o próximo Domingo entre as 21h30 e as 22h30
stop_times_tub2 = filter_stop_times(TUB, "2025-06-01", "20:00:00", "22:00:00")
duracao_viagem2 = travel_times(stop_times_tub2, "MONSENHOR AIROSA", stop_dist_check = FALSE)

summary(duracao_viagem2$travel_time/60) # resumo em minutos

duracao_viagem_Braga_Dom = ESTACOES_tub |> 
  left_join(duracao_viagem2, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(transfers >= 1) |> # remover viagens com mais de 1 transferência
  filter(!is.na(travel_time)) |>  # remover as plataformas não usadas
  filter(travel_time < 60*60) # remover viagens com mais de 60 minutos

# mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
ggplot(duracao_viagem_Braga_Dom) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde o IPCA (TUB)",
          subtitle = "às 22h00 de Domingo, 24 Nov 2024 - máx 1 transf") +
  labs(color = "Tempo de viagem [min]") +
  theme_bw()





# metro expansion fom PTV Lines -------------------------------------------

library(GTFShift)
metro = GTFShift::load_feed("data/r5r/metro_gtfs.zip")
metro_expansion = GTFShift::load_feed("data/r5r/metro_expansion_gtfs.zip")
metro_expansion_tidy = tidytransit::read_gtfs("data/r5r/metro_expansion_gtfs.zip")
metro_expansion_routes = tidytransit::shapes_as_sf(metro_expansion_tidy$shapes)
mapview::mapview(metro_expansion_routes)
metro_expansion_tidy_validation = tidytransit::validate_gtfs(metro_expansion_tidy)

# upload gtfs com transfers -----------------------------------------------

piggyback::pb_upload("original/carris_metropolitana.zip", "carris_metropolitana.zip", repo = "rosamfelix/PGmob360", tag = "latest")
piggyback::pb_upload("original/carris_gtfs.zip", "carris_gtfs.zip", repo = "rosamfelix/PGmob360", tag = "latest")
piggyback::pb_upload("original/metro_gtfs.zip", "metro_gtfs.zip", repo = "rosamfelix/PGmob360", tag = "latest")
piggyback::pb_upload("original/tub_gtfs.zip", "tub_gtfs.zip", repo = "rosamfelix/EITbraga", tag = "latest")
piggyback::pb_upload("data/r5r/metro_expansion_gtfs.zip", "metro_expansion_gtfs.zip", repo = "U-Shift/Traffic-Simulation-Models", tag = "2025")
