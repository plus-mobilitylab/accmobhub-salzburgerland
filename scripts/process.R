library(sf)
library(sfnetworks)
library(tidyverse)
library(tidygraph)
library(here)

source(here("scripts/funcs.R"))

# --------------------------- 
# SET PARAMETERS
# ---------------------------

BIKE_RADIUS = 3000
WALK_RADIUS = 750

# --------------------------- 
# PRE-PROCESS STREETS
# ---------------------------

streets = here("data/source/salzburgerland-netascore-20240123.gpkg") |>
  read_sf(layer = "edge") |>
  select(-c(ends_with("_robustness"), ends_with("_explanation"), ends_with(("_node")))) |>
  select(-ends_with("_tf")) |>
  rename_with(\(x) gsub("_ft", "", x), ends_with("_ft")) |>
  filter(!access_car & !access_bicycle & !access_pedestrian) |>
  mutate(index_bike = replace(index_bike, !access_bicycle, NA)) |>
  mutate(index_walk = replace(index_walk, !access_pedestrian, 0)) |>
  select(
    osm_id,
    access_car,
    access_bicycle,
    access_pedestrian,
    bicycle_infrastructure,
    pedestrian_infrastructure,
    road_category,
    pavement,
    gradient,
    crossings,
    index_bike,
    index_walk
  )

# --------------------------- 
# PRE-PROCESS HOUSEHOLDS
# ---------------------------

# addrfile = "data/source/Adresse_Relationale_Tabellen_Stichtagsdaten_20211001.zip"
# 
# addrs = read_delim(unzip(here(addrfile), "ADRESSE.csv", exdir = here("data/source")), delim = ";", col_types = "cccccccccccccccddicc") |>
#   select(ADRCD, GKZ, RW, HW, EPSG) |>
#   filter(str_starts(GKZ, "50")) |>
#   group_by(EPSG) |>
#   group_split() |>
#   lapply(\(x) st_transform(st_as_sf(x, crs = x$EPSG[1], coords = c("RW", "HW")), st_crs(streets))) |>
#   bind_rows() |>
#   select(-EPSG, -GKZ) |>
#   rename(id = ADRCD)
# 
# popfile = "data/source/salzburgerland-population.gpkg"
# 
# pop = read_sf(here(popfile)) |>
#   select(id, fz_hws) |>
#   rename(pop = fz_hws) |>
#   filter(!is.na(pop)) |>
#   st_transform(st_crs(streets))
# 
# popaddrs = populate_addresses(addrs, pop)
# 
# write_sf(popaddrs, "data/source/addresses.gpkg")

households = read_sf(here("data/source/addresses.gpkg"))

# --------------------------- 
# PRE-PROCESS HUBS
# ---------------------------

hubs = here("data/source/hubs.csv") |>
  read_delim(delim = ";", col_types = "iccccc") |>
  mutate(hst_x = as.numeric(str_replace(hst_x, ",", "."))) |>
  mutate(hst_y = as.numeric(str_replace(hst_y, ",", "."))) |>
  st_as_sf(coords = c("hst_x", "hst_y"), crs = 4326) |>
  st_transform(st_crs(streets)) |>
  rename(id = OID, name = hst_name, municipality = hst_gem_na) |>
  mutate(short_name = c("Hauptbahnhof", "Mirabell", "Itzling", "Neumarkt", "Hallein", "Golling", "Aiglhof", "Zell am See", "Gnigl", "Hauptbahnhof Ost")) |>
  select(id, name, short_name, municipality)

# --------------------------- 
# CREATE DATA FOR EACH HUB
# ---------------------------

bike_data = process(hubs, streets, households, proximity_threshold = BIKE_RADIUS, index_column = "index_bike")
walk_data = process(hubs, streets, households, proximity_threshold = WALK_RADIUS, egress_threshold = 50, index_column = "index_walk")

# ---------------------------
# EXPORT
# ---------------------------

save(bike_data, walk_data, file = here("data/data.RData"))
zip(here("data/data.zip"), here("data/data.RData"))