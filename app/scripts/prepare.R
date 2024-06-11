library(sf)
library(sfnetworks)
library(tidyverse)
library(here)

load(here("data/data.RData"))

merge_data = function(data, type, layer = NULL) {
  if (is.null(layer)) {
    out = lapply(data, \(x) mutate(x[[type]], hub = x$hub$short_name))
  } else {
    out = lapply(data, \(x) mutate(st_as_sf(x[[type]], layer), hub = x$hub$short_name))
  }
  out |>
    bind_rows() |>
    mutate(hub = as.factor(hub))
}

bike_data$Hauptbahnhof$hub$short_name = "Salzburg"
bike_data = bike_data[names(bike_data) %in% c("Hauptbahnhof", "Neumarkt", "Hallein", "Golling")]

hubs = merge_data(bike_data, "hub") |> st_transform(4326)
edgs = merge_data(bike_data, "network", layer = "edges") |> st_transform(4326)
nets = merge_data(bike_data, "suitable_networks") |> st_transform(4326)
hlds = merge_data(bike_data, "connected_households") |> st_transform(4326)

save(hubs, edgs, nets, hlds, file = here("app/data/data.RData"))
zip(here("app/data/data.zip"), here("app/data/data.RData"))