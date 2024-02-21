library(sf)
library(sfnetworks)
library(tidyverse)
library(tidygraph)
library(maptiles)
library(tidyterra)
library(glue)
library(here)

map_accessibility_colored = function(data, detour = 1.5, zoom = 14,
                                     mode_name = "bike") {
  points = data$connected_households |>
    filter(detour_threshold == detour) |>
    filter(!st_is_empty(geom))
  geoms = st_geometry(points)
  new_geoms = list()
  for (i in seq_along(geoms)) {
    if (i == nrow(points)) {
      new_geom = geoms[i]
    } else {
      new_geom = st_difference(geoms[i], st_union(geoms[i + 1:nrow(points)]))
    }
    if (length(new_geom) == 0) {
      new_geom = st_sfc(st_geometrycollection(), crs = st_crs(points))
    }
    new_geoms[[i]] = new_geom
  }
  points = points |>
    st_set_geometry(do.call("c", new_geoms)) |>
    filter(!st_is_empty(geom))
  center = network |>
    activate("nodes") |>
    filter(is_hub) |>
    st_geometry()
  basemap = network |>
    activate("nodes") |>
    filter(in_proximity) |>
    st_bbox() |>
    st_as_sfc() |>
    st_transform(3857) |>
    get_tiles(provider = "CartoDB.Positron", zoom = zoom, crop = TRUE)
  ggplot() +
    geom_spatraster_rgb(data = basemap, maxcell = 5e9) +
    geom_sf(data = points, aes(color = index_threshold), size = 1, alpha = 0.8) +
    geom_sf(data = center, cex = 5, pch = 8) +
    ggtitle(
      glue("Households with ", mode_name, "able access to the hub"),
      glue("Given a detour threshold of ", detour)
    ) +
    scale_color_viridis_c(
      glue(str_to_title(mode_name), "ability threshold"),
      limits = c(0, 1),
      breaks = c(0, 0.5, 1)
    ) +
    theme(
      plot.title = element_text(family = "AppleGothic", hjust = 0.5),
      plot.subtitle = element_text(family = "AppleGothic", hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(family = "AppleGothic", vjust = 0.75),
      legend.text = element_text(family = "AppleGothic")
    )
}

map_accessibility_facetted = function(data,
                                      indices = c(0.25, 0.5, 0.75),
                                      detours = c(1, 1.5, 2),
                                      zoom = 14, mode_name = "bike") {
  locations = data$connected_households |>
    filter(detour_threshold %in% detours) |>
    filter(index_threshold %in% indices)
  center = data$network |>
    activate("nodes") |>
    filter(is_hub) |>
    st_geometry()
  basemap = data$network |>
    activate("nodes") |>
    filter(in_proximity) |>
    st_bbox() |>
    st_as_sfc() |>
    st_transform(3857) |>
    get_tiles(provider = "CartoDB.Positron", zoom = zoom, crop = TRUE)
  ggplot(locations) +
    geom_spatraster_rgb(data = basemap, maxcell = 5e9) +
    geom_sf(data = data$households, color = "darkgrey", size = 0.7) +
    geom_sf(aes(color = detour_threshold), size = 1, alpha = 0.8) +
    geom_sf(data = center, cex = 5, pch = 8) +
    facet_grid(vars(index_threshold), vars(detour_threshold)) +
    ggtitle(
      glue("Households with ", mode_name, "able access to the hub"),
      "For different index thresholds (rows) and detour thresholds (columns)"
    ) +
    scale_color_viridis_c(
      "Detour threshold",
      limits = c(1, 2),
      breaks = c(1, 1.5, 2)
    ) +
    theme(
      plot.title = element_text(family = "AppleGothic", hjust = 0.5),
      plot.subtitle = element_text(family = "AppleGothic", hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
}

map_input_data = function(data, zoom = 14, mode_name = "bike") {
  edges = data$network |>
    activate("nodes") |>
    filter(in_proximity) |>
    activate("edges") |>
    filter(!is.na(index)) |>
    st_as_sf()
  center = data$network |>
    activate("nodes") |>
    filter(is_hub) |>
    st_geometry()
  basemap = data$network |>
    activate("nodes") |>
    filter(in_proximity) |>
    st_bbox() |>
    st_as_sfc() |>
    st_transform(3857) |>
    get_tiles(provider = "CartoDB.Positron", zoom = zoom, crop = TRUE)
  ggplot() +
    geom_spatraster_rgb(data = basemap, maxcell = 5e9) +
    geom_sf(data = edges, aes(color = index), lwd = 1) +
    geom_sf(data = data$households, cex = 0.1) +
    geom_sf(data = center, cex = 2, pch = 15, color = "firebrick") +
    ggtitle(
      glue(str_to_title(mode_name), "ability of streets around the hub")
    ) +
    scale_color_viridis_c(
      glue(str_to_title(mode_name), "ability index"),
      limits = c(0, 1),
      breaks = c(0, 0.5, 1)
    ) +
    theme(
      plot.title = element_text(family = "AppleGothic", hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(family = "AppleGothic", vjust = 0.75),
      legend.text = element_text(family = "AppleGothic")
    )
}

plot_accessibility = function(data, mode_name = "bike") {
  access = data$connected_households |>
    mutate(detour_threshold = as.factor(detour_threshold))
  ggplot(access, aes(x = index_threshold, y = share, group = detour_threshold, color = detour_threshold)) +
    geom_point() +
    geom_line() +
    ggtitle(
      glue("Accessibility to the hub by ", mode_name),
      "For different index thresholds and detour thresholds"
    ) +
    xlab(
      "Index threshold"
    ) +
    ylab(
      glue("Households with ", mode_name, "able access [%]")
    ) +
    scale_color_viridis_d(
      "Detour threshold"
    ) +
    theme(
      plot.title = element_text(family = "AppleGothic", hjust = 0.5),
      plot.subtitle = element_text(family = "AppleGothic", hjust = 0.5),
      axis.title = element_text(family = "AppleGothic", size = 10),
      axis.text = element_text(family = "AppleGothic", size = 8),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(family = "AppleGothic"),
      legend.text = element_text(family = "AppleGothic")
    ) +
    guides(
      color = guide_legend(
        ncol = length(unique(access$detour_threshold)),
        title.position = "bottom",
        title.hjust = 0.5
      )
    )
}

load(here("data/data.RData"))

for (mode in c("bike")) {
  all_data = get(glue(mode, "_data"))
  for (hub in names(all_data)) {
    data = all_data[[hub]]
    zoom = ifelse(mode == "bike", 14, 15)
    # Input data map.
    file = here(glue("plots/", hub, "-data_map_", mode, ".png"))
    map_input_data(data, zoom = zoom)
    ggsave(file, width = 16, height = 16.8, units = "cm", dpi = 300)
    # Accessibility facet map.
    file = here(glue("plots/", hub, "-accessibility_map_", mode, ".png"))
    map_accessibility_facetted(data, zoom = zoom)
    ggsave(file, width = 16, height = 16, units = "cm", dpi = 300)
    # Accessibility curves.
    file = here(glue("plots/", hub ,"-accessibility_plot_", mode, ".png"))
    plot_accessibility(data)
    ggsave(file, width = 16, height = 11, units = "cm", dpi = 300)
  }
}
