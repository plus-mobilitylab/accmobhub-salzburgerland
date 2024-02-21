library(sf)
library(sfnetworks)
library(tidyverse)
library(tidygraph)
library(here)
library(units)

process = function(hubs, streets, households, directed = FALSE, penalty = 4,
                   proximity_threshold = 3000, egress_threshold = 250,
                   index_thresholds = seq(0, 1, by = 0.05),
                   detour_thresholds = seq(1, 2, by = 0.25),
                   name_column = "short_name",
                   population_column = "pop",
                   index_column = "index_bike_ft") {
  out = list()
  for (hub in hubs[[name_column]]) {
    hubdata = filter(hubs, .data[[name_column]] == hub)
    data = make_data(
      hub = hubdata,
      streets = streets,
      households = households,
      directed = directed,
      proximity_threshold = proximity_threshold,
      egress_threshold = egress_threshold,
      population_column = population_column,
      index_column = index_column
    )
    data$suitable_networks = find_suitable_networks(
      network = data$network,
      index_thresholds = index_thresholds
    )
    data$connected_households = find_connected_households(
      households = data$households,
      network = data$network,
      index_thresholds = index_thresholds,
      detour_thresholds = detour_thresholds,
      penalty = penalty
    )
    data$hub = hubdata
    data$network = filter(data$network, in_proximity)
    out[[hub]] = data
  }
  out
}

make_data = function(hub, streets, households, directed = FALSE,
                     proximity_threshold = 3000, egress_threshold = 250,
                     population_column = "pop", index_column = "index_bike_ft") {
  # Define the extent of the analysis.
  # This area exceeds the proximity thresholds.
  # Such that routes from households inside the proximity threshold ...
  # ... can use streets that fall just outside of the proximity threshold.
  extent = st_buffer(hub, set_units(proximity_threshold * 1.5, "m"))
  # Prepare the street data.
  # --> Filter by the extent.
  # --> Add and select relevant columns.
  streets = streets |>
    st_filter(extent) |>
    mutate(index = .data[[index_column]])
  # Compute the geographic length of each street.
  # These will be used as edge weights in the network analysis.
  streets$length = st_length(streets)
  # Create the street network.
  network = streets |>
    as_sfnetwork(directed = directed)
  # Mark the nodes within the egress threshold of the hub.
  # This part of the network can be used to walk the last part to the hub.
  network = network |>
    activate("nodes") |>
    morph(to_spatial_neighborhood, hub, egress_threshold, weights = "length") |>
    mutate(in_egress = TRUE) |>
    unmorph() |>
    mutate(in_egress = replace_na(in_egress, FALSE))
  # Remove edges on which cycling is not allowed (e.g. walking paths).
  # Only the non-accessible edges within the egress threshold are kept.
  # Such that they can be used to walk the last part to the hub.
  # Do note that streets for which the index could not be computed are also NA.
  # That means those streets are assumed to be not allowed for cycling.
  network = network |>
    activate("edges") |>
    mutate(in_egress = .N()$in_egress[from] & .N()$in_egress[to]) |>
    filter(!is.na(index) | in_egress) |>
    activate("nodes") |>
    filter(!node_is_isolated())
  # Mark the nodes within the proximity threshold of the hub.
  # This defines the core of our analysis area.
  # Only households located within this part of the network are analyzed.
  network = network |>
    activate("nodes") |>
    morph(to_spatial_neighborhood, hub, proximity_threshold, weights = "length") |>
    mutate(in_proximity = TRUE) |>
    unmorph() |>
    mutate(in_proximity = replace_na(in_proximity, FALSE))
  # Select only the largest connected component of the network.
  # Such that small disconnected components are removed.
  network = network |>
    convert(to_components, .clean = TRUE)
  # Mark the nodes that represents the hub.
  network = network |>
    activate("nodes") |>
    morph(to_spatial_neighborhood, hub, 0, weights = "length") |>
    mutate(is_hub = TRUE) |>
    unmorph() |>
    mutate(is_hub = replace_na(is_hub, FALSE))
  # Prepare the households data.
  # --> Filter by the extent.
  # --> Add and select relevant columns.
  households = households |>
    st_filter(extent) |>
    mutate(id = seq_len(n()), pop = .data[[population_column]]) |>
    select(id, pop)
  # Find the nearest node to each household.
  nodes = st_as_sf(network, "nodes")
  households$node = st_nearest_feature(households, nodes)
  # Only keep households for which the nearest node is in proximity of the hub.
  # In contradiction to the pre-filter, this uses network distance.
  households = households |>
    filter(nodes$in_proximity[node])
  # Join household information to their nearest nodes in the network.
  households_per_node = households |>
    st_drop_geometry() |>
    group_by(node) |>
    summarize(households = list(id), pop = sum(pop))
  network = network |>
    activate("nodes") |>
    mutate(id = seq_len(n())) |>
    left_join(households_per_node, by = join_by(id == node)) |>
    mutate(pop = replace_na(pop, 0))
  # Return both the constructed street network and the updated household data.
  list(network = network, households = households)
}

find_suitable_networks = function(network, index_thresholds = seq(0, 1, by = 0.05)) {
  # Initialize the output object.
  out = list()
  # Extract streets from the street network in proximity of the hub.
  streets = network |>
    activate("nodes") |>
    filter(in_proximity) |>
    activate("edges") |>
    st_as_sf()
  # Compute the total length of these streets.
  total_length = sum(streets$length)
  # Subset the suitable streets for each of the given index thresholds.
  # Combine them into a single feature.
  for (i in seq_along(index_thresholds)) {
    it = index_thresholds[i]
    is_suitable = !is.na(streets$index) & streets$index >= it
    suitable_streets = streets[is_suitable, ]
    if (nrow(suitable_streets) == 0) {
      length = set_units(0, "m")
      minidx = NA
      maxidx = NA
      avgidx = NA
      geom = st_sfc(NA, crs = st_crs(streets))
    } else {
      lengths = suitable_streets$length
      indices = suitable_streets$index
      length = sum(lengths)
      minidx = min(indices, na.rm = TRUE)
      maxidx = max(indices, na.rm = TRUE)
      avgidx = weighted.mean(indices, drop_units(lengths), na.rm = TRUE)
      geom = st_combine(st_geometry(suitable_streets))
    }
    out[[i]] = st_sf(tibble(
      index_threshold = it,
      length = round(length, 2),
      share = round(drop_units(set_units(length / total_length, "%")), 2),
      index_worst = round(minidx, 2),
      index_best = round(maxidx, 2),
      index_mean = round(avgidx, 2),
      geom = geom
    ))
  }
  bind_rows(out)
}

find_connected_households = function(households, network,
                                     index_thresholds = seq(0, 1, by = 0.05),
                                     detour_thresholds = seq(1, 2, by = 0.25),
                                     penalty = 4) {
  # Initialize output object.
  out = list()
  # Extract the nodes from the street network.
  nodes = st_as_sf(network, "nodes")
  # Infer the indices of the nodes that:
  # --> Represent the hub (this will be the destination for routing).
  # --> Are in proximity of the hub (these will be the origins for routing).
  to_idx = which(nodes$is_hub)
  from_idxs = which(nodes$in_proximity)
  # Infer the total population that lives in the proximity of the hub.
  # This will be used to compute population shares for different thresholds.
  total_pop = sum(nodes[nodes$in_proximity, ]$pop)
  # Create the full street network for routing.
  # This equals the input network with edge weights added.
  # Edge weights equal edge length.
  # The not-suitable edges for walking the last part to the hub get a penalty.
  # Such that it is preferred to bike all the way to the hub.
  full_net = network |>
    activate("edges") |>
    mutate(is_suitable = !is.na(index)) |>
    mutate(weight = ifelse(is_suitable, length, length * penalty))
  # Compute travel costs on the full network.
  shortest_costs = st_network_cost(
    full_net,
    from = from_idxs,
    to = to_idx,
    weights = "weight"
  )[, 1]
  # Compute travel costs on the suitable network, i.e. "suitable travel costs".
  # What the suitable network is, is defined by the given index thresholds.
  for (i in seq_along(index_thresholds)) {
    # Define which streets are considered suitable.
    # Update the network accordingly.
    it = index_thresholds[i]
    suitable_net = full_net |>
      activate("edges") |>
      mutate(is_suitable = is_suitable & index >= it) |>
      filter(is_suitable | in_egress) |>
      mutate(weight = ifelse(is_suitable, length, length * penalty))
    # Compute suitable travel costs.
    suitable_costs = st_network_cost(
      suitable_net,
      from = from_idxs,
      to = to_idx,
      weights = "weight"
    )[, 1]
    # Compute the ratios between suitable costs and shortest costs.
    # These we call the detours.
    detours = suitable_costs / shortest_costs
    detours[which(from_idxs == to_idx)] = 0
    # Subset households are "acceptably" connected to the hub.
    # This depends on the accepted detour compared to the full network routes.
    # Combine them into a single feature.
    subout = list()
    for (j in seq_along(detour_thresholds)) {
      dt = detour_thresholds[j]
      is_acceptable = !is.na(detours) & detours <= dt
      connected_households = filter(households, node %in% from_idxs[is_acceptable]) 
      acceptable_detours = detours[is_acceptable]
      if (nrow(connected_households) == 0) {
        pop = 0
        mindetour = NA
        maxdetour = NA
        avgdetour = NA
        geom = st_sfc(NA, crs = st_crs(households))
      } else {
        pops = nodes$pop[from_idxs[is_acceptable]]
        pop = sum(pops)
        mindetour = min(acceptable_detours)
        maxdetour = max(acceptable_detours)
        avgdetour = weighted.mean(acceptable_detours, pops)
        geom = st_union(st_geometry(connected_households))
      }
      subout[[j]] = st_sf(tibble(
        index_threshold = it,
        detour_threshold = dt,
        pop = pop,
        share = round(pop / total_pop * 100, 2),
        detour_longest = round(maxdetour, 2),
        detour_shortest = round(mindetour, 2),
        detour_mean = round(avgdetour, 2),
        geom = geom
      ))
    }
    out[[i]] = bind_rows(subout)
  }
  bind_rows(out)
}

populate_households = function(households, population) {
  # Add population cell to each household.
  households_with_cellid = households |>
    st_join(rename(population, cellid = id)) |>
    st_drop_geometry() |>
    select(-pop)
  # Initialize sample.
  sample = list()
  # Sample one household per inhabitant, with replacement.
  for (i in seq_len(nrow(population))) {
    pop = population$pop[i]
    pool = filter(households_with_cellid, cellid == population$id[i])
    if (nrow(pool) == 0) {
      sample[[i]] = NULL
    } else {
      rows = sample(seq_len(nrow(pool)), pop, replace = TRUE)
      sample[[i]] = pool[rows, ]
    }
  }
  sample = bind_rows(sample)
  # Define number of inhabitants per sampled household.
  population_per_household = sample |>
    group_by(id) |>
    summarise(pop = n())
  # Write number of inhabitants to original household data.
  populated_households = households |>
    inner_join(population_per_household, by = "id")
  populated_households
}
