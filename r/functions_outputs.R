displace_outputs_main_path <- here("outputs")
displace_inputs_main_path <- "/Users/Shared/Dropbox/mpa-outcomes/data/confidential/displace/displace_inputs"

closure_areas_file <- here(
  "raw_inputs",
  "GRAPH",
  "shp",
  "ca_lease_areas_2024",
  "ca_lease_areas_2024.shp"
)

blocks_shore_eez_file <- here(
  "raw_inputs",
  "GRAPH",
  "shp",
  "spatial_grid",
  "Blocks_Shore_EEZ.shp"
)

displace_processed_inputs_path <- here("processed_inputs")
displace_raw_inputs_path <- here("raw_inputs")


metiers_names <- here::here(
  displace_processed_inputs_path,
  "metiersspe_california_example",
  "metier_names.dat"
) |>
  read.table(header = TRUE)


pop_names <- here::here(
  displace_raw_inputs_path,
  "POPULATIONS",
  "pop_names_california_example.txt"
) |>
  read.table(header = TRUE)


displace_baseline_outputs_path <- here(displace_outputs_main_path, "baseline")


generate_fe_dis_baseline_output <- function(
  displace_outputs_path
) {
  # List all matching files
  baseline_fe_sim_files <- list.files(
    path = displace_outputs_path,
    pattern = "^popnodes_cumftime_simu[0-9]+\\.dat$",
    full.names = TRUE
  )

  # Define column names
  column_names <- c("tstep", "graph_id", "long", "lat", "fe")

  # Read all files and bind into one dataframe
  # baseline_fe_sim_all <- baseline_fe_sim_files |>
  #   purrr::map(~ read.table(.x, header = FALSE, col.names = column_names)) |>
  #   bind_rows()

  baseline_fe_sim_all <- baseline_fe_sim_files |>
    purrr::map(\(f) {
      sim_label <- stringr::str_extract(f, "simu\\d+")
      read.table(f, header = FALSE, col.names = column_names) |>
        dplyr::mutate(simulation = sim_label)
    }) |>
    dplyr::bind_rows()

  # Some simulations may have been shut off early.
  # Here we identify any that did not run for the full time series and exclude them.
  error_simu <- baseline_fe_sim_all |>
    group_by(simulation) |>
    summarise(tstep = last(tstep)) |>
    ungroup() |>
    mutate(
      is_off = abs(tstep - max(tstep)) >= 4000
    ) |>
    filter(is_off) |>
    pull(simulation)

  baseline_fe_sim_all <- baseline_fe_sim_all |>
    filter(!simulation %in% error_simu)

  # Compute mean fe by tstep, graph_id, long, lat
  baseline_fe_sim <- baseline_fe_sim_all |>
    group_by(tstep, graph_id, long, lat) |>
    summarise(fe = mean(fe, na.rm = TRUE)) |>
    ungroup()

  baseline_fe_sim <- baseline_fe_sim |>
    # dplyr::filter(lat < 45) |>
    filter(tstep == max(tstep)) # DISPLACE outputs cumulative fishing time. Thus we will select those reported times at the end of the timesieries.

  fe_dis_baseline_output_sf <- st_as_sf(
    baseline_fe_sim,
    coords = c("long", "lat"),
    crs = 4326
  )

  return(list(
    fe_dis_baseline_output_sf = fe_dis_baseline_output_sf,
    baseline_fe_sim_all = baseline_fe_sim_all
  ))
}


fe_dis_baseline_output_sf <- generate_fe_dis_baseline_output(
  displace_baseline_outputs_path
)


# Read and process loglike outputs ----
generate_loglike_dis_baseline_output <- function(
  displace_baseline_outputs_path,
  pop_names,
  implicit_ssp = c("EOJ", "SGO", "OTH")
) {
  # List all matching files
  baseline_loglike_sim_files <- list.files(
    path = displace_baseline_outputs_path,
    pattern = "^loglike_simu[0-9]+\\.dat$",
    full.names = TRUE
  )

  idx_spp <- pop_names |> pull(idx) |> max()

  idx_exp_spp <- idx_spp -
    pop_names |>
      filter(spp %in% implicit_ssp) |>
      nrow()

  column_names <- c(
    'tstep_dep',
    'tstep_arr',
    'reason_back',
    'cumsteaming',
    'idx_node',
    'idx_vessel',
    'VE_REF',
    'timeatsea',
    'fuelcons',
    'traveled_dist',
    paste('pop.', 0:idx_spp, sep = ''),
    "freq_metiers",
    "revenue",
    "rev_from_av_prices",
    "rev_explicit_from_av_prices",
    "fuelcost",
    "vpuf",
    "gav",
    "gradva",
    "sweptr",
    "revpersweptarea",
    paste('disc.', 0:idx_exp_spp, sep = ''),
    "GVA",
    "GVAPerRevenue",
    "LabourSurplus",
    "GrossProfit",
    "NetProfit",
    "NetProfitMargin",
    "GVAPerFTE",
    "RoFTA",
    "BER",
    "CRBER",
    "NetPresentValue",
    "numTrips"
  )

  # Read all files and bind into one dataframe
  baseline_loglike_sim <- baseline_loglike_sim_files |>
    purrr::map(\(f) {
      sim_label <- stringr::str_extract(f, "simu\\d+")
      read.table(f, header = FALSE, col.names = column_names) |>
        dplyr::mutate(simulation = sim_label)
    }) |>
    dplyr::bind_rows()

  # Some simulations may have been shut off early.
  # Here we identify any that did not run for the full time series and exclude them.
  error_simu <- baseline_loglike_sim |>
    group_by(simulation) |>
    summarise(last_tstep = last(tstep_arr)) |>
    ungroup() |>
    mutate(
      is_off = abs(last_tstep - max(last_tstep)) >= 4000
    ) |>
    filter(is_off) |>
    pull(simulation)

  baseline_loglike_sim <- baseline_loglike_sim |>
    filter(!simulation %in% error_simu)

  return(baseline_loglike_sim)
}


loglike_dis_baseline_output <- generate_loglike_dis_baseline_output(
  displace_baseline_outputs_path,
  pop_names,
  implicit_ssp = c("EOJ", "SGO", "OTH")
)


# Read and process population statistics outputs ----
generate_popstats_dis_baseline_output <- function(
  displace_baseline_outputs_path,
  pop_names
) {
  pop_sim_file <- file.path(
    displace_baseline_outputs_path,
    "popstats_simu1.dat"
  )

  # List all matching files
  pop_sim_files <- list.files(
    path = displace_baseline_outputs_path,
    pattern = "^popstats_simu[0-9]+\\.dat$",
    full.names = TRUE
  )

  column_names <- c(
    "tstep",
    "stk",
    paste0("N", 0:13),
    paste0("W", 0:13),
    paste0("SSB", 0:13)
  )

  # Read all files and bind into one dataframe
  pop_stats <- pop_sim_files |>
    purrr::map(\(f) {
      sim_label <- stringr::str_extract(f, "simu\\d+")
      read.table(f, header = FALSE, col.names = column_names) |>
        dplyr::mutate(simulation = sim_label)
    }) |>
    dplyr::bind_rows()

  pop_stats <- pop_stats |>
    left_join(pop_names, by = c("stk" = "idx")) |>
    mutate(stknames = spp)

  # Some simulations may have been shut off early.
  # Here we identify any that did not run for the full time series and exclude them.
  error_simu <- pop_stats |>
    group_by(simulation) |>
    summarise(last_tstep = last(tstep)) |>
    ungroup() |>
    mutate(
      is_off = abs(last_tstep - max(last_tstep)) >= 4000
    ) |>
    filter(is_off) |>
    pull(simulation)

  agg_pop_stats <- pop_stats |>
    rowwise() |>
    mutate(
      totN = sum(c_across(starts_with("N"))),
      meanW = mean(c_across(starts_with("W"))),
      totSSB = sum(c_across(starts_with("SSB")))
    ) |>
    ungroup() |>
    dplyr::select(tstep, stk, totN, meanW, totSSB, spp, simulation)

  agg_pop_stats_year_sim <- agg_pop_stats |>
    mutate(year_code = floor(tstep / 8762) + 1, year = 2009 + year_code) |>
    group_by(year, spp, simulation) |>
    summarise(
      SSB = first(totSSB) / 1000
    ) |>
    ungroup() |>
    group_by(year, spp) |>
    summarise(
      SSB = mean(SSB)
    ) |>
    ungroup()

  return(list(
    agg_pop_stats = agg_pop_stats,
    agg_pop_stats_year_sim = agg_pop_stats_year_sim
  ))
}

agg_pop_stats_year_sim <- generate_popstats_dis_baseline_output(
  displace_baseline_outputs_path,
  pop_names
)

# Simulated fishing effort map ----

generate_notebook_fe_maps <- function(fe_distribution) {
  # ---- Static pieces used for all plots ----
  custom_palette <- c(
    "#d9d9d9",
    "#b2e2e2",
    "#66c2a4",
    "#fecc5c",
    "#fd8d3c",
    "#f03b20",
    "#bd0026"
  )

  # State boundaries (California)
  states_df <- ggplot2::map_data("state", region = c("california"))
  xlim <- range(states_df$long, na.rm = TRUE)
  ylim <- range(states_df$lat, na.rm = TRUE)

  # ---- Helper to build one plot ----
  build_one_plot <- function(displace_fe_sf, plot_title) {
    if (!inherits(displace_fe_sf, "sf")) {
      stop("All selected items must be 'sf' objects.")
    }

    if (!("fe_cumT" %in% names(displace_fe_sf))) {
      stop("Each 'sf' object must contain a 'fe_cumT' column.")
    }

    # Natural breaks for fe_cumT
    brks_obj <- classIntervals(
      displace_fe_sf$fe_cumT,
      n = length(custom_palette) - 1,
      style = "quantile"
    )
    breaks <- brks_obj$brks

    # Round breaks; increase precision if duplicates appear
    decimal_places <- 0
    rounded_breaks <- round(breaks, decimal_places)
    guard <- 0
    while (length(unique(rounded_breaks)) < length(breaks) && guard < 10) {
      decimal_places <- decimal_places + 1
      rounded_breaks <- round(breaks, decimal_places)
      guard <- guard + 1
    }

    # Labels like "low-high"
    labels <- paste(
      head(rounded_breaks, -1),
      tail(rounded_breaks, -1),
      sep = "-"
    )

    # Categorize
    displace_fe_sf$fe_cat <- cut(
      displace_fe_sf$fe_cumT,
      breaks = breaks,
      include.lowest = TRUE,
      labels = labels
    )

    # Map
    ggplot(displace_fe_sf) +
      geom_polygon(
        data = states_df,
        aes(x = long, y = lat, group = group),
        fill = NA,
        color = "black",
        linewidth = 0.1
      ) +
      geom_sf(aes(fill = fe_cat), color = NA) +
      scale_fill_manual(
        values = custom_palette,
        name = "",
        na.translate = FALSE
      ) +
      coord_sf(
        xlim = c(xlim[1] - 1, xlim[2] - 3),
        ylim = c(ylim[1] - 0.1, ylim[2])
      ) +
      scale_x_continuous(breaks = c(-120, -123)) +
      labs(title = plot_title) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11),
        strip.text = element_text(hjust = 0, vjust = 3),
        panel.spacing = unit(-3, "lines"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        legend.position = "top",
        legend.background = element_rect(
          fill = alpha('white', 0.6),
          color = NA
        ),
        legend.box = "horizontal",
        plot.caption.position = "panel"
      )
  }

  # ---- Build plots for every element in fe_distribution ----
  plots <- mapply(
    build_one_plot,
    displace_fe_sf = fe_distribution,
    plot_title = names(fe_distribution),
    SIMPLIFY = FALSE
  )

  return(plots) # named list of ggplot objects
}

generate_fe_displace_map_report_figures <- function(
  fe_dis_baseline_output_sf,
  blocks_shore_eez_file,
  closure_areas_file
) {
  closure_areas_sf <- st_read(closure_areas_file, quiet = TRUE) |> st_union()

  fe_dis_output_sf <- st_as_sf(
    fe_dis_baseline_output_sf,
    coords = c("long", "lat"),
    crs = 4326
  )

  leaseblock <- st_read(blocks_shore_eez_file, quiet = TRUE) |>
    st_zm() |>
    st_transform(crs = st_crs(fe_dis_output_sf))

  gridded_fe_distributions <- st_join(
    leaseblock,
    fe_dis_output_sf,
    join = st_intersects
  ) |>
    na.omit() |>
    group_by(geometry) |>
    summarise(fe = sum(fe, na.rm = TRUE)) |>
    ungroup()

  displace_fe_sf <- gridded_fe_distributions |>
    rename(fe_cumT = fe) |>
    mutate(source = "sim")

  custom_palette <- c(
    "#d9d9d9",
    "#b2e2e2",
    "#66c2a4",
    "#fecc5c",
    "#fd8d3c",
    "#f03b20",
    "#bd0026"
  )

  # Extract state boundaries
  states <- map_data("state", region = c("california"))

  # Create natural breaks for the 'fe' variable
  breaks <- classIntervals(
    displace_fe_sf |> pull(fe_cumT),
    n = length(custom_palette) - 1,
    style = "quantile"
  )$brks

  # Round the breaks
  rounded_breaks <- round(breaks)

  # Increase rounding precision if duplicates are detected
  while (length(unique(rounded_breaks)) < length(breaks)) {
    decimal_places <- decimal_places + 1
    rounded_breaks <- round(breaks, decimal_places)
  }

  # Generate labels using the rounded breaks, separating values with "-"
  labels <- paste(
    head(rounded_breaks, -1),
    tail(rounded_breaks, -1),
    sep = "-"
  )

  # Apply the cut function with custom labels
  displace_fe_sf$fe_cat <- cut(
    displace_fe_sf$fe_cumT,
    breaks = breaks,
    include.lowest = TRUE,
    labels = labels
  )

  # Get the bounding box of the states
  states_bbox <- st_bbox(st_as_sf(
    states,
    coords = c("long", "lat"),
    crs = 4326
  ))

  # Plot
  displace_fe_map <- ggplot(displace_fe_sf) +
    geom_polygon(
      data = states,
      aes(x = long, y = lat, group = group),
      fill = NA,
      color = "black",
      linewidth = 0.1
    ) +
    # Observed + Simulated
    geom_sf(
      aes(fill = fe_cat),
      color = NA
    ) +
    scale_fill_manual(
      values = custom_palette,
      name = "Fishing Effort (h)",
      na.translate = FALSE
    ) +
    geom_sf(
      data = closure_areas_sf,
      aes(geometry = geometry),
      fill = "white",
      color = "blue",
      alpha = 0.1
    ) +
    coord_sf(
      xlim = c(states_bbox["xmin"] - 1, states_bbox["xmax"] - 3),
      ylim = c(states_bbox["ymin"] - 0.1, states_bbox["ymax"])
    ) +
    scale_x_continuous(breaks = c(-120, -123)) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 12, face = "bold"),
      strip.text = element_text(hjust = 0, vjust = 3),
      panel.spacing = unit(-3, "lines"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.position = "top",
      legend.background = element_rect(
        fill = alpha('white', 0.6),
        color = NA
      ),
      legend.box = "horizontal",
      plot.caption.position = "panel"
    )

  return(list(
    displace_fe_map = displace_fe_map,
    displace_fe_sf = displace_fe_sf
  ))
}


displace_fe_map <- generate_fe_displace_map_report_figures(
  fe_dis_baseline_output_sf$fe_dis_baseline_output_sf,
  blocks_shore_eez_file,
  closure_areas_file
)


# Outputs summary ----
generate_metrics_summary <- function(
  loglike_dis_baseline_output,
  filtered_vmstix_data,
  vessels_id_mapping
) {
  # Simulated data:
  loglike <- loglike_dis_baseline_output

  sim_trips <- loglike |>
    mutate(total_catch = rowSums(across(starts_with("pop")), na.rm = TRUE)) |>
    group_by(VE_REF, simulation) |>
    summarise(
      timeatsea = sum(timeatsea),
      steamtime = sum(cumsteaming),
      fishingtime = timeatsea - steamtime,
      total_catch = sum(total_catch, na.rm = TRUE) / 1000,
      numTrips = last(numTrips |> na.omit()),
      timeatsea = sum(timeatsea, na.rm = TRUE),
      traveled_dist = sum(traveled_dist, na.rm = TRUE),
    ) |>
    ungroup() |>
    group_by(simulation) |>
    summarise(
      fishingtime = sum(fishingtime, na.rm = TRUE),
      total_catch = sum(total_catch, na.rm = TRUE),
      numTrips = sum(numTrips),
      timeatsea = sum(timeatsea, na.rm = TRUE),
      traveled_dist = sum(traveled_dist, na.rm = TRUE)
    ) |>
    ungroup() |>
    summarise(
      n = n(),

      fishingtime_mean = mean(fishingtime),
      fishingtime_sd = sd(fishingtime),
      fishingtime_ci_lower = fishingtime_mean - 1.96 * fishingtime_sd / sqrt(n),
      fishingtime_ci_upper = fishingtime_mean + 1.96 * fishingtime_sd / sqrt(n),

      total_catch_mean = mean(total_catch),
      total_catch_sd = sd(total_catch),
      total_catch_ci_lower = total_catch_mean - 1.96 * total_catch_sd / sqrt(n),
      total_catch_ci_upper = total_catch_mean + 1.96 * total_catch_sd / sqrt(n),

      numTrips_mean = mean(numTrips),
      numTrips_sd = sd(numTrips),
      numTrips_ci_lower = numTrips_mean - 1.96 * numTrips_sd / sqrt(n),
      numTrips_ci_upper = numTrips_mean + 1.96 * numTrips_sd / sqrt(n),

      timeatsea_mean = mean(timeatsea),
      timeatsea_sd = sd(timeatsea),
      timeatsea_ci_lower = timeatsea_mean - 1.96 * timeatsea_sd / sqrt(n),
      timeatsea_ci_upper = timeatsea_mean + 1.96 * timeatsea_sd / sqrt(n),

      traveled_dist_mean = mean(traveled_dist),
      traveled_dist_sd = sd(traveled_dist),
      traveled_dist_ci_lower = traveled_dist_mean -
        1.96 * traveled_dist_sd / sqrt(n),
      traveled_dist_ci_upper = traveled_dist_mean +
        1.96 * traveled_dist_sd / sqrt(n),

      .groups = "drop"
    )

  sim_summary_table <- sim_trips |>
    dplyr::select(
      contains("_mean"),
      contains("_ci_lower"),
      contains("_ci_upper")
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = c("variable", ".value"),
      names_pattern = "(.*)_(mean|ci_lower|ci_upper)"
    ) |>
    mutate(
      Metric = recode(
        variable,
        fishingtime = "Fishing time (h)",
        total_catch = "Total catch (mt)",
        numTrips = "Number of trips",
        timeatsea = "Time at sea (h)",
        traveled_dist = "Distance traveled (km)"
      ),
      simulation_mean = mean,
      simulation_ci_halfwidth = (ci_upper - ci_lower) / 2,
      simulation_ci_halfwidth_pct = 100 *
        simulation_ci_halfwidth /
        simulation_mean
      # summary = sprintf("%.1f ± %.1f", mean, ci_halfwidth)
    ) |>
    dplyr::select(
      Metric,
      simulation_mean,
      simulation_ci_halfwidth,
      simulation_ci_halfwidth_pct
    )

  return(sim_summary_table)
}


metrics_summary <- generate_metrics_summary(
  loglike_dis_baseline_output,
  filtered_vmstix_data,
  vessels_id_mapping
)

generate_ssb_fit_plot <- function(
  stock_assessment_data_folder,
  agg_pop_stats_year_sim
) {
  # Read data
  shortspine_ssb_timeseries_in_mt <- readRDS(glue::glue(
    stock_assessment_data_folder,
    "shortspine_ssb_timeseries_in_mt.Rds"
  )) |>
    mutate(species = "Shortspine thornyhead", era = "Historical") |>
    dplyr::select(species, era, year, ssb_mt)

  DTS_spawning_stock_biomass <- readRDS(glue::glue(
    stock_assessment_data_folder,
    "DTS_spawning_stock_biomass.Rds"
  ))

  # Prepare for plotting
  agg_pop_stats_year_obs <- DTS_spawning_stock_biomass |>
    filter(!species %in% c("Shortspine thornyhead", "Petrale sole")) |>
    dplyr::select(species, era, year, ssb_mt) |>
    rbind(shortspine_ssb_timeseries_in_mt) |>
    # filter(era == "Historical") |>
    rename(SSB = ssb_mt, spp = species) |>
    mutate(
      spp_code = recode(
        spp,
        "Sablefish" = "SAB",
        "Shortspine thornyhead" = "SJU",
        "Longspine thornyhead" = "SJZ",
        "Dover sole" = "MIP"
      )
    ) |>
    dplyr::select(-era)

  combined_pop_stats <- agg_pop_stats_year_sim |>
    rename(spp_code = spp) |>
    left_join(
      agg_pop_stats_year_obs,
      by = c("year", "spp_code"),
      suffix = c("_sim", "_obs")
    ) |>
    arrange(spp, year)

  # Calculate R^2
  lm_model <- lm(SSB_sim ~ SSB_obs, data = combined_pop_stats)
  r_squared <- summary(lm_model)$r.squared

  # Determine axis limits (minimum and maximum values for both variables)
  axis_limits <- range(
    combined_pop_stats$SSB_obs,
    combined_pop_stats$SSB_sim,
    na.rm = TRUE
  )
  axis_limits[1] <- max(axis_limits[1], 0.01) # Ensure minimum is positive
  log_breaks <- get_log_breaks(axis_limits)

  ssb_exploration_plot <- ggplot(
    combined_pop_stats,
    aes(x = SSB_obs / 1000, y = SSB_sim / 1000, color = year)
  ) +
    geom_point(size = 3, alpha = 0.6) +
    geom_text_repel(
      aes(label = year),
      size = 2.5,
      max.overlaps = 20,
      min.segment.length = 0
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    facet_wrap(~spp, ncol = 2, scales = "free") + # Allow individual scales per facet
    labs(x = "Observed SSB (1000s mt)", y = "Simulated SSB (1000s mt)") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.25, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
      aspect.ratio = 1,
      legend.position = "none" # Remove legend
    )

  ssb_fit_plot <- ggplot(
    combined_pop_stats,
    aes(x = SSB_obs, y = SSB_sim, color = spp)
  ) +
    geom_point(size = 3, alpha = 0.6, stroke = 0) +
    geom_smooth(
      method = "lm",
      linewidth = 0.5,
      color = "black",
      linetype = "solid",
      se = FALSE
    ) + # Trend line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") + # Diagonal line
    scale_x_log10(
      limits = axis_limits,
      breaks = log_breaks,
      labels = log_breaks
    ) + # Log scale for x-axis with custom breaks
    scale_y_log10(
      limits = axis_limits,
      breaks = log_breaks,
      labels = log_breaks
    ) + # Log scale for y-axis with custom breaks
    scale_color_discrete(name = "") +
    coord_fixed(ratio = 1, clip = "on") + # Ensure 1:1 ratio (perfect square)
    labs(
      x = "Observed SSB (mt)",
      y = "Simulated SSB (mt)"
    ) +
    annotate(
      "text",
      x = axis_limits[2], # Position annotation horizontally
      y = axis_limits[1], # Position annotation vertically
      label = paste0("R² = ", round(r_squared, 3)), # Display rounded R² value
      hjust = 1,
      size = 4
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), # Remove major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      axis.line = element_line(color = "black"), # Add contour (axis lines)
      axis.ticks = element_line(color = "black"), # Add axis ticks
      axis.ticks.length = unit(0.25, "cm"), # Adjust tick length
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7), # Rotate x-axis tick labels
      aspect.ratio = 1 # Enforce square aspect in layout regardless of device
    )

  # Initial SSB

  initial_combined_pop_stats <- combined_pop_stats |>
    filter(year == min(year))

  # Calculate R^2
  lm_model <- lm(SSB_sim ~ SSB_obs, data = initial_combined_pop_stats)
  r_squared <- summary(lm_model)$r.squared

  # Determine axis limits (minimum and maximum values for both variables)
  axis_limits <- range(
    initial_combined_pop_stats$SSB_obs,
    initial_combined_pop_stats$SSB_sim,
    na.rm = TRUE
  )
  axis_limits[1] <- max(axis_limits[1], 0.01) # Ensure minimum is positive
  log_breaks <- get_log_breaks(axis_limits)

  initial_ssb_fit_plot <- ggplot(
    initial_combined_pop_stats,
    aes(x = SSB_obs, y = SSB_sim, color = spp)
  ) +
    geom_point(size = 3, alpha = 0.6, stroke = 0) +
    geom_smooth(
      method = "lm",
      linewidth = 0.5,
      color = "black",
      linetype = "solid",
      se = FALSE
    ) + # Trend line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") + # Diagonal line
    scale_x_log10(
      limits = axis_limits,
      breaks = log_breaks,
      labels = log_breaks
    ) + # Log scale for x-axis with custom breaks
    scale_y_log10(
      limits = axis_limits,
      breaks = log_breaks,
      labels = log_breaks
    ) + # Log scale for y-axis with custom breaks
    scale_color_discrete(name = "") +
    coord_fixed(ratio = 1, clip = "on") + # Ensure 1:1 ratio (perfect square)
    labs(
      x = "Observed SSB (mt)",
      y = "Simulated SSB (mt)"
    ) +
    annotate(
      "text",
      x = axis_limits[2], # Position annotation horizontally
      y = axis_limits[1], # Position annotation vertically
      label = paste0("R² = ", round(r_squared, 3)), # Display rounded R² value
      hjust = 1,
      size = 4
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), # Remove major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      axis.line = element_line(color = "black"), # Add contour (axis lines)
      axis.ticks = element_line(color = "black"), # Add axis ticks
      axis.ticks.length = unit(0.25, "cm"), # Adjust tick length
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7), # Rotate x-axis tick labels
      aspect.ratio = 1 # Enforce square aspect in layout regardless of device
    )

  return(list(
    ssb_fit_plot = ssb_fit_plot,
    ssb_exploration_plot = ssb_exploration_plot,
    initial_ssb_fit_plot = initial_ssb_fit_plot
  ))
}
