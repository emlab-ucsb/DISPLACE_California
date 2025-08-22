# DISPLACE Validation Outputs functions ----

# Read and process outputs ----

generate_loglike_dis_baseline_output <- function(
  displace_outputs_path,
  table_spp,
  implicit_ssp = c("EOJ", "SGO", "OTH")
) {
  # List all matching files
  baseline_loglike_sim_files <- list.files(
    path = displace_outputs_path,
    pattern = "^loglike_simu[0-9]+\\.dat$",
    full.names = TRUE
  )

  idx_spp <- table_spp |> pull(idx) |> max()
  idx_exp_spp <- idx_spp -
    table_spp |>
      filter(spp %in% implicit_ssp) |>
      nrow()
  # idx_spp <- 3
  # id_exp_spp <- 3

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


generate_popstats_dis_baseline_output <- function(
  displace_outputs_path,
  pop_names
) {
  pop_sim_file <- file.path(
    displace_outputs_path,
    "popstats_simu1.dat"
  )

  # List all matching files
  pop_sim_files <- list.files(
    path = displace_outputs_path,
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

# Fleet validation ----

generate_summary_report_table <- function(
  loglike_dis_baseline_output,
  filtered_vmstix_data
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

  # Observed data:
  tix_merged_data <- filtered_vmstix_data$filtered_tix_data_from_merge |>
    distinct()

  vms_merged_data <- filtered_vmstix_data$filtered_matched_vmstix

  # Assessing number of trips:
  obs_tix_metrics <- tix_merged_data |>
    summarise(
      numTrips = n_distinct(landingreceiptnum),
      total_catch = sum(lbs, na.rm = TRUE) * 0.453592
    )

  obs_vms_metrics <- vms_merged_data |>
    summarise(
      timeatsea = sum(tmdiff_hr, na.rm = TRUE),
      traveled_dist = sum(segment_dist, na.rm = TRUE) / 1000
    )

  obs_fishingtime <- vms_merged_data |>
    filter(fishingornot == "fish") |>
    pull(tmdiff_hr) |>
    sum()

  obs_summary_table <- data.frame(
    Metric = c(
      "Fishing time (h)",
      "Total catch (mt)",
      "Number of trips",
      "Time at sea (h)",
      "Distance traveled (km)"
    ),
    Observed = c(
      obs_fishingtime,
      obs_tix_metrics |> pull(total_catch) / 1000,
      obs_tix_metrics |> pull(numTrips),
      obs_vms_metrics |> pull(timeatsea),
      obs_vms_metrics |> pull(traveled_dist)
    )
  )

  df <- obs_summary_table |> left_join(sim_summary_table, by = "Metric")

  # Set parameter as row name and format it
  df <- obs_summary_table |>
    left_join(sim_summary_table, by = "Metric") |>
    mutate(
      # calculate % difference before formatting
      percent_diff = round(((simulation_mean - Observed) / Observed) * 100, 1),
      percent_diff_ci = round((simulation_ci_halfwidth / Observed) * 100, 1),

      # store formatted versions
      Observed_fmt = formatC(
        Observed,
        format = "f",
        big.mark = ",",
        digits = 0
      ),
      simulation_mean_fmt = formatC(
        simulation_mean,
        format = "f",
        big.mark = ",",
        digits = 0
      ),
      simulation_ci_halfwidth_fmt = formatC(
        simulation_ci_halfwidth,
        format = "f",
        big.mark = ",",
        digits = 0
      ),
      # combine into text strings
      Simulated = sprintf(
        "%s ± %s",
        simulation_mean_fmt,
        simulation_ci_halfwidth_fmt
      ),
      Difference = sprintf("%.1f ± %.1f", percent_diff, percent_diff_ci)
    ) |>
    dplyr::select(Metric, Observed = Observed_fmt, Simulated, Difference)

  # Generate kable with conditional formatting
  kable_df <- df |>
    kable(
      format = "html",
      align = c("l", "r", "r", "r"),
      caption = "Table 1. Comparison of simulated and observed metrics with 95% confidence intervals (10 replicates per scenario).",
    ) |>
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE
    )

  return(list(summary_table_df = df, summary_table_kable = kable_df))
}


generate_fe_displace_map_report_figures <- function(
  fe_dis_baseline_output_sf,
  vms_cumtime_leaseblock_vessel,
  blocks_shore_eez_file,
  rule_3 = TRUE,
  mask_3rule,
  closure_areas_file,
  filtered_matched_vmstix
) {
  closure_areas_sf <- st_read(closure_areas_file, quiet = TRUE) |> st_union()

  ca_fe_grouped <- do.call(rbind, vms_cumtime_leaseblock_vessel) |>
    group_by(Index, geometry) |>
    summarise(
      n_ship = sum(n_ship, na.rm = TRUE),
      fe_len = sum(fe_len, na.rm = TRUE),
      fe_cumT = sum(fe_cumT, na.rm = TRUE)
    ) |>
    ungroup() |>
    filter(fe_len != 0, fe_cumT != 0)

  leaseblock <- st_read(blocks_shore_eez_file, quiet = TRUE) |>
    st_zm() |>
    st_transform(crs = st_crs(ca_fe_grouped))

  fe_dis_output_sf <- st_as_sf(
    fe_dis_baseline_output_sf,
    coords = c("long", "lat"),
    crs = 4326
  ) |>
    st_transform(crs = st_crs(ca_fe_grouped))

  gridded_fe_distributions <- st_join(
    leaseblock,
    fe_dis_output_sf,
    join = st_intersects
  ) |>
    na.omit() |>
    group_by(geometry) |>
    summarise(fe = sum(fe, na.rm = TRUE)) |>
    ungroup()

  fe_displace <- gridded_fe_distributions |> rename(fe_cumT = fe)

  ca_fe_grouped_to_plot <- ca_fe_grouped |>
    mutate(source = "obs")

  fe_displace_to_plot <- fe_displace |>
    mutate(source = "sim")

  # Function to compute difference scenario for one metier
  compute_difference_sf <- function(
    ca_fe_grouped_to_plot,
    fe_displace_to_plot
  ) {
    # Join both directions to capture all matches
    comparison_left <- st_join(
      fe_displace_to_plot,
      ca_fe_grouped_to_plot,
      suffix = c("_sim", "_obs"),
      join = st_equals
    )
    comparison_right <- st_join(
      ca_fe_grouped_to_plot,
      fe_displace_to_plot,
      suffix = c("_obs", "_sim"),
      join = st_equals
    )
    comparison <- bind_rows(comparison_left, comparison_right) |>
      distinct()

    # Replace NA and compute difference
    comparison <- comparison |>
      mutate(
        fe_cumT_sim = replace_na(fe_cumT_sim, 0),
        fe_cumT_obs = replace_na(fe_cumT_obs, 0), # to make sure all cells are represented in both scenarios
        fe_cumT_diff = fe_cumT_sim - fe_cumT_obs,
        fe_cumT = fe_cumT_diff,
        source = "diff"
      ) |>
      dplyr::select("geometry", "fe_cumT", "source")

    return(comparison)
  }

  difference_to_plot <- compute_difference_sf(
    ca_fe_grouped_to_plot,
    fe_displace_to_plot
  )

  fe_to_plot <- bind_rows(ca_fe_grouped_to_plot, fe_displace_to_plot)

  closure_areas_sf <- st_read(closure_areas_file, quiet = TRUE) |> st_union()

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
    fe_to_plot |> pull(fe_cumT),
    n = length(custom_palette) - 1,
    style = "quantile"
  )$brks

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
  fe_to_plot$fe_cat <- cut(
    fe_to_plot$fe_cumT,
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

  facet_labels <- c(
    "sim" = "Simulation",
    "obs" = "Observed",
    "diff" = "Difference"
  )

  if (rule_3) {
    mask_3rule_geom <- mask_3rule |> pull(geometry)

    fe_to_plot <- fe_to_plot |>
      filter(geometry %in% mask_3rule_geom)

    difference_to_plot <- difference_to_plot |>
      filter(geometry %in% mask_3rule_geom)
  }

  # Initialize list
  fe_displace_plots <- list()

  # Define pseudo-log scale
  pseudo_log <- trans_new(
    name = "pseudo_log",
    transform = function(x) sign(x) * log10(abs(x) + 1),
    inverse = function(x) sign(x) * (10^abs(x) - 1),
    domain = c(-Inf, Inf)
  )

  # Fake diff facet
  fe_diff_fake <- difference_to_plot |>
    mutate(
      source = "diff",
      fe_diff = fe_cumT,
    ) |>
    dplyr::select(geometry, fe_diff, source)

  q <- quantile(fe_diff_fake$fe_diff, probs = c(0.01, 0.99), na.rm = TRUE)

  # limit fe_diff to 0.01 and 0.99 quantiles
  fe_diff_fake_q <- fe_diff_fake |>
    mutate(
      fe_diff = ifelse(
        fe_diff < q[[1]],
        q[[1]],
        fe_diff
      ),
      fe_diff = ifelse(
        fe_diff > q[[2]],
        q[[2]],
        fe_diff
      )
    )

  # Subset original
  fe_main_subset <- fe_to_plot |>
    dplyr::select(geometry, fe_cat, source)

  # Combine
  fe_augmented <- bind_rows(
    fe_main_subset,
    fe_diff_fake_q
  )

  fe_augmented <- fe_augmented |>
    mutate(source = factor(source, levels = c("obs", "sim", "diff")))

  # Plot
  fe_map_sim_vs_obs <- ggplot(fe_augmented) +
    geom_polygon(
      data = states,
      aes(x = long, y = lat, group = group),
      fill = NA,
      color = "black",
      linewidth = 0.1
    ) +

    # Observed + Simulated
    geom_sf(
      data = ~ subset(.x, source != "diff"),
      aes(fill = fe_cat),
      color = NA
    ) +
    scale_fill_manual(
      values = custom_palette,
      name = "Fishing Effort (h)",
      na.translate = FALSE
    ) +

    ggnewscale::new_scale_fill() +

    # Difference layer
    geom_sf(
      data = ~ subset(.x, source == "diff"),
      aes(fill = fe_diff),
      color = NA
    ) +
    geom_sf(
      data = closure_areas_sf,
      aes(geometry = geometry),
      fill = "white",
      color = "blue",
      alpha = 0.1
    ) +
    scale_fill_gradient2(
      low = "black",
      mid = "white",
      high = "red",
      midpoint = 0,
      name = "Δ Effort (h)\nSimulated - Observed",
      trans = "identity",
      breaks = c(
        q[[1]],
        -600,
        -300,
        0,
        200,
        q[[2]]
      ), # only the ends
      labels = c("Q1", "-600", "-300", "0", "200", "Q99"),
      na.value = "transparent",
      guide = guide_colourbar(
        label.theme = element_text(
          angle = 45,
          size = 8,
          margin = margin(t = 9)
        ),
        title.theme = element_text(size = 9, margin = margin(b = 20, r = 5))
      )
    ) +

    facet_wrap(~source, labeller = as_labeller(facet_labels)) +
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

  fe_diff_fake$plot_title <- "Full distribution of Δ effort (h)"
  diff_distribution <- fe_diff_fake |>
    ggplot(aes(x = "", y = fe_diff)) +
    geom_violin(
      fill = "gray80",
      color = "black",
      linewidth = 0.1
    ) +
    # Ticks that intersect the violin
    geom_segment(
      data = data.frame(y = q),
      aes(x = 0.8, xend = 1.2, y = y, yend = y),
      color = "red",
      linewidth = 0.3
    ) +
    annotate(
      "text",
      x = 1.25,
      y = q[1],
      label = "Q1",
      hjust = 0,
      vjust = 4,
      size = 2.5,
      color = "red"
    ) +
    annotate(
      "text",
      x = 1.25,
      y = q[2],
      label = "Q99",
      hjust = 0,
      vjust = 4,
      size = 2.5,
      color = "red"
    ) +
    facet_grid(. ~ plot_title) +
    coord_flip() +
    labs(
      y = "",
      x = NULL
    ) +
    theme_bw(base_size = 8) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(
        fill = "white",
        color = "black",
        linewidth = 0.05
      ),
      strip.background = element_rect(
        fill = "white",
        color = "black",
        linewidth = 0.2
      ),
      strip.text = element_text(
        hjust = 0,
        vjust = 1,
        face = "bold",
        size = 7.5
      )
    )

  final_plot <- ggdraw(fe_map_sim_vs_obs) +
    draw_plot(
      diff_distribution,
      x = 0.73,
      y = 0.55,
      width = 0.25,
      height = 0.15
    )

  ## ZOOM in maps

  # ZOOM in lease areas plots

  # Bounding boxes
  bbox_list <- list(
    morrobay = list(xmin = -122.5, xmax = -120.7, ymin = 34.555, ymax = 36.855),
    Humboldt = list(xmin = -125.4, xmax = -123.6, ymin = 39.7, ymax = 42)
  )

  # Initialize list to store zoomed plots
  zoomed_plot_list <- list()

  # Loop through each plot and bbox

  original_plot <- fe_map_sim_vs_obs

  for (bbox_name in names(bbox_list)) {
    bbox <- bbox_list[[bbox_name]]

    zoomed_plot <- original_plot +
      coord_sf(
        xlim = c(bbox$xmin, bbox$xmax),
        ylim = c(bbox$ymin, bbox$ymax)
      )

    zoomed_plot_list[[bbox_name]] <- zoomed_plot
  }

  # Observed FE from vessels fishign within the closure areas ----

  # Agregate grid data by vessel
  vessel_names <- names(vms_cumtime_leaseblock_vessel)
  vessel_ids <- str_extract(vessel_names, "USA\\d{4}")
  vessel_groups <- split(vessel_names, vessel_ids)

  vms_cumtime_by_vessel <- map_dfr(
    vessel_groups,
    ~ bind_rows(vms_cumtime_leaseblock_vessel[.x]) |>
      group_by(Index, geometry) |>
      summarise(
        n_ship = sum(n_ship, na.rm = TRUE),
        fe_len = sum(fe_len, na.rm = TRUE),
        fe_cumT = sum(fe_cumT, na.rm = TRUE),
        .groups = "drop"
      ),
    .id = "vessel_id" # adds a column with the vessel ID (e.g., "USA0006")
  )

  # Select vessels fishing within the closure areas
  filtered_merge_oi <- filtered_matched_vmstix |>
    filter(fishingornot == "fish")

  vms_points <- filtered_merge_oi |>
    distinct(VesselId, longitude, latitude, tmdiff_hr, vessel_doc_tripno) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  closure_areas_sf_by_lease <- st_read(closure_areas_file, quiet = TRUE) |>
    st_union() |>
    st_cast("POLYGON") |>
    st_sf() |>
    mutate(
      lease_area = c("morrobay", "humboldt")
    )

  int <- st_intersection(vms_points, closure_areas_sf_by_lease) |>
    suppressWarnings() |>
    suppressMessages() |>
    st_drop_geometry()

  vessel_lease_fishing_morrobay <- int |>
    filter(lease_area == "morrobay") |>
    distinct(VesselId) |>
    pull(VesselId)

  vessel_lease_fishing_humboldt <- int |>
    filter(lease_area == "humboldt") |>
    distinct(VesselId) |>
    pull(VesselId)

  # Display FE associated to those vessels
  fe_vessel_lease_fishing_morrobay <- vms_cumtime_by_vessel |>
    filter(vessel_id %in% vessel_lease_fishing_morrobay) |>
    group_by(Index, geometry) |>
    summarise(
      n_ship = sum(n_ship, na.rm = TRUE),
      fe_len = sum(fe_len, na.rm = TRUE),
      fe_cumT = sum(fe_cumT, na.rm = TRUE)
    ) |>
    ungroup() |>
    filter(fe_len != 0, fe_cumT != 0) |>
    mutate(lease_area = "morrobay")

  fe_vessel_lease_fishing_humboldt <- vms_cumtime_by_vessel |>
    filter(vessel_id %in% vessel_lease_fishing_humboldt) |>
    group_by(Index, geometry) |>
    summarise(
      n_ship = sum(n_ship, na.rm = TRUE),
      fe_len = sum(fe_len, na.rm = TRUE),
      fe_cumT = sum(fe_cumT, na.rm = TRUE)
    ) |>
    ungroup() |>
    filter(fe_len != 0, fe_cumT != 0) |>
    mutate(lease_area = "humboldt")

  fe_vessel_lease_fishing <- bind_rows(
    fe_vessel_lease_fishing_morrobay,
    fe_vessel_lease_fishing_humboldt
  )

  # Apply the cut function with custom labels
  fe_vessel_lease_fishing$fe_cat <- cut(
    fe_vessel_lease_fishing$fe_cumT,
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

  facet_labels <- c(
    "sim" = "Simulation",
    "obs" = "Observed",
    "diff" = "Difference"
  )

  if (rule_3) {
    mask_3rule_geom <- mask_3rule |> pull(geometry)

    fe_vessel_lease_fishing <- fe_vessel_lease_fishing |>
      filter(geometry %in% mask_3rule_geom)
  }

  # Subset original
  fe_main_subset <- fe_vessel_lease_fishing |>
    dplyr::select(geometry, fe_cat, lease_area) |>
    mutate(lease_area = factor(lease_area, levels = c("humboldt", "morrobay")))

  lease_labels <- c(
    morrobay = "Morro Bay",
    humboldt = "Humboldt"
  )

  # Plot
  vessels_fishign_leases <- ggplot(fe_main_subset) +
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
    facet_wrap(~lease_area, labeller = labeller(lease_area = lease_labels)) +

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
    fe_displace_plots = final_plot,
    fe_displace_zoomed_plot = zoomed_plot_list,
    obs_fe_sf = ca_fe_grouped_to_plot,
    sim_fe_sf = fe_displace_to_plot,
    vessels_fishign_leases = vessels_fishign_leases
  ))
}

assess_fe_within_closures <- function(
  baseline_fe_sim_all,
  filtered_vmstix_data,
  closure_areas_file
) {
  closure_areas_sf <- st_read(closure_areas_file, quiet = TRUE) |>
    mutate(
      lease_area = if_else(
        PROTRACT_1 == "Crescent City",
        "Humboldt",
        "Morro Bay"
      )
    ) |>
    group_by(lease_area) |>
    summarise(.groups = "drop")

  # Observed FE
  filtered_merge_oi <- filtered_vmstix_data$filtered_matched_vmstix |>
    filter(fishingornot == "fish")

  vms_points <- filtered_merge_oi |>
    distinct(docnum, longitude, latitude, tmdiff_hr, vessel_doc_tripno) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  int <- st_intersection(vms_points, closure_areas_sf) |>
    suppressWarnings() |>
    suppressMessages() |>
    st_drop_geometry()

  lease_fe_obs <- int |>
    group_by(lease_area) %>%
    summarise(fe_cumT = sum(tmdiff_hr)) %>%
    ungroup() %>%
    as.data.frame() %>%
    dplyr::select(lease_area, obs_fe = fe_cumT)

  # DISPLACE simulation FE

  # Convert to sf with point geometry per simulation
  fe_displace_baseline <- baseline_fe_sim_all |>
    group_by(simulation) |>
    filter(tstep == max(tstep)) |>
    mutate(total_fe = sum(fe)) |>
    ungroup() |>
    st_as_sf(coords = c("long", "lat"), crs = 4326)

  # Intersect each simulation's points with lease areas
  lease_fe_by_sim <- st_intersection(fe_displace_baseline, closure_areas_sf) |>
    group_by(simulation, lease_area) |>
    summarise(fe_leases = sum(fe, na.rm = TRUE), .groups = "drop")

  # Join total_fe back using simulation ID
  total_fe_df <- fe_displace_baseline |>
    st_drop_geometry() |>
    distinct(simulation, total_fe)

  lease_fe_by_sim <- lease_fe_by_sim |>
    left_join(total_fe_df, by = "simulation")

  # Add proportion column
  lease_fe_stats <- lease_fe_by_sim |>
    st_drop_geometry() |>
    mutate(fe_prop = fe_leases / total_fe)

  # Calculate mean and 95% CI for each lease area
  lease_fe_summary <- lease_fe_stats |>
    group_by(lease_area) |>
    summarise(
      mean_fe_abs = mean(fe_leases),
      sd_fe_abs = sd(fe_leases),
      n = n(),
      ci_low_fe_abs = mean_fe_abs - 1.96 * sd_fe_abs / sqrt(n),
      ci_high_fe_abs = mean_fe_abs + 1.96 * sd_fe_abs / sqrt(n),
      ci_halfwidth_fe_abs = (ci_high_fe_abs - ci_low_fe_abs) / 2,

      mean_total_fe_abs = mean(total_fe),
      sd_total_fe_abs = sd(total_fe),
      ci_low_total_fe_abs = mean_total_fe_abs -
        1.96 * sd_total_fe_abs / sqrt(n),
      ci_high_total_fe_abs = mean_total_fe_abs +
        1.96 * sd_total_fe_abs / sqrt(n),
      ci_halfwidth_total_fe_abs = (ci_high_total_fe_abs - ci_low_total_fe_abs) /
        2,

      mean_fe_pct = mean(fe_prop) * 100,
      sd_fe_pct = sd(fe_prop) * 100,
      ci_low_fe_pct = mean_fe_pct - 1.96 * sd_fe_pct / sqrt(n),
      ci_high_fe_pct = mean_fe_pct + 1.96 * sd_fe_pct / sqrt(n),
      ci_halfwidth_fe_pct = (ci_high_fe_pct - ci_low_fe_pct) / 2,

      .groups = "drop"
    ) |>
    dplyr::select(
      lease_area,
      mean_fe_abs,
      ci_halfwidth_fe_abs,
      mean_fe_pct,
      ci_halfwidth_fe_pct
    ) |>
    left_join(lease_fe_obs, by = "lease_area") |>
    mutate(
      percent_diff = round(((mean_fe_abs - obs_fe) / obs_fe) * 100, 1),
      percent_diff_ci = round((ci_halfwidth_fe_abs / obs_fe) * 100, 1)
    )

  # Format final table
  obs_total_cumhours <- filtered_merge_oi |>
    pull(tmdiff_hr) |>
    sum()

  lease_fe_pct_df <- lease_fe_summary %>%
    mutate(
      percent_diff = paste0(percent_diff, "±", percent_diff_ci, "%"),
      fe_cumT_sim = paste0(
        mean_fe_abs %>% round(0),
        "±",
        ci_halfwidth_fe_abs %>% round(0),
        "h (",
        mean_fe_pct %>% round(2),
        "±",
        ci_halfwidth_fe_pct %>% round(2),
        "%)"
      ),
      fe_cumT_obs = paste0(
        obs_fe %>% round(0),
        "h (",
        round(obs_fe / obs_total_cumhours * 100, 2),
        "%)"
      )
    ) |>
    dplyr::select(lease_area, fe_cumT_obs, fe_cumT_sim, percent_diff)

  lease_fe_pct_kable_df <- lease_fe_pct_df %>%
    kable(
      format = "html",
      col.names = c("Area", "Observed", "Simulated", "% Diff."),
      digits = 0,
      align = c("l", "r", "r", "r")
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE
    )

  return(list(
    lease_fe_pct_df = lease_fe_pct_df,
    lease_fe_pct_kable_df = lease_fe_pct_kable_df
  ))
}


generate_vessels_level_fit_plots_report_figures <- function(
  loglike_dis_baseline_output,
  filtered_matched_vmstix,
  filtered_tix_data_from_merge,
  vessels_id_mapping
) {
  # Fishing effort fit:
  loglike <- loglike_dis_baseline_output

  sim_effort_vessel <- loglike |>
    group_by(VE_REF, numTrips, simulation) |>
    summarise(
      timeatsea = sum(timeatsea),
      steamtime = sum(cumsteaming),
      fishingtime = timeatsea - steamtime,
    ) |>
    ungroup() |>
    group_by(VE_REF, numTrips) |>
    summarise(fishingtime = mean(fishingtime)) |>
    ungroup() |>
    group_by(VE_REF) |>
    summarise(fishingtime = sum(fishingtime)) |>
    ungroup() |>
    rename(VesselId = VE_REF)

  obs_effort_vessel <- filtered_matched_vmstix |>
    filter(vesselid %in% vessels_id_mapping$vesselid) |>
    filter(fishingornot == "fish") |>
    distinct(
      vesselid,
      docnum,
      landingreceiptnum,
      VMS_RECNO,
      date.time,
      tmdiff_hr
    ) |>
    group_by(vesselid, docnum) |>
    summarise(fishingtime = sum(tmdiff_hr)) |>
    ungroup() |>
    left_join(vessels_id_mapping, by = c("vesselid")) |>
    dplyr::select(VesselId, fishingtime)

  combined_data <- sim_effort_vessel |>
    left_join(obs_effort_vessel, by = c("VesselId"), suffix = c("_sim", "_obs"))

  # Calculate R^2
  lm_model <- lm(fishingtime_sim ~ fishingtime_obs, data = combined_data)
  r_squared <- summary(lm_model)$r.squared

  # Determine axis limits (minimum and maximum values for both variables)
  axis_limits <- range(
    combined_data$fishingtime_obs,
    combined_data$fishingtime_sim,
    na.rm = TRUE
  )
  axis_limits[1] <- max(axis_limits[1], 0.01) # Ensure minimum is positive
  log_breaks <- get_log_breaks(axis_limits)

  vessel_fe_plot_rule3 <- ggplot(
    combined_data,
    aes(x = fishingtime_obs, y = fishingtime_sim)
  ) +
    geom_bin2d(
      bins = 15,
      aes(
        fill = after_stat(count),
        alpha = after_stat(ifelse(count < 3, 0, 1)) # Full transparency for counts < 3
      )
    ) +
    scale_fill_distiller(
      palette = "Blues", # Brewer Blues color scale
      trans = "reverse" # Invert the color scale
    ) +
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
    coord_fixed(ratio = 1, clip = "on") + # Ensure 1:1 ratio (perfect square)
    labs(
      x = "Observed fishing effort (h)",
      y = "Simulated fishing effort (h)"
    ) +
    annotate(
      "text",
      x = axis_limits[1], # Position annotation horizontally
      y = axis_limits[2], # Position annotation vertically
      label = paste0("R² = ", round(r_squared, 3)), # Display rounded R² value
      hjust = 0,
      size = 4
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), # Remove major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      axis.line = element_line(color = "black"), # Add contour (axis lines)
      axis.ticks = element_line(color = "black"), # Add axis ticks
      axis.ticks.length = unit(0.25, "cm"), # Adjust tick length
      legend.position = "none", # Remove legend
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7), # Rotate x-axis tick labels
      aspect.ratio = 1 # Enforce square aspect in layout regardless of device
    )

  vessel_fe_plot <- ggplot(
    combined_data,
    aes(x = fishingtime_obs, y = fishingtime_sim)
  ) +
    geom_point(size = 3, alpha = 0.6, color = "dodgerblue3", stroke = 0) +
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
    coord_fixed(ratio = 1, clip = "on") + # Ensure 1:1 ratio (perfect square)
    labs(
      x = "Observed fishing effort (h)",
      y = "Simulated fishing effort (h)"
    ) +
    annotate(
      "text",
      x = axis_limits[1], # Position annotation horizontally
      y = axis_limits[2], # Position annotation vertically
      label = paste0("R² = ", round(r_squared, 3)), # Display rounded R² value
      hjust = 0,
      size = 4
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), # Remove major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      axis.line = element_line(color = "black"), # Add contour (axis lines)
      axis.ticks = element_line(color = "black"), # Add axis ticks
      axis.ticks.length = unit(0.25, "cm"), # Adjust tick length
      legend.position = "none", # Remove legend
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7), # Rotate x-axis tick labels
      aspect.ratio = 1 # Enforce square aspect in layout regardless of device
    )

  # Landings:
  sim_landings_vessel <- loglike |>
    mutate(total_catch = rowSums(across(starts_with("pop")), na.rm = TRUE)) |>
    group_by(VE_REF, numTrips, simulation) |>
    summarise(catch = sum(total_catch, na.rm = TRUE)) |>
    ungroup() |>
    group_by(VE_REF, numTrips) |>
    summarise(catch = mean(catch)) |>
    ungroup() |>
    group_by(VE_REF) |>
    summarise(catch = sum(catch)) |>
    ungroup() |>
    rename(VesselId = VE_REF)

  obs_landings_vessel <- filtered_tix_data_from_merge %>%
    filter(vesselid %in% vessels_id_mapping$vesselid) |>
    mutate(kg = lbs * 0.453592) %>%
    group_by(vesselid, docnum) %>%
    summarise(catch = sum(kg)) %>%
    ungroup() %>%
    left_join(vessels_id_mapping, by = c("vesselid", "docnum")) %>%
    dplyr::select(VesselId, catch)

  combined_data <- sim_landings_vessel %>%
    left_join(
      obs_landings_vessel,
      by = c("VesselId"),
      suffix = c("_sim", "_obs")
    ) %>%
    mutate(tons_sim = catch_sim / 1000, tons_obs = catch_obs / 1000)

  # Calculate R^2
  lm_model <- lm(tons_sim ~ tons_obs, data = combined_data)
  r_squared <- summary(lm_model)$r.squared

  # Determine axis limits (minimum and maximum values for both variables)
  axis_limits <- range(
    combined_data$tons_obs,
    combined_data$tons_sim,
    na.rm = TRUE
  )
  axis_limits[1] <- max(axis_limits[1], 0.01) # Ensure minimum is positive
  log_breaks <- get_log_breaks(axis_limits)

  vessel_landings_plot_rule3 <- ggplot(
    combined_data,
    aes(x = tons_obs, y = tons_sim)
  ) +
    geom_bin2d(
      bins = 20,
      aes(
        fill = after_stat(count),
        alpha = after_stat(ifelse(count < 3, 0, 1)) # Full transparency for counts < 3
      )
    ) +
    scale_fill_distiller(
      palette = "Blues", # Brewer Blues color scale
      trans = "reverse" # Invert the color scale
    ) +
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
    coord_fixed(ratio = 1, clip = "on") + # Ensure 1:1 ratio (perfect square)
    labs(
      x = "Observed landings (mt)",
      y = "Simulated landings (mt)"
    ) +
    annotate(
      "text",
      x = axis_limits[1], # Position annotation horizontally
      y = axis_limits[2], # Position annotation vertically
      label = paste0("R² = ", round(r_squared, 3)), # Display rounded R² value
      hjust = 0,
      size = 4
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), # Remove major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      axis.line = element_line(color = "black"), # Add contour (axis lines)
      axis.ticks = element_line(color = "black"), # Add axis ticks
      axis.ticks.length = unit(0.25, "cm"), # Adjust tick length
      legend.position = "none", # Remove legend
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7), # Rotate x-axis tick labels
      aspect.ratio = 1 # Enforce square aspect in layout regardless of device
    )

  vessel_landings_plot <- ggplot(
    combined_data,
    aes(x = tons_obs, y = tons_sim)
  ) +
    geom_point(size = 3, alpha = 0.6, color = "dodgerblue3", stroke = 0) +
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
    coord_fixed(ratio = 1, clip = "on") + # Ensure 1:1 ratio (perfect square)
    labs(
      x = "Observed landings (mt)",
      y = "Simulated landings (mt)"
    ) +
    annotate(
      "text",
      x = axis_limits[1], # Position annotation horizontally
      y = axis_limits[2], # Position annotation vertically
      label = paste0("R² = ", round(r_squared, 3)), # Display rounded R² value
      hjust = 0,
      size = 4
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), # Remove major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      axis.line = element_line(color = "black"), # Add contour (axis lines)
      axis.ticks = element_line(color = "black"), # Add axis ticks
      axis.ticks.length = unit(0.25, "cm"), # Adjust tick length
      legend.position = "none", # Remove legend
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7), # Rotate x-axis tick labels
      aspect.ratio = 1 # Enforce square aspect in layout regardless of device
    )

  # Create the combined plot with labels a) and b)
  profitability_plots <- plot_grid(
    vessel_fe_plot_rule3,
    vessel_landings_plot_rule3,
    labels = c("a)", "b)"),
    label_size = 12,
    ncol = 2,
    align = "hv"
  )

  caption <- ggdraw() +
    theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 20))

  # Final plot with caption
  final_plot <- plot_grid(
    profitability_plots,
    caption,
    ncol = 1,
    rel_heights = c(1, 0.18)
  )

  return(list(
    vessel_fe_landing_plots = final_plot,
    vessel_fe_plot_rule3 = vessel_fe_plot_rule3,
    vessel_fe_plot = vessel_fe_plot,
    vessel_landings_plot_rule3 = vessel_landings_plot_rule3,
    vessel_landings_plot = vessel_landings_plot
  ))
}


generate_overall_landings_fit_plot_report_figures <- function(
  loglike_dis_baseline_output,
  filtered_tix_data_from_merge,
  explicit_sp = c('sablefish', 'dover', 'thornyhead')
) {
  loglike <- loglike_dis_baseline_output

  pattern_sp <- paste(explicit_sp, collapse = "|")

  sim_landings <- loglike |>
    mutate(year_code = floor(tstep_arr / 8762) + 1) |>
    group_by(year_code, simulation) |>
    summarise(
      "sablefish" = sum(pop.0),
      "sole  dover" = sum(pop.1),
      "thornyhead  longspine" = sum(pop.2),
      "thornyhead  shortspine" = sum(pop.3),
      "sole  petrale" = sum(pop.4),
      "rockfish  chilipepper" = sum(pop.5),
      "other" = sum(pop.6)
    ) |>
    ungroup() |>
    pivot_longer(
      cols = -c(year_code, simulation), # Exclude year_code from pivoting
      names_to = "species", # Create a new column for species
      values_to = "kg" # Rename the values column to "kg"
    ) |>
    mutate(
      sp_type = ifelse(
        grepl(pattern_sp, species, ignore.case = TRUE),
        "explicit_sp",
        "implicit_sp"
      )
    )

  tix_merged_data <- filtered_tix_data_from_merge |>
    mutate(
      newdate = as.Date(newdate),
      year = format(newdate, "%Y"),
      kg = lbs * 0.453592,
      species = tolower(species)
    )

  obs_landings <- tix_merged_data |>
    mutate(
      year = as.numeric(year), # Ensure year is numeric
      year_code = year - min(year) + 1 # Recode to start at 1
    ) |>
    group_by(year_code, species) |>
    summarise(kg = sum(kg)) |>
    ungroup() |>
    mutate(
      sp_type = ifelse(
        grepl(pattern_sp, species, ignore.case = TRUE),
        "explicit_sp",
        "implicit_sp"
      )
    )

  # Total landings

  obs_landings <- obs_landings |>
    group_by(species, sp_type) |>
    summarise(kg = sum(kg)) |>
    ungroup()

  sim_landings <- sim_landings |>
    group_by(species, sp_type, simulation) |>
    summarise(kg = sum(kg)) |>
    ungroup()

  combined_data <- sim_landings |>
    left_join(
      obs_landings,
      by = c(
        "species"
      ),
      suffix = c("_sim", "_obs")
    ) |>
    mutate(tons_sim = kg_sim / 1000, tons_obs = kg_obs / 1000)

  # combined_data |> pull(kg_sim) |> sum()

  # Determine axis limits (minimum and maximum values for both variables)
  axis_limits <- range(
    c(combined_data$tons_obs, combined_data$tons_sim),
    na.rm = TRUE
  )

  # Calculate R^2
  lm_model <- lm(tons_sim ~ tons_obs, data = combined_data)
  r_squared <- summary(lm_model)$r.squared

  # Determine axis limits (minimum and maximum values for both variables)
  axis_limits <- range(
    combined_data$tons_obs,
    combined_data$tons_sim,
    na.rm = TRUE
  ) |>
    round()
  axis_limits[1] <- max(axis_limits[1], 0.01) # Ensure minimum is positive
  log_breaks <- get_log_breaks(axis_limits)

  round_up_nice <- function(x) {
    base <- 10^floor(log10(x)) # find the base order of magnitude
    ceiling(x / base) * base # round up to next "nice" multiple
  }

  round_down_nice <- function(x) {
    base <- 10^floor(log10(x)) # find the base order of magnitude
    floor(x / base) * base # round up to next "nice" multiple
  }

  summary_data <- combined_data |>
    group_by(species, tons_obs, sp_type_obs) |>
    summarise(
      median_sim = median(tons_sim, na.rm = TRUE),
      p5 = quantile(tons_sim, 0.05, na.rm = TRUE),
      p95 = quantile(tons_sim, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  species_labels <- c(
    "sablefish" = "Sablefish",
    "sole  dover" = "Dover sole",
    "thornyhead  longspine" = "Longspine thornyhead",
    "thornyhead  shortspine" = "Shortspine thornyhead",
    "sole  petrale" = "Petrale sole",
    "rockfish  chilipepper" = "Chilipepper rockfish",
    "other" = "Other"
  )

  custom_order <- c(
    "Sablefish",
    "Dover sole",
    "Longspine thornyhead",
    "Shortspine thornyhead",
    "Petrale sole",
    "Chilipepper rockfish",
    "Other"
  )

  summary_data <- summary_data |>
    mutate(
      species = recode(species, !!!species_labels),
      species = factor(species, levels = custom_order)
    ) |>
    arrange(species)

  # Plot
  overall_landings_fit_plot <- ggplot(
    summary_data,
    aes(x = tons_obs, y = median_sim)
  ) +
    geom_point(aes(color = species, shape = sp_type_obs), size = 3) +
    geom_errorbar(aes(ymin = p5, ymax = p95), width = 0, color = "grey50") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    scale_shape_manual(
      values = c("explicit_sp" = 16, "implicit_sp" = 17),
      labels = c("Explicit species", "Implicit species"),
      name = "Species type"
    ) +
    geom_smooth(
      method = "lm",
      linewidth = 0.5,
      color = "grey",
      linetype = "solid",
      se = FALSE,
      fullrange = TRUE
    ) +
    scale_x_log10(
      limits = c(
        axis_limits[1] |> round_down_nice(),
        axis_limits[2] |> round_up_nice()
      ),
      breaks = log_breaks,
      labels = log_breaks
    ) +
    scale_y_log10(
      limits = c(
        axis_limits[1] |> round_down_nice(),
        axis_limits[2] |> round_up_nice()
      ),
      breaks = log_breaks,
      labels = log_breaks
    ) +
    annotate(
      "text",
      x = axis_limits[1] |> round_down_nice(),
      y = axis_limits[2] |> round_up_nice(),
      label = paste0("R² = ", round(r_squared, 3)), # Display rounded R² value
      hjust = 0,
      size = 4
    ) +
    coord_fixed() +
    labs(
      x = "Observed landings (mt)",
      y = "Simulated landings (mt)",
      color = ""
    ) +
    scale_shape_manual(
      values = c("explicit_sp" = 16, "implicit_sp" = 17),
      guide = "none"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.25, "cm"),
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
      plot.caption.position = "panel", # <-- key: span across entire plotting area
      plot.caption = element_textbox_simple(
        hjust = 0.2,
        vjust = 1.5,
        size = 10,
        width = unit(1.5, "npc"), # <-- ensures it fills the entire width
        margin = margin(b = 10, t = 5)
      ),
      plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
    )

  return(overall_landings_fit_plot)
}

# Populations validation ----

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
