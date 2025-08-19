source("r/config.R")


## Prepare DISPLACE R routine setup ----
generate_general_list <- function(
  application_name,
  igraph,
  project_directory,
  displace_raw_inputs_path,
  displace_processed_inputs_path
) {
  general <- list()
  general$application <- application_name

  general$main.path.param.gis <- here::here(
    displace_raw_inputs_path
  )
  general$main_path_gis <- here::here(
    displace_raw_inputs_path
  )
  general$main.path.ibm <- here::here(
    displace_processed_inputs_path
  )
  general$main_path_R_inputs <- here::here(
    project_directory,
    "routines"
  )
  general$igraph <- igraph

  return(general)
}

cat(paste("START \n"))
cat("Generate directories and features list \n")

general_directories <- generate_general_list(
  application_name = "california_example",
  igraph = 1,
  project_directory = here::here(),
  displace_raw_inputs_path = here::here("raw_inputs"),
  displace_processed_inputs_path = here::here("processed_inputs")
)

do_plot <- FALSE

# Assign general and do_plot to global variables
assign("general", general_directories, envir = .GlobalEnv)
assign("do_plot", do_plot, envir = .GlobalEnv)

main_path_R_inputs <- general$main_path_R_inputs


# Generate POPULATIONS files ----
cat("Generate POPULATIONS files \n")

# Remove folders that the R routines will regenerate
file.path(general$main_path_gis, "POPULATIONS", "pops_config_files") |>
  unlink(recursive = TRUE)
file.path(general$main.path.ibm, paste0("popsspe_", general$application)) |>
  unlink(recursive = TRUE)

# Scripts to run from R routines
cat("Running 01_GeneratePopulationsFeatures.R")
source(
  here::here(
    main_path_R_inputs,
    "01_GeneratePopulationsFeatures.R" # Same as before
  )
)
cat("Running 02_GeneratePopulationsConfigFiles.R")
source(
  here::here(
    main_path_R_inputs,
    "02_GeneratePopulationsConfigFiles.R" # Same as before
  )
)
cat("Running 03_RunPopulationsConfigFiles.R")
source(
  here::here(
    main_path_R_inputs,
    "03_RunPopulationsConfigFiles.R" # updated coord and graph methods to create objects (but contents are the same) updated terra
  )
)
cat("Running 04_GeneratePopulationsPreferencesPerSpeciesMatrix.R")
source(
  here::here(
    main_path_R_inputs,
    "04_GeneratePopulationsPreferencesPerSpeciesMatrix.R"
  )
)
cat("Running 05_GeneratePopulationsVariousFiles.R")
source(
  here::here(
    main_path_R_inputs,
    "05_GeneratePopulationsVariousFiles.R"
  )
)


# Generate FLEET files ----

# Remove folders that the R routines will regenerate
file.path(general$main_path_gis, "FISHERIES", "vessels_config_files") %>%
  unlink(recursive = TRUE)


file.path(
  general$main.path.ibm,
  paste0("vesselsspe_", general$application)
) %>%
  unlink(recursive = TRUE)

file.path(
  general$main.path.ibm,
  paste0("metiersspe_", general$application)
) %>%
  unlink(recursive = TRUE)

# Scripts to run
source(
  here::here(
    main_path_R_inputs,
    "06_GenerateVesselsConfigFiles.R"
  )
)
source(
  here::here(
    main_path_R_inputs,
    "07_RunVesselsConfigFiles.R"
  )
)
source(
  here::here(
    main_path_R_inputs,
    "08_GenerateVesselsEconomicsFile.R"
  )
)
source(
  here::here(
    main_path_R_inputs,
    "09_GenerateOtherCatchesOnNodes.R"
  )
)
source(
  here::here(
    main_path_R_inputs,
    "10_GenerateMetiersSelectivityPerStockFiles.R"
  )
)
source(
  here::here(
    main_path_R_inputs,
    "11_GenerateMetiersVariousFiles.R"
  )
)

file.path(general$main_path_gis, "FISHERIES", "temp_spatial_layers") %>%
  unlink(recursive = TRUE)
file.path(
  general$main_path_gis,
  "FISHERIES",
  "SpatialLayers_combined.RData"
) %>%
  unlink(recursive = TRUE)
file.path(general$main_path_gis, "FISHERIES", "vessels_config_files") %>%
  unlink(recursive = TRUE)
file.path(general$main_path_gis, "POPULATIONS", "pops_config_files") |>
  unlink(recursive = TRUE)

# Generate ADDITIONAL files ----

# HARBOURS
source(here(main_path_R_inputs, "12_GenerateHarboursFiles.R"))
# SHIPPING. We are not using this feature, but we need to create a mock file
source(here(main_path_R_inputs, "13_GenerateShippingFiles.R"))
# FISHFARMS. We are not using this feature, but we need to create a mock file
source(here(main_path_R_inputs, "14_GenerateFishfarmsFiles.R"))
# WINDMILLS. We are not using this feature, but we need to create a mock file
source(here(main_path_R_inputs, "15_GenerateWindmillsFiles.R"))
# FIRMS. We are not using this feature, but we need to create a mock file
source(here(main_path_R_inputs, "16_GenerateFirmsFiles.R"))
# SIMULATION CONFIGURATION
source(here(main_path_R_inputs, "17_GenerateSimulationsConfigFiles.R"))
# BENTHOS
source(here(main_path_R_inputs, "18_GenerateBenthosLandscapeOnNodes.R")) # Requires files from GenerateSimulationsConfigFiles.R

# Clean up global variables
rm(general, envir = .GlobalEnv)
rm(do_plot, envir = .GlobalEnv)


# Updating processed input files ----
# Below, we update some of the generated input files with specifics from our analysis.

# Update config.dat file
update_config_file <- function(
  general_directories,
  implicit_sp_code = c("EOJ", "SGO", "OTH"),
  ...
) {
  main_path_gis <- general_directories$main_path_gis
  main_path_R_inputs <- general_directories$main.path.ibm
  app <- general_directories$application

  lines <- readLines(glue::glue(
    "{main_path_R_inputs}/simusspe_{app}/config.dat"
  ))
  imp_spp <- read_table(glue::glue(
    "{main_path_gis}/POPULATIONS/pop_names_{app}.txt"
  )) |>
    filter(spp %in% implicit_sp_code) |>
    pull(idx)

  implicit_lines <- grep("^# implicit stocks", lines)

  # Insert values after each matching line
  for (i in implicit_lines) {
    if (i < length(lines)) {
      lines[i + 1] <- paste(imp_spp, collapse = " ")
    }
  }

  writeLines(
    lines,
    glue::glue("{main_path_R_inputs}/simusspe_{app}/config.dat")
  )
}

# We update the config.dat file to indicate the implicit species
update_config_file(
  general_directories,
  implicit_sp_code = c("EOJ", "SGO", "OTH"),
  # Re-run if changes are applied in other_scripts
)


# Update baseline.dat file
update_scenario_file <- function(
  general_directories,
  scenarios = c("baseline", "closer_port"),
  ...
) {
  main_path_gis <- general_directories$main_path_gis
  main_path_R_inputs <- general_directories$main.path.ibm
  app <- general_directories$application

  lines <- readLines(glue::glue(
    "{main_path_R_inputs}/simusspe_{app}/baseline.dat"
  ))

  scenarios_line <- grep("^# dyn_alloc_sce", lines)
  lines[scenarios_line + 1] <- paste(scenarios, collapse = " ")

  writeLines(
    lines,
    glue::glue("{main_path_R_inputs}/simusspe_{app}/baseline.dat")
  )
}

# We update the baseline.dat file to include the "closer_port" scenario as part of our baseline scenario
update_scenario_file(
  general_directories,
  scenarios = c("baseline", "closer_port"),
  # Re-run if changes are applied in other_scripts
)

# Move Dtree to processed_inputs folder
source_dir <- glue::glue("{general_directories$main_path_gis}/FISHERIES/dtrees")
destination_dir <- glue::glue("{general_directories$main.path.ibm}/dtrees")
# Get all files in source_dir
files_to_copy <- list.files(source_dir, full.names = TRUE)
# Copy to destination with overwrite = TRUE
file.copy(from = files_to_copy, to = destination_dir, overwrite = TRUE)


# Generate DISPLACE run files ----
# (to run the model outside the GUI)
generate_dis_run_files <- function(
  general_directories,
  directory = "Users\\guill", # Define this based on the machine from which you run DISPLACE
  scenario = "baseline",
  sim_name = "simu",
  replicates = 1:10,
  years = 1,
  store_years = 1,
  num_threads = 4,
  ...
) {
  processed_data_dir <- general_directories$main.path.ibm
  app_name <- general_directories$application
  steps <- years * 8762

  # Generate the contents of the sch .dat file
  sch_baseline_contents <- c("echo Generated by Displace Scheduler Editor \n")
  for (i in replicates) {
    replicate_line <- paste0(
      'start /w C:\\Displace\\displace.exe ',
      '--outdir C: -a C:\\',
      directory,
      '\\Dropbox\\mpa-outcomes\\data\\confidential\\displace\\displace_inputs\\DISPLACE_processed_inputs\\DISPLACE_input_',
      app_name,
      ' ',
      '-f "',
      app_name,
      '" --f2 "',
      scenario,
      '" -s "',
      sim_name,
      i,
      '" -i ',
      steps,
      ' -p 0 -e ',
      store_years,
      ' --huge 1 -v 0 ',
      '--disable-sqlite --without-gnuplot -V 1 --num_threads ',
      num_threads,
      ' > ..\\',
      app_name,
      '-',
      sim_name,
      i,
      '.txt',
      '\n'
    )
    sch_baseline_contents <- c(sch_baseline_contents, replicate_line)
  }
  sch_baseline_contents <- c(sch_baseline_contents, "PAUSE")
  sch_file <- glue::glue(
    processed_data_dir,
    "/simusspe_",
    app_name,
    "/sch_",
    scenario,
    ".bat"
  )
  writeLines(sch_baseline_contents, sch_file)

  # Generate the contents of the .dsf file
  dsf_contents <- c()
  for (i in replicates) {
    replicate_line <- paste0(
      'C:,C:/',
      gsub("\\\\", "/", directory),
      '/Dropbox/mpa-outcomes/data/confidential/displace/displace_inputs/DISPLACE_processed_inputs/DISPLACE_input_',
      app_name,
      ',',
      app_name,
      ',',
      scenario,
      ',',
      sim_name,
      i,
      ',',
      steps,
      ',',
      num_threads
    )

    dsf_contents <- c(dsf_contents, replicate_line)
  }
  dsf_file <- glue::glue(
    processed_data_dir,
    "/simusspe_",
    app_name,
    "/",
    scenario,
    ".dsf"
  )
  writeLines(dsf_contents, dsf_file)

  return("Done")
}

# Prepare files to run a 14 year simulation with 1 replicate
generate_dis_run_files(
  general_directories,
  directory = "Users\\guill",
  scenario = "baseline",
  sim_name = "simu",
  replicates = 1, # For more replicates input sequence. e.g. 1:10
  years = 10,
  store_years = 1, # You can store vmslike data up to 10 years
  num_threads = 4 # Set based on your machine specs
  # Re-run if changes are applied in other_scripts
)
