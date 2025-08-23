cat(paste("START \n"))


a_size_group_bin_in_cm <- 0.5 # caution: hardcoding....
mid <- a_size_group_bin_in_cm / 2
cat(paste("Caution: Hardcoding for size bins....\n"))

# (caution: give the order for naming stocks in integer from 0 to n-1)
spp_table <- read.table(
  file = file.path(
    general$main_path_gis,
    "POPULATIONS",
    paste("pop_names_", general$application, ".txt", sep = '')
  ),
  header = TRUE
)
spp <- as.character(spp_table$spp)
cat(paste(
  "Reading the stock names in",
  paste(
    general$main_path_gis,
    "POPULATIONS",
    paste("pop_names_", general$application, ".txt", sep = '')
  ),
  "....done \n"
))


dir.create(file.path(
  general$main.path.ibm,
  paste("metiersspe_", general$application, sep = '')
))


options(scipen = 999)


# reuse the exported metier names in GenerateVesselConfigFiles.R
metier_names <- read.table(
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metier_names.dat"
  ),
  header = TRUE
)


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

create_metier_selectivity_files <- function(csv_selectivity_table, sce = 1) {
  for (met in unique(csv_selectivity_table[, 2])) {
    selectivities <- csv_selectivity_table[
      csv_selectivity_table[, 2] == met,
      -c(1:3)
    ]

    # save the .dat file per metier
    write.table(
      selectivities,
      file = file.path(
        general$main.path.ibm,
        paste("metiersspe_", general$application, sep = ''),
        paste(
          met,
          "metier_selectivity_per_stock_ogives_fleetsce",
          sce,
          ".dat",
          sep = ''
        )
      ),
      col.names = FALSE,
      row.names = FALSE,
      sep = ' ',
      quote = FALSE
    )
    cat(paste(
      "Write in metiersspe: ",
      met,
      "metier_selectivity_per_stock_ogives.dat\n",
      sep = ''
    ))
  }

  # selectivity for other_land. Borrow from relevant metiers...
  selectivities <- csv_selectivity_table[csv_selectivity_table[, 1] == "OTB", ] # default (will create an empty file. We dont need this feature, but displace requires the file even empty to run)
  selectivities <- selectivities[, -c(1:3)]

  # save the .dat file for OTH_LAND
  write.table(
    selectivities,
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      paste(
        "metier_selectivity_per_stock_ogives_fleetsce",
        sce,
        "_for_oth_land.dat",
        sep = ''
      )
    ),
    col.names = FALSE,
    row.names = FALSE,
    sep = ' ',
    quote = FALSE
  )
  cat(paste(
    "Write in metier_selectivity_per_stock_ogives_for_oth_land.dat\n",
    sep = ''
  ))

  return()
}


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

##-------------------
##---utils-----------
## a function to read the fleet scenario file before we expand it with a new field...
## this suppose to replicate all the existing fleet files...
add_a_new_scenario_type_to_fleet_scenarios <- function(
  a_new_field_defining_scenarios = "selectivity_L50_multiplier",
  values_for_this_scenario_type = c(0.75, 1, 1.5)
) {
  # if(all(values_for_this_scenario_type)!=1) stop("need at least the value '1' for this new scenario field")
  multiplier_for_fleetsce <- read.table(
    file = file.path(
      general$main.path.ibm,
      paste("multiplier_for_fleetsce", general$application, ".dat", sep = "")
    ),
    header = TRUE
  )
  new_sces <- expand.grid(
    sce = multiplier_for_fleetsce$sce,
    values_for_this_scenario_type
  )
  colnames(new_sces)[ncol(new_sces)] <- a_new_field_defining_scenarios
  multiplier_for_fleetsce <- merge(multiplier_for_fleetsce, new_sces)
  if (length(values_for_this_scenario_type) > 1) {
    nr <- nrow(multiplier_for_fleetsce[
      multiplier_for_fleetsce[, a_new_field_defining_scenarios] == 1,
    ]) # baseline sce for this new field
    nr2 <- nrow(multiplier_for_fleetsce[
      multiplier_for_fleetsce[, a_new_field_defining_scenarios] != 1,
    ]) # other sces

    multiplier_for_fleetsce$initial_sce <- multiplier_for_fleetsce$sce
    multiplier_for_fleetsce[
      multiplier_for_fleetsce[, a_new_field_defining_scenarios] != 1,
      "sce"
    ] <- (1:nr2) + nr
    library(doBy)
    multiplier_for_fleetsce <- orderBy(~sce, data = multiplier_for_fleetsce)

    # then replicate all the fleetsce files for this new numbering.
    all_fleet_files <- list.files(file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = '')
    ))
    for (sce in ((1:nr2) + nr)) {
      initial_sce_number_for_this_new_sce <- multiplier_for_fleetsce[
        multiplier_for_fleetsce$sce == sce,
        "initial_sce"
      ] # duplicate the ones correponding to the initial sce number
      all_filenames_to_replicates <- all_fleet_files[grep(
        paste("fleetsce", initial_sce_number_for_this_new_sce, sep = ''),
        all_fleet_files
      )]

      all_filenames_to_replicates_new_name <- gsub(
        paste("fleetsce", initial_sce_number_for_this_new_sce, sep = ''),
        paste("fleetsce", sce, sep = ""),
        all_filenames_to_replicates
      )
      for (i in 1:length(all_filenames_to_replicates)) {
        file.copy(
          from = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ""),
            all_filenames_to_replicates[i]
          ),
          to = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ""),
            all_filenames_to_replicates_new_name[i]
          )
        )
      }
    }

    multiplier_for_fleetsce <- multiplier_for_fleetsce[,
      -ncol(multiplier_for_fleetsce)
    ] # remove no longer useful initial sce field
  }
  return(multiplier_for_fleetsce)
}
#--------------------
#--------------------

# caution fleet sce
fleetsce <- data.frame(sce = 1, namesce = c('scebaseline'))

write.table(
  fleetsce,
  quote = FALSE,
  file = file.path(
    general$main.path.ibm,
    paste("multiplier_for_fleetsce", general$application, ".dat", sep = '')
  ),
  append = FALSE,
  row.names = FALSE,
  col.names = TRUE
)

# add a new field for some fleet scenarios:
# CAUTION: the 0spe_stecf_oth_land_per_month_per_node_semester1.dat types of file are in /POPSSPE !!
multiplier_for_fleetsce <- add_a_new_scenario_type_to_fleet_scenarios(
  a_new_field_defining_scenarios = "selectivity_L50_multiplier",
  values_for_this_scenario_type = c(0.9, 1)
)
write.table(
  multiplier_for_fleetsce,
  quote = FALSE,
  file = file.path(
    general$main.path.ibm,
    paste("multiplier_for_fleetsce", general$application, ".dat", sep = '')
  ),
  append = FALSE,
  row.names = FALSE,
  col.names = TRUE
)

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!CALLS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# save the .dat file all metiers for display in object editor

csv_selectivity_table <- read.table(
  file = file.path(
    general$main_path_gis,
    "FISHERIES",
    paste("fishing_gear_selectivity_ogives_per_stock.csv", sep = '')
  ),
  header = TRUE,
  sep = ','
)
cat(paste(
  "Use fishing_gear_selectivity_ogives_per_stock.csv to deduce metier selectivity files\n",
  sep = ''
))

create_metier_selectivity_files(csv_selectivity_table, sce = 1)


cat(paste("....done\n"))
