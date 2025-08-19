# some args for the bunch of vessels to be created....
# Usage:
# GenerateVesselsConfigFiles.R application gis_path input_application_path igraph

# GENERAL SETTINGS
#
#    args <- commandArgs(trailingOnly = TRUE)
#
#    general <- list()
#
#    if (length(args) < 2) {
#      if(.Platform$OS.type == "windows") {
#        general$application           <- "MEESO"
#        general$main_path_gis         <- file.path("D:","FBA", paste("DISPLACE_input_gis_", general$application, sep=""))
#        general$main.path.ibm         <- file.path("D:","FBA", paste("DISPLACE_input_", general$application, sep=''))
#        general$igraph                <- 206 # 110  # caution: should be consistent with existing objects already built upon a given graph
#       do_plot                        <- TRUE
#
#      } else{
#      if(Sys.info()["sysname"] == "Darwin") {
#        general$application           <- "MEESO"
#        general$main_path_gis         <- file.path("usr","local","GitHub",paste("DISPLACE_input_gis_", general$application, sep=""))
#        general$main.path.ibm         <- file.path("usr","local","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
#        general$igraph                <- 1  # caution: should be consistent with existing objects already built upon a given graph
#       do_plot                        <- TRUE
#
#      } else {
#        general$application           <- args[1]
#        general$main_path_gis         <- args[2]
#        general$main.path.ibm         <- args[3]
#        general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
#       do_plot                        <- FALSE
#   }}}
#   cat(paste("START \n"))

dir.create(
  path = file.path(general$main_path_gis, "FISHERIES", "vessels_config_files")
)
dir.create(path = file.path(general$main.path.ibm))
dir.create(
  path = file.path(
    general$main.path.ibm,
    paste("vesselsspe_", general$application, sep = '')
  )
)
dir.create(
  path = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = '')
  )
)


cat(paste(
  "Config files will be stored in /FISHERIES/vessels_config_files folder\n"
))


filename <- file.path(
  general$main_path_gis,
  "FISHERIES",
  "vessels_specifications_per_harbour_metiers.csv"
)
cnts <- count.fields(filename, sep = ";")
# vessel_specifications <- read.table(file=filename, sep=";", header=TRUE)
vessel_specifications <- read.csv2(
  file = filename,
  sep = ",",
  header = TRUE,
  dec = "."
)
vessel_specifications <- cbind.data.frame(
  vessel_specifications,
  id = 1:nrow(vessel_specifications)
)
#=> CAUTION: do not leave white rows at the end of this file! otherwise will create some extra wrong config_files
cat(paste("Read vessels_specifications_per_harbour_metiers.csv \n"))


# a quick important check on the metier names (should be consistent with the ones available in spatialLayers)
for (met in 1:length(vessel_specifications$Gear)) {
  if (
    length(
      grep(
        as.character(vessel_specifications$name_gis_file_for_fishing_effort_per_polygon[
          met
        ]),
        list.files(file.path(
          general$main_path_gis,
          "FISHERIES",
          "spatialLayers"
        ))
      )
    ) ==
      0
  ) {
    warning(paste(
      "a spatial layer for ",
      vessel_specifications$name_gis_file_for_fishing_effort_per_polygon[met],
      "is missing!! rename the metier in specs or better to get this layer"
    ))
  }
}


# caution: super-individuals to reduce the total nb of vessels to be simulated
# so no always 1 in vessel_specifications[, "N..of.vessels"]

port_names <- read.table(
  file = file.path(
    general$main_path_gis,
    "GRAPH",
    paste("harbours.dat", sep = '')
  ),
  sep = ";",
  header = TRUE
)
cat(paste("Read harbours.dat \n"))

# check consistencies with port names
declared_ports <- as.character(unique(vessel_specifications[, "Port"]))
check <- declared_ports[!declared_ports %in% unique(port_names$port_name)]
if (length(check) != 0) {
  warning(paste("inconsitent port names! "))
}


# order along vid
library(doBy)
vessel_specifications <- orderBy(~VesselId, data = vessel_specifications)


# export metier names
vessel_specifications$Gear <- factor(vessel_specifications$Gear)
metier_names <- cbind.data.frame(
  idx = 0:(length(levels(vessel_specifications$Gear)) - 1),
  name = levels(vessel_specifications$Gear)
)
write.table(
  metier_names,
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metier_names.dat"
  ),
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE
)
cat(paste(
  "Read metier names from the Vessels specs and write metier names in metiersspe_ folder \n"
))


# pre-computation for missing fuel related variables
# 1. i.e. fuel cons
# but first, fill in missing kW info (bc nor in vessel register) assuming DNK relationship hold true for all type of vessel...
filename <- file.path(
  general$main_path_gis,
  "FISHERIES",
  "DanishkWvsLOAModel.Rdata"
)
load(file = filename) # nls1
vessel_specifications[, "kW2"] <- round(predict(
  nls1,
  newdata = data.frame(
    VE_LEN = as.numeric(as.character(vessel_specifications$VE_LEN))
  )
)) # a guess on kW
vessel_specifications[
  !is.na(vessel_specifications[, "kW"]),
  "kW2"
] <- vessel_specifications[!is.na(vessel_specifications[, "kW"]), "kW"] # ...but make sure to use the existing info if kW not at NAs


table.fuelcons.per.engine <- read.table(
  file = file.path(
    general$main_path_gis,
    "FISHERIES",
    "IBM_datainput_engine_consumption.txt"
  ),
  header = TRUE,
  sep = ""
)
linear.model <- lm(
  calc_cons_L_per_hr_max_rpm ~ kW2,
  data = table.fuelcons.per.engine
) # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
vessel_specifications[, "fuel.cons.h"] <- predict(
  linear.model,
  newdata = data.frame(
    kW2 = as.numeric(as.character(vessel_specifications$kW2))
  )
) # Liter per hour

# 2. i.e. fuel tank capacity
load(
  file = file.path(
    general$main_path_gis,
    "FISHERIES",
    "DanishFuelTankCapModel.Rdata"
  )
)
vessel_specifications$VE_LEN <- vessel_specifications$VE_LEN
vessel_specifications$fuel.tank.liter <- round(predict(
  nls1,
  vessel_specifications
))


vessel_specifications$gear_effort <- vessel_specifications$RelativeEffort # we could do better if it was from national statistics...

vessel_specifications[
  is.na(vessel_specifications$multip.fuel.fishing),
  "multip.fuel.fishing"
] <- 0.0 # passive geAR

# downscale a bit the conso per h to avoid assuming engine at full speed all the time
towed_gears <- c(
  "OTB",
  "TBB",
  "SSC",
  "SDN_DEM",
  "OT_MIX_NEP",
  "OT_DMF_PEL",
  "OT_DMF",
  "OT_DEM_PEL",
  "OT_SPF",
  "DRB_MOL",
  "OT_CRU",
  "TBB_CRU",
  "SSC_DEM",
  "TBB_DMF",
  "DRB",
  "OTM",
  "PTM",
  "SDN",
  "PS",
  "DRH",
  "OTT"
)
vessel_specifications[
  vessel_specifications$Gear %in% towed_gears,
  "multip.fuel.fishing"
] <- 0.7
vessel_specifications[
  vessel_specifications$Gear %in% towed_gears,
  "multip.fuel.steaming"
] <- 0.8
vessel_specifications[
  vessel_specifications$Gear %in% towed_gears,
  "multip.fuel.ret.port.fish"
] <- 0.9

vessel_specifications <- vessel_specifications[
  !is.na(vessel_specifications$gear_effort),
] # debug


cat(paste("Now, read the specs table one by one \n"))
for (i in 1:nrow(vessel_specifications)) {
  cat(paste("line", i, " \n"))

  if (
    !any("VesselId" %in% colnames(vessel_specifications)) ||
      any("VesselId" %in% colnames(vessel_specifications)) &&
        i > 1 &&
        vessel_specifications[i, "VesselId"] !=
          vessel_specifications[i - 1, "VesselId"] ||
      any("VesselId" %in% colnames(vessel_specifications)) && i == 1
  ) {
    # test for truly individual vessel data i.e. one vessel => one config file

    spp_table <- read.table(
      file = file.path(
        general$main_path_gis,
        "POPULATIONS",
        paste("pop_names_", general$application, ".txt", sep = '')
      ),
      header = TRUE
    )
    spp <- as.character(spp_table$spp)

    do_append <- TRUE
    nbvids <- vessel_specifications[i, "nb_vessels"]

    vesselids <- as.character(vessel_specifications[i, "VesselId"]) # caution: three first letters (nationality) should be consistent with  popsspe/XXctrysspe_relative_stability_semesterXX

    if (any("VesselId" %in% colnames(vessel_specifications))) {
      all_records_this_vid <- vessel_specifications[
        vessel_specifications$VesselId == vessel_specifications[i, "VesselId"],
      ]
      all_records_this_vid[
        all_records_this_vid[, 'gear_effort'] == 0,
        "gear_effort"
      ] <- 0.001 # debug
      sum_per_met_this_vid <- tapply(
        all_records_this_vid$RelativeEffort,
        all_records_this_vid$Gear,
        sum,
        na.rm = TRUE
      )
      prop_per_met_this_vid <- sum_per_met_this_vid /
        sum(sum_per_met_this_vid, na.rm = TRUE)
      metierids <- which(prop_per_met_this_vid != 0) - 1 # OFF -1 IN c++
      # e.g. c(0,1) # look at /metiersspe.... 0:trawler; 1 gillnetter
      metierids_frequencies <- prop_per_met_this_vid[
        !is.na(prop_per_met_this_vid)
      ] #  c(1)  # pure fishery        # or e.g. c(0.2,0.8)
      # caution: likely to create 0 catch trips if the below is badly set (e.g. if range too large therefore not reachable by slow vessels, the smaller possibly also having daily trips so a short period to fish within the day):
      vessel_range_km <- tapply(
        all_records_this_vid$vessel_range_km,
        all_records_this_vid$Gear,
        mean,
        na.rm = TRUE
      ) # as many as Gear
      vessel_range_km <- vessel_range_km[!is.na(vessel_range_km)]
      #if(vessel_specifications[i, "VesselId"]=="DNK000007262") browser()
      imax <- which.max(all_records_this_vid$gear_effort) # assume the spatial distribution from the metier with the highest record in effort
      #name_gis_file_for_fishing_effort_per_polygon <- as.character(all_records_this_vid[imax, "name_gis_file_for_fishing_effort_per_polygon"])
      #name_gis_layer_field                         <- as.character(all_records_this_vid[imax, "name_gis_layer_field"])       # giving releative effort ditribtion e.g. in 5 categories: 1 to 5 with 1 high occurence
      names_gis_layers <- all_records_this_vid[
        !duplicated(data.frame(all_records_this_vid$Gear)),
      ]
      name_gis_file_for_fishing_effort_per_polygon <- as.character(names_gis_layers[,
        "name_gis_file_for_fishing_effort_per_polygon"
      ])
      name_gis_layer_field <- as.character(names_gis_layers[,
        "name_gis_layer_field"
      ]) # giving releative effort ditribtion e.g. in 5 categories: 1 to 5 with 1 high occurence
      is_gis_layer_field_relative_numbers <- all_records_this_vid[
        imax,
        "is_gis_layer_field_relative_numbers"
      ] # if relative effort categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
      xfold_gis_layer_field <- all_records_this_vid[
        imax,
        "xfold_gis_layer_field"
      ] # giving relative importance of the 5 categories e.g. visting an area of cat 1 is 10000 times more probable than for cat 5
    } else {
      metierids <- metier_names[
        metier_names$name == vessel_specifications[i, "LE_MET"],
        "idx"
      ] # adapt if necessary
      metierids_frequencies <- c(1) # pure fishery        # or e.g. c(0.2,0.8)
      name_gis_file_for_fishing_effort_per_polygon <- as.character(vessel_specifications[
        i,
        "name_gis_file_for_fishing_effort_per_polygon"
      ])
      name_gis_layer_field <- as.character(vessel_specifications[
        i,
        "name_gis_layer_field"
      ]) # giving releative effort ditribtion e.g. in 5 categories: 1 to 5 with 1 high occurence
      is_gis_layer_field_relative_numbers <- vessel_specifications[
        i,
        "is_gis_layer_field_relative_numbers"
      ] # if relative effort categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
      xfold_gis_layer_field <- vessel_specifications[i, "xfold_gis_layer_field"] # giving relative importance of the 5 categories e.g. visting an area of cat 1 is 10000 times more probable than for cat 5
    }

    if (any("VesselId" %in% colnames(vessel_specifications))) {
      all_records_this_vid <- vessel_specifications[
        vessel_specifications$VesselId == vessel_specifications[i, "VesselId"],
      ]
      sum_per_harbour_this_vid <- tapply(
        all_records_this_vid[, "gear_effort"],
        all_records_this_vid[, "Port"],
        sum,
        na.rm = TRUE
      )
      prop_per_harbour_this_vid <- sum_per_harbour_this_vid /
        sum(sum_per_harbour_this_vid, na.rm = TRUE)
      visited_ports <- names(prop_per_harbour_this_vid)[which(
        prop_per_harbour_this_vid != 0
      )]
      visited_ports_frequencies <- prop_per_harbour_this_vid[
        !is.na(prop_per_harbour_this_vid)
      ]
      harbcode <- paste(
        substr(visited_ports[which.max(visited_ports_frequencies)], 1, 5),
        vessel_specifications[i, "id"],
        sep = ""
      )
    } else {
      visited_ports <- vessel_specifications[i, "Harbor"] # e.g. c("ANCONA", "RIMINI") # should exist in harbour.dat!
      name_file_ports <- "harbours.dat"
      visited_ports_frequencies <- c(1) # e.g. c(0.8, 0.2)
      harbcode <- paste(
        substr(vessel_specifications[i, "Harbor"], 1, 4),
        vessel_specifications[i, "id"],
        sep = ""
      )
    }

    if (!all(visited_ports %in% port_names[, 1])) {
      stop("Inconsistencies in port names!")
    }
    name_file_ports <- "harbours.dat"

    if (!any("VesselId" %in% colnames(vessel_specifications))) {
      vesselids <- paste(
        "USA_",
        harbcode,
        "_",
        metierids,
        "_",
        1:nbvids,
        sep = ""
      ) # caution: three first letters (nationality) should be consistent with  popsspe/XXctrysspe_relative_stability_semesterXX
    }

    nb_stocks <- length(spp) # from 0 in c++
    if (any("VesselId" %in% colnames(vessel_specifications))) {
      all_records_this_vid <- vessel_specifications[
        vessel_specifications$VesselId == vessel_specifications[i, "VesselId"],
      ]

      # check consistency
      spp_to_find <- paste(substr(spp, 1, 3), "_kg_h", sep = '')
      not_found <- spp_to_find[!spp_to_find %in% colnames(all_records_this_vid)]

      #fixed_cpue_per_stock      <- apply(all_records_this_vid[, paste(substr(spp, 1,3), "_kg_h", sep='') ], 2, function(x, weight) mean(x*weight, na.rm=TRUE), weight=all_records_this_vid$gear_effort)  *vessel_specifications[i, "nb_vessels"]# kg per hour
      # substituted by:  remove the weighting which sounds silly
      fixed_cpue_per_stock <- apply(
        all_records_this_vid[, paste(substr(spp, 1, 3), "_kg_h", sep = '')],
        2,
        function(x, weight) mean(x * weight, na.rm = TRUE),
        weight = 1
      ) *
        vessel_specifications[i, "nb_vessels"] # kg per hour
      fixed_cpue_per_stock[is.na(fixed_cpue_per_stock)] <- 0.0
      #=> assuming the same catch rate for a species whatever the stock i.e. COD specific and not COD.nsea COD.2224 specifc etc.
    } else {
      fixed_cpue_per_stock <- vessel_specifications[
        i,
        paste(substr(spp, 1, 3), "_kg_h", sep = '')
      ] *
        vessel_specifications[i, "nb_vessels"] # kg per hour
    }
    gshape_cpue_per_stock <- rep(1, nb_stocks) # for Gamma on each node
    #gscale_cpue_per_stock      <-apply(all_records_this_vid[, paste(substr(spp, 1,3), "_kg_h", sep='') ], 2, function(x, weight) mean(x*weight, na.rm=TRUE), weight=all_records_this_vid$gear_effort)  *vessel_specifications[i, "nb_vessels"]# for Gamma on each node e.g. hist(rgamma(1000,shape=0.74,scale=1))
    # substituted by: remove the weighting which sounds silly
    gscale_cpue_per_stock <- apply(
      all_records_this_vid[, paste(substr(spp, 1, 3), "_kg_h", sep = '')],
      2,
      function(x, weight) mean(x * weight, na.rm = TRUE),
      weight = 1
    ) *
      vessel_specifications[i, "nb_vessels"] # for Gamma on each node e.g. hist(rgamma(1000,shape=0.74,scale=1))
    gscale_cpue_per_stock[is.na(gscale_cpue_per_stock)] <- 0.0

    vessel_specifications[i, "is_active"] <- 1
    vessel_specifications[i, "is_belong_to_ref_fleet"] <- 0
    vessel_specifications[i, "firm_id"] <- 1

    vessel_features <- c(
      vessel_specifications[i, "is_active"],
      vessel_specifications[i, "cruise.speed.knots"],
      vessel_specifications[i, "fuel.cons.h"] *
        vessel_specifications[i, "nb_vessels"],
      vessel_specifications[i, "VE_LEN"] *
        vessel_specifications[i, "nb_vessels"],
      vessel_specifications[i, "kW2"] * vessel_specifications[i, "nb_vessels"],
      vessel_specifications[i, "ave.storage.fish.kg"] *
        vessel_specifications[i, "nb_vessels"],
      vessel_specifications[i, "fuel.tank.liter"] *
        vessel_specifications[i, "nb_vessels"],
      vessel_specifications[i, "nb_pings_per_trip"], # nb_pings_per_trip
      0.4485, #vessel_specifications[i, "Gamma_shape"], # by default use: 0.4485, anyway not in use if GoFishing decision tree is active
      336.7618, #vessel_specifications[i, "Gamma_scale"], # by default use: 336.7618  anyway not in use if GoFishing decision tree is active
      vessel_specifications[i, "trip.duration.h"],
      vessel_specifications[i, "multip.fuel.steaming"],
      vessel_specifications[i, "multip.fuel.fishing"],
      vessel_specifications[i, "multip.fuel.ret.port.fish"],
      vessel_specifications[i, "multip.fuel.inactive"],
      vessel_specifications[i, "weekEndStartDay"],
      vessel_specifications[i, "WeekEndEndDay"],
      vessel_specifications[i, "WorkHoursStart"],
      vessel_specifications[i, "WorkHoursEnd"],
      vessel_specifications[i, "firm_id"],
      vessel_specifications[i, "is_belong_to_ref_fleet"]
    )
    if (length(vessel_features) != 21) {
      stop("Missing field(s) in the vessel specification input file!!!")
    }

    step_in_share <- rep(
      vessel_specifications[i, "nb_vessels"] /
        sum(vessel_specifications[, "nb_vessels"], na.rm = TRUE),
      nb_stocks
    )
    # i.e. 100 % of each TAC per stock will be booked for these new vessels
    # catch equation parameters: totcatch_inkgperhour_thisspecies = exp( vesselspe_sp + metierspe_sp + (avaispe_sp_szgr * avai_sp_szgr *1000 * gearsel_szgr ) )
    # by default, only the vessel effect is !=0....
    vesselsspe_betas <- log(
      fixed_cpue_per_stock * vessel_specifications[i, "nb_vessels"] + 0.01
    ) # catch rate log(kg per hour)  CAUTION: log(0)=-Inf !
    # ....but we could imagine informing metier effect here:
    metiersspe_betas <- matrix(
      rep(0, length(metierids)),
      nrow = length(spp),
      ncol = length(metierids)
    ) # 0 means not adding to the effect
    # (but metiers are having catch if >=0, so consider putting -20 if the metier is not catching the species)
    # ....and we could also imagine informing sizegroup avai effect here:
    avaisspe_betas <- matrix(
      round(seq(-0.01, 0.01, length = 14), 3),
      nrow = length(spp),
      ncol = 14,
      byrow = TRUE
    ) # 0 means not adding to the effect
    create_file_for_fuel_price_per_vessel_size <- TRUE # u15m, 15-18m 18-24m 24-40m
    some_fuel_price_per_vessel_size <- c(1.28, 1.28, 1.28, 1.28, 1.28) # dollar per litre (TODO: will need to be substituted with vessel specs ones in the c++ code)
    step_in_share_credits <- vessel_specifications[i, "nb_vessels"] /
      sum(vessel_specifications[, "nb_vessels"]) # i.e. % of the credits will be booked for these new vessels

    # create a (intermediate) config file
    if (any("VesselId" %in% colnames(vessel_specifications))) {
      namefile <- file.path(
        general$main_path_gis,
        "FISHERIES",
        "vessels_config_files",
        paste(
          "vessels_creator_args_",
          general$application,
          "_",
          as.character(vessel_specifications[i, "VesselId"]),
          "_",
          harbcode,
          "_",
          all_records_this_vid[
            imax,
            "name_gis_file_for_fishing_effort_per_polygon"
          ],
          ".dat",
          sep = ''
        )
      )
    } else {
      namefile <- file.path(
        general$main_path_gis,
        "FISHERIES",
        "vessels_config_files",
        paste(
          "vessels_creator_args_",
          general$application,
          "_",
          harbcode,
          "_",
          vessel_specifications[
            i,
            "name_gis_file_for_fishing_effort_per_polygon"
          ],
          ".dat",
          sep = ''
        )
      )
    }

    cat(paste("write the config file.. \n"))

    write(
      "# config file for the vessel editor: adding some vessel(s)",
      file = namefile
    )
    write(
      "# (the shortestPaths library will have to be re-created for the graph)",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write("# --------------", file = namefile, ncolumns = 1, append = TRUE)

    write(
      "# [input_folder_for_config_file]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(general$main_path_gis, file = namefile, ncolumns = 1, append = TRUE)

    write(
      "# [input_folder_for_DISPLACE]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(general$main.path.ibm, file = namefile, ncolumns = 1, append = TRUE)

    write(
      "# [name_of_the_application]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(general$application, file = namefile, ncolumns = 1, append = TRUE)

    write(
      "# [name_of_the_graph_for_this_application]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(general$igraph, file = namefile, ncolumns = 1, append = TRUE)

    write(
      "# [append_to_existing_vessel_files]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(do_append, file = namefile, ncolumns = 1, append = TRUE)

    write(
      "# [name_gis_file_for_total_effort_per_polygon]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      name_gis_file_for_fishing_effort_per_polygon,
      file = namefile,
      ncolumns = length(name_gis_file_for_fishing_effort_per_polygon),
      append = TRUE
    )

    write(
      "# [name_gis_layer_field]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      name_gis_layer_field,
      file = namefile,
      ncolumns = length(name_gis_layer_field),
      append = TRUE
    )

    write(
      "# [is_gis_layer_field_relative_numbers]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      is_gis_layer_field_relative_numbers,
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )

    write(
      "# [xfold_gis_layer_field]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      xfold_gis_layer_field,
      file = namefile,
      ncolumns = length(xfold_gis_layer_field),
      append = TRUE
    )

    write("# [vesselids]", file = namefile, ncolumns = 1, append = TRUE)
    write(
      vesselids,
      file = namefile,
      ncolumns = length(vesselids),
      append = TRUE
    )

    write("# [vessel_range_km]", file = namefile, ncolumns = 1, append = TRUE)
    write(
      vessel_range_km,
      file = namefile,
      ncolumns = length(vessel_range_km),
      append = TRUE
    )

    write("# [metierids]", file = namefile, ncolumns = 1, append = TRUE)
    write(
      metierids,
      file = namefile,
      ncolumns = length(metierids),
      append = TRUE
    )

    write(
      "# [metierids_frequencies]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      metierids_frequencies,
      file = namefile,
      ncolumns = length(metierids_frequencies),
      append = TRUE
    )

    write(
      "# [visited_ports_but_look_at_names_in_harbours.dat_in_harboursspe_folder]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      as.character(visited_ports),
      file = namefile,
      ncolumns = length(as.character(visited_ports)),
      append = TRUE
    )

    write(
      "# [visited_ports_frequencies]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      visited_ports_frequencies,
      file = namefile,
      ncolumns = length(visited_ports_frequencies),
      append = TRUE
    )

    write("# [name_file_ports]", file = namefile, ncolumns = 1, append = TRUE)
    write(name_file_ports, file = namefile, ncolumns = 1, append = TRUE)

    write(
      "# [nb_fish_or_shellfish_stocks_which_should_be_consistent_with_popsspe_folder]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      nb_stocks,
      file = namefile,
      ncolumns = length(nb_stocks),
      append = TRUE
    )

    write(
      "# [fixed_cpue_per_stock_on_fgrounds_for_planB]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      as.character(as.numeric(fixed_cpue_per_stock)),
      file = namefile,
      ncolumns = length(as.character(as.numeric(fixed_cpue_per_stock))),
      append = TRUE
    )

    write(
      "# [Gamma_shape_parameter_for_cpue_per_stock_on_fgrounds_for_planA_but_for_implicit_stocks_or_out_of_range_nodes]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      as.character(as.numeric(gshape_cpue_per_stock)),
      file = namefile,
      ncolumns = length(as.character(as.numeric(gshape_cpue_per_stock))),
      append = TRUE
    )

    write(
      "# [Gamma_scale_parameter_for_cpue_per_stock_on_fgrounds_for_planA_but_for_implicit_stocks_or_out_of_range_nodes]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      as.character(as.numeric(gscale_cpue_per_stock)),
      file = namefile,
      ncolumns = length(as.character(as.numeric(gscale_cpue_per_stock))),
      append = TRUE
    )

    write(
      "# [vessel_features_speed_fuelconsrate_length_kW_carryingcapacity_tankcapacity_nbpingspertrip_shapeinbtw_scaleinbtw_avtripduration]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )

    write(
      "# [multfuelconswhensteaming_multfuelconswhenfishing_multfuelconswhenreturning_multfuelconswheninactive_firmid_isreffleet]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      vessel_features,
      file = namefile,
      ncolumns = length(vessel_features),
      append = TRUE
    )

    write(
      "# [percent_step_in_share_for_TAC_per_stock_for_these_incoming_vessels_but_only_used_if_existing_vessels_already]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      step_in_share,
      file = namefile,
      ncolumns = length(step_in_share),
      append = TRUE
    )

    write(
      "# [vessel_effect_per_stock_in_the_catch_rate_equation]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      as.character(as.numeric(vesselsspe_betas)),
      file = namefile,
      ncolumns = length(as.character(as.numeric(vesselsspe_betas))),
      append = TRUE
    )

    write(
      "# [metier_effect_per_stock_in_the_catch_rate_equation]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write.table(
      metiersspe_betas,
      file = namefile,
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      append = TRUE
    )

    write(
      "# [avai_effect_per_size_group_per_stock_in_the_catch_rate_equation]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write.table(
      avaisspe_betas,
      file = namefile,
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      append = TRUE
    )

    write(
      "# [create_the_file_for_fuel_price_per_vessel_size]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      create_file_for_fuel_price_per_vessel_size,
      file = namefile,
      ncolumns = length(create_file_for_fuel_price_per_vessel_size),
      append = TRUE
    )

    write(
      "# [some_fuel_prices_per_vessel_size_euro_per_litre]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      some_fuel_price_per_vessel_size,
      file = namefile,
      ncolumns = length(some_fuel_price_per_vessel_size),
      append = TRUE
    )

    write(
      "# [percent_fishing_credits_taken_by_incomers_for_RTI_management]",
      file = namefile,
      ncolumns = 1,
      append = TRUE
    )
    write(
      step_in_share_credits,
      file = namefile,
      ncolumns = length(step_in_share_credits),
      append = TRUE
    )
  } # end test

  cat(paste("write the config file...done \n"))
} # end for loop over record


cat(paste("......done.  stored in /FISHERIES/vessels_config_files folder"))
