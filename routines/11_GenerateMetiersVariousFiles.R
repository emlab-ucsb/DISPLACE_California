##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!VARIOUS METIERS FILES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!met_target_names.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!metier_fspeed.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!metierspe_mls_cat_semesterXX.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!percent_revenue_completenesses.dat!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!metier_gear_widths_param_a.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!metier_gear_widths_param_b.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!metier_gear_widths_model_type.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!metiersspe_discardratio_limits_semester.dat!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!! metiersspe_avoided_stocks_semester.dat!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!! metier_suitable_seabottomtypes.dat!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!20loss_after_one_passage_per_landscape_per_func_group.dat!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# some args for the bunch of vessels to be created....
# Usage:
# GenerateMetiersVariousFiles.R application gis_path input_application_path

#
#  # GENERAL SETTINGS
# general <- list()
#
#   if (length(args) < 2) {
#     if(.Platform$OS.type == "windows") {
#       general$application           <- "MEESO"
#       general$main_path_gis         <- file.path("D:","FBA", paste("DISPLACE_input_gis_", general$application, sep=""))
#       general$main.path.ibm         <- file.path("D:","FBA", paste("DISPLACE_input_", general$application, sep=''))
#       general$igraph                <- 206 # 110  # caution: should be consistent with existing objects already built upon a given graph
#      do_plot                        <- TRUE
#
#     } else{
#     if(Sys.info()["sysname"] == "Darwin") {
#       general$application           <- "MEESO"
#       general$main_path_gis         <- file.path("usr","local","GitHub",paste("DISPLACE_input_gis_", general$application, sep=""))
#       general$main.path.ibm         <- file.path("usr","local","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
#       general$igraph                <- 1  # caution: should be consistent with existing objects already built upon a given graph
#      do_plot                        <- TRUE
#
#     } else {
#       general$application           <- args[1]
#       general$main_path_gis         <- args[2]
#       general$main.path.ibm         <- args[3]
#       general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
#      do_plot                        <- FALSE
#  }}}
#  cat(paste("START \n"))
#

dir.create(file.path(
  general$main.path.ibm,
  paste("metiersspe_", general$application, sep = '')
))


a_size_group_bin_in_cm <- 0.5 # caution: hardcoding....
mid <- a_size_group_bin_in_cm / 2
cat(paste("caution: harcoded bin size \n"))

spp_table <- read.table(
  file = file.path(
    general$main_path_gis,
    "POPULATIONS",
    paste("pop_names_", general$application, ".txt", sep = '')
  ),
  header = TRUE
)
spp <- as.character(spp_table$spp)

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


#########################################
## metier_target_name ###################
#########################################
write(
  c("LE_MET_level6", "mapped_stk_code"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "met_target_names.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)
cat(paste("Write met_target_names.dat \n"))


# by default, create a fake selectivity ogive i.e. all at 1 (and not species-specific...)
for (met in unique(metier_names$idx)) {
  met_target_names <- NULL
  the_met <- as.character(metier_names[metier_names[, 'idx'] == met, "name"])

  demersal_gear <- FALSE
  pelagic_gear <- FALSE
  passive_gear <- FALSE
  dredge_gear <- FALSE

  if (
    length(grep("BOT", the_met)) != 0 ||
      length(grep("BT", the_met)) != 0 ||
      length(grep("OTB", the_met)) != 0 ||
      length(grep("OT_CRU", the_met)) != 0 ||
      length(grep("OT_MIX_NEP", the_met)) != 0 ||
      length(grep("OT_DEM_PEL", the_met)) != 0 ||
      length(grep("OT_DMF_PEL", the_met)) != 0 ||
      length(grep("OT_DMF", the_met)) != 0 ||
      length(grep("OTT", the_met)) != 0 ||
      length(grep("PS", the_met)) != 0 ||
      length(grep("PTB", the_met)) != 0 ||
      length(grep("SDR", the_met)) != 0 ||
      length(grep("SPR", the_met)) != 0 ||
      length(grep("SSC", the_met)) != 0 ||
      length(grep("SSC_DEM", the_met)) != 0 ||
      length(grep("SDN", the_met)) != 0 ||
      length(grep("SDN_DEM", the_met)) != 0 ||
      length(grep("TBB_CRU", the_met)) != 0 ||
      length(grep("TBB_DMF", the_met)) != 0 ||
      length(grep("TBB", the_met)) != 0
  ) {
    demersal_gear <- TRUE
    pelagic_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("PT", the_met)) != 0 ||
      length(grep("S", the_met)) != 0 ||
      length(grep("OTM", the_met)) != 0 ||
      length(grep("OT_SPF", the_met)) != 0 ||
      length(grep("PTM", the_met)) != 0
  ) {
    demersal_gear <- FALSE
    pelagic_gear <- TRUE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("GN", the_met)) != 0 ||
      length(grep("A", the_met)) != 0 ||
      length(grep("GND", the_met)) != 0 ||
      length(grep("GNS", the_met)) != 0 ||
      length(grep("GTN", the_met)) != 0 ||
      length(grep("GTR", the_met)) != 0 ||
      length(grep("SB", the_met)) != 0
  ) {
    demersal_gear <- FALSE
    pelagic_gear <- FALSE
    passive_gear <- TRUE
    dredge_gear <- FALSE
  }
  if (
    length(grep("FPO", the_met)) != 0 ||
      length(grep("A", the_met)) != 0 ||
      length(grep("LHM", the_met)) != 0 ||
      length(grep("LHP", the_met)) != 0 ||
      length(grep("LLD", the_met)) != 0 ||
      length(grep("LLS", the_met)) != 0 ||
      length(grep("LTL", the_met)) != 0
  ) {
    demersal_gear <- FALSE
    pelagic_gear <- FALSE
    passive_gear <- TRUE
    dredge_gear <- FALSE
  }
  if (
    length(grep("DRB", the_met)) != 0 ||
      length(grep("DRH", the_met)) != 0 ||
      length(grep("DRB_MOL", the_met)) != 0 ||
      length(grep("HMD", the_met)) != 0
  ) {
    demersal_gear <- FALSE
    pelagic_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- TRUE
  }

  all_species <- sapply(spp, function(spp) substr(spp, 1, 3))
  count <- -1
  for (spid in 1:length(all_species)) {
    count <- count + 1 # idx sp
    demersal_sp <- FALSE
    pelagic_sp <- FALSE
    molluscs_sp <- FALSE
    if (
      names(all_species)[spid] %in%
        c(
          'BLL.nsea',
          'COD.2224',
          'COD.2532',
          'COD.nsea',
          'COD.kat',
          'CSH.nsea',
          'DAB.nsea',
          'DAB.2232',
          'FLE.2223',
          'FLE.2425',
          'FLE.nsea',
          'GUG.nsea',
          'HAD.nsea',
          'HKE.nsea',
          'LEM.kask',
          'MON.nsea',
          'MUR.nsea',
          'MON.nsea',
          'NEP.nsea',
          'NEP.kask',
          'NOP.nsea',
          'PLE.nsea',
          'PLE.2123',
          'PLE.2432',
          'POK.nsea',
          'POL.nsea',
          'PRA.nsea',
          'PRA.kask',
          'SAN.nsea',
          'SOL.3a2223',
          'SOL.nsea',
          'TUR.2232',
          'TUR.kask',
          'WHG.nsea',
          'WHG.kask',
          'WHG.2232',
          'WIT.nsea'
        )
    ) {
      pelagic_sp <- FALSE
      demersal_sp <- TRUE
      molluscs_sp <- FALSE
    }
    if (
      names(all_species)[spid] %in%
        c(
          'HER.nsea',
          'HER.3a22',
          'HER.2532',
          'HER.nsea',
          'HOM.nsea',
          'MAC.nsea',
          'SPR.nsea',
          'SPR.kask',
          'SPR.2232'
        )
    ) {
      pelagic_sp <- TRUE
      demersal_sp <- FALSE
      molluscs_sp <- FALSE
    }
    if (names(all_species)[spid] %in% c('MUS.kask', 'MUS.2224')) {
      pelagic_sp <- FALSE
      demersal_sp <- FALSE
      molluscs_sp <- TRUE
    }

    if (
      demersal_gear &&
        demersal_sp ||
        passive_gear && demersal_sp ||
        pelagic_gear && pelagic_sp ||
        dredge_gear && molluscs_sp
    ) {
      write(
        c(met, count),
        file = file.path(
          general$main.path.ibm,
          paste("metiersspe_", general$application, sep = ''),
          "met_target_names.dat"
        ),
        ncolumns = 2,
        sep = ' ',
        append = TRUE
      )
    }
  }
}
cat(paste("Write met_target_names.dat....done \n"))


#########################################
## metier_suitable_seabottomtypes #######
#########################################
write(
  c("LE_MET_level6", "mapped_stk_code"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metier_suitable_seabottomtypes.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)
cat(paste("Write met_target_names.dat \n"))

landscape_types <- unique(read.table(
  file = file.path(
    general$main_path_gis,
    "GRAPH",
    paste("coord", general$igraph, "_with_landscape.dat", sep = '')
  ),
  header = FALSE
)[, 1])

## JUST A GUESS FOR NOW- TO BE ADAPTED IF MORE INFO ON BOTTOM TYPE IS PROVIDED........

#bottomtype   <- substr(landscape_types, 1,1) # here we use the BALANCE project coding
#bottom_levels <- factor(bottomtype)
#levels(bottom_levels) <- c('Mud', 'Bedrock', 'Hard Bottom', 'Sand', 'Hard Clay', 'Mud')

# by default
for (met in unique(metier_names$idx)) {
  met_target_names <- NULL
  the_met <- as.character(metier_names[metier_names[, 'idx'] == met, "name"])

  demersal_gear <- FALSE
  pelagic_gear <- FALSE
  passive_gear <- FALSE
  dredge_gear <- FALSE

  if (
    length(grep("BOT", the_met)) != 0 ||
      length(grep("BT", the_met)) != 0 ||
      length(grep("OTB", the_met)) != 0 ||
      length(grep("OT_CRU", the_met)) != 0 ||
      length(grep("OT_MIX_NEP", the_met)) != 0 ||
      length(grep("OT_DEM_PEL", the_met)) != 0 ||
      length(grep("OT_DMF_PEL", the_met)) != 0 ||
      length(grep("OT_DMF", the_met)) != 0 ||
      length(grep("OTT", the_met)) != 0 ||
      length(grep("PS", the_met)) != 0 ||
      length(grep("PTB", the_met)) != 0 ||
      length(grep("SDR", the_met)) != 0 ||
      length(grep("SPR", the_met)) != 0 ||
      length(grep("SSC", the_met)) != 0 ||
      length(grep("SSC_DEM", the_met)) != 0 ||
      length(grep("SDN", the_met)) != 0 ||
      length(grep("SDN_DEM", the_met)) != 0 ||
      length(grep("TBB_CRU", the_met)) != 0 ||
      length(grep("TBB_DMF", the_met)) != 0 ||
      length(grep("TBB", the_met)) != 0
  ) {
    demersal_gear <- TRUE
    pelagic_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("PT", the_met)) != 0 ||
      length(grep("S", the_met)) != 0 ||
      length(grep("OTM", the_met)) != 0 ||
      length(grep("OT_SPF", the_met)) != 0 ||
      length(grep("PTM", the_met)) != 0
  ) {
    demersal_gear <- FALSE
    pelagic_gear <- TRUE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("GN", the_met)) != 0 ||
      length(grep("A", the_met)) != 0 ||
      length(grep("GND", the_met)) != 0 ||
      length(grep("GNS", the_met)) != 0 ||
      length(grep("GTN", the_met)) != 0 ||
      length(grep("GTR", the_met)) != 0 ||
      length(grep("SB", the_met)) != 0
  ) {
    demersal_gear <- FALSE
    pelagic_gear <- FALSE
    passive_gear <- TRUE
    dredge_gear <- FALSE
  }
  if (
    length(grep("FPO", the_met)) != 0 ||
      length(grep("A", the_met)) != 0 ||
      length(grep("LHM", the_met)) != 0 ||
      length(grep("LHP", the_met)) != 0 ||
      length(grep("LLD", the_met)) != 0 ||
      length(grep("LLS", the_met)) != 0 ||
      length(grep("LTL", the_met)) != 0
  ) {
    demersal_gear <- FALSE
    pelagic_gear <- FALSE
    passive_gear <- TRUE
    dredge_gear <- FALSE
  }
  if (
    length(grep("DRB", the_met)) != 0 ||
      length(grep("DRH", the_met)) != 0 ||
      length(grep("DRB_MOL", the_met)) != 0 ||
      length(grep("HMD", the_met)) != 0
  ) {
    demersal_gear <- FALSE
    pelagic_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- TRUE
  }

  for (a_landscape in landscape_types) {
    hard <- FALSE
    soft <- FALSE
    mud <- FALSE
    if (
      a_landscape %in%
        landscape_types[
          landscape_types %in%
            c("35", "45", "32", "31", "33", '41', '42', '43', '61')
        ]
    ) {
      hard <- TRUE
      soft <- FALSE
      mud <- FALSE
    }
    if (
      a_landscape %in%
        landscape_types[landscape_types %in% c("51", "54", '62', "63")]
    ) {
      hard <- FALSE
      soft <- TRUE
      mud <- FALSE
    }
    if (
      a_landscape %in%
        landscape_types[landscape_types %in% c('0', '53', '52', '65', '64')]
    ) {
      hard <- FALSE
      soft <- FALSE
      mud <- TRUE
    }
    # EUNIS level 3 coding

    if (
      demersal_gear &&
        soft ||
        demersal_gear && mud ||
        dredge_gear && mud ||
        dredge_gear && soft ||
        passive_gear && hard ||
        passive_gear && soft ||
        pelagic_gear && hard ||
        pelagic_gear && soft ||
        pelagic_gear && mud
    ) {
      write(
        c(met, a_landscape),
        file = file.path(
          general$main.path.ibm,
          paste("metiersspe_", general$application, sep = ''),
          "metier_suitable_seabottomtypes.dat"
        ),
        ncolumns = 2,
        sep = ' ',
        append = TRUE
      )
    }
  }
}
cat(paste("Write metier_suitable_seabottomtypes.dat....done \n"))


#########################################
## metier_fspeed ########################
#########################################
write(
  c("LE_MET_level6", "fspeed"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metier_fspeed.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)


# by default, create a fake selectivity ogive i.e. all at 1 (and not species-specific...)
for (met in unique(metier_names$idx)) {
  fspeed <- 4 # knots

  write(
    c(met, fspeed),
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      "metier_fspeed.dat"
    ),
    ncolumns = 2,
    sep = ' ',
    append = TRUE
  )
} # this will affect the computation of area swept by the towed gears

cat(paste("Write metier_fspeed.dat....done \n"))


#########################################
## metierspe_mls_cat_semesterXX #########
#########################################
# pop parameters
pa <- read.csv(
  file = file.path(
    general$main_path_gis,
    "POPULATIONS",
    paste("Stock_biological_traits.csv", sep = '')
  ),
  sep = ',',
  header = TRUE
)
rownames(pa) <- pa$stock


write(
  c("LE_MET_level6", "mls"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metierspe_mls_cat_semester1.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)
write(
  c("LE_MET_level6", "mls"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metierspe_mls_cat_semester2.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)


for (met in unique(metier_names$idx)) {
  for (sp in spp) {
    sz <- pa[sp, "mls_cat"]

    write(
      c(met, sz),
      file = file.path(
        general$main.path.ibm,
        paste("metiersspe_", general$application, sep = ''),
        "metierspe_mls_cat_semester1.dat"
      ),
      ncolumns = 2,
      sep = ' ',
      append = TRUE
    )
    write(
      c(met, sz),
      file = file.path(
        general$main.path.ibm,
        paste("metiersspe_", general$application, sep = ''),
        "metierspe_mls_cat_semester2.dat"
      ),
      ncolumns = 2,
      sep = ' ',
      append = TRUE
    )
  }
}


cat(paste("Write metierspe_mls_cat_semesterXX.dat....done \n"))


#########################################
## percent_revenue_completenesses########
#########################################

write(
  c("LE_MET_level6", "completeness"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "percent_revenue_completenesses.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)


for (met in unique(metier_names$idx)) {
  percent_revenue_completenesses <- 100 # % revenue of this metier from described spp

  write(
    c(met, percent_revenue_completenesses),
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      "percent_revenue_completenesses.dat"
    ),
    ncolumns = 2,
    sep = ' ',
    append = TRUE
  )
}

cat(paste("Write percent_revenue_completenesses.dat....done \n"))

#########################################
## metier_gear_widths_model_type ########
## metier_gear_widths_param_a   #########
## metier_gear_widths_param_b  ##########
#########################################
write(
  c("LE_MET_level6", "model_type"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metier_gear_widths_model_type.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)
write(
  c("LE_MET_level6", "param_a"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metier_gear_widths_param_a.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)
write(
  c("LE_MET_level6", "param_b"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metier_gear_widths_param_b.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)


for (met in unique(metier_names$idx)) {
  met_target_names <- NULL
  the_met <- as.character(metier_names[metier_names[, 'idx'] == met, "name"])

  bottom_trawl_gear <- TRUE
  pelagic_trawl_gear <- FALSE
  passive_gear <- FALSE
  seine_gear <- FALSE
  dredge_gear <- FALSE # default

  if (
    length(grep("BOT", the_met)) != 0 ||
      length(grep("BT", the_met)) != 0 ||
      length(grep("OTB", the_met)) != 0 ||
      length(grep("OTT", the_met)) != 0 ||
      length(grep("OT_CRU", the_met)) != 0 ||
      length(grep("OT_DEM_PEL", the_met)) != 0 ||
      length(grep("OT_DMF", the_met)) != 0 ||
      length(grep("OT_DMF_PEL", the_met)) != 0 ||
      length(grep("OT_MIX_NEP", the_met)) != 0 ||
      length(grep("PS", the_met)) != 0 ||
      length(grep("PTB", the_met)) != 0 ||
      length(grep("PT", the_met)) != 0 ||
      length(grep("OTM", the_met)) != 0 ||
      length(grep("PTM", the_met)) != 0 ||
      length(grep("TBB_CRU", the_met)) != 0 ||
      length(grep("TBB_DMF", the_met)) != 0 ||
      length(grep("TBB", the_met)) != 0
  ) {
    bottom_trawl_gear <- TRUE
    pelagic_trawl_gear <- FALSE
    seine_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("OTM", the_met)) != 0 ||
      length(grep("OT_SPF", the_met)) != 0 ||
      length(grep("PTM", the_met)) != 0
  ) {
    bottom_trawl_gear <- FALSE
    pelagic_trawl_gear <- TRUE
    seine_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("S", the_met)) != 0 ||
      length(grep("SDR", the_met)) != 0 ||
      length(grep("SPR", the_met)) != 0 ||
      length(grep("SDM_DEM", the_met)) != 0 ||
      length(grep("SSC_DEM", the_met)) != 0 ||
      length(grep("SSC", the_met)) != 0
  ) {
    bottom_trawl_gear <- FALSE
    pelagic_trawl_gear <- FALSE
    seine_gear <- TRUE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("GN", the_met)) != 0 ||
      length(grep("A", the_met)) != 0 ||
      length(grep("GND", the_met)) != 0 ||
      length(grep("GNS", the_met)) != 0 ||
      length(grep("GTN", the_met)) != 0 ||
      length(grep("GTR", the_met)) != 0 ||
      length(grep("SB", the_met)) != 0 ||
      length(grep("FPO", the_met)) != 0 ||
      length(grep("A", the_met)) != 0 ||
      length(grep("LHM", the_met)) != 0 ||
      length(grep("LHP", the_met)) != 0 ||
      length(grep("LLD", the_met)) != 0 ||
      length(grep("LLS", the_met)) != 0 ||
      length(grep("LTL", the_met)) != 0
  ) {
    bottom_trawl_gear <- FALSE
    pelagic_trawl_gear <- FALSE
    seine_gear <- FALSE
    passive_gear <- TRUE
    dredge_gear <- FALSE
  }
  if (
    length(grep("DRB", the_met)) != 0 ||
      length(grep("DRB_MOL", the_met)) != 0
  ) {
    bottom_trawl_gear <- FALSE
    pelagic_trawl_gear <- FALSE
    seine_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- TRUE
  }

  # for bottom-contact swept area computation e.g. no contact for pelagic gears  (Eigaard et al 2016)
  model_type <- "DoS=a*(kW^b)"
  param_a <- 9.6053549509854
  param_b <- 0.433672763959314 # default
  if (bottom_trawl_gear) {
    model_type <- "a*(kW^b)"
    param_a <- 9.6053549509854
    param_b <- 0.433672763959314
  }
  if (pelagic_trawl_gear) {
    model_type <- "a*(kW^b)"
    param_a <- 0.001
    param_b <- 0.5
  }
  if (passive_gear) {
    model_type <- "if_LOA<20_a_else_b"
    param_a <- 0.001
    param_b <- 0.5
  }
  if (seine_gear) {
    model_type <- "a*(LOA^b)"
    param_a <- 4461.27004311913
    param_b <- 0.117589220782479
  }
  if (dredge_gear) {
    model_type <- "a*(LOA^b)"
    param_a <- 0.3142
    param_b <- 1.2454
  }

  write(
    c(met, model_type),
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      "metier_gear_widths_model_type.dat"
    ),
    ncolumns = 2,
    sep = ' ',
    append = TRUE
  )
  write(
    c(met, param_a),
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      "metier_gear_widths_param_a.dat"
    ),
    ncolumns = 2,
    sep = ' ',
    append = TRUE
  )
  write(
    c(met, param_b),
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      "metier_gear_widths_param_b.dat"
    ),
    ncolumns = 2,
    sep = ' ',
    append = TRUE
  )
}

cat(paste("Write metier_gear_widths_model_type.dat....done \n"))
cat(paste("Write metier_gear_widths_param_a.dat....done \n"))
cat(paste("Write metier_gear_widths_param_b.dat....done \n"))

#########################################
## combined_met_types.dat ###############
#########################################
write(
  c("LE_MET_level6", "model_type"),
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "combined_met_types.dat"
  ),
  ncolumns = 2,
  sep = ' ',
  append = FALSE
)


for (met in unique(metier_names$idx)) {
  met_target_names <- NULL
  the_met <- as.character(metier_names[metier_names[, 'idx'] == met, "name"])

  bottom_trawl_gear <- TRUE
  pelagic_trawl_gear <- FALSE
  passive_gear <- FALSE
  seine_gear <- FALSE
  dredge_gear <- FALSE # default

  if (
    length(grep("BOT", the_met)) != 0 ||
      length(grep("BT", the_met)) != 0 ||
      length(grep("OTB", the_met)) != 0 ||
      length(grep("OTT", the_met)) != 0 ||
      length(grep("OT_CRU", the_met)) != 0 ||
      length(grep("OT_DEM_PEL", the_met)) != 0 ||
      length(grep("OT_DMF", the_met)) != 0 ||
      length(grep("OT_DMF_PEL", the_met)) != 0 ||
      length(grep("OT_MIX_NEP", the_met)) != 0 ||
      length(grep("PS", the_met)) != 0 ||
      length(grep("PTB", the_met)) != 0 ||
      length(grep("PT", the_met)) != 0 ||
      length(grep("OTM", the_met)) != 0 ||
      length(grep("PTM", the_met)) != 0 ||
      length(grep("TBB_CRU", the_met)) != 0 ||
      length(grep("TBB_DMF", the_met)) != 0 ||
      length(grep("TBB", the_met)) != 0
  ) {
    bottom_trawl_gear <- TRUE
    pelagic_trawl_gear <- FALSE
    seine_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("OTM", the_met)) != 0 ||
      length(grep("OT_SPF", the_met)) != 0 ||
      length(grep("PTM", the_met)) != 0
  ) {
    bottom_trawl_gear <- FALSE
    pelagic_trawl_gear <- TRUE
    seine_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("S", the_met)) != 0 ||
      length(grep("SDR", the_met)) != 0 ||
      length(grep("SPR", the_met)) != 0 ||
      length(grep("SDM_DEM", the_met)) != 0 ||
      length(grep("SSC_DEM", the_met)) != 0 ||
      length(grep("SSC", the_met)) != 0
  ) {
    bottom_trawl_gear <- FALSE
    pelagic_trawl_gear <- FALSE
    seine_gear <- TRUE
    passive_gear <- FALSE
    dredge_gear <- FALSE
  }
  if (
    length(grep("GN", the_met)) != 0 ||
      length(grep("A", the_met)) != 0 ||
      length(grep("GND", the_met)) != 0 ||
      length(grep("GNS", the_met)) != 0 ||
      length(grep("GTN", the_met)) != 0 ||
      length(grep("GTR", the_met)) != 0 ||
      length(grep("SB", the_met)) != 0 ||
      length(grep("FPO", the_met)) != 0 ||
      length(grep("A", the_met)) != 0 ||
      length(grep("LHM", the_met)) != 0 ||
      length(grep("LHP", the_met)) != 0 ||
      length(grep("LLD", the_met)) != 0 ||
      length(grep("LLS", the_met)) != 0 ||
      length(grep("LTL", the_met)) != 0
  ) {
    bottom_trawl_gear <- FALSE
    pelagic_trawl_gear <- FALSE
    seine_gear <- FALSE
    passive_gear <- TRUE
    dredge_gear <- FALSE
  }
  if (
    length(grep("DRB", the_met)) != 0 ||
      length(grep("DRB_MOL", the_met)) != 0
  ) {
    bottom_trawl_gear <- FALSE
    pelagic_trawl_gear <- FALSE
    seine_gear <- FALSE
    passive_gear <- FALSE
    dredge_gear <- TRUE
  }

  combined_met_types <- 1 # default
  if (bottom_trawl_gear || pelagic_trawl_gear) {
    combined_met_types <- 1
  }
  if (passive_gear) {
    combined_met_types <- 0
  }
  if (seine_gear) {
    combined_met_types <- 0
  }
  if (dredge_gear) {
    combined_met_types <- 0
  }

  write(
    c(met, combined_met_types),
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      "combined_met_types.dat"
    ),
    ncolumns = 2,
    sep = ' ',
    append = TRUE
  )

  cat(paste("Write combined_met_types.dat....done \n"))
}

#########################################
## metiersspe_discardratio_limits_semester.dat ###############
#########################################
for (a.semester in 1:2) {
  #-----------
  #-----------
  ## METIER SPE----------
  # export betas specific to the metier given this pop
  # mean estimates
  nb_met <- (nrow(metier_names))
  nb_stk <- length(spp)

  metiersspe_discardratio_limits_semester <- data.frame(
    rep(metier_names[, 1], each = nb_stk),
    rep(0.2, each = nb_met * nb_stk)
  )
  # disc/land = in the North Sea, choose less than 0.5, which is somewhat low but then realistic i.e. max 33% of the catch (i.e. land+disc) being discarded [because 50/100=0.50 converts into 50/(50+100)=0.33]
  # disc/land = in the Med, choose 2, which is high but then not that limiting
  colnames(metiersspe_discardratio_limits_semester) <- c(
    'LE_MET_level6',
    'discardratio_limits'
  )
  if (
    length(unique(metiersspe_discardratio_limits_semester[,
      'LE_MET_level6'
    ])) !=
      nb_met
  ) {
    stop("missing metier(s) for info on metier effects")
  }

  # reorder:
  library(doBy)
  metiersspe_discardratio_limits_semester <- orderBy(
    ~LE_MET_level6,
    data = metiersspe_discardratio_limits_semester
  )

  # save .dat files
  write.table(
    metiersspe_discardratio_limits_semester,
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      paste(
        "metierspe_discardratio_limits_semester",
        a.semester,
        ".dat",
        sep = ''
      )
    ),
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE,
    append = FALSE,
    sep = " "
  )
} # end a.semester


#########################################
## metiersspe_avoided_stocks_semester.dat ###############
#########################################
for (a.semester in 1:2) {
  #-----------
  #-----------
  ## METIER SPE----------
  # export betas specific to the metier given this pop
  # mean estimates
  nb_met <- (nrow(metier_names))
  nb_stk <- length(spp)
  # CODED 0/1
  # Note that this info will be used in predicting where to fish and if should move away (possibly in the dtrees)
  # espacially relevant in a EU LO context

  metiersspe_avoided_stocks_semester <- data.frame(
    rep(metier_names[, 1], each = nb_stk),
    rep(0, each = nb_met * nb_stk)
  ) # 0s by default
  colnames(metiersspe_avoided_stocks_semester) <- c(
    'LE_MET_level6',
    'is_avoided'
  )
  if (
    length(unique(metiersspe_avoided_stocks_semester[, 'LE_MET_level6'])) !=
      nb_met
  ) {
    stop("missing metier(s) for info on metier effects")
  }

  # reorder:
  library(doBy)
  metiersspe_avoided_stocks_semester <- orderBy(
    ~LE_MET_level6,
    data = metiersspe_avoided_stocks_semester
  )

  # save .dat files
  write.table(
    metiersspe_avoided_stocks_semester,
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      paste(
        "metierspe_is_avoided_stocks_semester",
        a.semester,
        ".dat",
        sep = ''
      )
    ),
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE,
    append = FALSE,
    sep = " "
  )
} # end a.semester


cat(paste(".......done \n"))
