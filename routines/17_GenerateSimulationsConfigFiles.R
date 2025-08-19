# GENERAL SETTINGS

#   args <- commandArgs(trailingOnly = TRUE)
#
#
#  if (length(args) < 2) {
#    if(.Platform$OS.type == "windows") {
#      general$application           <- "MEESO"
#      general$main_path_gis         <- file.path("D:","FBA", paste("DISPLACE_input_gis_", general$application, sep=""))
#      general$main.path.ibm         <- file.path("D:","FBA", paste("DISPLACE_input_", general$application, sep=''))
#      general$igraph                <- 206 # 110  # caution: should be consistent with existing objects already built upon a given graph
#     do_plot                        <- TRUE
#
#    } else{
#    if(Sys.info()["sysname"] == "Darwin") {
#      general$application           <- "MEESO"
#      general$main_path_gis         <- file.path("usr","local","GitHub",paste("DISPLACE_input_gis_", general$application, sep=""))
#      general$main.path.ibm         <- file.path("usr","local","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
#      general$igraph                <- 1  # caution: should be consistent with existing objects already built upon a given graph
#     do_plot                        <- TRUE
#
#    } else {
#      general$application           <- args[1]
#      general$main_path_gis         <- args[2]
#      general$main.path.ibm         <- args[3]
#      general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
#     do_plot                        <- FALSE
# }}}
cat(paste("START \n"))


if (general$application == "NorthSea") {
  params_pops <- read.csv(
    file = file.path(
      general$main_path_gis,
      "POPULATIONS",
      paste("Stock_biological_traits.csv", sep = ',')
    ),
    sep = ';',
    header = TRUE
  )
  rownames(params_pops) <- params_pops$stock
  feasible_pops <- as.character(params_pops[
    params_pops$UseIt == "Yes" & params_pops$NorthSeaCaseStudy == "Yes",
    "stock"
  ])
  pa <- params_pops[params_pops$stock %in% feasible_pops, ]
  implicit_stocks <- rownames(pa[pa$Implicit == "Yes", ])
  explicit_stocks <- rownames(pa[pa$Implicit == "No", ])
  spp_table <- read.table(
    file = file.path(
      general$main_path_gis,
      "POPULATIONS",
      paste("pop_names_", general$application, ".txt", sep = '')
    ),
    header = TRUE
  )
  implicit_stocks_idx <- spp_table[spp %in% implicit_stocks, "idx"]

  general$implicit_stocks <- c(paste(0:45, collapse = " ")) # implicit level1 = we don?t know the absolute abundance
  general$implicit_stocks_level2 <- c("") # implicit level2 = we don?t know the absolute abundance but we use the relative abudance to draw some catch rates
} else {
  # stop("adapt the script to this app")
  general$implicit_stocks <- "" # implicit level1 = we don?t know the absolute abundance
  general$implicit_stocks_level2 <- c("") # implicit level2 = we don?t know the absolute abundance but we use the relative abudance to draw some catch rates
  implicit_stocks_idx <- ""
}


dir.create(file.path(
  general$main.path.ibm,
  paste("simusspe_", general$application, sep = '')
))

#load
# Updated from frabas to work on newest R versions
coord <- read.table(
  file = file.path(
    general$main_path_gis,
    "GRAPH",
    paste("coord", general$igraph, ".dat", sep = "")
  )
) # build from the c++ gui
coord <- unlist(coord)
dd <- coord
part_length <- length(coord) %/% 3
x <- as.numeric(coord[1:part_length])
y <- as.numeric(coord[(part_length + 1):(2 * part_length)])
harb <- as.numeric(coord[(2 * part_length + 1):(3 * part_length)])
coord <- cbind(x, y, harb)
# For older versions of R
# coord <- read.table(file=file.path(general$main_path_gis, "GRAPH",  paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
# dd    <- coord
# coord <- as.matrix(as.vector(coord))
# coord <- matrix(coord, ncol=3)
# colnames(coord) <- c('x', 'y', 'harb')
if (do_plot) {
  plot(coord[, 1], coord[, 2])
}

# Updated from frabas to work on newest R versions
graph <- read.table(
  file = file.path(
    general$main_path_gis,
    "GRAPH",
    paste("graph", general$igraph, ".dat", sep = "")
  )
) # build from the c++ gui
graph <- unlist(graph)
part_length <- length(graph) %/% 3
x <- as.numeric(graph[1:part_length])
y <- as.numeric(graph[(part_length + 1):(2 * part_length)])
harb <- as.numeric(graph[(2 * part_length + 1):(3 * part_length)])
graph <- cbind(x, y, harb)

# For older versions of R
# graph <- read.table(file=file.path(general$main_path_gis, "GRAPH",  paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
# graph <- as.matrix(as.vector(graph))
# graph <- matrix(graph, ncol=3)
if (do_plot) {
  segments(
    coord[graph[, 1] + 1, 1],
    coord[graph[, 1] + 1, 2],
    coord[graph[, 2] + 1, 1],
    coord[graph[, 2] + 1, 2],
    col = 4
  )
} # CAUTION: +1, because c++ to R


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## a scenario file e.g. baseline.dat ##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
namefile <- file.path(
  general$main.path.ibm,
  paste("simusspe_", general$application, sep = ''),
  paste("baseline.dat", sep = '')
)

dyn_alloc_sce <- c('baseline') # focus_on_high_profit_grounds removed from original script
dyn_pop_sce <- c('baseline')
biolsce <- 1 # for static (i.e. external to displace) scenarios e.g. different bio parameterisation for growth, etc.
fleetsce <- 1 # for static (i.e. external to displace) scenarios  e.g. different monthly catch pattenrs for 'other' landings, etc.
Frequency <- 3 # code for setting frequency for applying the growth transition matrix  CAUTION!! this is assuming the parameterisation GeneratePopulationsFeatures.R is coherent to it
Frequency2 <- 3 # code for setting frequency for applying the distpatching procedure to stocks
a_graph <- general$igraph # an integer...
nrow_coord <- nrow(coord) # might be automatically filled in...
nrow_graph <- nrow(graph) # might be automatically filled in...
a_port <- which(coord[, 'harb'] != 0)[1] # should be a node idx corresponding to a port!
grid_res_km <- 4 # just for visualisation purpose of the node in the GUI
is_individual_vessel_quotas <- 0 # 0/1
check_all_stocks <- 0 # play a role when scenario on vessels looking at fish prices before making their decision
Go_Fishing_DTree <- "GoFishing_Example.dt.csv" # can be left empty or filled in with a dtree file name...
Choose_Ground_DTree <- "" # can be left empty or filled in with a dtree file name...
Start_Fishing_DTree <- "" # can be left empty or filled in with a dtree file name...
Change_Ground_DTree <- "" # can be left empty or filled in with a dtree file name...
Stop_Fishing_DTree <- "" # can be left empty or filled in with a dtree file name...
Change_Port_DTree <- "" # can be left empty or filled in with a dtree file name...
Use_Dtrees <- 1 # 0/1 Use or Not use
tariff_pop <- "" # tbc...
freq_update_tariff_code <- "" # tbc...
arbitrary_breaks_for_tariff <- "" # tbc...
total_amount_credited <- "" # tbc...
tariff_annual_ <- "" # tbc...
banned_metiers <- "" # relevant with area_closures and monthly_area_closures dyn. options

write("# dyn_alloc_sce", file = namefile)
write(dyn_alloc_sce, file = namefile, ncolumns = 1, append = TRUE)

write("# dyn_pop_sce", file = namefile, ncolumns = 1, append = TRUE)
write(dyn_pop_sce, file = namefile, ncolumns = 1, append = TRUE)

write("# biolsce", file = namefile, ncolumns = 1, append = TRUE)
write(biolsce, file = namefile, ncolumns = 1, append = TRUE)

write("# fleetsce", file = namefile, ncolumns = 1, append = TRUE)
write(fleetsce, file = namefile, ncolumns = 1, append = TRUE)

write(
  "# Frequency to apply growth (0:daily; 1:weekly; 2:monthly; 3:quarterly; 4:semester)",
  file = namefile,
  ncolumns = 1,
  append = TRUE
)
write(Frequency, file = namefile, ncolumns = 1, append = TRUE)

write(
  "# Frequency to redispatch the pop (0:daily; 1:weekly; 2:monthly; 3:quarterly; 4:semester)",
  file = namefile,
  ncolumns = 1,
  append = TRUE
)
write(Frequency2, file = namefile, ncolumns = 1, append = TRUE)

write("# a_graph", file = namefile, ncolumns = 1, append = TRUE)
write(a_graph, file = namefile, ncolumns = 1, append = TRUE)

write("# nrow_coord", file = namefile, ncolumns = 1, append = TRUE)
write(nrow_coord, file = namefile, ncolumns = 1, append = TRUE)

write("# nrow_graph", file = namefile, ncolumns = 1, append = TRUE)
write(nrow_graph, file = namefile, ncolumns = 1, append = TRUE)

write("# a_port", file = namefile, ncolumns = 1, append = TRUE)
write(a_port, file = namefile, ncolumns = 1, append = TRUE)

write("# grid res km", file = namefile, ncolumns = 1, append = TRUE)
write(grid_res_km, file = namefile, ncolumns = 1, append = TRUE)

write(
  "# is_individual_vessel_quotas",
  file = namefile,
  ncolumns = 1,
  append = TRUE
)
write(is_individual_vessel_quotas, file = namefile, ncolumns = 1, append = TRUE)

write(
  "#  check all stocks before going fishing (otherwise, explicit pops only)",
  file = namefile,
  ncolumns = 1,
  append = TRUE
)
write(check_all_stocks, file = namefile, ncolumns = 1, append = TRUE)

write("# Go Fishing DTree", file = namefile, ncolumns = 1, append = TRUE)
write(Go_Fishing_DTree, file = namefile, ncolumns = 1, append = TRUE)

write("# Choose Ground DTree", file = namefile, ncolumns = 1, append = TRUE)
write(Choose_Ground_DTree, file = namefile, ncolumns = 1, append = TRUE)

write("# Start Fishing DTree", file = namefile, ncolumns = 1, append = TRUE)
write(Start_Fishing_DTree, file = namefile, ncolumns = 1, append = TRUE)

write("# Change Ground DTree", file = namefile, ncolumns = 1, append = TRUE)
write(Change_Ground_DTree, file = namefile, ncolumns = 1, append = TRUE)

write("# Stop Fishing DTree", file = namefile, ncolumns = 1, append = TRUE)
write(Stop_Fishing_DTree, file = namefile, ncolumns = 1, append = TRUE)

write("# Change Port DTree", file = namefile, ncolumns = 1, append = TRUE)
write(Change_Port_DTree, file = namefile, ncolumns = 1, append = TRUE)

write("# Use Dtrees", file = namefile, ncolumns = 1, append = TRUE)
write(Use_Dtrees, file = namefile, ncolumns = 1, append = TRUE)

write("#tariff_pop", file = namefile, ncolumns = 1, append = TRUE)
write(tariff_pop, file = namefile, ncolumns = 1, append = TRUE)

write("#req_update_tariff_code", file = namefile, ncolumns = 1, append = TRUE)
write(freq_update_tariff_code, file = namefile, ncolumns = 1, append = TRUE)

write(
  "#arbitrary_breaks_for_tariff",
  file = namefile,
  ncolumns = 1,
  append = TRUE
)
write(arbitrary_breaks_for_tariff, file = namefile, ncolumns = 1, append = TRUE)

write("#total_amount_credited", file = namefile, ncolumns = 1, append = TRUE)
write(total_amount_credited, file = namefile, ncolumns = 1, append = TRUE)

write("#tariff_annual_", file = namefile, ncolumns = 1, append = TRUE)
write(tariff_annual_, file = namefile, ncolumns = 1, append = TRUE)

write("# banned metiers", file = namefile, ncolumns = 1, append = TRUE)
write(banned_metiers, file = namefile, ncolumns = 1, append = TRUE)


cat(paste("Write baseline.dat ...done \n"))


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## a config.dat                      ##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
namefile <- file.path(
  general$main.path.ibm,
  paste("simusspe_", general$application, sep = ''),
  paste("config.dat", sep = '')
)

# caution: give the order for naming stocks in integer from 0 to n-1
spp_table <- read.table(
  file = file.path(
    general$main_path_gis,
    "POPULATIONS",
    paste("pop_names_", general$application, ".txt", sep = '')
  ),
  header = TRUE
)
spp <- as.character(spp_table$spp)

metier_names <- read.table(
  file = file.path(
    general$main.path.ibm,
    paste("metiersspe_", general$application, sep = ''),
    "metier_names.dat"
  ),
  header = TRUE
)

nbpops <- length(spp)
nbmets <- nrow(metier_names)
nbbenthospops <- 4
implicit_stocks <- implicit_stocks_idx
implicit_stocks_level2 <- general$implicit_stocks_level2
calib_other_landings <- rep(1, nbpops)
calib_weight_at_size_group <- rep(1, nbpops)
calib_cpue_multipliers <- rep(1, nbpops)
interesting_harbours <- ""
lgnb_coupling <- ""
grouped_tacs <- 0:(nbpops - 1)


write("# nbpops", file = namefile)
write(nbpops, file = namefile, ncolumns = 1, append = TRUE)

write("# nbmets", file = namefile, append = TRUE)
write(nbmets, file = namefile, ncolumns = 1, append = TRUE)

write("# nbbenthospops", file = namefile, ncolumns = 1, append = TRUE)
write(nbbenthospops, file = namefile, ncolumns = 1, append = TRUE)

write("# implicit stocks", file = namefile, ncolumns = 1, append = TRUE)
write(
  implicit_stocks,
  file = namefile,
  ncolumns = length(implicit_stocks),
  append = TRUE
)

write(
  "# calib the other landings per stock",
  file = namefile,
  ncolumns = 1,
  append = TRUE
)
write(
  calib_other_landings,
  file = namefile,
  ncolumns = length(calib_other_landings),
  append = TRUE
)

write(
  "# calib weight-at-szgroup per stock",
  file = namefile,
  ncolumns = 1,
  append = TRUE
)
write(
  calib_weight_at_size_group,
  file = namefile,
  ncolumns = length(calib_weight_at_size_group),
  append = TRUE
)

write(
  "# calib the cpue multiplier per stock",
  file = namefile,
  ncolumns = 1,
  append = TRUE
)
write(
  calib_cpue_multipliers,
  file = namefile,
  ncolumns = length(calib_weight_at_size_group),
  append = TRUE
)

write("# interesting harbours", file = namefile, ncolumns = 1, append = TRUE)
write(interesting_harbours, file = namefile, ncolumns = 1, append = TRUE)

write("# implicit stocks level 2", file = namefile, ncolumns = 1, append = TRUE)
write(implicit_stocks_level2, file = namefile, ncolumns = 1, append = TRUE)

write(
  "# grouped tacs (a group id for each stock e.g. 1 1 2 2 3 4 if 6 stocks but only 4 tacs)",
  file = namefile,
  ncolumns = 1,
  append = TRUE
)
write(
  grouped_tacs,
  file = namefile,
  ncolumns = length(grouped_tacs),
  append = TRUE
)

write("# nbcp coupling pop ids", file = namefile, ncolumns = 1, append = TRUE)
write(
  lgnb_coupling,
  file = namefile,
  ncolumns = length(lgnb_coupling),
  append = TRUE
)


cat(paste("Write config.dat ...done \n"))


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## a tstep_months_.dat               ##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
start_y <- 2010
end_y <- 2020

#start_y <- 2012
#end_y   <- 2017
t.seq <- seq(
  as.POSIXct(paste(start_y, "-01-01 00:00:00", sep = '')),
  as.POSIXct(paste(end_y, "-12-31 00:00:00", sep = '')),
  by = "hours"
)


#year-days
idx <- NULL
idx <- which(
  t.seq %in%
    as.POSIXct(paste(
      rep(start_y:end_y, each = 365),
      "-",
      c(
        rep('01', each = 31),
        rep('02', each = 28),
        rep('03', each = 31),
        rep('04', each = 30),
        rep('05', each = 31),
        rep('06', each = 30),
        rep('07', each = 31),
        rep('08', each = 31),
        rep('09', each = 30),
        rep('10', each = 31),
        rep('11', each = 30),
        rep('12', each = 31)
      ),
      "-",
      sprintf(
        "%02d",
        c(
          1:31,
          1:28,
          1:31,
          1:30,
          1:31,
          1:30,
          1:31,
          1:31,
          1:30,
          1:31,
          1:30,
          1:31
        )
      ),
      " 00:00:00",
      sep = ''
    ))
)
idx <- c(idx, -1) # sentinel

write.table(
  idx[-1],
  file = file.path(
    general$main.path.ibm,
    paste("simusspe_", general$application, sep = ''),
    paste("tstep_days.dat", sep = "")
  ),
  col.names = FALSE,
  row.names = FALSE
)


#year-months
idx <- NULL
idx <- which(
  t.seq %in%
    as.POSIXct(paste(
      rep(start_y:end_y, each = 12),
      "-",
      c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'),
      "-01 00:00:00",
      sep = ''
    ))
)

idx <- c(idx, -1) # sentinel

write.table(
  idx[-1],
  file = file.path(
    general$main.path.ibm,
    paste("simusspe_", general$application, sep = ''),
    paste("tstep_months.dat", sep = "")
  ),
  col.names = FALSE,
  row.names = FALSE
)


#year-quarters
idx <- NULL
for (y in c(start_y:end_y)) {
  idx <- c(
    idx,
    which(t.seq == as.POSIXct(paste(y, "-01-01 00:00:00", sep = ''))),
    which(t.seq == as.POSIXct(paste(y, "-04-01 00:00:00", sep = ''))),
    which(t.seq == as.POSIXct(paste(y, "-07-01 00:00:00", sep = ''))),
    which(t.seq == as.POSIXct(paste(y, "-10-01 00:00:00", sep = '')))
  )
}
idx <- c(idx, -1) # sentinel

write.table(
  idx[-1],
  file = file.path(
    general$main.path.ibm,
    paste("simusspe_", general$application, sep = ''),
    paste("tstep_quarters.dat", sep = "")
  ),
  col.names = FALSE,
  row.names = FALSE
)

#semesters
idx <- NULL
for (y in c(start_y:end_y)) {
  idx <- c(
    idx,
    which(t.seq == as.POSIXct(paste(y, "-01-01 00:00:00", sep = ''))),
    which(t.seq == as.POSIXct(paste(y, "-07-01 00:00:00", sep = '')))
  )
}
idx <- c(idx, -1) # sentinel


write.table(
  idx[-1],
  file = file.path(
    general$main.path.ibm,
    paste("simusspe_", general$application, sep = ''),
    paste("tstep_semesters.dat", sep = "")
  ),
  col.names = FALSE,
  row.names = FALSE
)

#years
idx <- NULL
for (y in c(start_y:end_y)) {
  idx <- c(
    idx,
    which(t.seq == as.POSIXct(paste(y, "-01-01 00:00:00", sep = '')))
  )
}
idx <- c(idx, -1) # sentinel

write.table(
  idx[-1],
  file = file.path(
    general$main.path.ibm,
    paste("simusspe_", general$application, sep = ''),
    paste("tstep_years.dat", sep = "")
  ),
  col.names = FALSE,
  row.names = FALSE
)


cat(paste("Write time steps related files ...done \n"))


# the graph-related files are needed....
dir.create(file.path(general$main.path.ibm, "graphsspe"))

file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",
    paste("coord", general$igraph, ".dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("coord", general$igraph, ".dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",
    paste("graph", general$igraph, ".dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("graph", general$igraph, ".dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",
    paste("coord", general$igraph, "_with_benthos_total_biomass.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("coord", general$igraph, "_with_benthos_total_biomass.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",
    paste("coord", general$igraph, "_with_benthos_total_number.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("coord", general$igraph, "_with_benthos_total_number.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("coord", general$igraph, "_with_landscape.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("coord", general$igraph, "_with_landscape.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("coord", general$igraph, "_with_salinity.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("coord", general$igraph, "_with_salinity.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("coord", general$igraph, "_with_sst.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("coord", general$igraph, "_with_sst.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("coord", general$igraph, "_with_wind.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("coord", general$igraph, "_with_wind.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month1.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month1.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month2.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month2.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month3.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month3.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month4.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month4.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month5.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month5.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month6.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month6.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month7.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month7.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month8.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month8.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month9.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month9.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month10.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month10.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month11.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month11.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("metier_closure_a_graph", general$igraph, "_month12.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("metier_closure_a_graph", general$igraph, "_month12.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month1.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month1.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month2.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month2.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month3.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month3.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month4.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month4.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month5.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month5.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month6.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month6.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month7.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month7.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month8.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month8.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month9.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month9.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month10.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month10.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month11.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month11.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("vsize_closure_a_graph", general$igraph, "_month12.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("vsize_closure_a_graph", general$igraph, "_month12.dat", sep = '')
  )
)
file.copy(
  from = file.path(
    general$main_path_gis,
    "GRAPH",

    paste("code_area_for_graph", general$igraph, "_points.dat", sep = '')
  ),
  to = file.path(
    general$main.path.ibm,
    paste("graphsspe", sep = ''),
    paste("code_area_for_graph", general$igraph, "_points.dat", sep = '')
  )
)


cat(paste("Transfer graph related files ...done \n"))

# createa folder for the shortPaths lib....CAUTION: the lib HAS TO BE CREATED FROM THE DISPLACE GUI!
dir.create(file.path(
  general$main.path.ibm,
  paste(
    "shortPaths_",
    general$application,
    "_a_graph",
    general$igraph,
    sep = ""
  )
))


dir.create(file.path(general$main.path.ibm, paste("dtrees")))

write(
  "#TreeVersion: 7",
  file.path(general$main.path.ibm, paste("dtrees"), "GoFishing_Example.dt.csv")
)
write(
  "#TreeType: GoFishing",
  file.path(general$main.path.ibm, paste("dtrees"), "GoFishing_Example.dt.csv"),
  append = TRUE
)
write(
  "# id,variable,posx,posy,nchld,children...,value",
  file.path(general$main.path.ibm, paste("dtrees"), "GoFishing_Example.dt.csv"),
  append = TRUE
)
write(
  "0,todayIs,2564,2378,2,1,0,2,1,0",
  file.path(general$main.path.ibm, paste("dtrees"), "GoFishing_Example.dt.csv"),
  append = TRUE
)
write(
  "1,probability,2465,2543,0,0.11",
  file.path(general$main.path.ibm, paste("dtrees"), "GoFishing_Example.dt.csv"),
  append = TRUE
) # Original default prob value 0.8. Updated to 0.1
write(
  "2,probability,2633,2544,0,0",
  file.path(general$main.path.ibm, paste("dtrees"), "GoFishing_Example.dt.csv"),
  append = TRUE
)

cat(paste("Create an example GoFishing dtree ...done \n"))


dir.create(file.path(general$main.path.ibm, paste("timeseries")))
file.create(file.path(general$main.path.ibm, paste("timeseries"), "ts.txt")) # for commit
dir.create(file.path(
  general$main.path.ibm,
  paste("externalforcing_", general$application, sep = '')
))
file.create(file.path(
  general$main.path.ibm,
  paste("externalforcing_", general$application, sep = ''),
  "empty.txt"
))


cat(paste("Create missing folders...done \n"))

cat(paste("Create missing coord files... \n"))
# Load required library
# library(data.table)

# Define the path to the coord file
coord_file <- file.path(
  general$main.path.ibm,
  "graphsspe",
  paste0("coord", general$igraph, ".dat")
)

# Check if the coord file exists
if (file.exists(coord_file)) {
  # Read the coord file
  coord1 <- data.table::fread(coord_file)

  # Get the number of rows in coord1.dat
  num_rows <- nrow(coord1)

  # Calculate one-third of the number of rows
  new_num_rows <- ceiling(num_rows / 3)

  # Create a data.table with one column of zeros
  new_data <- data.table::data.table(V1 = rep(0, new_num_rows))

  # Define the list of new files to be created
  new_files <- c(
    paste0("coord", general$igraph, "_with_bathymetry.dat"),
    paste0("coord", general$igraph, "_with_benthos_total_biomass.dat"),
    paste0("coord", general$igraph, "_with_benthos_total_number.dat"),
    paste0("coord", general$igraph, "_with_dissolvedcarbon.dat"),
    paste0("coord", general$igraph, "_with_landscape.dat"),
    paste0("coord", general$igraph, "_with_nitrogen.dat"),
    paste0("coord", general$igraph, "_with_oxygen.dat"),
    paste0("coord", general$igraph, "_with_phosphorus.dat"),
    paste0("coord", general$igraph, "_with_salinity.dat"),
    paste0("coord", general$igraph, "_with_shippingdensity.dat"),
    paste0("coord", general$igraph, "_with_siltfraction.dat"),
    paste0("coord", general$igraph, "_with_sst.dat"),
    paste0("coord", general$igraph, "_with_wind.dat")
  )

  # Loop through the new file names and write the new_data to each file if it does not exist
  for (file in new_files) {
    new_file_path <- file.path(general$main.path.ibm, "graphsspe", file)
    if (!file.exists(new_file_path)) {
      data.table::fwrite(new_data, file = new_file_path, col.names = FALSE)
    }
  }
} else {
  message("The coord file does not exist.")
}


cat(paste(".......done \n"))
