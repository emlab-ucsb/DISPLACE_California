#  args <- commandArgs(trailingOnly = TRUE)

#  general <- list()

#  if (length(args) < 2) {
#    if(.Platform$OS.type == "windows") {
#      general$application           <- "NorthSea"
#      general$main_path_gis         <- file.path("C:", paste("DISPLACE_input_gis_", general$application, sep=""))
#      general$main.path.ibm         <- file.path("C:", paste("DISPLACE_input_", general$application, sep=''))
#      general$igraph                <- 3  # caution: should be consistent with existing objects already built upon a given graph
#     do_plot                        <- TRUE

#    } else{
#    if(Sys.info()["sysname"] == "Darwin") {
#      general$application           <- "VME"
#      general$main_path_gis         <- file.path("usr","local","GitHub",paste("DISPLACE_input_gis_", general$application, sep=""))
#      general$main.path.ibm         <- file.path("usr","local","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
#      general$igraph                <- 12  # caution: should be consistent with existing objects already built upon a given graph
#     do_plot                        <- TRUE

#    } else {
#      general$application           <- args[1]
#      general$main_path_gis         <- args[2]
#      general$main.path.ibm         <- args[3]
#      general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
#     do_plot                        <- FALSE
# }}}
# cat(paste("START \n"))

############### EDIT PETER DEPENDING ON NAME LAYER on line 101

# caution fleet sce
# fleetsce <-  data.frame(sce=1, namesce=c('baseline'))

# fleetsce <- read.table(
#   file = file.path(
#     general$main.path.ibm,
#     paste("multiplier_for_fleetsce", general$application, ".dat", sep = '')
#   ),
#   header = TRUE,
#   sep = " "
# )

fleetsce <- data.frame(sce = 1, namesce = c('baseline'))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

library("readxl")
library("dplyr")
# landings <- read_excel(
#   file.path(
#     general$main_path_gis,
#     "OTHERS",
#     "OfficialNominalCatches_2006-2022",
#     "ICESCatchDataset2006-2022.xlsx"
#   ),
#   3
# )
# landings <- as.data.frame(landings) # in tons

# # quick and dirty...TODO: obtain a key per stock.
# # for now, done per species, which is different from stocks...
# landings <- landings[landings$Area == "27", ] # this app

# landings$species <- paste0(tolower(landings$Column1), ".", landings$Area)

# spp_table <- read.table(
#   file = file.path(
#     general$main_path_gis,
#     "POPULATIONS",
#     paste0("pop_names_", general$application, ".txt")
#   ),
#   header = TRUE
# )
# spp_table$spp <- tolower(spp_table$spp)
# land <- landings[
#   landings$species %in% paste0(substr(spp_table$spp, 1, 3), ".27"),
# ]
# land_2022 <- land[, c("species", "Country", "2022")]

# catches_in_tons <- data.frame(tapply(
#   as.numeric(as.character(land_2022$'2022')),
#   list(land_2022$species, land_2022$Country),
#   sum,
#   na.rm = TRUE
# ))

# catches_in_tons$species <- substr(rownames(catches_in_tons), 1, 3)
# catches_in_tons <- data.frame(
#   stock = spp_table$spp,
#   species = substr(spp_table$spp, 1, 3)
# ) %>%
#   left_join(catches_in_tons, by = c("species" = "species"))

# catches_in_tons[is.na(catches_in_tons)] <- 0

# catches_in_tons <- as.data.frame(catches_in_tons)

catches_in_tons <- read.table(
  file.path(general$main_path_gis, "OTHERS", "othercatchespercountry.csv"),
  sep = ";",
  header = TRUE
) # in tons
cat(paste("Read the specs othercatchespercountry.csv\n"))

popnames <- as.character(catches_in_tons$POP)

############## Perhaps keep a part of dutch fisheries unaccounted for in the simulation

# catches_in_tons[, 'NL'] <- 0

############### see comment below, we need better data on depletion for different stocks

# CAUTION: FOR NOW, TONS ARE REPEATED PER SPECIES, WHICH SHOULD NOT BE....
# REDO WHEN MORE INFO WILL BE AVAILABLE I.E. TRULY PER STOCK!
# anyway, will not have incidence as long as these pops will be DISPLACE implicit pops

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

lst_layers <- list.files(file.path(
  general$main_path_gis,
  "POPULATIONS",
  "SpatialLayers"
))


for (sce in fleetsce$sce) {
  for (popid in 1:length(catches_in_tons$POP)) {
    a_file <- lst_layers[grep(
      paste("contour", popid - 1, "_", sep = ''),
      lst_layers
    )]
    a_file <- a_file[grep(".shp", a_file)]
    a_file <- a_file[1] # take the larger fish cat

    # get the spatial distribution of the stock that will obviously constraint where the depletion can actually occur
    cat(paste("Read the GIS layer for pop", popid, "\n"))
    library(sf)
    shp <- st_read(
      file.path(general$main_path_gis, "POPULATIONS", "SpatialLayers", a_file),
      crs = "+proj=longlat +datum=WGS84"
    )
    name_gis_layer_field <- "GRIDCODE" ############### EDIT PETER DEPENDING ON NAME LAYER
    shp <- shp[, name_gis_layer_field]
    shp$pop <- popid

    # load the graph
    coord <- read.table(
      file = file.path(
        general$main_path_gis,
        "GRAPH",
        paste("coord", general$igraph, ".dat", sep = "")
      )
    ) # build from the c++ gui
    coord <- matrix(as.numeric(as.character(coord[, 1])), ncol = 3)
    coord <- cbind(coord, 1:nrow(coord))
    colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')
    if (do_plot) {
      plot(coord[, 1], coord[, 2])
    }

    saved_coord <- coord

    graph <- read.table(
      file = file.path(
        general$main_path_gis,
        "GRAPH",
        paste("graph", general$igraph, ".dat", sep = "")
      )
    ) # build from the c++ gui
    graph <- matrix(as.numeric(as.character(graph[, 1])), ncol = 3)
    #if(do_plot) segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R

    cat(paste("Read graph...done\n"))

    # extract abundance (or whatever name)
    e <- terra::ext(terra::vect(shp))
    r <- terra::rast(
      e,
      ncols = 5000,
      nrows = 5000,
      crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    )
    shp_WGS84_rast <- terra::rasterize(
      terra::vect(shp),
      field = name_gis_layer_field,
      r,
      fun = mean
    )
    shp_WGS84_rast_values_on_coord <- terra::extract(
      shp_WGS84_rast,
      as.matrix(coord[, c(1, 2)])
    )

    coord <- cbind(coord, abundance = shp_WGS84_rast_values_on_coord)

    # keep only nodes where an abundance is informed i.e. where the stock distributes
    coord <- coord[!is.na(coord[, name_gis_layer_field]), ]

    # if there are some locations included then....
    if (nrow(coord) != 0) {
      # retrieve the 'other' landings...

      total_catches_this_year_in_kg <- sum(
        catches_in_tons[
          catches_in_tons$POP == popnames[popid],
          -ncol(catches_in_tons)
        ],
        na.rm = TRUE
      ) *
        1000 # convert in kg from tons

      # ...and dispatch (just an assumption, one can do otherwise provided the data format is respected)
      coord <- cbind(
        coord,
        landings = total_catches_this_year_in_kg / nrow(coord) / 12
      ) # e.g. catches are evenly dispatched among the relevant nodes and given 12 months

      # caution: other_land likely to be 0 (because rounded at the end of the day) if a small amount is dispatched over a large amount of nodes....

      # save .dat files
      coord[, 'pt_graph'] <- as.numeric(as.character(coord[, 'pt_graph'])) - 1 ##!!! OFFSET FOR C++ !!!##    because R to c++
      library(doBy)
      coord <- orderBy(~pt_graph, data = coord)
      if (do_plot) points(coord[, 1], coord[, 2], col = 2)
    } else {
      coord <- matrix(
        c(0, 0),
        ncol = 2,
        nrow = 2,
        dimnames = list(c(0, 1), c('pt_graph', 'landings'))
      )
    }

    # save and export to multimap c++ for this pop
    # assuming for the testexample that the same amount in kg is depleted each month on the same location
    for (month in c(
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10",
      "11",
      "12"
    )) {
      # save for a multimap in c++  pt_graph / landings kg per month (to be used every months)
      # to fill in the pop attribute
      write.table(
        round(coord[, c('pt_graph', 'landings')], 1),
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste(
            (popid) - 1,
            'spe_stecf_oth_land_per_month_per_node_month',
            month,
            "_fleetsce",
            sce,
            ".dat",
            sep = ''
          )
        ),
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )

      cat(paste(
        "Write ",
        (popid) - 1,
        'spe_stecf_oth_land_per_month_per_node_month',
        month,
        ".dat\n",
        sep = ''
      ))
    }
  } # end popid
} # end sce

cat(paste(".....done \n"))
