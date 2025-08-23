cat(paste("START \n"))

dir.create(file.path(
  general$main.path.ibm,
  paste("vesselsspe_", general$application, sep = '')
))
dir.create(file.path(
  general$main.path.ibm,
  paste("metiersspe_", general$application, sep = '')
))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----speed up the routine by loading all the GIS shape files at start----------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

if (TRUE) {
  # CAUTION: Assuming that there will be combinations of only two metiers areas...
  # might not be true...

  library(sf)

  # A TRICK: pre-processing: read all combinations
  #  of spatial layers because input data structured in primary and secondary gears.
  # e.g. DRB, DRB-OTB, DRB-passive etc.
  # and assuming the relevant field is "feffort" or whatever name
  name_gis_layer_field <- "fe"
  filename <- file.path(
    general$main_path_gis,
    "FISHERIES",
    "vessels_specifications_per_harbour_metiers.csv"
  )
  cnts <- count.fields(filename, sep = ";")
  vessel_specifications <- read.table(file = filename, sep = ",", header = TRUE)
  names_gis_layers <- unique(as.character(
    vessel_specifications$name_gis_file_for_fishing_effort_per_polygon
  ))
  if (any(is.na(names_gis_layers))) {
    stop(
      "Check why some GIS layers are not informed for some metiers in vessels_specifications_per_harbour_metiers.csv and correct"
    )
  }

  #   for (a_layer in 1:length(names_gis_layers)) {
  #     shp <- st_read(file.path(
  #       general$main_path_gis,
  #       "FISHERIES",
  #       "SpatialLayers",
  #       paste0(names_gis_layers[a_layer], ".shp")
  #     ))
  #     shp <- st_transform(
  #       shp,
  #       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #     ) # convert to longlat
  #     shp <- shp[, name_gis_layer_field]
  #     shp$metier <- names_gis_layers[a_layer]

  #     assign(paste0("spatialLayer_", names_gis_layers[a_layer]), shp)
  #     cat(paste0("spatialLayer_", names_gis_layers[a_layer]), "...OK \n")

  #     for (a_layer2 in 1:length(names_gis_layers)) {
  #       if (a_layer != a_layer2) {
  #         shp2 <- st_read(file.path(
  #           general$main_path_gis,
  #           "FISHERIES",
  #           "SpatialLayers",
  #           paste0(names_gis_layers[a_layer2], ".shp")
  #         ))
  #         shp2 <- st_transform(
  #           shp2,
  #           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #         ) # convert to longlat
  #         shp2 <- shp2[, name_gis_layer_field]
  #         shp2$metier <- names_gis_layers[a_layer]

  #         # here we go:
  #         shp2 <- rbind(shp2, shp)
  #         # should be continue with a st_union() to dissolve objects?

  #         assign(
  #           paste0(
  #             "spatialLayer_",
  #             names_gis_layers[a_layer],
  #             "_",
  #             names_gis_layers[a_layer2]
  #           ),
  #           shp2
  #         )
  #         cat(
  #           paste0(
  #             "spatialLayer_",
  #             names_gis_layers[a_layer],
  #             "_",
  #             names_gis_layers[a_layer2]
  #           ),
  #           "...OK \n"
  #         )
  #         rm(shp2)
  #       }
  #     }
  #     rm(shp)
  #   }
  #   ly <- ls()[grep("spatialLayer_", ls())]
  #   save(
  #     list = ly,
  #     file = file.path(
  #       general$main_path_gis,
  #       "FISHERIES",
  #       "SpatialLayers_combined.RData"
  #     )
  #   )
  # }

  library(future.apply)

  temp_save_path <- file.path(
    general$main_path_gis,
    "FISHERIES",
    "temp_spatial_layers"
  )
  dir.create(temp_save_path, showWarnings = FALSE)

  future_lapply(seq_along(names_gis_layers), function(a_layer) {
    shp <- st_read(
      file.path(general$main_path_gis, "FISHERIES", "SpatialLayers"),
      names_gis_layers[a_layer]
    )

    shp <- st_transform(
      shp,
      crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    )

    shp <- shp[, name_gis_layer_field]
    d <- nrow(as.data.frame(shp))
    addition <- data.frame(metier = rep(names_gis_layers[a_layer], d))
    rownames(addition) <- 0:(d - 1)
    shp <- cbind(shp, addition)

    # Save individual layer temporarily
    # save(shp, file=file.path(temp_save_path, paste0("spatialLayer_", names_gis_layers[a_layer], ".RData")))
    saveRDS(
      shp,
      file = file.path(
        temp_save_path,
        paste0("spatialLayer_", names_gis_layers[a_layer], ".rds")
      )
    )
    # cat(paste0("spatialLayer_", names_gis_layers[a_layer], "...OK \n"))

    # for (a_layer2 in seq_along(names_gis_layers)) {
    #   if(a_layer != a_layer2){
    #     shp2 <- readOGR(file.path(main_path_gis, "FISHERIES", "SpatialLayers"), names_gis_layers[a_layer2])
    #     projection(shp2) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    #     row.names(shp2) <- as.character(length(row.names(shp)) + as.numeric(row.names(shp2)))
    #     shp2 <- shp2[, name_gis_layer_field]
    #     d <- nrow(as.data.frame(shp2))
    #     addition <- data.frame(metier=rep(names_gis_layers[a_layer2], d))
    #     row.names(addition) <- row.names(shp2)
    #     shp2 <- spCbind(shp2[, name_gis_layer_field], addition)
    #     shp2 <- spRbind(shp2, shp)
    #
    #     # Save combined layers temporarily
    #     # combined_name <- paste0("spatialLayer_", names_gis_layers[a_layer], "_", names_gis_layers[a_layer2])
    #     # save(shp2, file=file.path(temp_save_path, paste0(combined_name, ".RData")))
    #     combined_name <- paste0("spatialLayer_", names_gis_layers[a_layer], "_", names_gis_layers[a_layer2])
    #     saveRDS(shp2, file = file.path(temp_save_path, paste0(combined_name, ".rds")))
    #     # cat(combined_name, "...OK \n")
    #     rm(shp2)
    #   }
    # }
    rm(shp)
  })

  all_temp_files <- list.files(
    temp_save_path,
    pattern = "\\.rds$",
    full.names = TRUE
  )

  # Load .rds files and assign to individual objects
  for (file in all_temp_files) {
    object_name <- gsub("\\.rds$", "", basename(file)) # Remove '.rds' to get the object name
    assign(object_name, readRDS(file))
  }

  # Now save these objects into a single .RData file
  ly <- ls()[grep("spatialLayer_", ls())]
  save(
    list = ly,
    file = file.path(
      general$main_path_gis,
      "FISHERIES",
      "SpatialLayers_combined.RData"
    )
  )
} else {
  # end FALSE

  library(sp)
  library(sf)
  load(
    file = file.path(
      general$main_path_gis,
      "FISHERIES",
      "SpatialLayers_combined.RData"
    )
  )
}


gc(TRUE)

#-------------------------------------------------------------------------------
#---------------one by one config file reading----------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# create from different set of vessels, for example:

path <- file.path(general$main_path_gis, "FISHERIES", "vessels_config_files")
namefiles <- list.files(file.path(path))


count <- 0
metierspe_betas_all <- NULL
#for (namefile in namefiles[690:1342]){ # LOOP OVER CONFIG FILES
for (namefile in namefiles) {
  # LOOP OVER CONFIG FILES

  if (length(grep("_Small", namefile)) == 0) {
    # POSSIBLE EXPLANATION FOR THIS LINE: AVOID PROCESSING SMALLER VESSELS FOR NOW....(THEIR CATCHES ARE COMPRISED IN OTHER_LAND, see \OTHERS)
    cat(paste("Treatment for", namefile, "\n"))
    gc(TRUE)
    count <- count + 1

    dat <- readLines(file.path(path, namefile))

    my_split <- function(x) unlist(strsplit(x, " "))
    my_split2 <- function(x) unlist(strsplit(x, "_"))

    do_append <- as.logical(dat[
      which(dat == "# [append_to_existing_vessel_files]") + 1
    ])
    if (count == 1) {
      do_append <- FALSE
    } # NOT ADDED TO PREVIOUS FILE......

    name_gis_file_for_fishing_effort_per_polygon <- as.character(my_split(dat[
      which(dat == "# [name_gis_file_for_total_effort_per_polygon]") + 1
    ]))
    #name_gis_layer_field                         <- dat[which(dat=="# [name_gis_layer_field]")+1] # caution: possible inconsistency
    is_gis_layer_field_relative_numbers <- dat[
      which(dat == "# [is_gis_layer_field_relative_numbers]") + 1
    ]
    xfold_gis_layer_field <- as.numeric(my_split(dat[
      which(dat == "# [xfold_gis_layer_field]") + 1
    ]))
    vesselids <- as.character(my_split(dat[which(dat == "# [vesselids]") + 1]))
    vessel_range_km <- as.numeric(my_split(dat[
      which(dat == "# [vessel_range_km]") + 1
    ]))
    metierids <- as.numeric(my_split(dat[which(dat == "# [metierids]") + 1]))
    metierids_frequencies <- as.numeric(my_split(dat[
      which(dat == "# [metierids_frequencies]") + 1
    ]))
    if (length(metierids) != length(metierids_frequencies)) {
      stop("Check config file for vessel creator - length(metierids)")
    }
    visited_ports <- as.character(my_split(dat[
      which(
        dat ==
          "# [visited_ports_but_look_at_names_in_harbours.dat_in_harboursspe_folder]"
      ) +
        1
    ]))
    visited_ports_frequencies <- as.numeric(my_split(dat[
      which(dat == "# [visited_ports_frequencies]") + 1
    ]))
    if (length(visited_ports) != length(visited_ports_frequencies)) {
      stop("Check config file for vessel creator - length(visited_ports)")
    }
    name_file_ports <- dat[which(dat == "# [name_file_ports]") + 1]
    nb_stocks <- as.numeric(my_split(dat[
      which(
        dat ==
          "# [nb_fish_or_shellfish_stocks_which_should_be_consistent_with_popsspe_folder]"
      ) +
        1
    ]))
    fixed_cpue_per_stock <- as.numeric(my_split(dat[
      which(dat == "# [fixed_cpue_per_stock_on_fgrounds_for_planB]") + 1
    ]))
    if (length(fixed_cpue_per_stock) != nb_stocks) {
      stop(
        "Check config file for vessel creator - length(fixed_cpue_per_stock)"
      )
    }
    gshape_cpue_per_stock <- as.numeric(my_split(dat[
      which(
        dat ==
          "# [Gamma_shape_parameter_for_cpue_per_stock_on_fgrounds_for_planA_but_for_implicit_stocks_or_out_of_range_nodes]"
      ) +
        1
    ]))
    if (length(gshape_cpue_per_stock) != nb_stocks) {
      stop(
        "Check config file for vessel creator - length(gshape_cpue_per_stock)"
      )
    }
    gscale_cpue_per_stock <- as.numeric(my_split(dat[
      which(
        dat ==
          "# [Gamma_scale_parameter_for_cpue_per_stock_on_fgrounds_for_planA_but_for_implicit_stocks_or_out_of_range_nodes]"
      ) +
        1
    ]))
    if (length(gscale_cpue_per_stock) != nb_stocks) {
      stop(
        "Check config file for vessel creator - length(gscale_cpue_per_stock)"
      )
    }
    vessel_features <- as.numeric(my_split(dat[
      which(
        dat ==
          "# [vessel_features_speed_fuelconsrate_length_kW_carryingcapacity_tankcapacity_nbpingspertrip_shapeinbtw_scaleinbtw_avtripduration]"
      ) +
        2
    ]))
    step_in_share <- as.numeric(my_split(dat[
      which(
        dat ==
          "# [percent_step_in_share_for_TAC_per_stock_for_these_incoming_vessels_but_only_used_if_existing_vessels_already]"
      ) +
        1
    ]))
    if (length(step_in_share) != nb_stocks) {
      stop("Check config file for vessel creator - length(step_in_share)")
    }

    # from this vessel, add catch equation vessel effect
    vesselsspe_betas <- as.numeric(my_split(dat[
      which(dat == "# [vessel_effect_per_stock_in_the_catch_rate_equation]") + 1
    ]))
    if (length(vesselsspe_betas) != nb_stocks) {
      stop("Check config file for vessel creator - length(vesselsspe_betas)")
    }

    # from this vessel, add catch equation metier effect informed for the vessel�s metiers (caution, we expect the same whatever the vessel if we talk about the same metiers)
    metierspe_betas <- as.numeric(my_split(dat[
      which(dat == "# [metier_effect_per_stock_in_the_catch_rate_equation]") + 1
    ]))
    if (length(nb_stocks) > 1) {
      for (st in 2:length(nb_stocks)) {
        metierspe_betas <- c(
          metierspe_betas,
          as.numeric(my_split(
            which(
              dat == "# [metier_effect_per_stock_in_the_catch_rate_equation]"
            ) +
              1 +
              (st - 1)
          ))
        )
      }
    }
    metierspe_betas <- matrix(
      metierspe_betas,
      ncol = length(metierids),
      nrow = (nb_stocks),
      byrow = TRUE
    )
    colnames(metierspe_betas) <- metierids
    metierspe_betas <- cbind(
      as.numeric(rep(colnames(metierspe_betas), each = nrow(metierspe_betas))),
      c(metierspe_betas)
    )
    # =>final format: met idx / met effect along st
    if (!is.null(metierspe_betas_all)) {
      metierspe_betas_all <- rbind(
        metierspe_betas_all,
        metierspe_betas[!(metierspe_betas[, 1] %in% metierspe_betas_all[, 1]), ]
      ) # add only if not already present
    } else {
      metierspe_betas_all <- rbind(metierspe_betas_all, metierspe_betas) # add
    }

    # from this vessel, add catch equation sizegroup effect (caution, we expect the same whatever the vessel)
    avaispe_betas <- as.numeric(my_split(dat[
      which(
        dat ==
          "# [avai_effect_per_size_group_per_stock_in_the_catch_rate_equation]"
      ) +
        1
    ]))
    if (length(nb_stocks) > 1) {
      for (st in 2:length(nb_stocks)) {
        avaispe_betas <- c(
          avaispe_betas,
          as.numeric(my_split(dat[
            which(
              dat ==
                "# [avai_effect_per_size_group_per_stock_in_the_catch_rate_equation]"
            ) +
              1 +
              (st - 1)
          ]))
        )
      }
    }
    avaispe_betas <- matrix(
      avaispe_betas,
      ncol = 14,
      nrow = (nb_stocks),
      byrow = TRUE
    )

    create_file_for_fuel_price_per_vessel_size <- as.logical(dat[
      which(dat == "# [create_the_file_for_fuel_price_per_vessel_size]") + 1
    ])
    some_fuel_price_per_vessel_size <- as.numeric(my_split(dat[
      which(dat == "# [some_fuel_prices_per_vessel_size_euro_per_litre]") + 1
    ]))
    step_in_share_credits <- as.numeric(dat[
      which(
        dat ==
          "# [percent_fishing_credits_taken_by_incomers_for_RTI_management]"
      ) +
        1
    ])

    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    #-----utils---------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------

    getPolyAroundACoord <- function(dat, a_dist_m) {
      lst <- list()
      for (i in 1:nrow(dat)) {
        x <- dat[i, 'coords.x1'] +
          a_dist_m * cos(seq(1, 360, by = 5) * pi / 180)
        y <- dat[i, 'coords.x2'] +
          a_dist_m * sin(seq(1, 360, by = 5) * pi / 180)
        assign(
          paste("poly", i, sep = ""),
          Polygon(cbind(c(x, x[1]), c(y, y[1])))
        )
        assign(
          paste("Poly", i, sep = ""),
          Polygons(
            list(get(paste("poly", i, sep = ""))),
            ID = paste(dat[i, "ID"])
          )
        )
        lst[[i]] <- get(paste("Poly", i, sep = ""))
      }
      return(lst)
    }

    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    # loop over GIS layers if more than 1 metier for this vessel
    coord_gr <- list()
    for (imetier in 1:length(metierids)) {
      # loop over metiers
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
      #if(do_plot) plot(coord[,1], coord[,2])

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

      #-------------------------------------------------------------------------------
      # make a circle as a proxy for geographical range---------------------------
      harbours <- read.table(
        file.path(general$main_path_gis, "GRAPH", name_file_ports),
        sep = ";",
        row.names = NULL,
        header = TRUE
      )
      harbours <- harbours[!duplicated(harbours$port_name), ]
      rownames(harbours) <- as.character(harbours$port_name)
      #harbours <- harbours[harbours[,1] %in% visited_ports,]
      harbours <- harbours[rownames(harbours) %in% visited_ports, ]
      harbours <- cbind.data.frame(harbours, ID = 1:nrow(harbours))
      # convert to UTM
      library(sp)
      SP <- SpatialPoints(
        cbind(
          as.numeric(as.character(harbours[, 'lon'])),
          as.numeric(as.character(harbours[, 'lat']))
        ),
        proj4string = CRS(
          "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
        )
      )
      UTMzone <- 10
      harbours <- cbind.data.frame(
        harbours,
        spTransform(
          SP,
          CRS(paste(
            "+proj=utm +zone=",
            UTMzone,
            " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
            sep = ''
          ))
        )
      ) # convert to UTM

      lst <- getPolyAroundACoord(harbours, a_dist_m = vessel_range_km * 1000)
      circle <- SpatialPolygons(lst, 1:nrow(harbours))

      library(raster)
      raster::projection(circle) <- CRS(paste(
        "+proj=utm +zone=",
        UTMzone,
        " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
        sep = ''
      ))
      # transform back to decimal longlat
      circle <- spTransform(
        circle,
        CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      )
      #-------------------------------------------------------------------------------

      #-------------------------------------------------------------------------------
      # GET ALL THE NODES IN THE RANGE OF THE VESSEL SPECIFIC HARBOURS... AND IN THE METIER SHAPEFILE
      cat(paste("Read the GIS layer for", namefile, "\n"))

      name_gis_file_for_fishing_effort_per_polygon <- unlist(strsplit(
        name_gis_file_for_fishing_effort_per_polygon,
        split = " "
      ))
      # name_gis_layer_field <- unlist(strsplit(
      #   name_gis_layer_field,
      #   split = " "
      # ))[imetier] # caution
      handmade <- get(paste0(
        "spatialLayer_",
        name_gis_file_for_fishing_effort_per_polygon[imetier]
      ))
      handmade$metier_reg <- metierids[imetier]

      handmade_WGS84 <- handmade
      #handmade_WGS84 <- spTransform(handmade, CRS("+proj=longlat +datum=WGS84"))    # convert to longlat

      names(handmade_WGS84) # see     name_gis_layer_field

      library(terra) ###################################### change field here for layer name in shapefiles

      # extract feffort (or mw_fshn or whatever name)
      e <- ext(terra::vect(handmade_WGS84))
      r <- terra::rast(
        e,
        ncols = 200,
        nrows = 200,
        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      )
      handmade_WGS84_rast <- terra::rasterize(
        terra::vect(handmade_WGS84),
        field = name_gis_layer_field,
        r,
        fun = mean
      )
      handmade_WGS84_values_on_coord <- extract(
        handmade_WGS84_rast,
        as.matrix(coord[, c(1, 2)])
      )

      coord <- cbind(coord, Value = handmade_WGS84_values_on_coord)

      ## e.g., WHY IT DOES NOT OVERLAP FOR MET 3?
      #plot(handmade_WGS84_rast, xlim=c(-10, -4), ylim=c(35, 38))
      #points(vect(as.matrix(coord[, c(1,2)]), crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

      # extract the circle
      vect_circle <- terra::vect(
        circle,
        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      )
      values(vect_circle) <- 1
      e <- ext(vect_circle)
      r <- terra::rast(e, ncols = 200, nrows = 200)
      circle_rast <- terra::rasterize(vect_circle, r, fun = mean)
      circle_values_on_coord <- extract(
        circle_rast,
        as.matrix(coord[, c(1, 2)])
      )

      coord <- cbind(coord, in_circle = circle_values_on_coord)

      # then keep the relevant coord only.
      coord_in_metier_area <- coord[!is.na(coord[[name_gis_layer_field]]), ]
      coord_in_range <- coord[!is.na(coord$layer), ]
      coord_in_metier_area_and_range <- coord_in_metier_area[
        !is.na(coord_in_metier_area$layer),
      ]

      # caution about the below assumption:
      if (nrow(coord_in_metier_area_and_range) == 0) {
        coord_in_metier_area_and_range <- coord_in_range
        cat(
          "Metier not found in range! Assume this vessel will stay in range for this metier despite the metier is absent in range. TODO: fix input data\n"
        )
      }

      # a visual check
      graphics.off()
      pts_sf <- st_as_sf(
        coord,
        coords = c("x", "y"),
        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      )
      #intersected            <- handmade_WGS84[which(unlist(st_intersects(pts_sf, handmade_WGS84)) == 1), "mw_fshn"]
      # plot(st_coordinates(pts_sf))
      # plot(circle_rast, add = TRUE, col = rgb(0.1, 0.2, 0.5, 0.2)) # the range...
      # #points(st_coordinates(handmade_WGS84["mw_fshn"]), col="red")
      # ##points(st_coordinates(intersected), col="green", pch=19, cex=1)
      # points(coord_in_metier_area, col = "yellow")
      # points(coord_in_metier_area_and_range, col = "blue")
      # text(
      #   mean(coord_in_metier_area_and_range$x),
      #   mean(coord_in_metier_area_and_range$y),
      #   "blue nodes are the ones selected"
      # )

      #-------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------
      #if(nrow(coord_in_metier_area_and_range)>100)
      #{
      # reduce the dimensionality by random selection
      co <- coord_in_metier_area_and_range

      a_var <- name_gis_layer_field ######################################should we edit feffort here into Value?

      # ... but at NA if metier not in range.
      if (all(is.na(co[, a_var]))) {
        co[, a_var] <- rlnorm(nrow(co)) # a trick for random selection
        coord_in_metier_area_and_range[, a_var] <- co[, a_var] # assign back
      }

      # 2. reduce spatial dimensionality further according to hotspot of effort
      idx <- 1:nrow(co)
      prop_to_keep <- 0.03
      idx_low_effort <- idx[co[, a_var] < quantile(co[, a_var])['50%']]
      idx_to_keep1 <- sample(
        x = idx_low_effort,
        size = ceiling(length(idx_low_effort) * prop_to_keep),
        replace = FALSE
      )
      idx <- 1:nrow(co)
      prop_to_keep <- 0.06
      idx_low_effort <- idx[
        co[, a_var] >= quantile(co[, a_var])['50%'] &
          co[, a_var] < quantile(co[, a_var])['75%']
      ]
      idx_to_keep2 <- sample(
        x = idx_low_effort,
        size = ceiling(length(idx_low_effort) * prop_to_keep),
        replace = FALSE
      )
      idx <- 1:nrow(co)
      prop_to_keep <- 0.1
      idx_low_effort <- idx[
        co[, a_var] >= quantile(co[, a_var])['75%'] &
          co[, a_var] < quantile(co[, a_var], 0.90)['90%']
      ]
      idx_to_keep3 <- sample(
        x = idx_low_effort,
        size = ceiling(length(idx_low_effort) * prop_to_keep),
        replace = FALSE
      )
      idx <- 1:nrow(co)
      prop_to_keep <- 0.4
      idx_low_effort <- idx[
        co[, a_var] >= quantile(co[, a_var], 0.90)['90%'] &
          co[, a_var] < quantile(co[, a_var], 0.95)['95%']
      ]
      idx_to_keep4 <- sample(
        x = idx_low_effort,
        size = ceiling(length(idx_low_effort) * prop_to_keep),
        replace = FALSE
      )
      idx <- 1:nrow(co)
      prop_to_keep <- 0.9
      idx_low_effort <- idx[co[, a_var] >= quantile(co[, a_var], 0.95)['95%']]
      idx_to_keep5 <- sample(
        x = idx_low_effort,
        size = ceiling(length(idx_low_effort) * prop_to_keep),
        replace = FALSE
      )

      #browser()
      # (0.02*0.5)+(0.05*0.25)+(0.1*0.15)+(0.4*0.05)+(0.9*0.05)
      # (0.03*0.5)+(0.06*0.25)+(0.1*0.15)+(0.4*0.05)+(0.9*0.05)   # percent kept...

      idx_to_keep <- unique(c(
        idx_to_keep1,
        idx_to_keep2,
        idx_to_keep3,
        idx_to_keep4,
        idx_to_keep5
      ))

      if (length(idx_to_keep) == 0) {
        idx_to_keep <- 1:nrow(coord_in_metier_area_and_range)
      } # debug

      metname <- as.character(metierids[imetier])
      coord_gr[[metname]] <- coord_in_metier_area_and_range[idx_to_keep, ]

      #} else{
      #   metname <- as.character(metierids[imetier])
      #   coord_gr[[ metname ]] <- coord_in_metier_area_and_range
      #}

      # visual check
      pts_sf <- st_as_sf(
        coord_gr[[metname]],
        coords = c("x", "y"),
        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      )
      # points(st_coordinates(pts_sf), col = "cyan")
      # text(
      #   mean(coord_in_metier_area_and_range$x),
      #   mean(coord_in_metier_area_and_range$y),
      #   "blue nodes are making the selection, cyan nodes are the ones finally selected"
      # )
    } # end imetier

    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------

    cat(paste(
      "Produce individual vessel DISPLACE input files from",
      namefile,
      "\n"
    ))

    # WORKFLOW 2 - QUARTER-BASED-----------
    # however, note that the seasonality of the spatial and total effort application
    # is not parameterized (given we don�t inform with quarter-based distribution data here) but is instead an emerging feature from the model.
    fgrounds <- NULL
    coord_grds <- do.call("rbind", coord_gr)
    an <- function(x) as.numeric(as.character(x))
    for (a.quarter in c("Q1", "Q2", "Q3", "Q4")) {
      name_gis_layer_field <- name_gis_layer_field ############################ EDIT PETER "feffort" has become 'Value'

      #############
      ############       DISCUSS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ############

      # dispatch the feffort among nodes by dividing proba in area per the number of included graph nodes
      if ("x" %in% colnames(coord_grds)) {
        fgrounds_this_quarter <- aggregate(
          coord_grds[, name_gis_layer_field],
          list(coord_grds$pt_graph, coord_grds$x, coord_grds$y),
          sum,
          na.rm = TRUE
        )
      }
      # required if several GIS layers when more than 1 metier

      colnames(fgrounds_this_quarter) <- c(
        "pt_graph",
        "CELL_LONG",
        "CELL_LATI",
        "feffort"
      )

      fgrounds_this_quarter <- cbind.data.frame(
        fgrounds_this_quarter,
        quarter = a.quarter
      ) # init

      fgrounds_this_quarter$freq_feffort <- an(fgrounds_this_quarter$feffort) /
        sum(an(fgrounds_this_quarter$feffort))
      #=> scale to 1 to obtain a proba of visit per node

      fgrounds_this_quarter$freq_feffort <- replace(
        fgrounds_this_quarter$freq_feffort,
        is.na(fgrounds_this_quarter$freq_feffort),
        1
      ) # debug

      # check
      if (
        nrow(fgrounds_this_quarter[
          duplicated(fgrounds_this_quarter$pt_graph),
        ]) >
          0
      ) {
        print("duplicated grounds")
      }
      fgrounds_this_quarter <- fgrounds_this_quarter[
        !duplicated(fgrounds_this_quarter$pt_graph),
      ]

      fgrounds <- rbind.data.frame(fgrounds, fgrounds_this_quarter)

      # check
      print(length(unique(fgrounds_this_quarter$pt_graph)))
    }
    cat(paste("fgrounds_this_quarter this layer....OK", "\n"))

    # duplicate per vessel id  (i.e. assuming the same parameterisation for all the vesselids)
    fgrounds <- fgrounds[fgrounds$freq_feffort != 0, ] # remove if 0

    # check
    print(nrow(fgrounds))
    print(length(unique(fgrounds$pt_graph)) * 4)

    fgrounds_allvessels <- NULL
    for (vid in vesselids) {
      cat(paste(vid, "\n"))
      fgrounds_allvessels <- rbind.data.frame(
        fgrounds_allvessels,
        cbind(fgrounds, vids = vid)
      )
    }

    # check
    print(nrow(fgrounds_allvessels))
    print(length(unique(fgrounds_allvessels$pt_graph)) * 4 * length(vesselids))

    # duplicate per metier id (i.e. assuming the same relative effort distribution per polygon for all the metierids)
    # but don�t worry, will later overwrite this...
    fgrounds_allvessels_allmet <- NULL
    for (met in metierids) {
      cat(paste(met, "\n"))
      fgrounds_allvessels_allmet <- rbind.data.frame(
        fgrounds_allvessels_allmet,
        cbind(fgrounds_allvessels, met)
      )
    }

    # check
    print(nrow(fgrounds_allvessels_allmet))
    print(
      length(unique(fgrounds_allvessels_allmet$pt_graph)) *
        4 *
        length(vesselids) *
        length(metierids)
    )

    # create the c++ input files  (CAUTION if also active Workflow 1 then need to append to the existing files...)
    # i.e.
    # vesselsspe_fgrounds_quarter
    # vesselsspe_freq_fgrounds_quarter
    # vesselsspe_harbours_quarter
    # vesselsspe_freq_harbours_quarter

    ####-------
    an <- function(x) as.numeric(as.character(x))
    for (a.quarter in c("Q1", "Q2", "Q3", "Q4")) {
      x <- fgrounds_allvessels[fgrounds_allvessels$quarter == a.quarter, ]
      x$freq <- round(an(x$freq_feffort), 6)

      # save .dat files
      x$pt_graph <- as.numeric(as.character(x$pt_graph)) - 1 ##!!! OFFSET FOR C++ !!!##
      vesselsspe_fgrounds_quarter <- x[, c('vids', 'pt_graph')]

      # check
      print(nrow(vesselsspe_fgrounds_quarter))
      length(unique(vesselsspe_fgrounds_quarter$pt_graph))
      NBGROUNDS <- length(unique(vesselsspe_fgrounds_quarter$pt_graph))

      write.table(
        vesselsspe_fgrounds_quarter,
        file = file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste(
            "vesselsspe_fgrounds_quarter",
            gsub("Q", "", a.quarter),
            ".dat",
            sep = ''
          )
        ),
        col.names = ifelse(do_append, FALSE, TRUE),
        row.names = FALSE,
        sep = ' ',
        quote = FALSE,
        append = do_append
      )
      vesselsspe_freq_fgrounds_quarter <- x[, c('vids', 'freq')]
      vesselsspe_freq_fgrounds_quarter$freq <- format(
        vesselsspe_freq_fgrounds_quarter$freq,
        scientific = FALSE
      )
      write.table(
        vesselsspe_freq_fgrounds_quarter,
        file = file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste(
            "vesselsspe_freq_fgrounds_quarter",
            gsub("Q", "", a.quarter),
            ".dat",
            sep = ''
          )
        ),
        col.names = ifelse(do_append, FALSE, TRUE),
        row.names = FALSE,
        sep = ' ',
        quote = FALSE,
        append = do_append
      )
      #-----------

      cat(paste(
        "vesselsspe_fgrounds_quarter",
        gsub("Q", "", a.quarter),
        ".dat....OK",
        "\n"
      ))
      cat(paste(
        "vesselsspe_freq_fgrounds_quarter",
        gsub("Q", "", a.quarter),
        ".dat....OK",
        "\n"
      ))

      #-----------
      # vesselsspe_harbours_quarter[xx].dat
      # vesselsspe_freq_harbours_quarter[xx].dat
      ## get back the port name
      port_names <- read.table(
        file.path(general$main_path_gis, "GRAPH", name_file_ports),
        sep = ";",
        row.names = NULL,
        header = TRUE
      )
      port_names <- port_names[!duplicated(port_names$port_name), ]
      port_names$pt_graph <- coord[
        match(port_names$idx_port, coord[, "harb"]),
        "pt_graph"
      ]
      #rownames(port_names)  <- port_names[,1]
      #rownames(port_names)  <- port_names[,"idx_port"]
      rownames(port_names) <- as.character(port_names$port_name)

      visited <- expand.grid(port_names[visited_ports, "pt_graph"], vesselids) # retrieve the corresponding pt_graph
      visited <- visited[, 2:1]
      colnames(visited) <- c('vids', 'pt_graph')
      visited_freq <- visited # init
      visited_freq[, 2] <- rep(visited_ports_frequencies)
      colnames(visited_freq) <- c('vids', 'freq')

      # save .dat files
      visited$pt_graph <- as.numeric(as.character(visited$pt_graph)) - 1 ##!!! FOR C++ !!!##
      write.table(
        visited,
        file = file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste(
            "vesselsspe_harbours_quarter",
            gsub("Q", "", a.quarter),
            ".dat",
            sep = ''
          )
        ),
        col.names = ifelse(do_append, FALSE, TRUE),
        row.names = FALSE,
        sep = ' ',
        quote = FALSE,
        append = do_append
      )
      visited_freq$freq <- format(visited_freq$freq, scientific = FALSE)
      write.table(
        visited_freq,
        file = file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste(
            "vesselsspe_freq_harbours_quarter",
            gsub("Q", "", a.quarter),
            ".dat",
            sep = ''
          )
        ),
        col.names = ifelse(do_append, FALSE, TRUE),
        row.names = FALSE,
        sep = ' ',
        quote = FALSE,
        append = do_append
      )

      # check for NAs
      dd <- visited[is.na(visited[, c('pt_graph')]), ]
      if (nrow(dd) != 0) {
        print("NAs in visited")
        browser()
      }
      dd <- visited_freq[is.na(visited_freq[, c('freq')]), ]
      if (nrow(dd) != 0) {
        print("NAs in visited_freq")
        browser()
      }
      #----------

      cat(paste(
        "vesselsspe_harbours_quarter",
        gsub("Q", "", a.quarter),
        ".dat....OK",
        "\n"
      ))
      cat(paste(
        "vesselsspe_freq_harbours_quarter",
        gsub("Q", "", a.quarter),
        ".dat....OK",
        "\n"
      ))

      #-----------

      x <- fgrounds_allvessels_allmet[
        fgrounds_allvessels_allmet$quarter == a.quarter,
      ]
      allgrds <- x$pt_graph

      # check
      print(nrow(x))
      print(length(unique(x$pt_graph)) * length(metierids))

      # but first, filter out the points lying outside the metier extent (this happens due to our optimisation joining the 2 metiers GIS layers)
      x_filtered <- NULL
      for (met in unique(as.character(x$met))) {
        restricted_to_met_area <- x[
          x$met == met & (x$pt_graph %in% coord_gr[[met]][, "pt_graph"]),
        ]
        if (nrow(restricted_to_met_area) > 0) {
          x_filtered <- rbind.data.frame(x_filtered, restricted_to_met_area)
        } else {
          x_filtered <- rbind.data.frame(x_filtered, x[x$met == met, ])
        }
      }
      x <- x_filtered
      if (any(allgrds %in% x$pt_graph == FALSE)) {
        stop("loosing grounds!!")
      }

      # check
      print(nrow(x))
      print(length(unique(x$pt_graph)) * length(metierids))

      x$vids <- factor(x$vids)
      for (vid in unique(x$vids)) {
        x.vid <- x[x$vids %in% vid, ]
        x.vid$VE_REF <- factor(x.vid$vids)
        x.vid$pt_graph <- factor(x.vid$pt_graph)
        x.vid$met <- factor(x.vid$met)

        # save .dat files (vessel-spe .dat files)
        x.vid$pt_graph <- as.numeric(as.character(x.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##
        vesselsspe_possible_metiers_quarter <- x.vid[, c('pt_graph', 'met')]
        vesselsspe_possible_metiers_quarter <- vesselsspe_possible_metiers_quarter[
          order(vesselsspe_possible_metiers_quarter$pt_graph),
        ]

        print(length(unique(vesselsspe_possible_metiers_quarter$pt_graph)))
        if (
          length(unique(vesselsspe_possible_metiers_quarter$pt_graph)) !=
            NBGROUNDS
        ) {
          stop("Check grounds!!!")
        }

        write.table(
          vesselsspe_possible_metiers_quarter,
          file = file.path(
            general$main.path.ibm,
            paste("vesselsspe_", general$application, sep = ''),
            paste(
              vid,
              "_possible_metiers_quarter",
              gsub("Q", "", a.quarter),
              ".dat",
              sep = ''
            )
          ),
          col.names = TRUE,
          row.names = FALSE,
          sep = ' ',
          quote = FALSE,
          append = FALSE
        )

        vesselsspe_freq_possible_metiers_quarter <- x.vid[, c(
          'pt_graph',
          'met'
        )]
        levels(vesselsspe_freq_possible_metiers_quarter[,
          'met'
        ]) <- metierids_frequencies

        vesselsspe_freq_possible_metiers_quarter[,
          2
        ] <- as.numeric(as.character(vesselsspe_freq_possible_metiers_quarter[,
          2
        ]))
        colnames(vesselsspe_freq_possible_metiers_quarter) <- c(
          'pt_graph',
          'freq'
        )
        vesselsspe_freq_possible_metiers_quarter <- vesselsspe_freq_possible_metiers_quarter[
          order(vesselsspe_freq_possible_metiers_quarter$pt_graph),
        ]

        # standardize to 1
        vesselsspe_freq_possible_metiers_quarter <-
          do.call(
            "rbind",
            lapply(
              split(
                vesselsspe_freq_possible_metiers_quarter,
                f = vesselsspe_freq_possible_metiers_quarter$pt_graph
              ),
              function(x) {
                x$freq <- x$freq / sum(x$freq)
                x
              }
            )
          )

        vesselsspe_freq_possible_metiers_quarter$freq <- format(
          vesselsspe_freq_possible_metiers_quarter$freq,
          scientific = FALSE
        )

        if (
          length(unique(vesselsspe_freq_possible_metiers_quarter$pt_graph)) !=
            NBGROUNDS
        ) {
          stop("Check grounds!!!")
        }

        write.table(
          vesselsspe_freq_possible_metiers_quarter,
          file = file.path(
            general$main.path.ibm,
            paste("vesselsspe_", general$application, sep = ''),
            paste(
              vid,
              "_freq_possible_metiers_quarter",
              gsub("Q", "", a.quarter),
              ".dat",
              sep = ''
            )
          ),
          col.names = TRUE,
          row.names = FALSE,
          sep = ' ',
          quote = FALSE,
          append = FALSE
        )

        # check for NAs
        dd <- vesselsspe_possible_metiers_quarter[
          is.na(vesselsspe_possible_metiers_quarter[, c('met')]),
        ]
        if (nrow(dd) != 0) {
          print("NAs in vesselsspe_possible_metiers_quarter")
          browser()
        }
        dd <- vesselsspe_freq_possible_metiers_quarter[
          is.na(vesselsspe_freq_possible_metiers_quarter[, c('freq')]),
        ]
        if (nrow(dd) != 0) {
          print("NAs in vesselsspe_freq_possible_metiers_quarter")
          browser()
        }

        cat(paste(
          vid,
          "_possible_metiers_quarter",
          gsub("Q", "", a.quarter),
          ".dat....OK",
          "\n"
        ))
        cat(paste(
          vid,
          "_freq_possible_metiers_quarter",
          gsub("Q", "", a.quarter),
          ".dat....OK",
          "\n"
        ))
      } # end vid
      #----------

      #-----------
      #-----------
      # DNK000XXX_gshape_cpue_per_stk_on_nodes_quarter  # plan A
      # DNK000XXX_gscale_cpue_per_stk_on_nodes_quarter  # plan A
      x <- fgrounds_allvessels[fgrounds_allvessels$quarter == a.quarter, ]
      x$vids <- factor(x$vids)

      # check
      print(nrow(x))
      print(length(unique(x$pt_graph)) * length(metierids))

      for (vid in unique(x$vids)) {
        x.vid <- x[x$vids %in% vid, ]
        x.vid$VE_REF <- factor(x.vid$vids)
        x.vid$pt_graph <- factor(x.vid$pt_graph)
        x.vid$pt_graph <- as.numeric(as.character(x.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##

        vesselsspe_gshape_cpue_per_stock_fgrounds_quarter <- cbind.data.frame(
          rep(x.vid$pt_graph, each = nb_stocks),
          gshape_cpue_per_stock
        )
        colnames(vesselsspe_gshape_cpue_per_stock_fgrounds_quarter) <- c(
          'pt_graph',
          'shape'
        )

        # check
        print(nrow(vesselsspe_gshape_cpue_per_stock_fgrounds_quarter))
        print(length(unique(
          vesselsspe_gshape_cpue_per_stock_fgrounds_quarter$pt_graph
        )))
        if (
          length(unique(
            vesselsspe_gshape_cpue_per_stock_fgrounds_quarter$pt_graph
          )) !=
            NBGROUNDS
        ) {
          stop("Check grounds!!!")
        }

        # save .dat files
        write.table(
          vesselsspe_gshape_cpue_per_stock_fgrounds_quarter,
          file = file.path(
            general$main.path.ibm,
            paste("vesselsspe_", general$application, sep = ''),
            paste(
              vid,
              "_gshape_cpue_per_stk_on_nodes_quarter",
              gsub("Q", "", a.quarter),
              ".dat",
              sep = ''
            )
          ),
          col.names = TRUE,
          row.names = FALSE,
          sep = ' ',
          quote = FALSE,
          append = FALSE
        )

        cat(paste(
          vid,
          "_gshape_cpue_per_stk_on_nodes_quarter",
          gsub("Q", "", a.quarter),
          ".dat....OK",
          "\n"
        ))
      }
      # {VesselId}_gscale_cpue_per_stk_on_nodes_quarter  # plan A
      #-----------
      x <- fgrounds_allvessels[fgrounds_allvessels$quarter == a.quarter, ]
      x$vids <- factor(x$vids)

      for (vid in unique(x$vids)) {
        x.vid <- x[x$vids %in% vid, ]
        x.vid$VE_REF <- factor(x.vid$vids)
        x.vid$pt_graph <- factor(x.vid$pt_graph)
        x.vid$pt_graph <- as.numeric(as.character(x.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##

        vesselsspe_gscale_cpue_per_stock_fgrounds_quarter <- cbind.data.frame(
          rep(x.vid$pt_graph, each = nb_stocks),
          gscale_cpue_per_stock
        )
        colnames(vesselsspe_gscale_cpue_per_stock_fgrounds_quarter) <- c(
          'pt_graph',
          'scale'
        )

        # check
        print(nrow(vesselsspe_gscale_cpue_per_stock_fgrounds_quarter))
        print(length(unique(
          vesselsspe_gscale_cpue_per_stock_fgrounds_quarter$pt_graph
        )))
        if (
          length(unique(
            vesselsspe_gscale_cpue_per_stock_fgrounds_quarter$pt_graph
          )) !=
            NBGROUNDS
        ) {
          stop("Check grounds!!!")
        }

        # save .dat files
        write.table(
          vesselsspe_gscale_cpue_per_stock_fgrounds_quarter,
          file = file.path(
            general$main.path.ibm,
            paste("vesselsspe_", general$application, sep = ''),
            paste(
              vid,
              "_gscale_cpue_per_stk_on_nodes_quarter",
              gsub("Q", "", a.quarter),
              ".dat",
              sep = ''
            )
          ),
          col.names = TRUE,
          row.names = FALSE,
          sep = ' ',
          quote = FALSE,
          append = FALSE
        )

        cat(paste(
          vid,
          "_gscale_cpue_per_stk_on_nodes_quarter",
          gsub("Q", "", a.quarter),
          ".dat....OK",
          "\n"
        ))
      }
      #-----------

      #-----------
      #-----------
      # {VesselId}__cpue_per_stk_on_nodes_quarter        # plan B
      x <- fgrounds_allvessels[fgrounds_allvessels$quarter == a.quarter, ]
      x$vids <- factor(x$vids)

      for (vid in unique(x$vids)) {
        x.vid <- x[x$vids %in% vid, ]
        x.vid$VE_REF <- factor(x.vid$vids)
        x.vid$pt_graph <- factor(x.vid$pt_graph)
        x.vid$pt_graph <- as.numeric(as.character(x.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##

        vesselsspe_fixed_cpues_fgrounds_quarter <- cbind.data.frame(
          rep(x.vid$pt_graph, each = nb_stocks),
          fixed_cpue_per_stock
        )
        colnames(vesselsspe_fixed_cpues_fgrounds_quarter) <- c(
          'pt_graph',
          'cpue_kghour'
        )

        # save .dat files
        write.table(
          vesselsspe_fixed_cpues_fgrounds_quarter,
          file = file.path(
            general$main.path.ibm,
            paste("vesselsspe_", general$application, sep = ''),
            paste(
              vid,
              "_cpue_per_stk_on_nodes_quarter",
              gsub("Q", "", a.quarter),
              ".dat",
              sep = ''
            )
          ),
          col.names = TRUE,
          row.names = FALSE,
          sep = ' ',
          quote = FALSE,
          append = FALSE
        )

        cat(paste(
          vid,
          "_cpue_per_stk_on_nodes_quarter",
          gsub("Q", "", a.quarter),
          ".dat....OK",
          "\n"
        ))
      }
      #-----------

      #-----------
      #-----------
      #vesselsspe_features_quarter1

      vesselsspe_features_quarter <- cbind.data.frame(
        vesselids,
        matrix(
          rep(vessel_features, length(vesselids)),
          ncol = length(vessel_features),
          byrow = TRUE
        )
      )
      # save .dat files
      write.table(
        vesselsspe_features_quarter,
        file = file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste(
            "vesselsspe_features_quarter",
            gsub("Q", "", a.quarter),
            ".dat",
            sep = ''
          )
        ),
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE,
        append = do_append,
        sep = "|"
      )

      cat(paste(
        "vesselsspe_features_quarter",
        gsub("Q", "", a.quarter),
        ".dat....OK",
        "\n"
      ))
      #-----------
    } # end a.quarter

    # WORKFLOW 2 - SEMESTER-BASED-----------
    for (a.semester in 1:2) {
      #-----------
      #-----------
      # vesselsspe_percent_tacs_per_pop_semester
      vesselsspe_percent_tacs_per_pop <- cbind.data.frame(
        rep(vesselids, each = nb_stocks),
        rep(100 / length(vesselids), length(vesselids))
      )
      #colnames(vesselsspe_percent_tacs_per_pop) <- c('vid', 'percent')
      colnames(vesselsspe_percent_tacs_per_pop) <- c('V1', 'V2')

      if (
        do_append &&
          file.exists(file.path(
            general$main.path.ibm,
            paste("vesselsspe_", general$application, sep = ''),
            paste(
              "vesselsspe_percent_tacs_per_pop_semester",
              a.semester,
              ".dat",
              sep = ''
            )
          ))
      ) {
        # need to re-compute the percent for ALL vessels i.e. existing and incomers depending on step_in_share
        existing <- read.table(
          file.path(
            general$main.path.ibm,
            paste("vesselsspe_", general$application, sep = ''),
            paste(
              "vesselsspe_percent_tacs_per_pop_semester",
              a.semester,
              ".dat",
              sep = ''
            )
          ),
          header = TRUE
        )
        existing[, 2] <- an(existing[, 2]) *
          (100 - rep(step_in_share, length(unique(existing[, 1])))) /
          100

        if (any(vesselsspe_percent_tacs_per_pop[, 1] %in% existing[, 1])) {
          stop("Check incomers vesselids...already there!")
        }

        vesselsspe_percent_tacs_per_pop[,
          2
        ] <- vesselsspe_percent_tacs_per_pop[, 2] *
          (rep(step_in_share, length(vesselids))) /
          100

        vesselsspe_percent_tacs_per_pop <- rbind.data.frame(
          existing,
          vesselsspe_percent_tacs_per_pop
        )
      }

      # if(floor(sum(vesselsspe_percent_tacs_per_pop[,2])) != (100*nb_stocks)) stop("The TAC share over vessels per stock should sum to 100%...check the dimension")

      # save .dat files
      write.table(
        vesselsspe_percent_tacs_per_pop,
        file = file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste(
            "vesselsspe_percent_tacs_per_pop_semester",
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

      cat(paste(
        "vesselsspe_percent_tacs_per_pop_semester",
        a.semester,
        ".dat....OK",
        "\n"
      ))

      #-----------

      #-----------
      #-----------
      ## VESSEL SPE----------
      # vesselsspe_betas_semester
      vesselsspe_betas_semester <- cbind.data.frame(
        rep(vesselids, each = nb_stocks),
        rep(vesselsspe_betas, length(vesselids))
      )
      colnames(vesselsspe_betas_semester) <- c('VE_REF', 'beta.VE_REF')

      # save .dat files
      write.table(
        vesselsspe_betas_semester,
        file = file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste("vesselsspe_betas_semester", a.semester, ".dat", sep = '')
        ),
        col.names = ifelse(do_append, FALSE, TRUE),
        row.names = FALSE,
        quote = FALSE,
        append = do_append,
        sep = " "
      )

      cat(paste("vesselsspe_betas_semester", a.semester, ".dat....OK", "\n"))
    }

    # WORKFLOW 2 - ADDITIONAL FILE(S)-----------
    if (create_file_for_fuel_price_per_vessel_size) {
      fuel_price_per_vessel_size <- data.frame(
        vsize = c(0:4),
        fuelprice_euro = some_fuel_price_per_vessel_size
      )
      # save .dat files
      write.table(
        fuel_price_per_vessel_size,
        file = file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste("fuel_price_per_vessel_size.dat", sep = '')
        ),
        col.names = TRUE,
        row.names = FALSE,
        quote = FALSE,
        append = FALSE,
        sep = " "
      )

      cat(paste("fuel_price_per_vessel_size.dat....OK", "\n"))
    }

    # fishing credits allocation taken from the pool
    share_of_fishing_credits <- cbind.data.frame(
      vesselids,
      100 / length(vesselids)
    )

    if (
      do_append &&
        file.exists(file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste("initial_fishing_credits_per_vid.dat", sep = '')
        ))
    ) {
      # need to re-compute the percent for ALL vessels i.e. existing and incomers depending on step_in_share
      existing <- read.table(
        file.path(
          general$main.path.ibm,
          paste("vesselsspe_", general$application, sep = ''),
          paste("initial_fishing_credits_per_vid.dat", sep = '')
        ),
        header = TRUE
      )
      existing[, 2] <- an(existing[, 2]) *
        (100 - rep(step_in_share_credits, length(unique(existing[, 1])))) /
        100

      if (any(share_of_fishing_credits[, 1] %in% existing[, 1])) {
        stop("Check incomers vesselids...already there!")
      }

      share_of_fishing_credits[, 2] <- share_of_fishing_credits[, 2] *
        (rep(step_in_share_credits, length(vesselids))) /
        100

      colnames(share_of_fishing_credits) <- colnames(existing)
      fishing_credits <- rbind.data.frame(existing, share_of_fishing_credits)
    } else {
      fishing_credits <- cbind.data.frame(
        vesselids,
        an(share_of_fishing_credits[, 2]) * 10000
      )
      colnames(fishing_credits) <- c("VE_REF", "annual_fishing_credits_per_vid")
      fishing_credits$annual_fishing_credits_per_vid <- format(
        fishing_credits$annual_fishing_credits_per_vid,
        scientific = FALSE
      )
    }

    # save .dat files
    write.table(
      fishing_credits,
      file = file.path(
        general$main.path.ibm,
        paste("vesselsspe_", general$application, sep = ''),
        paste("initial_fishing_credits_per_vid.dat", sep = '')
      ),
      col.names = TRUE,
      row.names = FALSE,
      quote = FALSE,
      append = FALSE,
      sep = " "
    )

    cat(paste("initial_fishing_credits_per_vid.dat....OK", "\n"))

    cat(paste("............done for", namefile, "\n"))
  }
} # END LOOP OVER CONFIG FILES


# end first loop PETER

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

cat(paste(
  "lastly, fix fgrounds and frequency on fgrounds files; a fground should not be a harbour node!",
  "\n"
))
## LASTLY,
# fix the files to avoid vessel fishing in harbour (!)
# but do not remove a vessel by inadvertance....

for (a.quarter in c("Q1", "Q2", "Q3", "Q4")) {
  idx_ports_in_graph <- saved_coord[saved_coord[, "harb"] > 0, "pt_graph"] - 1 ##!!! -1 FOR C++ !!!##

  # 1
  vesselsspe_fgrounds_quarter <- read.table(
    file.path(
      general$main.path.ibm,
      paste("vesselsspe_", general$application, sep = ''),
      paste(
        "vesselsspe_fgrounds_quarter",
        gsub("Q", "", a.quarter),
        ".dat",
        sep = ''
      )
    ),
    header = TRUE
  )
  idx <- vesselsspe_fgrounds_quarter$pt_graph %in% idx_ports_in_graph
  vids_before <- vesselsspe_fgrounds_quarter[, 1]
  vesselsspe_fgrounds_quarter <- vesselsspe_fgrounds_quarter[!idx, ] # correct
  vids_after <- vesselsspe_fgrounds_quarter[, 1]
  #print(vids_before[!vids_before %in% vids_after])  #check if all vid kept on board...if not you are in trouble....

  colnames(vesselsspe_fgrounds_quarter) <- c("vids", "pt_graph")

  write.table(
    vesselsspe_fgrounds_quarter,
    file = file.path(
      general$main.path.ibm,
      paste("vesselsspe_", general$application, sep = ''),
      paste(
        "vesselsspe_fgrounds_quarter",
        gsub("Q", "", a.quarter),
        ".dat",
        sep = ''
      )
    ),
    col.names = TRUE,
    row.names = FALSE,
    sep = ' ',
    quote = FALSE,
    append = FALSE
  )

  # 2
  vesselsspe_freq_fgrounds_quarter <- read.table(
    file.path(
      general$main.path.ibm,
      paste("vesselsspe_", general$application, sep = ''),
      paste(
        "vesselsspe_freq_fgrounds_quarter",
        gsub("Q", "", a.quarter),
        ".dat",
        sep = ''
      )
    ),
    header = TRUE
  )
  vesselsspe_freq_fgrounds_quarter <- vesselsspe_freq_fgrounds_quarter[!idx, ] # correct
  colnames(vesselsspe_freq_fgrounds_quarter) <- c("vids", "freq")

  vesselsspe_freq_fgrounds_quarter$freq <- format(
    vesselsspe_freq_fgrounds_quarter$freq,
    scientific = FALSE
  )
  write.table(
    vesselsspe_freq_fgrounds_quarter,
    file = file.path(
      general$main.path.ibm,
      paste("vesselsspe_", general$application, sep = ''),
      paste(
        "vesselsspe_freq_fgrounds_quarter",
        gsub("Q", "", a.quarter),
        ".dat",
        sep = ''
      )
    ),
    col.names = TRUE,
    row.names = FALSE,
    sep = ' ',
    quote = FALSE,
    append = FALSE
  )
}
#-----------

cat(paste(
  "lastly, produce additional files e.g. related to the catch equation",
  "\n"
))
cat(paste("metier effect in the catch equation", "\n"))
## ADDITIONAL FILES FOR THE CATCH EQUATION - METIER EFFECT
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

for (a.semester in 1:2) {
  #-----------
  #-----------
  ## METIER SPE----------
  # export betas specific to the metier given this pop
  # mean estimates
  nb_met <- (nrow(metier_names))
  nb_stk <- length(spp)
  # Note that we assume exp(vesseleffect+metiereffect+stockdensityeffect*sel) in the catch equation
  # so you will have to put -20 if this actually metier not catching this stock at all....for now we put 0

  metiersspe_gamma_semester <- as.data.frame(metierspe_betas_all)
  colnames(metiersspe_gamma_semester) <- c(
    'LE_MET_level6',
    'gamma.LE_MET_level6'
  )
  #if(nrow(unique(metiersspe_gamma_semester[,'LE_MET_level6'])) != nb_met)   stop("missing metier(s) for info on metier effects")

  # complete all combis in case not all metiers are actually represented by the simulated vessels (e.g. when small vessels not in use)
  missing_met_idx <- metier_names[, 1][
    !metier_names[, 1] %in% unique(metiersspe_gamma_semester[, 1])
  ]
  if (length(missing_met_idx) != 0) {
    metiersspe_gamma_semester <- rbind.data.frame(
      metiersspe_gamma_semester,
      cbind.data.frame(
        LE_MET_level6 = rep(missing_met_idx, each = nb_stocks),
        gamma.LE_MET_level6 = 0
      )
    )
  }

  # reorder:
  library(doBy)
  metiersspe_gamma_semester <- orderBy(
    ~LE_MET_level6,
    data = metiersspe_gamma_semester
  )

  # save .dat files
  write.table(
    metiersspe_gamma_semester,
    file = file.path(
      general$main.path.ibm,
      paste("metiersspe_", general$application, sep = ''),
      paste("metierspe_betas_semester", a.semester, ".dat", sep = '')
    ),
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE,
    append = FALSE,
    sep = " "
  )
} # end a.semester


cat(paste("metier effect in the catch equation", "\n"))
## ADDITIONAL FILES FOR THE CATCH EQUATION - AVAI EFFECT
selected_szgroups <- c('0', '2', '3', '5', '7') # DISPLACE hardcoding
colnames(avaispe_betas) <- 0:(ncol(avaispe_betas) - 1)
for (a.semester in 1:2) {
  #-----------
  #-----------
  ## POP SPE----------
  for (rg in selected_szgroups) {
    # export betas specific to the avai szgroup given this pop (caution: remenber the scaling i.e *1000)
    # mean estimates
    #popsspe_delta_semester <- cbind.data.frame(0:(length(spp)-1), rep(0, length(0:(length(spp)-1))) )
    popsspe_delta_semester <- cbind.data.frame(
      0:(length(spp) - 1),
      avaispe_betas[, rg]
    )
    colnames(popsspe_delta_semester) <- c('pop', 'delta.nb_indiv')

    # save .dat files
    write.table(
      popsspe_delta_semester,
      file = file.path(
        general$main.path.ibm,
        paste("popsspe_", general$application, sep = ''),
        paste("avai", rg, "_betas_semester", a.semester, ".dat", sep = '')
      ),
      col.names = TRUE,
      row.names = FALSE,
      quote = FALSE,
      append = FALSE,
      sep = " "
    )

    cat(paste(
      "Write avai",
      rg,
      "_betas_semester",
      a.semester,
      ".dat....done \n"
    ))
  } # end rg
} # end a.semester


# the selected groups in the catch rate equation
write(
  c('nm2', 'selected_szgroups'),
  file = file.path(
    general$main.path.ibm,
    paste("popsspe_", general$application, sep = ''),
    paste("the_selected_szgroups.dat", sep = ' ')
  ),
  append = FALSE,
  ncol = 2,
  sep = " "
)

for (sp in (0:(length(spp) - 1))) {
  write.table(
    cbind(nm2 = sp, selected_szgroups = selected_szgroups),
    file = file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = ''),
      paste("the_selected_szgroups.dat", sep = ' ')
    ),
    append = TRUE,
    quote = FALSE,
    sep = " ",
    col.names = FALSE,
    row.names = FALSE
  )
} # end sp
cat(paste("Write the_selected_szgroups.dat....done \n"))


# deal with the reference fleet coding: put 0 as default because huge output files will be generated from DISPLACE otherwise....not that useful
for (a.quarter in c("Q1", "Q2", "Q3", "Q4")) {
  vesselsspe_features_quarter <- read.table(
    file = file.path(
      general$main.path.ibm,
      paste("vesselsspe_", general$application, sep = ''),
      paste(
        "vesselsspe_features_quarter",
        gsub("Q", "", a.quarter),
        ".dat",
        sep = ''
      )
    ),
    sep = "|",
    header = FALSE
  )

  vesselsspe_features_quarter[ncol(vesselsspe_features_quarter)] <- 0 # put 0 by default

  # save .dat files
  write.table(
    vesselsspe_features_quarter,
    file = file.path(
      general$main.path.ibm,
      paste("vesselsspe_", general$application, sep = ''),
      paste(
        "vesselsspe_features_quarter",
        gsub("Q", "", a.quarter),
        ".dat",
        sep = ''
      )
    ),
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    append = FALSE,
    sep = "|"
  )

  cat(paste(
    "vesselsspe_features_quarter",
    gsub("Q", "", a.quarter),
    ".dat....OK",
    "\n"
  ))
}


# a quick check to avoid deadly runtime bug:
dd <- read.table(
  file = file.path(
    general$main.path.ibm,
    paste("vesselsspe_", general$application, sep = ''),
    "vesselsspe_features_quarter1.dat"
  ),
  sep = "|"
)
vid <- unique(dd[, 1])
bb <- read.table(
  file = file.path(
    general$main.path.ibm,
    paste("vesselsspe_", general$application, sep = ''),
    "vesselsspe_fgrounds_quarter1.dat"
  ),
  sep = " ",
  header = TRUE
)
vid2 <- unique(bb[, 1])
vid <- as.character(vid)
vid2 <- as.character(vid2)
if (length(vid[!vid %in% vid2]) > 0) {
  stop("check vids: inconsistencies in presence of vessel ids in files")
} # in features.dat but not in fgrounds.dat!
if (length(vid2[!vid2 %in% vid]) > 0) {
  stop("check vids")
}


# apply a fix (because some vessels are getting ultimately removed when filtering out the activty in ports only....):
for (a.quarter in c("Q1", "Q2", "Q3", "Q4")) {
  dd <- read.table(
    file = file.path(
      general$main.path.ibm,
      paste("vesselsspe_", general$application, sep = ''),
      paste(
        "vesselsspe_features_quarter",
        gsub("Q", "", a.quarter),
        ".dat",
        sep = ''
      )
    ),
    sep = "|"
  )
  vids_to_remove <- vid[!vid %in% vid2]
  if (length(vids_to_remove) != 0) {
    dd <- dd[!dd[, 1] %in% vids_to_remove, ]
    write.table(
      dd,
      file = file.path(
        general$main.path.ibm,
        paste("vesselsspe_", general$application, sep = ''),
        paste(
          "vesselsspe_features_quarter",
          gsub("Q", "", a.quarter),
          ".dat",
          sep = ''
        )
      ),
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE,
      append = FALSE,
      sep = "|"
    )
  }
}

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
