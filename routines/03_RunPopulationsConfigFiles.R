cat(paste("START \n"))

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


path <- file.path(general$main_path_gis, "POPULATIONS", "pops_config_files")
namefiles <- list.files(file.path(path))
namefiles <- namefiles[grep(general$application, namefiles)]

cat(paste("Entering /POPULATIONS/pops_config_files folder....done \n"))

# loop over population config files------
avai_allszgroups_allpops_no_sz <- NULL
lst_nodes_idx_distrib <- NULL


################# start loop

for (a_file in namefiles) {
  cat(paste("Process ", a_file, "\n"))

  dat <- readLines(file.path(path, a_file))

  my_split <- function(x) unlist(strsplit(x, " "))
  my_split2 <- function(x) unlist(strsplit(x, "_"))

  dir.create(file.path(
    general$main.path.ibm,
    paste("popsspe_", general$application, sep = '')
  ))
  dir.create(file.path(
    general$main.path.ibm,
    paste("popsspe_", general$application, sep = ''),
    "static_avai"
  ))

  do_append <- as.logical(dat[15])
  name_gis_file_for_total_abundance_per_polygon <- my_split(dat[17])
  name_gis_layer_field <- dat[19]
  is_gis_layer_field_relative_numbers <- dat[21]
  xfold_gis_layer_field <- as.numeric(my_split(dat[23]))
  popids <- as.character(my_split(dat[25]))
  if (length(my_split2(dat[27])) > 1) {
    szgroups <- sapply(my_split2(dat[27]), my_split, simplify = FALSE) # Should return a list()
  } else {
    szgroups <- list(as.character(my_split(dat[27]))) # return a list()
  }
  selected_szgroups <- as.character(my_split(dat[29]))

  if (as.numeric(popids) %in% as.numeric(spp_table$idx)) {
    cat(paste("this stock", popids, "is in the list...\n"))

    avai_allszgroups <- NULL
    # loop over set of szgroups------
    for (ly in 1:length(name_gis_file_for_total_abundance_per_polygon)) {
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

      cat(paste("Read the graph....done\n"))

      #-------------------------------------------------------------------------------
      #-Read and overlay from GIS layers----------------------------------------------
      #-------------------------------------------------------------------------------

      cat(paste("Read the GIS layer for", a_file, "\n"))

      library(sf)
      # library(terra)
      shp <- st_read(file.path(
        general$main_path_gis,
        "POPULATIONS",
        paste0(name_gis_file_for_total_abundance_per_polygon[ly], ".shp")
      ))
      st_crs(shp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      shp <- st_transform(
        shp,
        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      ) # convert to longlat

      shp <- shp[, name_gis_layer_field]
      # shp$abundance <- shp[[name_gis_layer_field]]
      shp$pop <- popids

      # # COMMENTED 2025-11-20 see updates below
      # # extract abundance (or whatever name)
      # e <- terra::ext(terra::vect(shp[name_gis_layer_field]))
      # r <- terra::rast(
      #   e,
      #   ncols = 5000,
      #   nrows = 5000,
      #   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      # )
      # shp_WGS84_rast <- terra::rasterize(
      #   terra::vect(shp),
      #   field = name_gis_layer_field,
      #   r,
      #   fun = mean
      # )
      # shp_WGS84_rast_values_on_coord <- extract(
      #   shp_WGS84_rast,
      #   as.matrix(coord[, c(1, 2)])
      # )

      # #nb nodes with presence for this stock
      # sum(shp_WGS84_rast_values_on_coord[[name_gis_layer_field]], na.rm = TRUE)

      # # #!#!#!#!#!#!#!
      # # #!#!#!#!#!#!#!
      # # #!#!#!#!#!#!#!
      # # # CAUTION: TO BE REMOVED/ADAPTED WHEN THE DYNAMIC MODELLING OF POPS WILL BE IMPLEMENTED:
      # # # reomve some presence on nodes to greatly speed up the shapefile creation, which will anyway not be used when pop are just implicit.
      # # # shp_WGS84_rast_values_on_coord$abundance   <- sapply(shp_WGS84_rast_values_on_coord$abundance, function(x) {idx <- rbinom(length(x[x==1]), 1, (1-prop_to_keep) ); x[x==1 & idx] <- NA; x})
      # # #=> keep 40% of presence randomly, just to make sure to create the required DISPLACE input avai.dat files for the demonstrator app
      # # #=> also avoid using the 30000 nodes where we know only a part of it would cover spat distrib of the stocks
      # # #!#!#!#!#!#!#!
      # # #!#!#!#!#!#!#!
      # # #!#!#!#!#!#!#!

      # # # FINAL nb nodes with presence for this stock
      # # cat(paste(
      # #   "NB OF NODES KEPT-----------------",
      # #   sum(!is.na(shp_WGS84_rast_values_on_coord[[name_gis_layer_field]])),
      # #   "\n",
      # #   "TOTAL ABUNDANCE CONSIDERED-----------------",
      # #   sum(
      # #     shp_WGS84_rast_values_on_coord[[name_gis_layer_field]],
      # #     na.rm = TRUE
      # #   )
      # # ))

      # coord <- cbind(
      #   coord,
      #   GRIDCODE = shp_WGS84_rast_values_on_coord,
      #   xfold = 1
      # )

      # UPDATED 2025-11-20
      coord_sf <- coord |>
        as.data.frame() |>
        st_as_sf(
          coords = c("x", "y"),
          crs = st_crs(shp)
        )

      shp_WGS84_values_on_coord_sf <- st_join(coord_sf, shp)

      # Transforme sf to dataframe. Get original coord now with pop density
      coord <- shp_WGS84_values_on_coord_sf |>
        dplyr::mutate(
          x = sf::st_coordinates(geometry)[, "X"],
          y = sf::st_coordinates(geometry)[, "Y"]
        ) |>
        sf::st_drop_geometry() |>
        dplyr::select(x, y, harb, pt_graph, GRIDCODE) |>
        dplyr::mutate(xfold = 1)

      # # FINAL nb nodes with presence for this stock
      # cat(paste(
      #   "NB OF NODES KEPT-----------------",
      #   sum(!is.na(coord |> pull(name_gis_layer_field))),
      #   "\n",
      #   "TOTAL ABUNDANCE CONSIDERED-----------------",
      #   sum(
      #     coord |> pull(name_gis_layer_field),
      #     na.rm = TRUE
      #   )
      # ))

      # check
      # if (do_plot) {
      #   plot(
      #     shp_WGS84_rast,
      #     add = FALSE,
      #     border = as.data.frame(shp_WGS84_rast)[, name_gis_layer_field]
      #   )
      #   points(
      #     as.numeric(as.character(coord[, "x"])),
      #     as.numeric(as.character(coord[, "y"])),
      #     col = 1,
      #     pch = 16
      #   )
      #   points(
      #     as.numeric(as.character(coord[, "x"])),
      #     as.numeric(as.character(coord[, "y"])),
      #     col = as.numeric(coord[, name_gis_layer_field]) + 2,
      #     pch = 16
      #   )
      # }
      #-------------------------------------------------------------------------------
      #-Produce the AVAI object-------------------------------------------------------
      #-------------------------------------------------------------------------------

      library(doBy)
      sdm_input <- "density"
      avai <- NULL
      an <- function(x) as.numeric(as.character(x))
      # loop per semester--------
      for (a.semester in c("S1", "S2")) {
        # dispatch the abundance among nodes by dividing 'abundance' per the number of included graph nodes

        if (sdm_input == "abundance") {
          abundance_this_semester <- coord[
            !is.na(coord[, name_gis_layer_field]) &
              !is.infinite(coord[, name_gis_layer_field]) &
              coord[, name_gis_layer_field] != 0,
          ]
          abundance_this_semester <- cbind.data.frame(
            abundance_this_semester,
            semester = a.semester,
            avai = factor(
              an(abundance_this_semester[, name_gis_layer_field]) *
                an(abundance_this_semester[, "xfold"])
            )
          ) # init
          abundance_this_semester$avai <- factor(abundance_this_semester$avai)
          levels(abundance_this_semester$avai) <- an(levels(
            abundance_this_semester$avai
          )) /
            table(abundance_this_semester$avai)
          abundance_this_semester$avai <- an(abundance_this_semester$avai) /
            sum(an(abundance_this_semester$avai))
          #=> scale to 1 to obtain a relative avai per node
        } else if (sdm_input == "density") {
          # UPDATED 2026-01-22
          abundance_this_semester <- coord[
            !is.na(coord[, name_gis_layer_field]) &
              !is.infinite(coord[, name_gis_layer_field]) &
              coord[, name_gis_layer_field] != 0,
          ]
          abundance_this_semester <- cbind.data.frame(
            abundance_this_semester,
            semester = a.semester
          )

          abundance_this_semester$avai <- abundance_this_semester$GRIDCODE /
            sum(abundance_this_semester$GRIDCODE)
        }
        avai <- rbind.data.frame(avai, abundance_this_semester)
      }

      # duplicate per size group of this layer (i.e. assuming the same parameterisation for all the pops)
      # (caution: avoid to push for a szgroup if already informed in another layer)
      for (sid in szgroups[[ly]]) {
        if (!any(avai_allszgroups$szgroups == sid)) {
          avai_allszgroups <- rbind.data.frame(
            avai_allszgroups,
            cbind(avai, szgroups = sid)
          )
        }
      }
    } # end loop over ly i.e. sets of size group

    #-------------------------------------------------------------------------------
    #-All combi---------------------------------------------------------------------
    #-------------------------------------------------------------------------------

    # caution: fill in the gap
    all_combi <- expand.grid(
      pt_graph = unique(avai_allszgroups$pt_graph),
      szgroups = 0:13,
      semester = c("S1", "S2")
    )
    avai_allszgroups_this_sp <- merge(avai_allszgroups, all_combi, all = TRUE)
    #avai_allszgroups$avai <- replace(avai_allszgroups$avai, is.na (avai_allszgroups$avai), 0.0000000000001)
    avai_allszgroups_this_sp$avai <- replace(
      avai_allszgroups_this_sp$avai,
      is.na(avai_allszgroups_this_sp$avai),
      0.0
    )

    #-------------------------------------------------------------------------------
    #-Export the DISPLACE input files for this popid--------------------------------
    #-------------------------------------------------------------------------------

    ####-------
    an <- function(x) as.numeric(as.character(x))
    options(scipen = 999)
    for (a.semester in c("S1", "S2")) {
      # COMMENTED 2025-12-16
      # avai_allszgroups_this_sp$avai <- round(avai_allszgroups_this_sp$avai, 8)
      # # a check
      # #tapply(an(avai_allszgroups$avai), list(avai_allszgroups$semester, avai_allszgroups$szgroups), sum, na.rm=TRUE  ) # should be full of 1
      # # save .dat files
      # avai_allszgroups_this_sp$pt_graph <- as.numeric(as.character(
      #   avai_allszgroups_this_sp$pt_graph
      # )) -
      #   1 ##!!! OFFSET FOR C++ !!!##
      # avai_allszgroups_this_sp <- orderBy(
      #   ~pt_graph,
      #   data = avai_allszgroups_this_sp
      # )
      # # ...and collect stock presence/absence distribution
      # idx_nodes <- unique(avai_allszgroups_this_sp[
      #   avai_allszgroups_this_sp$semester == a.semester,
      #   c('pt_graph')
      # ])
      # lst_nodes_idx_distrib <- rbind.data.frame(
      #   lst_nodes_idx_distrib,
      #   cbind.data.frame(
      #     pt_graph = idx_nodes,
      #     popid = popids,
      #     semester = a.semester
      #   )
      # )
      # popsspe_avai_semester <- avai_allszgroups_this_sp[
      #   avai_allszgroups_this_sp$semester == a.semester,
      #   c('pt_graph', 'avai')
      # ]
      # write.table(
      #   popsspe_avai_semester[, c('pt_graph', 'avai')], # the szgroup dim is implicit....
      #   file = file.path(
      #     general$main.path.ibm,
      #     paste("popsspe_", general$application, sep = ''),
      #     "static_avai",
      #     paste(
      #       popids,
      #       "spe_full_avai_szgroup_nodes_semester",
      #       gsub("S", "", a.semester),
      #       ".dat",
      #       sep = ''
      #     )
      #   ),
      #   col.names = TRUE,
      #   row.names = FALSE,
      #   sep = ' ',
      #   quote = FALSE,
      #   append = FALSE
      # )
      # cat(paste(
      #   "Write",
      #   popids,
      #   "spe_full_avai_szgroup_nodes_semester",
      #   gsub("S", "", a.semester),
      #   ".dat....done \n"
      # ))
      # popsspe_avai_semester_this_pop_these_sz <- avai_allszgroups_this_sp[
      #   avai_allszgroups_this_sp$semester == a.semester &
      #     avai_allszgroups_this_sp$szgroups %in% selected_szgroups,
      #   c('pt_graph', 'avai')
      # ]
      # write.table(
      #   popsspe_avai_semester_this_pop_these_sz[, c('pt_graph', 'avai')], # the szgroup dim is implicit....
      #   file = file.path(
      #     general$main.path.ibm,
      #     paste("popsspe_", general$application, sep = ''),
      #     "static_avai",
      #     paste(
      #       popids,
      #       "spe_avai_szgroup_nodes_semester",
      #       gsub("S", "", a.semester),
      #       ".dat",
      #       sep = ''
      #     )
      #   ),
      #   col.names = TRUE,
      #   row.names = FALSE,
      #   sep = ' ',
      #   quote = FALSE,
      #   append = FALSE
      # )
      # cat(paste(
      #   "Write",
      #   popids,
      #   "spe_avai_szgroup_nodes_semester",
      #   gsub("S", "", a.semester),
      #   ".dat....done \n"
      # ))

      # UPDATED 2025-12-16

      avai_allszgroups_this_spe <- avai_allszgroups_this_sp

      # avai_allszgroups_this_spe$avai <- round(avai_allszgroups_this_spe$avai, 8) # COMMENTED 2026-01-22

      # a check

      #tapply(an(avai_allszgroups$avai), list(avai_allszgroups$semester, avai_allszgroups$szgroups), sum, na.rm=TRUE  ) # should be full of 1

      # save .dat files

      avai_allszgroups_this_spe$pt_graph <- as.numeric(as.character(
        avai_allszgroups_this_spe$pt_graph
      )) -
        1 ##!!! OFFSET FOR C++ !!!##

      avai_allszgroups_this_spe <- orderBy(
        ~pt_graph,
        data = avai_allszgroups_this_spe
      )

      # ...and collect stock presence/absence distribution

      idx_nodes <- unique(avai_allszgroups_this_spe[
        avai_allszgroups_this_spe$semester == a.semester,
        c('pt_graph')
      ])

      lst_nodes_idx_distrib <- rbind.data.frame(
        lst_nodes_idx_distrib,
        cbind.data.frame(
          pt_graph = idx_nodes,
          popid = popids,
          semester = a.semester
        )
      )

      popsspe_avai_semester <- avai_allszgroups_this_spe[
        avai_allszgroups_this_spe$semester == a.semester,
        c('pt_graph', 'avai')
      ]

      write.table(
        popsspe_avai_semester[, c('pt_graph', 'avai')], # the szgroup dim is implicit....
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          "static_avai",
          paste(
            popids,
            "spe_full_avai_szgroup_nodes_semester",
            gsub("S", "", a.semester),
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
        "Write",
        popids,
        "spe_full_avai_szgroup_nodes_semester",
        gsub("S", "", a.semester),
        ".dat....done \n"
      ))

      popsspe_avai_semester_this_pop_these_sz <- avai_allszgroups_this_spe[
        avai_allszgroups_this_spe$semester == a.semester &
          avai_allszgroups_this_spe$szgroups %in% selected_szgroups,
        c('pt_graph', 'avai')
      ]

      write.table(
        popsspe_avai_semester_this_pop_these_sz[, c('pt_graph', 'avai')], # the szgroup dim is implicit....
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          "static_avai",
          paste(
            popids,
            "spe_avai_szgroup_nodes_semester",
            gsub("S", "", a.semester),
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
        "Write",
        popids,
        "spe_avai_szgroup_nodes_semester",
        gsub("S", "", a.semester),
        ".dat....done \n"
      ))
    }

    #-----------------------
    # Input Files FOr the DIffuse Pop N Option (adding spillover feature) -----
    # COMMENTED 2026-01-13
    # for (a.semester in c("S1", "S2")) {
    #   # Diffuse N coefficients - here, avai used as a proxy for long/short residence time
    #   popsspe_coeffs_semester_this_pop <- popsspe_avai_semester

    #   some_breaks <- quantile(
    #     popsspe_coeffs_semester_this_pop$avai[
    #       popsspe_coeffs_semester_this_pop$avai != 0
    #     ],
    #     prob = c(0, 0.5, 0.75, 1)
    #   )

    #   # debug
    #   if (length(unique(some_breaks)) != length(some_breaks)) {
    #     rn <- runif(4, 0, 0.00001)
    #     some_breaks <- some_breaks + rn[order(rn, decreasing = FALSE)]
    #   }

    #   popsspe_coeffs_semester_this_pop$quant <- cut(
    #     popsspe_coeffs_semester_this_pop$avai + .00000001,
    #     breaks = some_breaks
    #   ) # just arbitrary values for this example!
    #   popsspe_coeffs_semester_this_pop$quant[is.na(
    #     popsspe_coeffs_semester_this_pop$quant
    #   )] <- levels(popsspe_coeffs_semester_this_pop$quant)[1] # AVOID NAs by all means!

    #   popsspe_coeffs_semester_this_pop$coeff <- popsspe_coeffs_semester_this_pop$quant
    #   levels(popsspe_coeffs_semester_this_pop$coeff) <- c(0.5, 0.1, 0.05) # just arbitrary for this example!

    #   write.table(
    #     popsspe_coeffs_semester_this_pop[, c('pt_graph', 'coeff')], # the szgroup dim is implicit....
    #     file = file.path(
    #       general$main.path.ibm,
    #       paste("popsspe_", general$application, sep = ''),
    #       "static_avai",
    #       paste(
    #         popids,
    #         "spe_field_of_coeff_diffusion_this_pop_nodes_semester",
    #         gsub("S", "", a.semester),
    #         ".dat",
    #         sep = ''
    #       )
    #     ),
    #     col.names = TRUE,
    #     row.names = FALSE,
    #     sep = ' ',
    #     quote = FALSE,
    #     append = FALSE
    #   )

    #   cat(paste(
    #     "Write",
    #     popids,
    #     "spe_field_of_coeff_diffusion_this_pop_nodes_semester",
    #     gsub("S", "", a.semester),
    #     ".dat....done \n"
    #   ))
    # }

    # UPDATED 2026-01-13
    for (a.semester in c("S1", "S2")) {
      # COMMENTED 2026-01-13
      # # Diffuse N coefficients - here, avai used as a proxy for long/short residence time
      # popsspe_coeffs_semester_this_pop <- popsspe_avai_semester

      # some_breaks <- quantile(
      #   popsspe_coeffs_semester_this_pop$avai[
      #     popsspe_coeffs_semester_this_pop$avai != 0
      #   ],
      #   prob = c(0, 0.5, 0.75, 1)
      # )

      # # debug
      # if (length(unique(some_breaks)) != length(some_breaks)) {
      #   rn <- runif(4, 0, 0.00001)
      #   some_breaks <- some_breaks + rn[order(rn, decreasing = FALSE)]
      # }

      # popsspe_coeffs_semester_this_pop$quant <- cut(
      #   popsspe_coeffs_semester_this_pop$avai + .00000001,
      #   breaks = some_breaks
      # ) # just arbitrary values for this example!
      # popsspe_coeffs_semester_this_pop$quant[is.na(
      #   popsspe_coeffs_semester_this_pop$quant
      # )] <- levels(popsspe_coeffs_semester_this_pop$quant)[1] # AVOID NAs by all means!

      # popsspe_coeffs_semester_this_pop$coeff <- popsspe_coeffs_semester_this_pop$quant
      # levels(popsspe_coeffs_semester_this_pop$coeff) <- c(0.5, 0.1, 0.05) # just arbitrary for this example!

      # UPDATED 2026-01-13
      # Calculate movement rates based on initial population.
      # The funcition below defines those rates that best preserve the initial species distribution

      calculate_equilibrium_rates_grid <- function(
        coord,
        graph,
        relative_abundances_by_node,
        equilibrium_min_rate,
        equilibrium_max_rate,
        equilibrium_strength,
        equilibrium_base_rate,
        max_iterations,
        tolerance,
        verbose = TRUE
      ) {
        # ==========================================================================
        # STEP 1: Build node data frame (ONLY nodes in relative_abundances_by_node)
        # ==========================================================================

        if (verbose) {
          cat("Building node data...\n")
        }

        # Get the list of valid pt_graph from abundances
        valid_pts <- relative_abundances_by_node$pt_graph

        # Convert coord to data frame
        coord_df <- data.frame(
          pt_graph = coord[, "pt_graph"],
          x = coord[, "x"],
          y = coord[, "y"],
          harb = coord[, "harb"]
        )

        # Filter to ONLY nodes that exist in relative_abundances_by_node
        node_data <- coord_df %>%
          filter(pt_graph %in% valid_pts) %>%
          inner_join(relative_abundances_by_node, by = "pt_graph") %>%
          rename(abundance = avai)

        n_nodes <- nrow(node_data)

        if (verbose) {
          cat(sprintf("  Nodes in abundance data: %d\n", length(valid_pts)))
          cat(sprintf("  Nodes with coordinates: %d\n", n_nodes))
        }

        # ==========================================================================
        # STEP 2: Build neighbor list from graph (only between valid nodes)
        # ==========================================================================

        if (verbose) {
          cat("Building neighbor list from graph...\n")
        }

        # Create a mapping from pt_graph to row index in node_data
        pt_to_idx <- setNames(1:n_nodes, node_data$pt_graph)

        # Initialize empty neighbor list for each node
        neighbors <- vector("list", n_nodes)
        for (i in 1:n_nodes) {
          neighbors[[i]] <- integer(0)
        }

        # Process graph edges
        for (row in 1:nrow(graph)) {
          # Convert 0-indexed to pt_graph (1-indexed)
          node_a_pt <- graph[row, 1] + 1
          node_b_pt <- graph[row, 2] + 1

          # Check if BOTH nodes are in our valid set
          if (!(node_a_pt %in% valid_pts) || !(node_b_pt %in% valid_pts)) {
            next
          }

          # Get row indices in node_data
          idx_a <- pt_to_idx[as.character(node_a_pt)]
          idx_b <- pt_to_idx[as.character(node_b_pt)]

          # Skip if nodes don't exist
          if (is.na(idx_a) || is.na(idx_b)) {
            next
          }

          # Add bidirectional connection
          neighbors[[idx_a]] <- c(neighbors[[idx_a]], idx_b)
          neighbors[[idx_b]] <- c(neighbors[[idx_b]], idx_a)
        }

        # Remove duplicates from neighbor lists
        neighbors <- lapply(neighbors, unique)

        # Calculate degree (number of neighbors) for each node
        node_data$degree <- sapply(neighbors, length)

        if (verbose) {
          avg_degree <- mean(node_data$degree)
          cat(sprintf("  Average degree: %.2f\n", avg_degree))
          cat(sprintf(
            "  Nodes with no neighbors: %d\n",
            sum(node_data$degree == 0)
          ))
        }

        # ==========================================================================
        # STEP 3: Calculate equilibrium rates iteratively (with convergence tracking)
        # ==========================================================================

        if (verbose) {
          cat("Calculating equilibrium rates...\n")
        }

        # Initialize rates
        rates <- rep(equilibrium_base_rate, n_nodes)

        # Get abundance vector
        abundance <- node_data$abundance

        # Track convergence
        convergence_history <- data.frame(
          iteration = integer(),
          max_change = numeric(),
          mean_rate = numeric(),
          min_rate = numeric(),
          max_rate = numeric()
        )

        # Iterative solver
        converged_at <- NA
        for (iter in 1:max_iterations) {
          old_rates <- rates

          for (i in 1:n_nodes) {
            # Skip if no neighbors (isolated node)
            if (length(neighbors[[i]]) == 0) {
              rates[i] <- equilibrium_base_rate
              next
            }

            # Calculate expected inflow from neighbors
            expected_inflow <- 0
            for (j in neighbors[[i]]) {
              if (node_data$degree[j] > 0) {
                # Node j sends (abundance_j * rate_j) total, split among its neighbors
                contribution <- abundance[j] * rates[j] / node_data$degree[j]
                expected_inflow <- expected_inflow + contribution
              }
            }

            # For equilibrium: outflow = inflow
            # outflow = abundance_i * rate_i = expected_inflow
            # Therefore: rate_i = expected_inflow / abundance_i

            if (expected_inflow > 0 && abundance[i] > 0) {
              ideal_rate <- expected_inflow / abundance[i]
            } else {
              ideal_rate <- equilibrium_base_rate
            }

            # Blend with base rate using equilibrium strength
            rates[i] <- equilibrium_strength *
              ideal_rate +
              (1 - equilibrium_strength) * equilibrium_base_rate

            # Clamp to bounds
            rates[i] <- max(
              equilibrium_min_rate,
              min(equilibrium_max_rate, rates[i])
            )
          }

          # Calculate convergence metrics
          max_change <- max(abs(rates - old_rates))

          # Store convergence history
          convergence_history <- rbind(
            convergence_history,
            data.frame(
              iteration = iter,
              max_change = max_change,
              mean_rate = mean(rates),
              min_rate = min(rates),
              max_rate = max(rates)
            )
          )

          if (max_change < tolerance && is.na(converged_at)) {
            converged_at <- iter
            if (verbose) {
              cat(sprintf(
                "  Converged after %d iterations (max change: %.2e)\n",
                iter,
                max_change
              ))
            }
            break
          }
        }

        if (is.na(converged_at)) {
          converged_at <- max_iterations
          if (verbose) {
            cat(sprintf(
              "  Warning: Did not converge after %d iterations (max change: %.6f)\n",
              max_iterations,
              max_change
            ))
          }
        }

        # ==========================================================================
        # STEP 4: Store results
        # ==========================================================================

        node_data$movement_rate <- rates

        # Create output dataframe with pt_graph and coeff
        rates_df <- data.frame(
          pt_graph = node_data$pt_graph,
          coeff = rates
        )

        if (verbose) {
          cat(sprintf(
            "  Rate range: %.2f%% - %.2f%%\n",
            min(rates) * 100,
            max(rates) * 100
          ))
          cat(sprintf("  Mean rate: %.2f%%\n", mean(rates) * 100))
        }

        # ==========================================================================
        # RETURN RESULTS
        # ==========================================================================

        return(list(
          rates_df = rates_df,
          node_data = node_data,
          neighbors = neighbors,
          convergence = convergence_history,
          converged_at = converged_at
        ))
      }

      # Execute funciton to calculate equilibrium rates
      # Apply to object before C++ correnction
      #  Group by node assuming all size groups have the same abundance
      relative_abundances_by_node <- avai_allszgroups_this_sp |>
        group_by(pt_graph) |>
        summarise(avai = mean(avai)) |>
        ungroup()

      # Calculate equilibrium rates
      eq_result <- calculate_equilibrium_rates_grid(
        coord = coord,
        graph = graph,
        relative_abundances_by_node = relative_abundances_by_node,
        equilibrium_min_rate = 0.02,
        equilibrium_max_rate = 0.7,
        equilibrium_strength = 0.99, # for most accurate use 1
        equilibrium_base_rate = 0.05,
        max_iterations = 400, # strength 1 needs about 4000 iterations
        tolerance = 1e-6,
        verbose = TRUE
      )

      # Uncomment to CHECK RESULTS
      # simulate_movement_grid <- function(
      #   result,
      #   n_timesteps = N_TIMESTEPS,
      #   verbose = TRUE
      # ) {
      #   if (verbose) {
      #     cat(sprintf("Running simulation for %d timesteps...\n", n_timesteps))
      #   }

      #   node_data <- result$node_data
      #   neighbors <- result$neighbors
      #   fixed_rates <- node_data$movement_rate

      #   n_nodes <- nrow(node_data)

      #   # Initialize abundance (use a copy)
      #   abundance <- node_data$abundance

      #   # Store results for all timesteps
      #   results <- data.frame(
      #     timestep = integer(),
      #     pt_graph = integer(),
      #     x = numeric(),
      #     y = numeric(),
      #     abundance = numeric()
      #   )

      #   # Record initial state (t=0)
      #   results <- rbind(
      #     results,
      #     data.frame(
      #       timestep = 0,
      #       pt_graph = node_data$pt_graph,
      #       x = node_data$x,
      #       y = node_data$y,
      #       abundance = abundance
      #     )
      #   )

      #   # Run simulation
      #   for (t in 1:n_timesteps) {
      #     new_abundance <- abundance

      #     for (i in 1:n_nodes) {
      #       # Skip if no neighbors
      #       if (length(neighbors[[i]]) == 0) {
      #         next
      #       }

      #       # Calculate outflow
      #       outflow <- abundance[i] * fixed_rates[i]

      #       if (outflow > 0) {
      #         # Remove from current node
      #         new_abundance[i] <- new_abundance[i] - outflow

      #         # Distribute equally to neighbors
      #         per_neighbor <- outflow / length(neighbors[[i]])
      #         for (j in neighbors[[i]]) {
      #           new_abundance[j] <- new_abundance[j] + per_neighbor
      #         }
      #       }
      #     }

      #     # Ensure no negative abundances
      #     new_abundance <- pmax(new_abundance, 0)
      #     abundance <- new_abundance

      #     # Record state
      #     results <- rbind(
      #       results,
      #       data.frame(
      #         timestep = t,
      #         pt_graph = node_data$pt_graph,
      #         x = node_data$x,
      #         y = node_data$y,
      #         abundance = abundance
      #       )
      #     )

      #     if (verbose && t %% 10 == 0) {
      #       cat(sprintf("  Completed timestep %d/%d\n", t, n_timesteps))
      #     }
      #   }

      #   if (verbose) {
      #     cat("Simulation complete.\n")
      #   }

      #   return(list(
      #     results = results,
      #     node_data = node_data,
      #     n_timesteps = n_timesteps
      #   ))
      # }

      # plot_comparison_points <- function(sim_result, point_size = 0.5) {
      #   results <- sim_result$results
      #   n_timesteps <- sim_result$n_timesteps

      #   # Get initial and final states
      #   initial <- results %>%
      #     filter(timestep == 0) %>%
      #     mutate(state = "Initial (t=0)")

      #   final <- results %>%
      #     filter(timestep == n_timesteps) %>%
      #     mutate(state = sprintf("Final (t=%d)", n_timesteps))

      #   plot_data <- rbind(initial, final)
      #   plot_data$state <- factor(
      #     plot_data$state,
      #     levels = c("Initial (t=0)", sprintf("Final (t=%d)", n_timesteps))
      #   )

      #   # Calculate pattern correlation
      #   initial_abund <- initial$abundance[order(initial$pt_graph)]
      #   final_abund <- final$abundance[order(final$pt_graph)]
      #   pattern_corr <- cor(initial_abund, final_abund)

      #   # Use same color scale for both
      #   abund_range <- range(plot_data$abundance)

      #   p <- ggplot(plot_data, aes(x = x, y = y, color = abundance)) +
      #     geom_point(size = point_size) +
      #     scale_color_viridis_c(
      #       option = "plasma",
      #       name = "Relative\nAbundance",
      #       limits = abund_range
      #     ) +
      #     facet_wrap(~state, ncol = 2) +
      #     coord_fixed() +
      #     labs(
      #       title = "Abundance Distribution: Initial vs Final",
      #       subtitle = sprintf(
      #         "Pattern correlation: %.4f | %d timesteps",
      #         pattern_corr,
      #         n_timesteps
      #       ),
      #       x = "Longitude",
      #       y = "Latitude"
      #     ) +
      #     theme_minimal() +
      #     theme(
      #       plot.title = element_text(face = "bold", hjust = 0.5),
      #       plot.subtitle = element_text(hjust = 0.5),
      #       strip.text = element_text(face = "bold", size = 12),
      #       panel.grid = element_line(color = "gray90")
      #     )

      #   return(p)
      # }

      # # Run simulation
      # sim_result <- simulate_movement_grid(eq_result, n_timesteps = 100)
      # # Plot comparison (initial vs final)
      # comparison_plot <- plot_comparison_points(sim_result, point_size = 0.5)
      # print(comparison_plot)

      # Convert results to DISPLACE expected format
      popsspe_coeffs_semester_this_pop <- eq_result$rates_df |>
        left_join(
          avai_allszgroups_this_sp |> filter(semester == a.semester),
          by = "pt_graph"
        ) |>
        dplyr::select(pt_graph, coeff, avai)

      # Adjust to C++
      popsspe_coeffs_semester_this_pop$pt_graph <- as.numeric(as.character(
        popsspe_coeffs_semester_this_pop$pt_graph
      )) -
        1 ##!!! OFFSET FOR C++ !!!##

      popsspe_coeffs_semester_this_pop <- orderBy(
        ~pt_graph,
        data = popsspe_coeffs_semester_this_pop
      )

      if (
        any(
          !popsspe_coeffs_semester_this_pop[, "pt_graph"] %in%
            unique(popsspe_coeffs_semester_this_pop$pt_graph)
        )
      ) {
        print(
          "inconsistency here: fix it to avoid a deadly DISPLACE runtime error. Missing nodes:"
        )
        dd <- popsspe_coeffs_semester_this_pop[, "pt_graph"]
        dd[!dd %in% unique(popsspe_coeffs_semester_this_pop$pt_graph)]
        stop()
      }

      # reduce a bit the dimensionality...
      # COMMENTED 2026-01-22 This is not needed with the latest DISPLACE update that runs faster
      # nodes_to_keep <- popsspe_coeffs_semester_this_pop[
      #   as.numeric(as.character(popsspe_coeffs_semester_this_pop$coeff)) > 0.1,
      #   "pt_graph"
      # ]
      # popsspe_coeffs_semester_this_pop <- popsspe_coeffs_semester_this_pop[
      #   popsspe_coeffs_semester_this_pop$pt_graph %in% nodes_to_keep,
      # ]

      write.table(
        popsspe_coeffs_semester_this_pop[, c('pt_graph', 'coeff')], # the szgroup dim is implicit....
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          "static_avai",
          paste(
            popids,
            "spe_field_of_coeff_diffusion_this_pop_nodes_semester",
            gsub("S", "", a.semester),
            "_biolsce1.dat",
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
        "Write",
        popids,
        "spe_field_of_coeff_diffusion_this_pop_nodes_semester",
        gsub("S", "", a.semester),
        ".dat....done \n"
      ))
    }

    #-----------------------
    for (a.semester in c("S1", "S2")) {
      # to go with Diffuse N coefficients

      nbhours_for_distance_internodes <- 730 # i.e. 8761/12 => ca. every month, assuming uniform distance DISPLACE grid nodes

      write.table(
        data.frame(
          pop = popids,
          nbhours_for_distance_internodes = nbhours_for_distance_internodes
        ), # the szgroup dim is implicit....
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          "static_avai",
          paste(
            popids,
            "spe_nbhours_for_distance_internodes_this_pop_semester",
            gsub("S", "", a.semester),
            "_biolsce1.dat",
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
        "Write",
        popids,
        "spe_nbhours_for_distance_internodes_this_pop_semester",
        gsub("S", "", a.semester),
        ".dat....done \n"
      ))
    }

    cat(paste("Process ", a_file, "....done \n"))
  } else {
    cat(paste(
      "....a config file is found but the stock is not in the list....config ignored\n"
    ))
  }
} # end a_file


# finally:
for (a.semester in c("S1", "S2")) {
  lst_nodes <- lst_nodes_idx_distrib[
    lst_nodes_idx_distrib$semester == a.semester,
    c('popid', 'pt_graph')
  ]
  lst_nodes <- orderBy(~popid, data = lst_nodes)

  write.table(
    lst_nodes, # the szgroup dimension is removed....
    file = file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = ''),
      "static_avai",
      paste(
        "lst_idx_nodes_per_pop_semester",
        gsub("S", "", a.semester),
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
    "Write lst_idx_nodes_per_pop_semester",
    gsub("S", "", a.semester),
    ".dat....done \n"
  ))
}


cat(paste("....done \n"))
