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
      library(terra)
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

      # extract abundance (or whatever name)
      e <- terra::ext(terra::vect(shp[name_gis_layer_field]))
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
      shp_WGS84_rast_values_on_coord <- extract(
        shp_WGS84_rast,
        as.matrix(coord[, c(1, 2)])
      )

      #nb nodes with presence for this stock
      sum(shp_WGS84_rast_values_on_coord[[name_gis_layer_field]], na.rm = TRUE)

      #!#!#!#!#!#!#!
      #!#!#!#!#!#!#!
      #!#!#!#!#!#!#!
      # CAUTION: TO BE REMOVED/ADAPTED WHEN THE DYNAMIC MODELLING OF POPS WILL BE IMPLEMENTED:
      # reomve some presence on nodes to greatly speed up the shapefile creation, which will anyway not be used when pop are just implicit.
      # shp_WGS84_rast_values_on_coord$abundance   <- sapply(shp_WGS84_rast_values_on_coord$abundance, function(x) {idx <- rbinom(length(x[x==1]), 1, (1-prop_to_keep) ); x[x==1 & idx] <- NA; x})
      #=> keep 40% of presence randomly, just to make sure to create the required DISPLACE input avai.dat files for the demonstrator app
      #=> also avoid using the 30000 nodes where we know only a part of it would cover spat distrib of the stocks
      #!#!#!#!#!#!#!
      #!#!#!#!#!#!#!
      #!#!#!#!#!#!#!

      # FINAL nb nodes with presence for this stock
      cat(paste(
        "NB OF NODES KEPT-----------------",
        sum(!is.na(shp_WGS84_rast_values_on_coord[[name_gis_layer_field]])),
        "\n",
        "TOTAL ABUNDANCE CONSIDERED-----------------",
        sum(
          shp_WGS84_rast_values_on_coord[[name_gis_layer_field]],
          na.rm = TRUE
        )
      ))

      coord <- cbind(
        coord,
        GRIDCODE = shp_WGS84_rast_values_on_coord,
        xfold = 1
      )

      # check
      if (do_plot) {
        plot(
          shp_WGS84_rast,
          add = FALSE,
          border = as.data.frame(shp_WGS84_rast)[, name_gis_layer_field]
        )
        points(
          as.numeric(as.character(coord[, "x"])),
          as.numeric(as.character(coord[, "y"])),
          col = 1,
          pch = 16
        )
        points(
          as.numeric(as.character(coord[, "x"])),
          as.numeric(as.character(coord[, "y"])),
          col = as.numeric(coord[, name_gis_layer_field]) + 2,
          pch = 16
        )
      }
      #-------------------------------------------------------------------------------
      #-Produce the AVAI object-------------------------------------------------------
      #-------------------------------------------------------------------------------

      library(doBy)
      avai <- NULL
      an <- function(x) as.numeric(as.character(x))
      # loop per semester--------
      for (a.semester in c("S1", "S2")) {
        # dispatch the abundance among nodes by dividing 'abundance' per the number of included graph nodes
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
      avai_allszgroups_this_sp$avai <- round(avai_allszgroups_this_sp$avai, 8)

      # a check
      #tapply(an(avai_allszgroups$avai), list(avai_allszgroups$semester, avai_allszgroups$szgroups), sum, na.rm=TRUE  ) # should be full of 1

      # save .dat files
      avai_allszgroups_this_sp$pt_graph <- as.numeric(as.character(
        avai_allszgroups_this_sp$pt_graph
      )) -
        1 ##!!! OFFSET FOR C++ !!!##
      avai_allszgroups_this_sp <- orderBy(
        ~pt_graph,
        data = avai_allszgroups_this_sp
      )

      # ...and collect stock presence/absence distribution
      idx_nodes <- unique(avai_allszgroups_this_sp[
        avai_allszgroups_this_sp$semester == a.semester,
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

      popsspe_avai_semester <- avai_allszgroups_this_sp[
        avai_allszgroups_this_sp$semester == a.semester,
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

      popsspe_avai_semester_this_pop_these_sz <- avai_allszgroups_this_sp[
        avai_allszgroups_this_sp$semester == a.semester &
          avai_allszgroups_this_sp$szgroups %in% selected_szgroups,
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
    for (a.semester in c("S1", "S2")) {
      # Diffuse N coefficients - here, avai used as a proxy for long/short residence time
      popsspe_coeffs_semester_this_pop <- popsspe_avai_semester

      some_breaks <- quantile(
        popsspe_coeffs_semester_this_pop$avai[
          popsspe_coeffs_semester_this_pop$avai != 0
        ],
        prob = c(0, 0.5, 0.75, 1)
      )

      # debug
      if (length(unique(some_breaks)) != length(some_breaks)) {
        rn <- runif(4, 0, 0.00001)
        some_breaks <- some_breaks + rn[order(rn, decreasing = FALSE)]
      }

      popsspe_coeffs_semester_this_pop$quant <- cut(
        popsspe_coeffs_semester_this_pop$avai + .00000001,
        breaks = some_breaks
      ) # just arbitrary values for this example!
      popsspe_coeffs_semester_this_pop$quant[is.na(
        popsspe_coeffs_semester_this_pop$quant
      )] <- levels(popsspe_coeffs_semester_this_pop$quant)[1] # AVOID NAs by all means!

      popsspe_coeffs_semester_this_pop$coeff <- popsspe_coeffs_semester_this_pop$quant
      levels(popsspe_coeffs_semester_this_pop$coeff) <- c(0.5, 0.1, 0.05) # just arbitrary for this example!

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
        "spe_field_of_coeff_diffusion_this_pop_nodes_semester",
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
