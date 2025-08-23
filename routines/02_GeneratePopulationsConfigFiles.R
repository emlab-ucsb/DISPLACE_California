cat(paste("START \n"))

dir.create(
  path = file.path(general$main_path_gis, "POPULATIONS", "pops_config_files")
)
cat(paste("Creating a /POPULATIONS/pops_config_files folder....done \n"))


path <- file.path(general$main_path_gis, "POPULATIONS", "SpatialLayers")
namefiles <- list.files(file.path(path))
cat(paste("Entering /POPULATIONS/SpatialLayers folder....done \n"))


# parse
dd <- data.frame()
for (a_file in namefiles) {
  infos <- unlist(strsplit(a_file, split = "_"))
  popid <- as.numeric(gsub("contour", "", infos[1]))
  size <- infos[2]
  if (length(grep(".dbf", infos[3])) != 0) {
    sizegroups <- as.character(gsub(".dbf", "", infos[3]))
  }
  if (length(grep(".shx", infos[3])) != 0) {
    sizegroups <- as.character(gsub(".shx", "", infos[3]))
  }
  if (length(grep(".shp", infos[3])) != 0) {
    sizegroups <- as.character(gsub(".shp", "", infos[3]))
  }
  dd <- rbind.data.frame(dd, cbind(popid, size, sizegroups))
}
colnames(dd) <- c("popid", "size", "sizegroups")
dd <- dd[!duplicated(data.frame(dd$popid, dd$size, dd$sizegroups)), ]

cat(paste("Parsing GIS layer file name....done \n"))


for (popid in as.numeric(as.character(unique(dd$popid)))) {
  cat(paste("popid", popid, "\n"))

  # create a config file
  szgroups <- gsub(
    "-",
    " ",
    paste(
      dd[dd$popid == popid & dd$size == "small", "sizegroups"],
      dd[dd$popid == popid & dd$size == "medium", "sizegroups"],
      dd[dd$popid == popid & dd$size == "large", "sizegroups"],
      sep = "_"
    )
  ) # 14 size groups
  selected_szgroups <- c(2, 5, 7, 9)
  name_gis_file_for_total_abundance_per_polygon <- c(
    paste(
      "SpatialLayers/contour",
      popid,
      "_small_",
      dd[dd$popid == popid & dd$size == "small", "sizegroups"],
      sep = ''
    ),
    paste(
      "SpatialLayers/contour",
      popid,
      "_medium_",
      dd[dd$popid == popid & dd$size == "medium", "sizegroups"],
      sep = ''
    ),
    paste(
      "SpatialLayers/contour",
      popid,
      "_large_",
      dd[dd$popid == popid & dd$size == "large", "sizegroups"],
      sep = ''
    )
  ) # in 3 size categories
  name_gis_layer_field <- "GRIDCODE" # e.g. giving occurences in polygon
  # name_gis_layer_field                          <- "layer"    # e.g. giving occurences in polygon
  is_gis_layer_field_relative_numbers <- FALSE # if relative categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
  xfold_gis_layer_field <- c(1, 1, 1, 1, 1) # [not used if is_gis_layer_field_relative_numbers is FALSE]

  do_append <- TRUE # caution here...used for lst_idx_nodes_per_pop_semesterXX.dat file

  a_comment <- popid
  namefile <- file.path(
    general$main_path_gis,
    "POPULATIONS",
    "pops_config_files",
    paste(
      a_comment,
      "pops_creator_args_",
      general$application,
      ".dat",
      sep = ''
    )
  )

  write(
    "# config file for the Object editor: adding some population(s)",
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
    "# input folder for config file",
    file = namefile,
    ncolumns = 1,
    append = TRUE
  )
  write(general$main_path_gis, file = namefile, ncolumns = 1, append = TRUE)

  write(
    "# output folder for parameterisation file",
    file = namefile,
    ncolumns = 1,
    append = TRUE
  )
  write(general$main.path.ibm, file = namefile, ncolumns = 1, append = TRUE)

  write(
    "# input folder for DISPLACE",
    file = namefile,
    ncolumns = 1,
    append = TRUE
  )
  write(general$main.path.ibm, file = namefile, ncolumns = 1, append = TRUE)

  write(
    "# name of the application",
    file = namefile,
    ncolumns = 1,
    append = TRUE
  )
  write(general$application, file = namefile, ncolumns = 1, append = TRUE)

  write(
    "# name of the graph for this application",
    file = namefile,
    ncolumns = 1,
    append = TRUE
  )
  write(general$igraph, file = namefile, ncolumns = 1, append = TRUE)

  write(
    "# append to existing pop files",
    file = namefile,
    ncolumns = 1,
    append = TRUE
  )
  write(do_append, file = namefile, ncolumns = 1, append = TRUE)

  write(
    "# name gis file for total abundance per polygon",
    file = namefile,
    ncolumns = length(name_gis_file_for_total_abundance_per_polygon),
    append = TRUE
  )
  write(
    name_gis_file_for_total_abundance_per_polygon,
    file = namefile,
    ncolumns = length(name_gis_file_for_total_abundance_per_polygon),
    append = TRUE
  )

  write("# name_gis_layer_field", file = namefile, ncolumns = 1, append = TRUE)
  write(name_gis_layer_field, file = namefile, ncolumns = 1, append = TRUE)

  write(
    "# is_gis_layer_field_relative_numbers",
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

  write("# xfold_gis_layer_field", file = namefile, ncolumns = 1, append = TRUE)
  write(
    xfold_gis_layer_field,
    file = namefile,
    ncolumns = length(xfold_gis_layer_field),
    append = TRUE
  )

  write("# popid", file = namefile, ncolumns = 1, append = TRUE)
  write(popid, file = namefile, ncolumns = length(popid), append = TRUE)

  write("# all size groups", file = namefile, ncolumns = 1, append = TRUE)
  write(szgroups, file = namefile, ncolumns = length(szgroups), append = TRUE)

  write("# selected size groups", file = namefile, ncolumns = 1, append = TRUE)
  write(
    selected_szgroups,
    file = namefile,
    ncolumns = length(selected_szgroups),
    append = TRUE
  )
}

# Added from GeneratePopulationsAvaiGISLayersFromStockAvai.R

cat(paste("Generate comcat_per_szgroup_done_by_hand.dat"))
cat(paste("Make sure to update it manually..."))

# UPDATED 2025-04-21
# Define species and code form pop_names file
table_spp <- read.table(
  file.path(
    general$main_path_gis,
    "POPULATIONS",
    paste("pop_names_", general$application, ".txt", sep = '')
  ),
  header = TRUE,
  stringsAsFactors = FALSE
)
species_list <- table_spp |> dplyr::pull(spp)

# Create size category list
all_sizes <- 0:13

# Define the breaks
size_breaks <- list(
  small = 0:5,
  medium = 6:9,
  large = 10:13
)

# Generate the list
size_cats <- list()

for (spp in species_list) {
  size_cats[[spp]] <- list()
  for (cat in names(size_breaks)) {
    size_cats[[spp]][[cat]] <- as.character(size_breaks[[cat]])
  }
}

# This could be also done manually
#  size_cats <- list()
#  size_cats[["SAB"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
#  size_cats[["SAB"]] [["medium"]] <- c("6","7","8", "9")
#  size_cats[["SAB"]] [["large"]]  <- c("10", "11", "12", "13")
#  size_cats[["SJU"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
#  size_cats[["SJU"]] [["medium"]] <- c("6","7","8", "9")
#  size_cats[["SJU"]] [["large"]]  <- c("10", "11", "12", "13")
#  size_cats[["SJZ"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
#  size_cats[["SJZ"]] [["medium"]] <- c("6","7","8", "9")
#  size_cats[["SJZ"]] [["large"]]  <- c("10", "11", "12", "13")
#  size_cats[["MIP"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
#  size_cats[["MIP"]] [["medium"]] <- c("6","7","8", "9")
#  size_cats[["MIP"]] [["large"]]  <- c("10", "11", "12", "13")

count <- 0
comcat_df <- NULL
for (sp in 1:length(spp)) {
  count <- count + 1
  dd <- unlist(lapply(size_cats[[sp]], length))
  comcat_per_szgroup <- c(
    rep(0, dd[1]),
    rep(1, dd[2]),
    rep(2, 14 - (dd[1] + dd[2]))
  )
  stock <- rep(sp - 1, 14)

  comcat_df <- rbind(cbind(stock, comcat_per_szgroup), comcat_df)
}

write.table(
  comcat_df,
  file = file.path(
    general$main.path.ibm,
    paste("popsspe_", general$application, sep = ''),
    paste("comcat_per_szgroup_done_by_hand.dat", sep = ' ')
  ),
  sep = " ",
  col.names = TRUE,
  row.names = FALSE,
  quote = FALSE
)

cat(paste("....done \n"))
