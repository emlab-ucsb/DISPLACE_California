# GENERAL SETTINGS
#
#     if (length(args) < 2) {
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
#

dir.create(file.path(
  general$main.path.ibm,
  paste("harboursspe_", general$application, sep = '')
))


### build quick and dirty fake harbour files

port_names <- read.table(
  file = file.path(
    general$main_path_gis,
    "GRAPH",
    paste("harbours.dat", sep = '')
  ),
  sep = ";",
  row.names = NULL,
  header = TRUE
)
cat(paste("Read port names...done\n"))


# a quick check of consistent port_names file
max_idx <- max(port_names$idx_port)
c(1:max_idx)[!c(1:max_idx) %in% port_names$idx.port] # should return integer(0)


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
coord <- cbind(x, y, harb, pt_graph = 1:length(x))
dd <- coord

# For older versions of R
# coord <- read.table(file=file.path(general$main_path_gis, "GRAPH",  "shapefiles_and_graphs", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
# dd    <- coord
# coord <- as.matrix(as.vector(coord))
# coord <- matrix(coord, ncol=3)
# coord <- cbind(coord, 1:nrow(coord))
# colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')
# if(do_plot) plot(coord[,1], coord[,2])
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
# graph <- read.table(file=file.path(general$main_path_gis, "GRAPH", "shapefiles_and_graphs", paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
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

cat(paste("Read graph...done\n"))

port_names <- port_names[!duplicated(port_names$port_name), ]
rownames(port_names) <- port_names[, 1]
port_names$pt_graph <- coord[
  match(port_names$idx_port, coord[, "harb"]),
  "pt_graph"
]
port_names$pt_graph <- as.numeric(as.character(port_names$pt_graph)) - 1 ##!!! FOR C++ !!!##


##-------------
#dep <-  ((nrow(dd)/3)+1) -nrow(port_names)
#idx <-  dep : (nrow(dd)/3)   # all idx nodes for ports

#idx <- idx - 1 ##!!## CAUTION OFFSET BY 1 in C++ ##!!##

##------------------------

# stock names
spp_table <- read.table(
  file = file.path(
    general$main_path_gis,
    "POPULATIONS",
    paste("pop_names_", general$application, ".txt", sep = '')
  ),
  header = TRUE
)
spp <- as.character(spp_table$spp)

cat(paste("Read stock names...done\n"))


##------------------------
# obtain a c++ multimap with stock / price for cat
prices_per_szcat <- read.table(
  file = file.path(
    general$main_path_gis,
    "POPULATIONS",
    paste("Stock_prices_data.csv")
  ),
  sep = ',',
  header = TRUE
)
#head(prices_per_szcat)
#   stock small medium large
#1 MA1.nor   0.4    0.4   0.4
#2 MA2.ice   0.4    0.4   0.4
#3 MA3.bob   0.4    0.4   0.4
#4 BH1.nor   0.4    0.4   0.4
#5 BH2.ice   0.4    0.4   0.4

cat(paste("Read stock prices per commercial category...done\n"))


for (i in port_names$pt_graph) {
  prices_per_species_per_cat <- prices_per_szcat[, 2:4] # price in euro per kilo for three size (small, medium, and large)
  # => ......default

  if (i == i) {
    prices_per_species_per_cat <- prices_per_species_per_cat
  }

  write.table(
    cbind(
      stock = rep(0:(length(spp) - 1), each = 3),
      price_per_cat = c(t(as.matrix(prices_per_species_per_cat)))
    ),
    file = file.path(
      general$main.path.ibm,
      paste("harboursspe_", general$application, sep = ''),
      paste(i, "_quarter1_each_species_per_cat.dat", sep = "")
    ),
    row.names = FALSE,
    quote = FALSE
  )
  write.table(
    cbind(
      stock = rep(0:(length(spp) - 1), each = 3),
      price_per_cat = c(t(as.matrix(prices_per_species_per_cat)))
    ),
    file = file.path(
      general$main.path.ibm,
      paste("harboursspe_", general$application, sep = ''),
      paste(i, "_quarter2_each_species_per_cat.dat", sep = "")
    ),
    row.names = FALSE,
    quote = FALSE
  )
  write.table(
    cbind(
      stock = rep(0:(length(spp) - 1), each = 3),
      price_per_cat = c(t(as.matrix(prices_per_species_per_cat)))
    ),
    file = file.path(
      general$main.path.ibm,
      paste("harboursspe_", general$application, sep = ''),
      paste(i, "_quarter3_each_species_per_cat.dat", sep = "")
    ),
    row.names = FALSE,
    quote = FALSE
  )
  write.table(
    cbind(
      stock = rep(0:(length(spp) - 1), each = 3),
      price_per_cat = c(t(as.matrix(prices_per_species_per_cat)))
    ),
    file = file.path(
      general$main.path.ibm,
      paste("harboursspe_", general$application, sep = ''),
      paste(i, "_quarter4_each_species_per_cat.dat", sep = "")
    ),
    row.names = FALSE,
    quote = FALSE
  )

  cat(paste(
    "Write ",
    i,
    "_quarterXX_each_species_per_cat.dat in /harboursspe...done\n"
  ))
}


write.table(
  cbind(
    node = port_names$pt_graph,
    name = sapply(as.character(port_names$port_name), function(x) {
      paste(unlist(strsplit(x, " ")), collapse = "_")
    })
  ),
  file = file.path(
    general$main.path.ibm,
    paste("harboursspe_", general$application, sep = ''),
    "names_harbours.dat"
  ),
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

cat(paste("Write names_harbours.dat in /harboursspe...done\n"))


# obtain a c++ map with fuel price per vessel size category
for (i in port_names$pt_graph) {
  write.table(
    cbind(vessel_size = 0:4, fuel_price = c(1.28, 1.28, 1.28, 1.28, 1.28)), # dollar per litre
    file = file.path(
      general$main.path.ibm,
      paste("harboursspe_", general$application, sep = ''),
      paste(i, "_quarter1_fuel_price_per_vessel_size.dat", sep = "")
    ),
    row.names = FALSE,
    quote = FALSE
  )
  write.table(
    cbind(vessel_size = 0:4, fuel_price = c(1.28, 1.28, 1.28, 1.28, 1.28)), # dollar per litre
    file = file.path(
      general$main.path.ibm,
      paste("harboursspe_", general$application, sep = ''),
      paste(i, "_quarter2_fuel_price_per_vessel_size.dat", sep = "")
    ),
    row.names = FALSE,
    quote = FALSE
  )
  write.table(
    cbind(vessel_size = 0:4, fuel_price = c(1.28, 1.28, 1.28, 1.28, 1.28)), # dollar per litre
    file = file.path(
      general$main.path.ibm,
      paste("harboursspe_", general$application, sep = ''),
      paste(i, "_quarter3_fuel_price_per_vessel_size.dat", sep = "")
    ),
    row.names = FALSE,
    quote = FALSE
  )
  write.table(
    cbind(vessel_size = 0:4, fuel_price = c(1.28, 1.28, 1.28, 1.28, 1.28)), # euro per litre
    file = file.path(
      general$main.path.ibm,
      paste("harboursspe_", general$application, sep = ''),
      paste(i, "_quarter4_fuel_price_per_vessel_size.dat", sep = "")
    ),
    row.names = FALSE,
    quote = FALSE
  )

  cat(paste(
    "Write ",
    i,
    "_quarterXX__fuel_price_per_vessel_size.dat in /harboursspe...done\n"
  ))
} #=> possibly, adapt this to account for spatial country specific variability in fuel price by detecting what is the country the port is lying in...


write.table(
  cbind(
    node = port_names$pt_graph,
    name = sapply(as.character(port_names$port_name), function(x) {
      paste(unlist(strsplit(x, " ")), collapse = "_")
    })
  ),
  file = file.path(
    general$main.path.ibm,
    paste("harboursspe_", general$application, sep = ''),
    "names_harbours.dat"
  ),
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

cat(paste("Write names_harbours.dat in /harboursspe...done\n"))


cat(paste("..........done\n"))
