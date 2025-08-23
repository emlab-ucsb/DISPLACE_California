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
#-----read input config file----------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# read a aggregation per fleet segment build from STECF AER (see in FISHERIES/STECF folder)
filename <- file.path(
  general$main_path_gis,
  "FISHERIES",
  "STECF",
  "Economics_fs.csv"
)
cnts <- count.fields(filename, sep = ",")
economics_fs <- read.table(file = filename, sep = ",", header = TRUE)


# retrieve vid - fleetSegment info
filename <- file.path(
  general$main_path_gis,
  "FISHERIES",
  "vessels_specifications_per_harbour_metiers.csv"
)
cnts <- count.fields(filename, sep = ",")
vessel_specifications <- read.table(file = filename, sep = ",", header = TRUE)
vessel_specifications <- cbind.data.frame(
  vessel_specifications,
  id = 1:nrow(vessel_specifications)
)
cat(paste("Read vessels_specifications_per_harbour_metiers.csv \n"))
#idx <- vessel_specifications$VE_LEN <12
#vessel_specifications$VesselId <- as.character(vessel_specifications$VesselId)
#vessel_specifications[idx , "VesselId"]  <-  paste0(vessel_specifications[idx , "VesselId"],  vessel_specifications[idx , "Port"], vessel_specifications[idx , "Gear"])

# we need to remove the area info from the FleetSeg definition before merging...
vessel_specifications$VesselSize <- cut(
  vessel_specifications$VE_LEN,
  breaks = c(0, 11.99, 17.99, 23.99, 39.99, 100),
  right = FALSE
) #12-18, 18-24, 24-40, o40
levels(vessel_specifications$VesselSize) <- c(
  "0012",
  "1218",
  "1824",
  "2440",
  "40XX"
)
vessel_specifications$FleetSeg <- paste0(
  substring(vessel_specifications$VesselId, 1, 3),
  "_",
  vessel_specifications$Gear,
  vessel_specifications$VesselSize
)


# then merge...
vesspeeco <- merge(
  vessel_specifications,
  economics_fs,
  by.x = "VesselId",
  by.y = "fs_name"
) # the original line used  NearestFleetSeg instead of VesselId?

# check
length(unique(vessel_specifications$VesselId))
length(unique(vesspeeco$VesselId))
nrow(vessel_specifications)
nrow(vesspeeco)

# obtain a cost structure per vessel...(i.e. dividing the agg by nb of vessels for the relevant variables)
# ...and multiply for the particular case of "super individuals" lines (i.e. the small boats)
vesspeeco$Nb_crew <- vesspeeco$Nb_crew / vesspeeco$Nb_Vessels
vesspeeco$Annual_other_income <- vesspeeco$Annual_other_income /
  vesspeeco$Nb_Vessels
vesspeeco$Landing_costs_percent <- abs(vesspeeco$Landing_costs_percent)
vesspeeco$Other_annual_fixed_costs <- vesspeeco$Other_annual_fixed_costs /
  vesspeeco$Nb_Vessels
vesspeeco$Vessel_value <- vesspeeco$Vessel_value / vesspeeco$Nb_Vessels
vesspeeco$Nb_Vessels <- 1 # finally

idx <- vesspeeco$VE_LEN < 12
vesspeeco[idx, ]$Annual_other_income <- vesspeeco[idx, ]$Annual_other_income *
  vesspeeco[idx, ]$nb_vessels
vesspeeco[idx, ]$Landing_costs_percent <- abs(
  vesspeeco[idx, ]$Landing_costs_percent
)
vesspeeco[idx, ]$Other_annual_fixed_costs <- vesspeeco[
  idx,
]$Other_annual_fixed_costs *
  vesspeeco[idx, ]$nb_vessels
vesspeeco[idx, ]$Vessel_value <- vesspeeco[idx, ]$Vessel_value *
  vesspeeco[idx, ]$nb_vessels


# check vids and be sure all vid is present:
nameobj <- paste("vesselsspe_features_quarter1.dat", sep = '') #....and possibly per vid!
vesselsspe_features <- read.table(
  file.path(
    general$main.path.ibm,
    paste("vesselsspe_", general$application, sep = ""),
    nameobj
  ),
  header = FALSE,
  sep = "|"
)


# check
allvids <- as.character(vesselsspe_features[, 1])
er <- allvids[!allvids %in% as.character(vessel_specifications$VesselId)] # should return character(0)
er2 <- allvids[!allvids %in% as.character(vesspeeco$VesselId)] # should return character(0)
if (length(er) != 0) {
  stop("We should not lose any vid! correct and redo")
}
if (length(er2) != 0) {
  stop("We should not lose any vid! correct and redo")
}

if (any(!vesspeeco$VesselId %in% allvids)) {
  print(
    "Too many vessels informed in the eco dataset compare to the one in vesselsspe_features...Remove them"
  )
}
vesspeeco <- vesspeeco[vesspeeco$VesselId %in% allvids, ] # correct


#vessel_specifications[vessel_specifications$VesselId=="SWE000010025",]

# keep the relevant economic data only
relevant_eco_variables <- c(
  "Nb_crew",
  "Annual_other_income",
  "Landing_costs_percent",
  "Crewshare_and_unpaid_labour_costs_percent",
  "Other_variable_costs_per_unit_effort",
  "Annual_insurance_costs_per_crew",
  "Standard_labour_hour_opportunity_costs",
  "Standard_annual_full_time_employment_hours",
  "Other_annual_fixed_costs",
  "Vessel_value",
  "Annual_depreciation_rate",
  "Opportunity_interest_rate",
  "Annual_discount_rate"
)


# aggregate because possibly several lines if polyvalent vessels
#vesspeeco <- aggregate(vesspeeco[, relevant_eco_variables], list(VesselId=vesspeeco$VesselId), mean)
# replaced by a weighted average aggregation:
# library(data.table)
DT <- data.table::data.table(vesspeeco)
vesspeeco <- as.data.frame(
  cbind(
    DT[, list(Nb_crew = weighted.mean(Nb_crew, RelativeEffort)), by = VesselId],
    DT[,
      list(
        Annual_other_income = weighted.mean(Annual_other_income, RelativeEffort)
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Landing_costs_percent = weighted.mean(
          Landing_costs_percent,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Crewshare_and_unpaid_labour_costs_percent = weighted.mean(
          Crewshare_and_unpaid_labour_costs_percent,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Other_variable_costs_per_unit_effort = weighted.mean(
          Other_variable_costs_per_unit_effort,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Annual_insurance_costs_per_crew = weighted.mean(
          Annual_insurance_costs_per_crew,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Standard_labour_hour_opportunity_costs = weighted.mean(
          Standard_labour_hour_opportunity_costs,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Standard_annual_full_time_employment_hours = weighted.mean(
          Standard_annual_full_time_employment_hours,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Other_annual_fixed_costs = weighted.mean(
          Other_annual_fixed_costs,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(Vessel_value = weighted.mean(Vessel_value, RelativeEffort)),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Annual_depreciation_rate = weighted.mean(
          Annual_depreciation_rate,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Opportunity_interest_rate = weighted.mean(
          Opportunity_interest_rate,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2],
    DT[,
      list(
        Annual_discount_rate = weighted.mean(
          Annual_discount_rate,
          RelativeEffort
        )
      ),
      by = VesselId
    ][, 2]
  )
)


# reorder the same
rownames(vesspeeco) <- vesspeeco$VesselId
vesspeeco <- vesspeeco[as.character(vesselsspe_features[, 1]), ]


# final check
er3 <- allvids[!allvids %in% as.character(vesspeeco$VesselId)] # should return character(0)
if (length(er3) != 0) {
  stop("We should not lose any vid! correct and redo")
}


# check NAs
er4 <- vesspeeco[!complete.cases(vesspeeco), ]
if (nrow(er4) != 0) {
  stop("No NAs permitted! check and redo")
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----export file as a DISPLACE input file--------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# save .dat files
write.table(
  vesspeeco,
  file = file.path(
    general$main.path.ibm,
    paste("vesselsspe_", general$application, sep = ''),
    paste("vesselsspe_economic_features.dat", sep = '')
  ),
  col.names = FALSE,
  row.names = FALSE,
  quote = FALSE,
  append = FALSE,
  sep = "|"
)

cat(paste("vesselsspe_economic_features.dat....OK", "\n"))
