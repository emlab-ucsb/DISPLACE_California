# some args for the bunch of vessels to be created....
# Usage:
# RunVesselsConfigFiles.R application gis_path input_application_path igraph

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


dir.create(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep='')))
dir.create(file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep='')))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----read input config file----------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

               # DNK000028586
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# read a aggregation per fleet segment build from STECF AER (see in FISHERIES/STECF folder)
filename     <- file.path(general$main_path_gis, "FISHERIES", "STECF", "Economics_fs.csv")
cnts         <- count.fields(filename, sep = ",") 
economics_fs <- read.table(file=filename, sep=",", header=TRUE )


# retrieve vid - fleetSegment info  
filename <- file.path(general$main_path_gis, "FISHERIES", "vessels_specifications_per_harbour_metiers.csv")
cnts     <- count.fields(filename, sep = ",") 
vessel_specifications <- read.table(file=filename, sep=",", header=TRUE )
vessel_specifications <- cbind.data.frame(vessel_specifications, id=1:nrow(vessel_specifications))
cat(paste("Read vessels_specifications_per_harbour_metiers.csv \n"))
#idx <- vessel_specifications$VE_LEN <12 
#vessel_specifications$VesselId <- as.character(vessel_specifications$VesselId)
#vessel_specifications[idx , "VesselId"]  <-  paste0(vessel_specifications[idx , "VesselId"],  vessel_specifications[idx , "Port"], vessel_specifications[idx , "Gear"])


# we need to remove the area info from the FleetSeg definition before merging...
vessel_specifications$VesselSize <- cut(vessel_specifications$VE_LEN, breaks=c(0,11.99, 17.99,23.99,39.99,100), right=FALSE)   #12-18, 18-24, 24-40, o40
levels(vessel_specifications$VesselSize) <- c("0012", "1218", "1824", "2440", "40XX")
vessel_specifications$FleetSeg <- paste0(substring(vessel_specifications$VesselId, 1,3), "_", vessel_specifications$Gear , vessel_specifications$VesselSize)



# Robin Wood approach: 
# unfortunately, because we miss info for some segments (e.g. OTB_SPF etc.) we have to borrow data from other similar segments...
# unique(vessel_specifications$FleetSeg)
 lookup <- c("FRA_DRB1218"="FRA_DRB1218",        "GBR_DRB1218"="GBR_DRB1218",        "NLD_DRB1218"="NLD_DRB2440",        "NLD_GNS1218"="NLD_PG",        "FRA_GNS1218"="FRA_MGP1218",        "SWE_GNS1218"="SWE_DFN1218",        "DEU_GNS1218"="DEU_DFN1218",        "BEL_GNS1218"="BEL_PMP1824",        "NLD_GNS1824"="NLD_DFN1824",        "GBR_GNS1824"="NLD_DFN1824",       
              "BEL_GNS1824"="BEL_PMP1824",        "GBR_OTB1218"="GBR_DTS1218",        "SWE_OTB1218"="SWE_DTS1218",        "FRA_OTB1218"="FRA_DTS1218",        "NLD_OTB1218"="NLD_DFN1218",        "BEL_OTB1218"="FRA_DFN1218",        "GBR_OTB1824"="GBR_DRB1824",        "SWE_OTB1824"="SWE_DTS1824",        "NLD_OTB1824"="NLD_DTS1824",       "BEL_OTB1824"="NLD_DTS1824",       
              "FRA_OTB1824"="FRA_DTS1824",        "DEU_OTB1824"="GBR_DTS1824",        "IRL_OTB1824"="SWE_DTS1824",        "FRA_OTB2440"="FRA_DTS2440",        "NLD_OTB2440"="NLD_DTS2440",        "GBR_OTB2440"="GBR_DTS2440",        "BEL_OTB2440"="BEL_TBB2440",        "SWE_OTB2440"="SWE_DTS2440",        "IRL_OTB2440"="IRL_DTS2440",       "DEU_OTB2440"="DEU_DTS2440",       
              "FIN_OTB2440"="FRA_DTS2440",        "NLD_OTB40XX"="NLD_TMO40XX",        "SWE_OTB40XX"="NLD_TMO40XX",        "GBR_OTB40XX"="GBR_TMO40XX",        "FRA_OTB40XX"="FRA_TMO40XX",        "IRL_OTB40XX"="IRL_TMO40XX",        "DEU_OTB40XX"="NLD_TMO40XX",        "SWE_OTM1218"="SWE_DTS1218",        "FRA_OTM1218"="FRA_DTS1218",        "GBR_OTM1218"="GBR_DTS1824",       
              "NLD_OTM1218"="NLD_DTS1824",        "FRA_OTM1824"="FRA_DTS1824",        "SWE_OTM1824"="SWE_DTS1824",        "NLD_OTM2440"="NLD_DTS2440",        "FRA_OTM2440"="FRA_DTS2440",        "SWE_OTM2440"="SWE_DTS2440",        "GBR_OTM2440"="GBR_DTS2440",       "GBR_OTM40XX"="GBR_TMO40XX",        "SWE_OTM40XX"="NLD_TMO40XX",        "NLD_OTM40XX"="NLD_TMO40XX",       
              "FRA_OTM40XX"="FRA_TMO40XX",        "IRL_OTM40XX"="IRL_TMO40XX",        "SWE_PS40XX"="NLD_TMO40XX",         "GBR_PS40XX"="GBR_TMO40XX",         "FRA_PTM1218"="FRA_DTS1218",        "FRA_PTM1824"="FRA_DTS1824",        "FRA_SDN1824"="FRA_DTS1824",        "SWE_SDN1824"="SWE_DTS1824",        "GBR_SDN1824"="GBR_DTS1824",       "GBR_SSC2440"="GBR_DTS2440",       
              "NLD_SSC2440"="NLD_DTS2440",        "IRL_SSC2440"="IRL_DTS2440",        "BEL_SSC2440"="BEL_DTS2440",        "FRA_SSC2440"="FRA_DTS2440",        "NLD_TBB1218"="NLD_TBB1218",        "GBR_TBB1218"="GBR_TBB1218",        "BEL_TBB1218"="NLD_TBB1218",        "FRA_TBB1218"="NLD_TBB1218",        "DEU_TBB1218"="DEU_TBB1218",        "NLD_TBB1824"="NLD_TBB1824",       
              "BEL_TBB1824"="BEL_TBB1824",        "DEU_TBB1824"="DEU_TBB1824",        "FRA_TBB1824"="DEU_TBB1824",        "GBR_TBB1824"="GBR_TBB1824",        "Sma_DRB0012"="NLD_PG",        "Sma_DRH0012"="NLD_PG",        "Sma_GND0012"="NLD_PG",        "Sma_GNS0012"="NLD_PG",        "Sma_GTN0012"="NLD_PG",        "Sma_GTR0012"="NLD_PG",       
              "Sma_LHM0012"="NLD_PG",        "Sma_LHP0012"="NLD_PG",        "Sma_LLD0012"="NLD_PG",        "Sma_LLS0012"="NLD_PG",        "Sma_LTL0012"="NLD_PG",        "Sma_OTB0012"="NLD_PG",        "Sma_OTT0012"="NLD_PG",        "Sma_SB0012"="NLD_PG",         "DNK_SDN_DEM1218"="DNK_DTS1218",    "DNK_OT_MIX_NEP1218"="DNK_DTS1218",
              "DNK_OT_DMF_PEL1218"="DNK_DTS1218", "DNK_OT_DMF_PEL2440"="DNK_DTS2440", "DNK_OT_DMF2440"="DNK_DTS2440",     "DNK_OT_DEM_PEL2440"="DNK_DTS2440", "DNK_OT_MIX_NEP2440"="DNK_DTS2440", "DNK_SDN_DEM1824"="DNK_DTS1824",    "DNK_OT_DMF1218"="DNK_DTS1218",     "DNK_OT_DMF40XX"="DNK_TMO40XX",    "DNK_OT_DMF1824"="DNK_DTS1824",     "DNK_OT_DMF_PEL1824"="DNK_DTS1824",
              "DNK_OT_MIX_NEP1824"="DNK_DTS1824", "DNK_OT_SPF40XX"="DNK_TMO40XX",     "DNK_DRB_MOL1218"="DNK_DRB1218",    "DNK_OT_DEM_PEL1824"="DNK_DTS1824", "DNK_OT_SPF2440"="DNK_DTS2440",     "DNK_OT_DEM_PEL1218"="DNK_DTS1218", "DNK_OT_CRU1218"="DNK_DTS1218",     "DNK_TBB_CRU1824"="DNK_TBB1824",    "DNK_OT_CRU2440"="DNK_DTS2440",     "DNK_OT_MIX_NEP0012"="DNK_DTS1012",
              "DNK_OT_DMF_PEL0012"="DNK_DTS1012", "DNK_TBB_CRU1218"="DNK_TBB1218",    "DNK_SSC_DEM2440"="DNK_DTS2440",    "DNK_TBB_CRU2440"="DNK_DTS2440",    "DNK_TBB_DMF2440"="DNK_DTS2440",    "DNK_TBB_DMF1218"="DNK_TBB1218",    "DNK_TBB_DMF1824"="DNK_TBB1824",    "DNK_SSC_DEM1824"="DNK_DTS1824",    "DNK_SDN_DEM2440"="DNK_DTS2440",    "DNK_SSC_DEM1218"="DNK_DTS1218",
              
              "DNK_OTM_Iceland40XX"="DNK_TMO40XX", #MEESO
              "DNK_OTM_Norway40XX"="DNK_TMO40XX", #MEESO
              "DNK_OTM_Biscay40XX"="DNK_TMO40XX" #MEESO
               
              )   

              
              
 vessel_specifications$initialFleetSeg <- vessel_specifications$FleetSeg
 vessel_specifications$NearestFleetSeg <- lookup[as.character(vessel_specifications$FleetSeg)]
 vessel_specifications[is.na(vessel_specifications$NearestFleetSeg), "NearestFleetSeg"]  <- 
      vessel_specifications[is.na(vessel_specifications$NearestFleetSeg), "FleetSeg"]
 vessel_specifications$FleetSeg <- vessel_specifications$NearestFleetSeg
 

 
# then merge...
# vesspeeco <- merge(vessel_specifications, economics_fs, by.x="NearestFleetSeg", by.y= "fs_name") # Why by NearestFleetSeg and not VesselId?
vesspeeco <- merge(vessel_specifications, economics_fs, by.x="VesselId", by.y= "fs_name") # the original line used  NearestFleetSeg instead of VesselId?

# check
length(unique(vessel_specifications$VesselId))
length(unique(vesspeeco$VesselId))
nrow(vessel_specifications)
nrow(vesspeeco)

# obtain a cost structure per vessel...(i.e. dividing the agg by nb of vessels for the relevant variables)
# ...and multiply for the particular case of "super individuals" lines (i.e. the small boats) 
vesspeeco$Nb_crew                  <- vesspeeco$Nb_crew / vesspeeco$Nb_Vessels
vesspeeco$Annual_other_income      <- vesspeeco$Annual_other_income / vesspeeco$Nb_Vessels
vesspeeco$Landing_costs_percent    <- abs(vesspeeco$Landing_costs_percent)
vesspeeco$Other_annual_fixed_costs <- vesspeeco$Other_annual_fixed_costs / vesspeeco$Nb_Vessels
vesspeeco$Vessel_value             <- vesspeeco$Vessel_value / vesspeeco$Nb_Vessels
vesspeeco$Nb_Vessels               <- 1 # finally

idx <- vesspeeco$VE_LEN <12 
vesspeeco[idx,]$Annual_other_income      <- vesspeeco[idx,]$Annual_other_income * vesspeeco[idx,]$nb_vessels
vesspeeco[idx,]$Landing_costs_percent    <- abs(vesspeeco[idx,]$Landing_costs_percent)
vesspeeco[idx,]$Other_annual_fixed_costs <- vesspeeco[idx,]$Other_annual_fixed_costs * vesspeeco[idx,]$nb_vessels
vesspeeco[idx,]$Vessel_value             <- vesspeeco[idx,]$Vessel_value * vesspeeco[idx,]$nb_vessels 


# check vids and be sure all vid is present:
nameobj           <- paste("vesselsspe_features_quarter1.dat",sep='')  #....and possibly per vid!
vesselsspe_features <- read.table(file.path(general$main.path.ibm, paste("vesselsspe_",general$application,sep=""), nameobj), header=FALSE, sep="|")

  
# check  
allvids <- as.character(vesselsspe_features[,1])  
er <- allvids[!allvids %in% as.character(vessel_specifications$VesselId)]   # should return character(0)
er2 <- allvids[!allvids %in% as.character(vesspeeco$VesselId)]  # should return character(0)
if(length(er)!=0) stop("We should not lose any vid! correct and redo")   
if(length(er2)!=0) stop("We should not lose any vid! correct and redo")   

if(any(!vesspeeco$VesselId %in%  allvids)) print("Too many vessels informed in the eco dataset compare to the one in vesselsspe_features...Remove them")
vesspeeco <- vesspeeco[vesspeeco$VesselId %in% allvids, ] # correct


#vessel_specifications[vessel_specifications$VesselId=="SWE000010025",]

                                      

# keep the relevant economic data only
relevant_eco_variables <-   c("Nb_crew", "Annual_other_income", "Landing_costs_percent",
                            "Crewshare_and_unpaid_labour_costs_percent",  "Other_variable_costs_per_unit_effort",
                            "Annual_insurance_costs_per_crew", "Standard_labour_hour_opportunity_costs",
                            "Standard_annual_full_time_employment_hours", "Other_annual_fixed_costs",
                            "Vessel_value", "Annual_depreciation_rate", "Opportunity_interest_rate", "Annual_discount_rate")
     

# aggregate because possibly several lines if polyvalent vessels                           
#vesspeeco <- aggregate(vesspeeco[, relevant_eco_variables], list(VesselId=vesspeeco$VesselId), mean)
# replaced by a weighted average aggregation:
# library(data.table)
DT <- data.table::data.table(vesspeeco)
vesspeeco <- as.data.frame(
   cbind(DT[,list(Nb_crew = weighted.mean(Nb_crew, RelativeEffort)),by=VesselId], 
      DT[,list(Annual_other_income = weighted.mean(Annual_other_income, RelativeEffort)),by=VesselId][,2],
      DT[,list(Landing_costs_percent = weighted.mean(Landing_costs_percent, RelativeEffort)),by=VesselId][,2],
      DT[,list(Crewshare_and_unpaid_labour_costs_percent = weighted.mean(Crewshare_and_unpaid_labour_costs_percent, RelativeEffort)),by=VesselId][,2],
      DT[,list(Other_variable_costs_per_unit_effort = weighted.mean(Other_variable_costs_per_unit_effort, RelativeEffort)),by=VesselId][,2],
      DT[,list(Annual_insurance_costs_per_crew = weighted.mean(Annual_insurance_costs_per_crew, RelativeEffort)),by=VesselId][,2],
      DT[,list(Standard_labour_hour_opportunity_costs = weighted.mean(Standard_labour_hour_opportunity_costs, RelativeEffort)),by=VesselId][,2],
      DT[,list(Standard_annual_full_time_employment_hours = weighted.mean(Standard_annual_full_time_employment_hours, RelativeEffort)),by=VesselId][,2],
      DT[,list(Other_annual_fixed_costs = weighted.mean(Other_annual_fixed_costs, RelativeEffort)),by=VesselId][,2],
      DT[,list(Vessel_value = weighted.mean(Vessel_value, RelativeEffort)),by=VesselId][,2],
      DT[,list(Annual_depreciation_rate = weighted.mean(Annual_depreciation_rate, RelativeEffort)),by=VesselId][,2],
      DT[,list(Opportunity_interest_rate = weighted.mean(Opportunity_interest_rate, RelativeEffort)),by=VesselId][,2],
      DT[,list(Annual_discount_rate = weighted.mean(Annual_discount_rate, RelativeEffort)),by=VesselId][,2]
      )
   )
   

 # reorder the same
rownames(vesspeeco) <- vesspeeco$VesselId                                          
vesspeeco <- vesspeeco [as.character(vesselsspe_features[,1]), ]                                          


# final check
er3 <- allvids[!allvids %in% as.character(vesspeeco$VesselId)]  # should return character(0)
if(length(er3)!=0) stop("We should not lose any vid! correct and redo")   


# check NAs 
er4 <- vesspeeco[!complete.cases(vesspeeco),]
if(nrow(er4)!=0) stop("No NAs permitted! check and redo")   




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----export file as a DISPLACE input file--------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


     # save .dat files
       write.table(vesspeeco,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("vesselsspe_economic_features.dat",sep='')),
               col.names=FALSE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = "|")
  
       cat(paste("vesselsspe_economic_features.dat....OK", "\n"))




