 # some args for the bunch of vessels to be created....
 # Usage:
 # GenerateMetierSelectivityPerStockFiles.R application gis_path input_application_path 

 
   # CAUTION: either using the already existing fishing_gear_selectivity_ogives_per_stock.csv file
   # or creating it from scratch from L50 hardcoded L50 parameters...
# 
#    # GENERAL SETTINGS
#  general <- list()
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

 


    a_size_group_bin_in_cm <- 0.5 # caution: hardcoding....
    mid                    <- a_size_group_bin_in_cm/2
   cat(paste("Caution: Hardcoding for size bins....\n"))

   # (caution: give the order for naming stocks in integer from 0 to n-1)
   spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS", 
                           paste("pop_names_", general$application,".txt",sep='')), header=TRUE)
   spp                        <- as.character(spp_table$spp)
   cat(paste("Reading the stock names in", paste(general$main_path_gis, "POPULATIONS", 
                           paste("pop_names_", general$application,".txt",sep='')),"....done \n"))


   dir.create(file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep='')))


   options(scipen=999)


  # reuse the exported metier names in GenerateVesselConfigFiles.R
    metier_names <-  read.table(
       file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''), "metier_names.dat"),
          header=TRUE)

 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 create_selectivity_from_L50_parameters <- function(a_multiplier=1){
 
   ## SELECTIVITY ###################
  # by default, create a fake selectivity ogive i.e. all at 1 (and not species-specific...)
  csv_selectivity_table  <- NULL
  
  params_sel_dem_trawl <- read.table(file.path(general$main_path_gis, "FISHERIES", "gear_selectivities_params_from_probyfish_JDepestele_review_demersal_trawl.csv"), sep=";", header=TRUE)
  params_sel_beam_trawl <- read.table(file.path(general$main_path_gis, "FISHERIES", "gear_selectivities_params_from_probyfish_JDepestele_review_beam_trawl.csv"), sep=";", header=TRUE)
  
  for (met in unique(metier_names$idx) ) {

  selectivities <- NULL
    the_met <- metier_names[metier_names[, 'idx']==met, "name"]
    the_met <- as.character(the_met)

    count <-0
    #for (sp in  sapply(spp, function (spp) substr(spp,1,3)) )  {
    for (sp in  spp)  {
    count <- count+1
    
    sel <- NULL
  
    beam_trawl <- FALSE; demersal_trawl_gear <- FALSE  ; pelagic_gear <- FALSE  ; passive_gear <- FALSE  ; dredge_gear <- FALSE
  
    if (length (grep("BOT", the_met))!=0 ||
        length (grep("BT", the_met))!=0 ||
        length (grep("OTB", the_met))!=0 ||
        length (grep("OT_CRU", the_met))!=0 ||
        length (grep("OT_MIX_NEP", the_met))!=0 ||
        length (grep("OT_DEM_PEL", the_met))!=0 ||
        length (grep("OT_DMF_PEL", the_met))!=0 ||
        length (grep("OT_DMF", the_met))!=0 ||
        length (grep("OTT", the_met))!=0 ||
        length (grep("PS", the_met))!=0 ||
        length (grep("PTB", the_met))!=0 ||
        length (grep("SDR", the_met))!=0 ||
        length (grep("SPR", the_met))!=0 ||
        length (grep("SSC", the_met))!=0 ||
        length (grep("SSC_DEM", the_met))!=0 ||
        length (grep("SDN", the_met))!=0 ||
        length (grep("SDN_DEM", the_met))!=0 
        )  {beam_trawl <- FALSE; demersal_trawl_gear <- TRUE; pelagic_gear <- FALSE ; passive_gear <- FALSE; dredge_gear <- FALSE}
   
       if (length (grep("TBB_CRU", the_met))!=0 ||
        length (grep("TBB_DMF", the_met))!=0 ||
        length (grep("TBB", the_met))!=0 
        )  {beam_trawl <- TRUE;  demersal_trawl_gear <- FALSE; pelagic_gear <- FALSE ; passive_gear <- FALSE; dredge_gear <- FALSE}
        
    if (length (grep("PT", the_met))!=0 ||
        length (grep("OTM", the_met))!=0 ||
        length (grep("OT_SPF", the_met))!=0 ||
        length (grep("PTM", the_met))!=0 
        )  {beam_trawl <- FALSE; demersal_trawl_gear <- FALSE; pelagic_gear <- TRUE;  passive_gear <- FALSE; dredge_gear <- FALSE}
        
    if (length (grep("GN", the_met))!=0 || 
        length (grep("GND", the_met))!=0 ||
        length (grep("GNS", the_met))!=0 ||
        length (grep("GTN", the_met))!=0 ||
        length (grep("GTR", the_met))!=0 ||
        length (grep("SB", the_met))!=0 
        )  {beam_trawl <- FALSE; demersal_trawl_gear <- FALSE; pelagic_gear <- FALSE;  passive_gear <- TRUE; dredge_gear <- FALSE}
        
    if (length (grep("FPO", the_met))!=0 || 
        length (grep("A", the_met))!=0 ||
        length (grep("LHM", the_met))!=0 ||
        length (grep("LHP", the_met))!=0 ||
        length (grep("LLD", the_met))!=0 ||
        length (grep("LLS", the_met))!=0 ||
        length (grep("LTL", the_met))!=0 
        ) {beam_trawl <- FALSE; demersal_trawl_gear <- FALSE; pelagic_gear <- FALSE;  passive_gear <- TRUE; dredge_gear <- FALSE}
          
    if (length (grep("DRB", the_met))!=0 || 
        length (grep("DRH", the_met))!=0 || 
        length (grep("DRB_MOL", the_met))!=0 || 
        length (grep("HMD", the_met))!=0
        )  {beam_trawl <- FALSE; demersal_trawl_gear <- FALSE; pelagic_gear <- FALSE;  passive_gear <- FALSE; dredge_gear <- TRUE}
   
   
      demersal_sp <- FALSE  ; pelagic_sp <- FALSE ; molluscs_sp <- FALSE 
      if(sp %in% c('BLL.nsea','COD.2224','COD.2532','COD.nsea','COD.kat','CSH.nsea','DAB.nsea','DAB.2232','FLE.2223',
                                          'FLE.2425', 'FLE.nsea', 'GUG.nsea', 'HAD.nsea','HKE.nsea','LEM.kask','MON.nsea','MUR.nsea','MON.nsea','NEP.nsea','NEP.kask','NOP.nsea',
                                            'PLE.nsea','PLE.2123','PLE.2432','POK.nsea', 'POL.nsea','PRA.nsea','PRA.kask','SAN.nsea', 
                                            'SOL.3a2223','SOL.nsea','TUR.2232','TUR.kask','WHG.nsea','WHG.kask','WHG.2232','WIT.nsea'   ) ) 
                     {pelagic_sp <- FALSE; demersal_sp <- TRUE;  molluscs_sp <- FALSE }
      if(sp %in% c('HER.nsea','HER.3a22','HER.2532','HER.nsea','HOM.nsea','MAC.nsea',
                                         'SPR.nsea','SPR.kask','SPR.2232', 'MA1.nor','MA2.ice','MA3.bob','BH1.nor','BH2.ice')) 
                    {pelagic_sp <- TRUE; demersal_sp <- FALSE;  molluscs_sp <- FALSE }
      if(sp %in% c('MUS.kask','MUS.2224')) 
                    {pelagic_sp <- FALSE; demersal_sp <- FALSE;  molluscs_sp <- TRUE }
   
  
    #### TO DO : PER METIER
    L50 <- 100    # default: will generate ogive at 0
    L75 <- 101  # default: will generate ogive at 0
    if(pelagic_sp && pelagic_gear){
     #L50         <- 16 # 36mm trawl, Suuronen and millar 1992
     #L75         <- 18 # 36mm trawl, Suuronen and millar 1992
     L50         <- 5 
     L75         <- 7 
    }
    if(demersal_sp && demersal_trawl_gear){
     L50         <- 38
     L75         <- 41
     params_sel_dem_trawl$'L50..cm.'   <- as.numeric(as.character(params_sel_dem_trawl$'L50..cm.'))
     params_sel_dem_trawl$'SR..cm.'   <- as.numeric(as.character(params_sel_dem_trawl$'SR..cm.'))
     a_mesh <- 100 # mm
     if(sp %in% c('COD.kat','COD.2224','COD.2532','COD.nsea')){L50<-params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="cod" & params_sel_dem_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="cod" & params_sel_dem_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
     if(sp %in% c('HAD.nsea','HKE.nsea')){L50<-params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="haddock" & params_sel_dem_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="haddock" & params_sel_dem_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
     if(sp %in% c('LEM.kask','BLL.nsea','PLE.2432','DAB.nsea','DAB.2232','FLE.2223','FLE.2425', 'FLE.nsea', 'SOL.3a2223','SOL.nsea', 'TUR.2232','TUR.kask','WIT.nsea')){L50<-params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="lemon sole" & params_sel_dem_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="lemon sole" & params_sel_dem_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
     if(sp %in% c('MON.nsea')){L50<-params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="monkfish" & params_sel_dem_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="monkfish" & params_sel_dem_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
     if(sp %in% c('NEP.nsea','NEP.kask', 'PRA.nsea','PRA.kask')){L50<-params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="Norway lobster" & params_sel_dem_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="Norway lobster" & params_sel_dem_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
     if(sp %in% c('PLE.nsea','PLE.2123')){L50<-params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="plaice" & params_sel_dem_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="plaice" & params_sel_dem_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
     if(sp %in% c('POK.nsea', 'POL.nsea')){L50<-params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="saithe" & params_sel_dem_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="saithe" & params_sel_dem_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
     if(sp %in% c('WHG.nsea','WHG.kask','WHG.2232')){L50<-params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="whiting" & params_sel_dem_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="whiting" & params_sel_dem_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
     if(sp %in% c('NOP.nsea','SAN.nsea','GUG.nsea')){L50<-params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="poor cod" & params_sel_dem_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_dem_trawl[params_sel_dem_trawl$Common.species.name=="poor cod" & params_sel_dem_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
    }
    
    if(beam_trawl){
           L50         <- 10 # CSH?
           L75         <- 15 # CSH?
           a_mesh <- 100 # mm
           params_sel_beam_trawl$'L50..cm.'   <- as.numeric(as.character(params_sel_beam_trawl$'L50..cm.'))
           params_sel_beam_trawl$'SR..cm.'   <- as.numeric(as.character(params_sel_beam_trawl$'SR..cm.'))
          if(sp %in% c('COD.kat','COD.2224','COD.2532','COD.nsea')){L50<-params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="cod" & params_sel_beam_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="cod" & params_sel_beam_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
          if(sp %in% c('LEM.kask','BLL.nsea','PLE.2432','DAB.nsea','DAB.2232','FLE.2223','FLE.2425', 'FLE.nsea', 'SOL.3a2223','SOL.nsea', 'TUR.2232','TUR.kask','WIT.nsea')){L50<-params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="dab" & params_sel_beam_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="dab" & params_sel_beam_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
          if(sp %in% c('GUG.nsea')){L50<-params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="gurnard" & params_sel_beam_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="gurnard" & params_sel_beam_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
          if(sp %in% c('HAD.nsea')){L50<-params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="haddock" & params_sel_beam_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="haddock" & params_sel_beam_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
          if(sp %in% c('LEM.nsea')){L50<-params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="lemon sole" & params_sel_beam_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="lemon sole" & params_sel_beam_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
          if(sp %in% c('PLE.nsea')){L50<-params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="plaice" & params_sel_beam_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="plaice" & params_sel_beam_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
          if(sp %in% c('NOP.nsea')){L50<-params_sel_dem_trawl[params_sel_beam_trawl$Common.species.name=="pouting" & params_sel_beam_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="pouting" & params_sel_beam_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
          if(sp %in% c('SOL.nsea')){L50<-params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="sole" & params_sel_beam_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="sole" & params_sel_beam_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
          if(sp %in% c('WHG.nsea','WHG.kask','WHG.2232')){L50<-params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="whiting" & params_sel_beam_trawl$Codend..mm.==a_mesh,'L50..cm.'] ; SR <- params_sel_beam_trawl[params_sel_beam_trawl$Common.species.name=="whiting" & params_sel_beam_trawl$Codend..mm.==a_mesh,'SR..cm.']; L75 <- L50+(1/2*SR)}
    }
    
    
    if(molluscs_sp && dredge_gear){
     L50         <- 5
     L75         <- 10
    }
    if(demersal_sp && passive_gear ){
     L50         <- 44 # gillnet Madsen 2007
     L75         <- 46 # gillnet Madsen 2007
    }
    cat(paste('the_met is ', the_met, ' and sp is ', sp, ' then clupeid is ',
                 pelagic_sp,', gadoid is ',demersal_sp,
                  ', trawl is ', demersal_trawl_gear, ', gillnet is ',passive_gear, '\n', sep=''))

 
     l           <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13) *a_size_group_bin_in_cm  # vector of 14 length groups of eg 10 cm bin or 5 cm
    length.fish <-  l + mid # i.e. mid size in cm
    equ.sel     <- paste("1/(1+exp(S1-S2*length.fish))")  # cf. Rasmus paper
    L50         <- L50*a_multiplier # fleetsce, if required
    S1          <- L50*log(3) / (L75 - L50)      # L75=L50+(1/2*SR)
    S2          <-  S1/L50
    # eval(parse("",text=equ.sel)) # a sigmoid....
    ogive              <- rep(met, 14)
    sel <-  round( eval(parse("",text=equ.sel)), 4)  # ...assuming 14 szgroup bins

    selectivities <- rbind(selectivities, sel)
 

    csv_selectivity_table <- rbind(csv_selectivity_table, cbind.data.frame(metiername= the_met, metier=met, stock=spp[count], matrix(sel,nrow=1)))

    }
    
 

  }

return(csv_selectivity_table)
}


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 create_metier_selectivity_files <- function(csv_selectivity_table, sce=1){
 
 
 for (met in unique(csv_selectivity_table[,2])) {
 
    selectivities <- csv_selectivity_table[csv_selectivity_table[,2]==met,-c(1:3)]
    
    # save the .dat file per metier
    write.table(selectivities,
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
                 paste(met, "metier_selectivity_per_stock_ogives_fleetsce",sce,".dat",sep='')),
                   col.names=FALSE,  row.names=FALSE, sep= ' ', quote=FALSE)
   cat( paste("Write in metiersspe: ", met, "metier_selectivity_per_stock_ogives.dat\n",sep=''))

   }
   
    
   # selectivity for other_land. Borrow from relevant metiers...
     selectivities <- csv_selectivity_table[csv_selectivity_table[,1]=="OTB",] # default
     for (spp in c('HAD.nsea','HER.nsea','HER.3a22','HER.2532','HER.281','HER.3031','HOM.nsea','MAC.nsea','POK.nsea', 
                                         'SPR.nsea','SPR.kask','SPR.2232')){
              if(spp %in% selectivities[,"stock"]) selectivities[selectivities[,"stock"] %in% spp, paste(1:14)] <- 
                 csv_selectivity_table[csv_selectivity_table[,"stock"]== spp & csv_selectivity_table[,"metiername"]=="OTM", paste(1:14)] # adapt for pelagic species
    }
    selectivities <- selectivities[,   -c(1:3)]
    
    # save the .dat file for OTH_LAND
    write.table(selectivities,
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
                 paste("metier_selectivity_per_stock_ogives_fleetsce", sce,"_for_oth_land.dat",sep='')),
                   col.names=FALSE,  row.names=FALSE, sep= ' ', quote=FALSE)
   cat( paste("Write in metier_selectivity_per_stock_ogives_for_oth_land.dat\n",sep=''))

   
 return()
 }


  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  

      ##-------------------
      ##---utils-----------
      ## a function to read the fleet scenario file before we expand it with a new field...
      ## this suppose to replicate all the existing fleet files...
      add_a_new_scenario_type_to_fleet_scenarios <- function(a_new_field_defining_scenarios="selectivity_L50_multiplier", 
                                                            values_for_this_scenario_type=c(0.75, 1, 1.5)){
        # if(all(values_for_this_scenario_type)!=1) stop("need at least the value '1' for this new scenario field")                                                    
         multiplier_for_fleetsce                   <- read.table (file=file.path(general$main.path.ibm, paste("multiplier_for_fleetsce", general$application, ".dat", sep=""))  , header=TRUE)   
         new_sces                                 <- expand.grid(sce=multiplier_for_fleetsce$sce, values_for_this_scenario_type)
         colnames(new_sces)[ncol(new_sces)]       <- a_new_field_defining_scenarios
         multiplier_for_fleetsce                   <- merge(multiplier_for_fleetsce, new_sces)
         if(length(values_for_this_scenario_type)>1){
         nr                                       <- nrow(multiplier_for_fleetsce [multiplier_for_fleetsce[,a_new_field_defining_scenarios]==1, ])  # baseline sce for this new field
         nr2                                      <- nrow(multiplier_for_fleetsce [multiplier_for_fleetsce[,a_new_field_defining_scenarios]!=1, ])  # other sces
         
         multiplier_for_fleetsce$initial_sce <- multiplier_for_fleetsce$sce
         multiplier_for_fleetsce [multiplier_for_fleetsce[,a_new_field_defining_scenarios]!=1, "sce"] <- (1:nr2)+nr
         library(doBy)
         multiplier_for_fleetsce    <- orderBy(~sce, data=multiplier_for_fleetsce)
         
      
         # then replicate all the fleetsce files for this new numbering.
         all_fleet_files <- list.files(file.path(general$main.path.ibm, paste("popsspe_",general$application,sep='')))
         for(sce in ((1:nr2)+nr)){
            initial_sce_number_for_this_new_sce <- multiplier_for_fleetsce[multiplier_for_fleetsce$sce==sce, "initial_sce"]  # duplicate the ones correponding to the initial sce number
            all_filenames_to_replicates         <- all_fleet_files[grep(paste("fleetsce",initial_sce_number_for_this_new_sce,sep=''), all_fleet_files)]
      
            all_filenames_to_replicates_new_name <- gsub(paste("fleetsce",initial_sce_number_for_this_new_sce,sep=''), paste("fleetsce",sce,sep=""), all_filenames_to_replicates)    
            for(i in 1:length(all_filenames_to_replicates)) {
              file.copy(from=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=""), all_filenames_to_replicates[i]), 
                            to=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=""), all_filenames_to_replicates_new_name[i]))
              }
            }
       
       multiplier_for_fleetsce <- multiplier_for_fleetsce[, -ncol(multiplier_for_fleetsce)] # remove no longer useful initial sce field      
       }
       return(multiplier_for_fleetsce)
       }
       #--------------------
       #--------------------
       
       
      
      
        
   # caution fleet sce
   fleetsce <-  data.frame(sce=1, namesce=c('scebaseline'))
   
    write.table(fleetsce, quote=FALSE,
                 file=file.path(general$main.path.ibm, paste("multiplier_for_fleetsce", general$application,".dat",sep='')), append=FALSE,
                   row.names=FALSE, col.names=TRUE)
 
      # add a new field for some fleet scenarios:
      # CAUTION: the 0spe_stecf_oth_land_per_month_per_node_semester1.dat types of file are in /POPSSPE !!
       multiplier_for_fleetsce <- add_a_new_scenario_type_to_fleet_scenarios(
                                                      a_new_field_defining_scenarios="selectivity_L50_multiplier", 
                                                      values_for_this_scenario_type=c(0.9, 1)
                                                      )
   write.table(multiplier_for_fleetsce, quote=FALSE,
                 file=file.path(general$main.path.ibm, paste("multiplier_for_fleetsce", general$application,".dat",sep='')), append=FALSE,
                   row.names=FALSE, col.names=TRUE)
 
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  
##!!!!!!!!!!!!!!!!!!!!CALLS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##  
  
  # save the .dat file all metiers for display in object editor
  if(!file.exists(file.path(general$main_path_gis,"FISHERIES", 
                    paste("fishing_gear_selectivity_ogives_per_stock.csv",sep=''))))
                 {
                  for(sce in multiplier_for_fleetsce$sce)
                  {
                     a_multiplier            <- multiplier_for_fleetsce[multiplier_for_fleetsce$sce==sce,"selectivity_L50_multiplier"]
                     csv_selectivity_table   <- create_selectivity_from_L50_parameters(a_multiplier)
                     create_metier_selectivity_files(csv_selectivity_table, sce=sce)
                  }
                  
                  
                  # and create the file....
                  colnames (csv_selectivity_table) <- c("metier_name", "met", "stock", paste("sz group", 0:13))
                  write.table(csv_selectivity_table,
                    file=file.path(general$main_path_gis,"FISHERIES", 
                    paste("fishing_gear_selectivity_ogives_per_stock.csv",sep='')),
                   col.names=TRUE,  row.names=FALSE, sep= ',', quote=FALSE)
                  cat( paste("Write in fishing_gear_selectivity_ogives_per_stock.csv\n",sep=''))
                 
                 }else{
                  
                  csv_selectivity_table <- read.table(
                    file=file.path(general$main_path_gis, "FISHERIES",
                    paste("fishing_gear_selectivity_ogives_per_stock.csv",sep='')),
                   header=TRUE, sep= ',')
                   cat( paste("Use fishing_gear_selectivity_ogives_per_stock.csv to deduce metier selectivity files\n",sep=''))
                   
                   create_metier_selectivity_files(csv_selectivity_table, sce=1)
                 
                 }

cat(paste("....done\n"))

