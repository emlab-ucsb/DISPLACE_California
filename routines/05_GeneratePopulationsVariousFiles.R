# # GENERAL SETTINGS
#  
#    args <- commandArgs(trailingOnly = TRUE)
# 
#    general <- list()
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




  
  
   cat(paste("START \n"))


   # (caution: give the order for naming stocks in integer from 0 to n-1)
   spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS", 
                           paste("pop_names_", general$application,".txt",sep='')), header=TRUE)
   spp                        <- as.character(spp_table$spp)
   cat(paste("Reading the stock names in", paste(general$main_path_gis, "POPULATIONS", 
                           paste("pop_names_", general$application,".txt",sep='')),"....done \n"))
 
   
   
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# XXctrysspe_relative_stability_semesterXX.dat
# percent_landings_from_simulated_vessels.dat
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
 
   if(general$application=="NorthSea"){
    
   library("readxl")
       landings <- read_excel(file.path(general$main_path_gis, "OTHERS", "ICESCatchDataset2006-2018.xlsx"), 2)
       landings <- as.data.frame(landings)
 
       # find out areas
       landings$code_area <- landings$Area
       landings[(landings$code_area %in% c('27.4.a', '27.4.b', '27.4.c', '27.4')), 'code_area'] <- 'nsea'
       landings[(landings$code_area %in% c('27.3.a.20','27.3.a')), 'code_area'] <- 'kask'
       landings[(landings$code_area %in% c('27.3.c.22',  '27.3.d.24')), 'code_area'] <- '2224'
       landings[(landings$code_area %in% c('27.3.d.25', '27.3.d.26', '27.3.d.28', '27.3.d.28.1', '27.3.d.28.2', '27.3.d.28_NK','27.3.d.29', '27.3.d.32', '27.3.d.30', '27.3.d.31')), 'code_area'] <- '2532'

       # temporary species names
       landings$species   <- paste(landings$Species,".", landings$code_area, sep='')

       # correct names for special cases (those across management areas)
       landings[ landings$Species %in% c("COD") & (landings$Area %in% c('27.4.a', '27.4.b', '27.4.c', '27.4')), "species"] <- 'COD.nsea'
       landings[ landings$Species %in% c("COD")& (landings$Area %in% c('27.3.a')), "species"] <- 'COD.kat'
       head(landings[landings$species=="COD.kat",])
       landings[ landings$Species %in% c("HER") & (landings$Area %in% c('27.3.a', '27.3.a.20', '27.3.c.22')), "species"] <- 'HER.3a22'
       landings[ landings$Species %in% c("HER") & (landings$Area %in% c('27.3.d.28.1')), "species"] <- 'HER.281'
       landings[ landings$Species %in% c("HER") & (landings$Area %in% c('27.3.d.30','27.3.d.31')), "species"] <- 'HER.3031'
       landings[ landings$Species %in% c("PLE") & (landings$Area %in% c('27.3.a', '27.3.c.22')), "species"] <- 'PLE.2123'
       landings[ landings$Species %in% c("PLE") & (landings$Area %in% c('27.3.d.24', '27.3.d.25', '27.3.d.26', '27.3.d.28', '27.3.d.28.1', '27.3.d.28.2', '27.3.d.28_NK','27.3.d.29', '27.3.d.32', '27.3.d.30', '27.3.d.31')), "species"] <- 'PLE.2432'
       landings[ landings$species %in% c("BLL.2224", "BLL.2532"), "species"] <- 'BLL.2232'
       landings[ landings$species %in% c("TUR.2224", "TUR.2532"), "species"] <- 'TUR.2232'
       landings[ landings$species %in% c("DAB.2224", "DAB.2532"), "species"] <- 'DAB.2232'
       landings[ landings$species %in% c("SPR.2224", "SPR.2532"), "species"] <- 'SPR.2232'
       landings[ landings$species %in% c("WHG.2224", "WHG.2532"), "species"] <- 'WHG.2232'
       landings[ landings$Species %in% c("FLE") & (landings$Area %in% c('27.3.c.22')), "species"] <- 'FLE.2223'
       landings[ landings$Species %in% c("FLE") & (landings$Area %in% c('27.3.d.24', '27.3.d.25')), "species"] <- 'FLE.2425'
       landings[ landings$Species %in% c("FLE") & (landings$Area %in% c('27.3.d.26', '27.3.d.28', '27.3.d.28.1', '27.3.d.28.2')), "species"] <- 'FLE.2628'
       landings[ landings$Species %in% c("FLE") & (landings$Area %in% c('27.3.d.29', '27.3.d.32', '27.3.d.30', '27.3.d.31')), "species"] <- 'FLE.2732'
       landings[ landings$Species %in% c("SOL") & (landings$Area %in% c('27.3.a','27.3.c.22','27.3.d.24')), "species"] <- 'SOL.3a2223'

       

       spp_table <- read.table(file=file.path(general$main_path_gis, "POPULATIONS", paste0("pop_names_", general$application, ".txt")), header=TRUE)
       land <- landings[ landings$species %in% as.character(spp_table$spp),]
       land_2018 <- land[,c("species", "Country", "2018")]

       catches_in_tons <- tapply(as.numeric(as.character(land_2018$'2018')), list(land_2018$species, land_2018$Country), sum, na.rm=TRUE) / 1000


       catches_in_tons[is.na(catches_in_tons)] <- 0
       catches_in_percent <- round(sweep(catches_in_tons, 1, apply(catches_in_tons, 1, sum, na.rm=TRUE), FUN="/"),3)*100 # in percent from 2018 landings....
       catches_in_percent[!complete.cases(catches_in_percent),] <- 0

     }
     

  for (pid in 0: (length(spp)-1)){
 
    for (a.semester in c(1,2)){
 
      if(general$application=="NorthSea"){
     
        # CAUTION: SHOULD BE CODED ACCORDING TO VESSEL NATIONALITY CODE!!!!! 
        #ctry_code <- c('BEL','DEU','DNK','FIN','FRA','GBR','IRL','NLD','SWE','Sma')
        #relative_stability_this_sp <- cbind(ctry=ctry_code,
        # percent=rep(0, length=length(ctry_code))) # not used if no individual quotas... 
     
      # ....inform from real data

        
        ## CAUTION: NAMING OF NATIONS SHOULD BE CONSISTENT WITH VESSELS NATIONALITY!
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "BE"] <- "BEL"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "DE"] <- "DEU"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "DK"] <- "DNK"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "EE"] <- "EST"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "ES"] <- "ESP"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "FI"] <- "FIN"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "FO"] <- "FOI" # Feroe
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "FR"] <- "FRA"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "GB"] <- "GBR"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "GL"] <- "GRE"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "IE"] <- "IRL"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "IS"] <- "ISL"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "LT"] <- "LTU"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "LV"] <- "LVA"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "NL"] <- "NLD"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "NO"] <- "NOR"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "PL"] <- "POL"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "PT"] <- "POR"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "RU"] <- "RUS"
        colnames(catches_in_percent)[colnames(catches_in_percent) %in% "SE"] <- "SWE"
     
     
        # check country names and be sure all ctry are present:
        #nameobj           <- paste("vesselsspe_features_quarter1.dat",sep='')  #....and possibly per vid!
        #vesselsspe_features <- read.table(file.path(general$main.path.ibm, paste("vesselsspe_",general$application,sep=""), nameobj), header=FALSE, sep="|")
        #print(unique(sapply(as.character(vesselsspe_features[,1]), substring, 1, 3)))

     
         
        relative_stability_this_sp <- cbind.data.frame(Country=colnames(catches_in_percent), Percent=catches_in_percent[spp[pid+1],])   # not used if no individual quotas... 
        #TODO if used this would need to be corrected for the official key because just an indication for now given discards not included, or no clue about relative TAC uptake etc. 
     
     
     
        } else{
        #stop('Relative stability key needs to be defined for your own app')
        relative_stability_this_sp <- data.frame(Country="DNK", Percent=100)
        }
        
      write.table(relative_stability_this_sp,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), 
              paste(pid, "ctrysspe_relative_stability_semester", a.semester,".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

      cat(paste("Write", pid, "ctrysspe_relative_stability_semester", a.semester,".dat....done \n"))

      }
   
   }   
 
 
 
  #------------------------------------
      percent_landings_from_simulated_vessels <- cbind(stock= 0: (length(spp)-1), percent=90) # not used if no individual quotas... 
      
      write.table(percent_landings_from_simulated_vessels,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), 
              paste("percent_landings_from_simulated_vessels.dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

      cat(paste("Write percent_landings_from_simulated_vessels.dat....done \n"))

   
   
   
  
   
   cat(paste("....done \n"))  