#
#  args <- commandArgs(trailingOnly = TRUE)
#
#  general <- list()
#
#  if (length(args) < 2) {
#    if(.Platform$OS.type == "windows") {
#      general$application           <- "MEESO"
#      general$main_path_gis         <- file.path("D:","FBA", paste("DISPLACE_input_gis_", general$application, sep=""))
#      general$main.path.ibm         <- file.path("D:","FBA", paste("DISPLACE_input_", general$application, sep=''))
#      general$igraph                <- 206 # older working one is 110  # caution: should be consistent with existing objects already built upon a given graph
#     do_plot                        <- TRUE
#
#    } else{
#    if(Sys.info()["sysname"] == "Darwin") {
#      general$application           <- "MEESO"
#      general$main_path_gis         <- file.path("usr","local","GitHub",paste("DISPLACE_input_gis_", general$application, sep=""))
#      general$main.path.ibm         <- file.path("usr","local","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
#      general$igraph                <- 1  # caution: should be consistent with existing objects already built upon a given graph
#     do_plot                        <- TRUE
#
#    } else {
#      general$application           <- args[1]
#      general$main_path_gis         <- args[2]
#      general$main.path.ibm         <- args[3]
#      general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
#     do_plot                        <- FALSE
# }}}
# cat(paste("START \n"))

dir.create(file.path(general$main.path.ibm))
dir.create(file.path(
  general$main.path.ibm,
  paste("popsspe_", general$application, sep = '')
))


## ---------------------------------
## read in pop parameters-----------
## ---------------------------------
params_pops <- read.csv(
  file = file.path(
    general$main_path_gis,
    "POPULATIONS",
    paste("Stock_biological_traits.csv", sep = ',')
  ),
  sep = ',',
  header = TRUE
)
rownames(params_pops) <- params_pops$stock


## ---------------------------------
## read in stock N-at-age parameters
## ---------------------------------
a.year <- 2022
quarter_growth <- TRUE
semester_growth <- FALSE

# pop number per age group
number_at_age <- read.csv(
  file = file.path(
    general$main_path_gis,
    "POPULATIONS",
    "Stock_abundances_at_age.csv"
  ),
  sep = ",",
  header = TRUE
)

number_yplus1 <- number_at_age # because data not available yet...
number_at_age <- number_at_age[number_at_age$year == a.year, ]
number_at_age <- number_at_age[!is.na(number_at_age$stock), ]
rownames(number_at_age) <- number_at_age$stock

## ---------------------------------
## filter out to keep feasible pops-
## ---------------------------------
feasible_pops <- as.character(params_pops[params_pops$UseIt == "Yes", "stock"])
pa <- params_pops[params_pops$stock %in% feasible_pops, ]
implicit_stocks <- rownames(pa[pa$Implicit == "Yes", ])
explicit_stocks <- rownames(pa[pa$Implicit == "No", ])
pa <- pa[, c(
  "Winf",
  "k",
  "Linf",
  "CV_Linf",
  "K",
  "t0",
  "a",
  "b",
  "L50",
  "mat_B",
  "alpha",
  "beta",
  "nat_M",
  "r_age",
  "tac_tons",
  "fbar_age_min",
  "fbar_age_max",
  "F_target",
  "F_percent",
  "TAC_percent",
  "B_trigger",
  "FMSYlower",
  "FMSYupper",
  "FMSY",
  "fbar_assessment",
  "ssb_assessment",
  "mls_cat",
  "mls",
  "sz_bin_cm",
  "unit_sizebin",
  "CV_recru",
  "mat",
  "mat_cat",
  "etha_m",
  "kappa",
  "q",
  "n",
  "fzeroest",
  "species"
)]
number <- number_at_age[feasible_pops, ]
number <- number[, c(
  "X0",
  "X1",
  "X2",
  "X3",
  "X4",
  "X5",
  "X6",
  "X7",
  "X8",
  "X9",
  "X10",
  "X11",
  "X12"
)]


## ---------------------------------
## read in pop parameters-----------
## ---------------------------------
# CAUTION: stock names given by first column in "Stock_abundances_at_age.csv"
spp <- rownames(number)
table_spp <- cbind(0:(length(spp) - 1), spp)
colnames(table_spp) <- c('idx', 'spp')
write.table(
  table_spp,
  quote = FALSE,
  file = file.path(
    general$main_path_gis,
    "POPULATIONS",
    paste("pop_names_", general$application, ".txt", sep = '')
  ),
  append = FALSE,
  row.names = FALSE,
  col.names = TRUE
)
write.table(
  cbind(idx = 0:(length(spp) - 1), spp = spp),
  file = file.path(
    general$main.path.ibm,
    paste("pop_names_", general$application, ".txt", sep = '')
  ),
  quote = FALSE,
  col.names = TRUE,
  row.names = FALSE
)


#
# check
if (any(is.na(pa))) {
  warning("Need for replacing NAs by 0s in Stock_biological_traits.csv")
  pa <- replace(pa, is.na(pa), 0)
  print("NAs by 0s replacement DONE...")
}


pa <- pa[spp, ] # reorder
pa$index_pops <- 0:(length(spp) - 1)


# info for config.dat
implicit_stocks_idx <- pa[implicit_stocks, "index_pops"]
implicit_stocks_idx


if (FALSE) {
  ## by the way, look at the nice way of getting the pop params to populate the input pa table:
  ## the brand new icesSAG R package:
  library("icesSAG")
  dd <- getListStocks(2016)
  dd$StockKeyLabel
  assessmentKey <- findAssessmentKey("cod-2224", year = 2016)
  refpts <- getFishStockReferencePoints(assessmentKey)
  refpts
  sumtab <- getSummaryTable(assessmentKey)
  sumtab[[1]][sumtab[[1]]$Year == 2015, c("F", "SSB")] # retrieve assessed F and SSB

  library("icesSAG")
  dd <- getListStocks(2016)
  dd$StockKeyLabel
  assessmentKey <- findAssessmentKey("cod-2224", year = 2016)
  refpts <- getFishStockReferencePoints(assessmentKey)
  refpts
  sumtab <- getSummaryTable(assessmentKey)
  sumtab[[1]][sumtab[[1]]$Year == 2015, c("F", "SSB", "landings")] # retrieve assessed F and SSB

  # e.g.
  #stock_to_screen <-  c("arg-oth", "boc-nea", "cod-7e-k", "dab-nsea", "gur-comb", "had-7b-k", "her-irls", "hke-nrtn", "mgw-78", "mon-7-8", "msf-celt", "nep-16",
  #"nep-17", "nep-19", "nep-2021", "nep-22", "nop-34-oct", "ple-celt", "pod-celt", "pok-celt", "pol-27-67", "rjn-678abd",
  #"syc-celt", "sol-celt", "whb-comb", "whg-7e-k", "ags-celt", "cly-celt")
  stock_to_screen <- c(
    NA,
    "boc-nea",
    "cod-7e-k",
    NA,
    NA,
    "had-7b-k",
    "her-irls",
    "hke-nrtn",
    "mgw-78",
    NA,
    NA,
    "nep-16",
    "nep-17",
    "nep-19",
    "nep-2021",
    "nep-22",
    "nop-34-oct",
    "ple-celt",
    NA,
    NA,
    "pol.27.67",
    "rjn-678abd",
    "syc-celt",
    "sol-celt",
    "whb-comb",
    "whg-7e-k",
    NA,
    NA
  )

  res <- data.frame(
    stock = stock_to_screen,
    TAC = NA,
    F_MSY = NA,
    B_trigger = NA,
    fbar_assessment = NA,
    ssb_assessment = NA,
    recruits = NA,
    Fage = NA,
    recruitment_a = NA
  )
  for (st in stock_to_screen) {
    cat(paste("----------------------------------------------------------\n"))
    if (!is.na(st)) {
      assessmentKey <- findAssessmentKey(st, year = 2016)
      er <- try(
        {
          refpts <- getFishStockReferencePoints(assessmentKey)
          print(refpts)
          res$F_MSY[res$stock == st] <- refpts[[1]]["FMSY"]
          res$B_trigger[res$stock == st] <- refpts[[1]]["MSYBtrigger"]

          sumtab <- getSummaryTable(assessmentKey)
          print(sumtab[[1]][sumtab[[1]]$Year == 2015, c("F", "SSB")]) # retrieve assessed F and SSB i.e. at y-1

          res$fbar_assessment[res$stock == st] <- sumtab[[1]][
            sumtab[[1]]$Year == 2015,
            c("F")
          ]
          res$ssb_assessment[res$stock == st] <- sumtab[[1]][
            sumtab[[1]]$Year == 2015,
            c("SSB")
          ]
          res$TAC[res$stock == st] <- sumtab[[1]][
            sumtab[[1]]$Year == 2015,
            c("landings")
          ]
          res$recruits[res$stock == st] <- sumtab[[1]][
            sumtab[[1]]$Year == 2015,
            c("recruitment_age")
          ]
          res$Fage[res$stock == st] <- sumtab[[1]][
            sumtab[[1]]$Year == 2015,
            c("Fage")
          ]
          res$recruitment_a[res$stock == st] <- sumtab[[1]][
            sumtab[[1]]$Year == 2015,
            c("recruitment")
          ]

          # just of a check
          landings_plots <- getLandingsGraph(assessmentKey)
          plot(landings_plots)
          F_plot <- getFishingMortalityGraph(assessmentKey)
          plot(F_plot)
          #  browser()
        },
        silent = TRUE
      )
    } else {
      cat(paste("this stock ", st, "is not an assessed stock\n"))
    }
  }
  if (class(er) == "try-error") {
    cat(paste("fail to retreive for this stock ", st, "\n"))
  }
} # end FALSE


##### DEFINE THE BIOLOGICAL SCENARIOS #################################
##### (RELATED TO STOCK CONDITIONING AND POTENTIAL MIXING #############

#von bertalanfy growth
vbg <- function(Linf, K, to = 0, timesteps) {
  Linf * (1 - exp(-K * (timesteps - to)))
}
# some checks...
#a_pop_pa <- pa[pa$pop.to.keeps =="SPR.2232", ]
#plot( 1:21, vbg (a_pop_pa[,'Linfs'],a_pop_pa[,'Ks'], 0, 1:21), type="b")
#lines( 1:21, vbg (a_pop_pa[,'Linfs'],a_pop_pa[,'Ks']*0.8, 0, 1:21), type="b", col=2)
## alter the brody growth curvature parameter k which determines how fast the fish approaches its Linf
#lines( 1:21, vbg (a_pop_pa[,'Linfs'],a_pop_pa[,'Ks']*0.5, 0, 1:21), type="b", col=3)

# SSB-R
ssbr <- function(alpha, beta, ssb) {
  alpha * ssb * exp(-beta * ssb)
}

# some checks...
#a_pop_pa <- pa[pa$pop.to.keeps =="SPR.2232", ]
#plot( seq(0, 1000000000, by=1e6), ssbr (alpha=a_pop_pa[,'a_SSB'], beta=a_pop_pa[,'b_SSB'], ssb=seq(0, 1000000000, by=1e6)), type="l")
#lines(seq(0, 1000000000, by=1e6), ssbr (a_pop_pa[,'a_SSB']*0.5,a_pop_pa[,'b_SSB']*0.5, seq(0, 1000000000, by=1e6)), type="l", col=2)
#lines(seq(0, 1000000000, by=1e6), ssbr (a_pop_pa[,'a_SSB']*1.2,a_pop_pa[,'b_SSB']*1.2, seq(0, 1000000000, by=1e6)), type="l", col=3)
#assess <- read.table(file='C:/Users/fba/Dropbox/ibm_vessels_param/summary_table_from_WGBFAS11_SPR_BE.txt',header = TRUE,  sep=",")
#points( assess$TOTSPBIO*1e6, assess$RECRUITS*1e6, pch="+", cex=2)

# build a matrix of (biological) scenarios
if (general$application == "testexample") {
  multiplier_for_biolsce_all_pops <- expand.grid(
    biolsce_maturity = 1,
    biolsce_M = c(1),
    biolsce_weight = c(1),
    biolsce_init_pops = 1,
    biolsce_init_pops = 1,
    biolsce_fecundity = 1,
    biolsce_Linfs = c(1, 0.9),
    biolsce_Ks = c(1),
    biolsce_recru = c(1),
    biolsce_mig = c(0),
    pop = c('COD.2532')
  ) # see SS3 model settings in ICES WKBALTCOD 2015

  multiplier_for_biolsce_all_pops <- cbind(
    sce = 1:(nrow(multiplier_for_biolsce_all_pops) /
      length(unique(multiplier_for_biolsce_all_pops$pop))),
    multiplier_for_biolsce_all_pops
  )

  write.table(
    multiplier_for_biolsce_all_pops,
    quote = FALSE,
    file = file.path(
      general$main.path.ibm,
      paste("multiplier_for_biolsce", general$application, ".dat", sep = '')
    ),
    append = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )
} else {
  the_base <- expand.grid(
    sce = 1,
    biolsce_maturity = 1,
    biolsce_M = 1,
    biolsce_weight = 1,
    biolsce_init_pops = 1,
    biolsce_init_pops = 1,
    biolsce_fecundity = 1,
    biolsce_Linfs = 1,
    biolsce_CVLinfs = c(1),
    biolsce_Ks = 1,
    biolsce_recru_rickeralpha = c(1),
    biolsce_recru_rickerbeta = c(1),
    biolsce_CVrecru = c(1),
    biolsce_mig = 0,
    spatial_shift = 0,
    with_shoke = 0,
    FMSYmulti = 1,
    pop = c("SAB", "MIP", "SJZ", "SJU", "EOJ", "SGO", "OTH")
  )

  the_sce_on_changing_productivity_severe_and_choke <- the_base
  the_sce_on_changing_productivity_severe_and_choke$sce <- 2
  the_sce_on_changing_productivity_severe_and_choke$biolsce_Linfs <- 0.8
  the_sce_on_changing_productivity_severe_and_choke$biolsce_Ks <- 1.5
  the_sce_on_changing_productivity_severe_and_choke$biolsce_recru_rickeralpha <- 0.9 # decrease max recru peak e.g. changed carrying cap?
  the_sce_on_changing_productivity_severe_and_choke$biolsce_recru_rickerbeta <- 1.5 # increase density-dependent effects e.g. from resource scarcity?
  the_sce_on_changing_productivity_severe_and_choke$with_shoke <- 1
  #the_sce_on_changing_productivity_severe_and_choke[the_sce_on_changing_productivity_severe_and_choke$pop=="SPR.nsea", "biolsce_recru_rickeralpha"] <- 1.1  # bc positive effect of warmer water column temperature on sprat
  #the_sce_on_changing_productivity_severe_and_choke[the_sce_on_changing_productivity_severe_and_choke$pop=="SPR.nsea", "biolsce_recru_rickerbeta"] <- 1.0   # bc positive effect of warmer water column temperature on sprat

  # stack scenarios
  multiplier_for_biolsce_all_pops <- rbind.data.frame(
    the_base,
    the_sce_on_changing_productivity_severe_and_choke
  )

  # then duplicate scenarios but changing the FMSY only
  multiplier_for_biolsce_all_pops_FMSYlower <- multiplier_for_biolsce_all_pops[
    multiplier_for_biolsce_all_pops$pop %in%
      c("SAB", "MIP", "SJZ", "SJU", "EOJ", "SGO", "OTH"),
  ]
  #pa[c("HER.nsea", "SPR.nsea"), "FMSYlower"] <- pa[c("HER.nsea", "SPR.nsea"), "FMSY"]*0.9   # an assumption in absence of defined FMSYlow
  multiplier_for_biolsce_all_pops_FMSYlower$FMSYmulti <- pa[
    c("SAB", "MIP", "SJZ", "SJU", "EOJ", "SGO", "OTH"),
    "FMSYlower"
  ] /
    pa[c("SAB", "MIP", "SJZ", "SJU", "EOJ", "SGO", "OTH"), "FMSY"]
  multiplier_for_biolsce_all_pops_FMSYlower$sce <- multiplier_for_biolsce_all_pops_FMSYlower$sce +
    2

  # fix NAs if any
  multiplier_for_biolsce_all_pops_FMSYlower[is.na(
    multiplier_for_biolsce_all_pops_FMSYlower
  )] <- 1

  multiplier_for_biolsce_all_pops <- rbind.data.frame(
    multiplier_for_biolsce_all_pops,
    multiplier_for_biolsce_all_pops_FMSYlower
  )

  write.table(
    multiplier_for_biolsce_all_pops,
    quote = FALSE,
    file = file.path(
      general$main.path.ibm,
      paste("multiplier_for_biolsce", general$application, ".dat", sep = '')
    ),
    append = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )
}


# some checks...
if (FALSE) {
  #MA1.nor
  ssb <- seq(0, 100000, by = 1e3) #SSB tons, Recruits in thousands of individuals
  plot(
    ssb,
    ssbr(
      alpha = pa["MA1.nor", 'alpha'],
      beta = pa["MA1.nor", 'beta'],
      ssb = ssb
    ),
    type = "l"
  )
  lines(
    ssb,
    ssbr(pa["MA1.nor", 'alpha'] * 0.5, pa["MA1.nor", 'beta'] * 0.5, ssb),
    type = "l",
    col = 2
  ) # no densitydepednece effect: bad shape
  lines(
    ssb,
    ssbr(pa["MA1.nor", 'alpha'] * 1.2, pa["MA1.nor", 'beta'] * 1.2, ssb),
    type = "l",
    col = 3
  ) # moving for an earlier peak: bad shape
  lines(
    ssb,
    ssbr(pa["MA1.nor", 'alpha'] * 0.8, pa["MA1.nor", 'beta'] * 1, ssb),
    type = "l",
    col = 4
  ) # decrease the max of the curve
  lines(
    ssb,
    ssbr(pa["MA1.nor", 'alpha'] * 1, pa["MA1.nor", 'beta'] * 1.5, ssb),
    type = "l",
    col = 5
  ) # increase the effect of density-dependence
  lines(
    ssb,
    ssbr(pa["MA1.nor", 'alpha'] * 0.7, pa["MA1.nor", 'beta'] * 1.5, ssb),
    type = "l",
    col = 6
  ) # large decrease the productivity + increase the effect of density-dependence

  # sces  COD.nsea
  ssb <- seq(0, 100000, by = 1e3) #SSB tons, Recruits in thousands of individuals
  tiff(
    file = file.path(
      general$main_path_gis,
      "DISPLACE_R_inputs_NorthSea",
      paste("SSB-R_scenarios_COD.nsea", ".tiff", sep = "")
    ),
    width = 2500,
    height = 2500,
    compression = "lzw",
    res = 600
  )
  plot(
    ssb,
    ssbr(
      alpha = pa["COD.nsea", 'alpha'],
      beta = pa["COD.nsea", 'beta'],
      ssb = ssb
    ),
    type = "l",
    lwd = 2,
    xlab = "SSB (tons)",
    ylab = "Recruits (thousands)"
  ) # baseline
  lines(
    ssb,
    ssbr(pa["COD.nsea", 'alpha'] * 0.9, pa["COD.nsea", 'beta'] * 1.5, ssb),
    type = "l",
    col = 2,
    lwd = 2
  ) #   severe
  legend(
    "bottomright",
    col = c(1, 6, 2),
    lty = 1,
    lwd = 2,
    legend = c("Baseline", "Climate Change"),
    bty = "n"
  )
  title("COD.nsea")
  dev.off()

  tiff(
    file = file.path(
      general$main_path_gis,
      "DISPLACE_R_inputs_MEESO",
      paste("VBFC_scenarios_MA1.nor", ".tiff", sep = "")
    ),
    width = 2500,
    height = 2500,
    compression = "lzw",
    res = 600
  )
  plot(
    1:21,
    vbg(pa["MA1.nor", 'Linf'], pa["MA1.nor", 'K'], 0, 1:21),
    type = "l",
    lwd = 2,
    xlab = "Semester",
    ylab = "Length (mm)"
  )
  points(
    1:21,
    vbg(pa["MA1.nor", 'Linf'] * 0.8, pa["MA1.nor", 'K'] * 1.5, 0, 1:21),
    type = "l",
    lwd = 2,
    col = 2,
    xlab = "Semester",
    ylab = "Length (mm)"
  )
  legend(
    "bottomright",
    col = c(1, 6, 2),
    lty = 1,
    lwd = 2,
    legend = c("Baseline", "Climate Change"),
    bty = "n"
  )
  title("MA1.nor")
  dev.off()
}


hyperstability_param <- cbind(
  pop = c(0:(nrow(pa) - 1)),
  hyperstability_param = 0.7
) # apply 0.7 to all pop
# in Harvey et al 2001 cjfas:  cod, flatfish, and gadiformes, finding strong evidence that CPUE was most likely to
# remain high while abundance declines (i.e., hyperstability, where b
# < 1). The range in the mean of the random effects distribution for b was quite small, 0.64?0.75
write.table(
  hyperstability_param,
  quote = FALSE,
  file = file.path(
    general$main.path.ibm,
    paste("popsspe_", general$application, sep = ''),
    paste("hyperstability_param.dat", sep = '')
  ),
  append = FALSE,
  row.names = FALSE,
  col.names = TRUE
)
cat(paste("hyperstability_param.dat\n", sep = ''))


# overall migration fluxes at 0 by default
for (sce in unique(multiplier_for_biolsce_all_pops$sce)) {
  write(
    "stock  init_prop_migrants_pops_per_szgroup",
    file = file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = ''),
      paste(
        "init_prop_migrants_pops_per_szgroup_biolsce",
        sce,
        ".dat",
        sep = ''
      )
    ),
    append = FALSE
  )

  for (x in 1:length(pa$K)) {
    if (!is.na(pa$index_pops[x])) {
      write.table(
        "# to_pop_num overall_fluxes_of_N_in_proportion_per_size_group",
        quote = FALSE,
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste(
            pa$index_pops[x],
            "overall_migration_fluxes_",
            "semester1",
            "_",
            "biolsce",
            sce,
            ".dat",
            sep = ''
          )
        ),
        append = FALSE,
        row.names = FALSE,
        col.names = FALSE
      )
      write.table(
        "# to_pop_num overall_fluxes_of_N_in_proportion_per_size_group",
        quote = FALSE,
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste(
            pa$index_pops[x],
            "overall_migration_fluxes_",
            "semester2",
            "_",
            "biolsce",
            sce,
            ".dat",
            sep = ''
          )
        ),
        append = FALSE,
        row.names = FALSE,
        col.names = FALSE
      )
      # => empty mig files per default

      # caution: potential pbl with  !migration_fluxes.empty() under linux!!
      e <- NULL
      write.table(
        e,
        quote = FALSE,
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste(
            pa$index_pops[x],
            "overall_migration_fluxes_",
            "semester1",
            "_",
            "biolsce",
            sce,
            ".dat",
            sep = ''
          )
        ),
        append = FALSE,
        row.names = FALSE,
        col.names = FALSE
      )
      write.table(
        e,
        quote = FALSE,
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste(
            pa$index_pops[x],
            "overall_migration_fluxes_",
            "semester2",
            "_",
            "biolsce",
            sce,
            ".dat",
            sep = ''
          )
        ),
        append = FALSE,
        row.names = FALSE,
        col.names = FALSE
      )
      write.table(
        cbind(rep(pa$index_pops[x], 14), rep(0, 14))[1:14, ],
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste(
            "init_prop_migrants_pops_per_szgroup_biolsce",
            sce,
            ".dat",
            sep = ''
          )
        ),
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE
      )
    }
  }
}
cat(paste("init_prop_migrants_pops_per_szgroup_biolsce.dat\n", sep = ''))


################################
################################
################################
################################

##### FOR-LOOP OVER BIOLOGICAL SCENARIOS ############
# ....because some parts of the parameterization are scenario specific!
for (sce in unique(multiplier_for_biolsce_all_pops$sce)) {
  #for (sce in c(3:4)){

  cat(paste("sce ", sce, "\n"))

  #timesteps   <- 21       # time steps 10 years with 2 semesters each
  #NbPeriods   <- 2        # 2 semesters within the year
  timesteps <- 41 # time steps 10 years with 4 quarters each
  NbPeriods <- 4 # 4 quarter within the year
  pop <- 10000 # number of simulated individuals

  # init the output file with headers (a multimap for c++ with / pop idx / values over the szgroup)
  write(
    "stock  init_maturity_per_szgroup",
    file = file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = ''),
      paste("init_maturity_per_szgroup_biolsce", sce, ".dat", sep = '')
    ),
    append = FALSE
  )
  write(
    "stock  init_M_per_szgroup",
    file = file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = ''),
      paste("init_M_per_szgroup_biolsce", sce, ".dat", sep = '')
    ),
    append = FALSE
  )
  write(
    "stock  init_weight_per_szgroup",
    file = file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = ''),
      paste("init_weight_per_szgroup_biolsce", sce, ".dat", sep = '')
    ),
    append = FALSE
  )
  write(
    "stock  init_pops_per_szgroup",
    file = file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = ''),
      paste("init_pops_per_szgroup_biolsce", sce, ".dat", sep = '')
    ),
    append = FALSE
  )
  write(
    "stock  init_fecundity_per_szgroup",
    file = file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = ''),
      paste("init_fecundity_per_szgroup_biolsce", sce, ".dat", sep = '')
    ),
    append = FALSE
  )
  write(
    "stock  init_proprecru_per_szgroup",
    file = file.path(
      general$main.path.ibm,
      paste("popsspe_", general$application, sep = ''),
      paste("init_proprecru_per_szgroup_biolsce", sce, ".dat", sep = '')
    ),
    append = FALSE
  )

  ##### FOR-LOOP OVER POP ############
  for (x in 1:length(pa$K)) {
    cat(paste("pop ", x - 1, "\n"))

    #for(x in c(11,12)){
    if (!is.na(pa$index_pops[x])) {
      # species-specific parameters
      t0 <- pa$t0[x] #t0 Bertalanffy
      K <- pa$K[x] #K Bertalanffy
      Linf <- pa$Linf[x] #Linf Bertalanfy
      CV_Linf <- pa$CV_Linf[x]
      l50 <- pa$L50[x] #L50
      mat_B <- pa$mat_B[x] # maturity ogive UPDATED the 2025-06-16
      d <- NA #d*L^e fecundity
      e <- NA
      stock <- rownames(pa)[x]
      aa <- pa$a[x] # e.g. aa*(length+2.5cm)^bb/1000
      bb <- pa$b[x]
      # Ricker stock-recruitment model fit: R=a*S*exp(-bS)
      # a is related to productivity (recruits per stock unit at small stock size) and b to density dependence. (a, b > 0).
      a_SSB <- pa$alpha[x] #Ricker param (unless the given value is >2000 then activating a shortcut to add a fixed nb of recruits in absolute value)
      b_SSB <- pa$beta[x]
      nat_M <- pa$nat_M[x] # natural mortality UPDATED the 2025-06-16
      r_age <- pa$r_age[x] # need to be 1 or integer?
      CV_recru <- pa$CV_recru[x]
      size_bin_cm <- pa$sz_bin_cm[x]
      unit_sizebin <- pa$unit_sizebin[x] # if 1 then cm, if 0.1 then mm etc.
      FMSY <- pa$FMSY[x]

      # get the sce matrix specific to this pop.
      multiplier_for_biolsce <- multiplier_for_biolsce_all_pops[
        multiplier_for_biolsce_all_pops$pop == as.character(stock) &
          multiplier_for_biolsce_all_pops$sce == sce,
      ]
      if (nrow(multiplier_for_biolsce) != 0) {
        # ie in case the pop is found in the sce matrix....
        # species-specific parameters AND sce
        K <- K *
          as.numeric(as.character(multiplier_for_biolsce[
            multiplier_for_biolsce$sce == sce &
              multiplier_for_biolsce$pop == stock,
            "biolsce_Ks"
          ]))
        Linf <- Linf *
          as.numeric(as.character(multiplier_for_biolsce[
            multiplier_for_biolsce$sce == sce &
              multiplier_for_biolsce$pop == stock,
            "biolsce_Linfs"
          ]))
        CV_Linf <- CV_Linf *
          as.numeric(as.character(multiplier_for_biolsce[
            multiplier_for_biolsce$sce == sce &
              multiplier_for_biolsce$pop == stock,
            "biolsce_CVLinfs"
          ]))
        l50 <- l50
        mat_B <- mat_B # UPDATED the 2025-06-16. Reconsider whether can be affected by the multiplier
        d <- d *
          as.numeric(as.character(multiplier_for_biolsce[
            multiplier_for_biolsce$sce == sce &
              multiplier_for_biolsce$pop == stock,
            "biolsce_fecundity"
          ]))
        e <- e
        aa <- aa
        bb <- bb *
          as.numeric(as.character(multiplier_for_biolsce[
            multiplier_for_biolsce$sce == sce &
              multiplier_for_biolsce$pop == stock,
            "biolsce_weight"
          ]))
        a_SSB <- a_SSB *
          as.numeric(as.character(multiplier_for_biolsce[
            multiplier_for_biolsce$sce == sce &
              multiplier_for_biolsce$pop == stock,
            "biolsce_recru_rickeralpha"
          ]))
        b_SSB <- b_SSB *
          as.numeric(as.character(multiplier_for_biolsce[
            multiplier_for_biolsce$sce == sce &
              multiplier_for_biolsce$pop == stock,
            "biolsce_recru_rickerbeta"
          ]))
        nat_M <- nat_M # UPDATED the 2025-06-16. Reconsider whether can be affected by the multiplier
        r_age <- r_age
        FMSY <- FMSY *
          as.numeric(as.character(multiplier_for_biolsce[
            multiplier_for_biolsce$sce == sce &
              multiplier_for_biolsce$pop == stock,
            "FMSYmulti"
          ]))
      }

      #simulate individual growth trajectories
      indlength <- mat.or.vec(pop, timesteps) # define growth matrix
      meanI <- mat.or.vec(pop, timesteps) # mean Increment
      inc <- mat.or.vec(pop, timesteps) # increment
      varI <- mat.or.vec(pop, timesteps) # variance of increment

      #assign initial size of recruits
      for (i in 1:pop) {
        indlength[i, 1] <- abs(rnorm(1, 0.5, 0.5))
      }

      # create growth trajectories
      if (!is.na(pa$Linf[x])) {
        for (ii in 1:pop) {
          for (jj in 2:timesteps) {
            varL <- CV_Linf * Linf
            Linfe <- rnorm(1, mean = Linf, sd = sqrt(varL)) #stochasticity in Linf
            if (Linfe <= indlength[ii, (jj - 1)]) {
              inc[ii, jj] <- 0
            } else {
              varK <- 0.1 * K
              Kr <- abs(rnorm(1, mean = K, sd = sqrt(varK))) #stochasticity in K

              meanI[ii, jj] <- (Linfe - indlength[ii, (jj - 1)]) *
                (1 - exp(-Kr * ((1 / NbPeriods) - (t0 / NbPeriods))))
              varI[ii, jj] <- 0.01 * meanI[ii, jj]

              inc[ii, jj] <- abs(rnorm(
                1,
                mean = meanI[ii, jj],
                sd = sqrt(varI[ii, jj])
              ))
            } #stochasticity in growth increments
            indlength[ii, jj] <- indlength[ii, (jj - 1)] + inc[ii, jj]
          }
        }

        times <- t(matrix(1, timesteps, pop) * (1:timesteps))
        if (do_plot) {
          plot(times, indlength)
        } #plot growth curves

        l <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1000) *
          size_bin_cm *
          unit_sizebin # vector of 14 length groups of eg 10 cm bin or 5 cm

        #size groups from growth trajectories
        S <- matrix(0, pop, timesteps)
        for (i in 1:timesteps) {
          S[, i] <- cut(indlength[, i], breaks = l, labels = FALSE)
        }

        ## ALK Age-Length Keys
        #1- build age distribution matrix A
        #2- build szgroup distribution matrix C
        A <- matrix(0, length(l), 11) # 11 age classes
        if (semester_growth) {
          B <- matrix(0, length(l), 21)
        } # 21 tsteps
        if (quarter_growth) {
          B <- matrix(0, length(l), 41)
        } # 41 tsteps
        C <- matrix(0, length(l), 11) # 11 age classes
        for (sz in 1:length(l)) {
          B[sz, ] <- apply(
            S,
            2,
            function(x, sz) {
              length(x[x == sz])
            },
            sz
          ) # nb in size groups per age and semesters , age group over ten years
        }
        C[, 1:2] <- B[, 1:2] # keep intact semesters 1 and 2 for the first year  #age 0 from first semester   #age 1 from second semester

        if (semester_growth) {
          count <- 2
          for (ij in c(3, 5, 7, 9, 11, 13, 15, 17, 19)) {
            count <- count + 1
            C[, count] <- B[, ij] + B[, ij + 1] # add semester 1 and semester 2
          }
        }

        if (quarter_growth) {
          count <- 0
          for (ij in c(1, 5, 9, 13, 17, 21, 25, 29, 33, 36)) {
            count <- count + 1
            C[, count] <- B[, ij] + B[, ij + 1] + B[, ij + 2] + B[, ij + 3] # add Q 1 and Q 2 and Q3 and Q4
          }
        }

        A <- sweep(C, 2, apply(C, 2, sum), FUN = "/") # then scale to 1 PER AGE => distribution of age over szgroups
        C <- sweep(C, 1, apply(C, 1, sum), FUN = "/") # then scale to 1 PER SZGROUP  => distribution of szgroup over ages
        C <- replace(C, is.na(C), 0)
        A <- replace(A, is.na(A), 0)

        A <- round(A, 7)
        As <- t(A)
        C <- round(C, 7)
        Cs <- t(C)

        # check for leaks and correct if required:
        if (!all(apply(As[1:11, 1:14], 1, sum) == 1)) {
          idx <- apply(As[1:11, 1:14], 1, sum) < 1.0
          As[idx, 14] <- 1 - apply(As[idx, , drop = FALSE], 1, sum) # a fix
        }
        if (!all(apply(Cs[1:11, 1:14], 2, sum) == 1)) {
          idx <- apply(Cs[1:11, 1:14], 2, sum) < 1.0
          Cs[11, idx] <- 1 - apply(Cs[, idx, drop = FALSE], 2, sum) # a fix
        }

        write(
          As[1:11, 1:14],
          file = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ''),
            paste(
              pa$index_pops[x],
              "spe_percent_age_per_szgroup_biolsce",
              sce,
              ".dat",
              sep = ''
            )
          ),
          ncolumns = 11,
          sep = " "
        ) #age 0-10
        cat(paste(
          "spe_percent_age_per_szgroup_biolsce",
          sce,
          ".dat\n",
          sep = ''
        ))
        write(
          Cs[1:11, 1:14],
          file = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ''),
            paste(
              pa$index_pops[x],
              "spe_percent_szgroup_per_age_biolsce",
              sce,
              ".dat",
              sep = ''
            )
          ),
          ncolumns = 11,
          sep = " "
        ) #age 0-10
        cat(paste(
          "spe_percent_szgroup_per_age_biolsce",
          sce,
          ".dat\n",
          sep = ''
        ))
      }

      #init proportion of recruits per size group
      # i.e. extract one column (the column of the age of recruit) from the percent_age_per_szgroup matrix....
      if (!is.na(r_age)) {
        proprecru <- As[r_age + 1, ] # caution: add + 1 for offset i.e. if age 0 => column 1
      } else {
        proprecru <- rep(0, 14)
      }
      init_proprecru <- rbind(pa$index_pops[x], proprecru)
      write(
        init_proprecru[, 1:14],
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste("init_proprecru_per_szgroup_biolsce", sce, ".dat", sep = '')
        ),
        sep = " ",
        ncolumns = 2,
        append = TRUE
      )
      cat(paste("init_proprecru_per_szgroup_biolsce", sce, ".dat\n", sep = ''))

      # init number per size group   comes in Thousands!
      if (!stock %in% implicit_stocks) {
        if (nrow(multiplier_for_biolsce) != 0) {
          number1 <- number[
            stock,
            c("X0", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
          ] *
            as.numeric(as.character(multiplier_for_biolsce[
              multiplier_for_biolsce$sce %in% sce,
              "biolsce_init_pops"
            ])) #age 0-10
        } else {
          number1 <- number[
            stock,
            c("X0", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
          ]
        }
        number1_yplus1 <- number_yplus1[
          stock,
          c("X0", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
        ] #age 0-10

        number1[is.na(number1)] <- 0
        number1_yplus1[is.na(number1_yplus1)] <- 0
        pops <- matrix(0, 15, 11)
        pops_yplus1 <- matrix(0, 15, 11)
        for (i in 1:10) {
          pops[, i] <- A[, i] * as.numeric(as.character(number1[1, i]))
          pops_yplus1[, i] <- A[, i] *
            as.numeric(as.character(number1_yplus1[1, i]))
        }

        init_pops <- rbind(pa$index_pops[x], rowSums(pops))
        init_pops_yplus1 <- rbind(pa$index_pops[x], rowSums(pops_yplus1))
        #init_pops<-round(init_pops)  not necessary because ind. in thousands
        write(
          init_pops[, 1:14],
          file = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ''),
            paste("init_pops_per_szgroup_biolsce", sce, ".dat", sep = '')
          ),
          sep = " ",
          ncolumns = 2,
          append = TRUE
        )
        write(
          init_pops_yplus1[, 1:14],
          file = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ''),
            paste(
              "init_pops_per_szgroup_",
              a.year + 1,
              "_biolsce",
              sce,
              ".dat",
              sep = ''
            )
          ),
          sep = " ",
          ncolumns = 2,
          append = TRUE
        )
        cat(paste("init_pops_per_szgroup_biolsce", sce, ".dat\n", sep = ''))
      } else {
        # fill in with fake numbers for implicit pops i.e.
        #  the pops for which we do not have info on N because not assessed by ICES
        # in this case the pop is not truly simulated in the IBM simulation but catches can still be done
        # using historic vessel and species-specific cpues...see Bastardie et al 2010
        options(scipen = 99)
        write.table(
          cbind(rep(pa$index_pops[x], 14), rep(100000, 14))[1:14, ],
          file = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ''),
            paste("init_pops_per_szgroup_biolsce", sce, ".dat", sep = '')
          ),
          append = TRUE,
          row.names = FALSE,
          col.names = FALSE
        )
        write.table(
          cbind(rep(pa$index_pops[x], 14), rep(100000, 14))[1:14, ],
          file = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ''),
            paste(
              "init_pops_per_szgroup_",
              a.year + 1,
              "_biolsce",
              sce,
              ".dat",
              sep = ''
            )
          ),
          append = TRUE,
          row.names = FALSE,
          col.names = FALSE
        )
        cat(paste("init_pops_per_szgroup_biolsce", sce, ".dat\n", sep = ''))
      }

      #init weight per size group
      if (!is.na(aa)) {
        weight <- aa * (l + (size_bin_cm / 2))^bb / 1000 #length-weight in cm-g from fishbase, here divided by 1000 ->> cm-kg
      } else {
        weight <- rep(0, 14)
      }
      init_weight <- rbind(pa$index_pops[x], weight)
      write(
        init_weight[, 1:14],
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste("init_weight_per_szgroup_biolsce", sce, ".dat", sep = '')
        ),
        sep = " ",
        ncolumns = 2,
        append = TRUE
      )
      cat(paste("init_weight_per_szgroup_biolsce", sce, ".dat\n", sep = ''))

      #build size transition matrix G
      incr <- inc[, -c(1)] # remove first column, growth from previous to present size
      increment <- c(incr) # vectorize
      leng <- indlength[, -c(21)] # remove last column , to combine length with growth increment to next size group
      len <- c(leng) # vectorize

      values <- mat.or.vec(length(len), 4)
      values[, 1] <- as.numeric(as.character(len))
      values[, 4] <- as.numeric(as.character(increment))

      val <- cut(values[, 1], breaks = l) # put into size bins
      values[, 2] <- val
      levels(val)
      levels(val) <- l # change labels of size bins
      values[, 3] <- as.numeric(as.character(val)) # create vector of lower bounds in 10cm intervals

      n <- length(l) - 1
      G <- matrix(0, (n), (n))

      for (b in 1:n) {
        for (a in 1:n) {
          if (b <= a) {
            value <- subset(values, values[, 2] == b)
            if (var(value[, 4]) == 0 | length(value[, 1]) < 2) {
              G[a, b] <- 0
            } else {
              mea <- mean(value[, 4])
              vari <- var(value[, 4])
              fun <- function(x) {
                dnorm(
                  x,
                  mean = (l[b] + (size_bin_cm / 2) + mea),
                  sd = sqrt(vari)
                )
              }
              G[a, b] <- integrate(fun, l[a], l[a + 1])$value
            }
          }
        }
      }

      G <- round(G, 4)
      if (all(G == 0)) {
        G[1, 1] <- 1
      } # e.g. blue mussels

      # G should sum to 1 in columns
      if (!all(apply(G[1:14, 1:14], 2, sum) == 1)) {
        idx2 <- apply(G[1:14, 1:14], 2, function(x) {
          ss <- which(x != 0)
          sss <- ss[length(ss)]
          if (length(sss) <= 0) {
            sss <- 14
          }
          return(sss)
        })
        a_leak <- 1 - apply(G[,, drop = FALSE], 2, sum)
        for (j in 1:14) {
          G[idx2[j], j] <- G[idx2[j], j] + a_leak[j]
        } # a fix to avoid leaks
      }

      write.table(
        G,
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste(
            pa$index_pops[x],
            "spe_size_transition_matrix_biolsce",
            sce,
            ".dat",
            sep = ''
          )
        ),
        sep = " ",
        col.names = FALSE,
        row.names = FALSE
      )
      cat(paste(
        pa$index_pops[x],
        "spe_size_transition_matrix_biolsce",
        sce,
        ".dat\n",
        sep = ''
      ))

      #build size distribution vector L
      # UPDATED the 2025-06-16
      # surv <- round(exp(-(0.12 * 27 * (l + (size_bin_cm / 2))^(-1))), 4) #length dependent mortality vector using the lower bound length (+1 to ignore 0) to get survival
      # mort <- round((1 - surv), 4)
      mort <- rep(nat_M, length.out = length(l)) # For our case study, antural mortality is the same across all sizes

      if (nrow(multiplier_for_biolsce) != 0) {
        mort <- mort *
          as.numeric(as.character(multiplier_for_biolsce[
            multiplier_for_biolsce$sce %in% sce,
            "biolsce_M"
          ]))
      }

      ## EXPORT
      mort <- cbind(pa$index_pops[x], mort)
      write.table(
        mort[1:14, ],
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste("init_M_per_szgroup_biolsce", sce, ".dat", sep = '')
        ),
        append = TRUE,
        sep = " ",
        col.names = FALSE,
        row.names = FALSE
      )
      cat(paste("init_M_per_szgroup_biolsce.dat\n", sep = ""))

      #need a first row to describe recruitment for stable size distribution from SSB
      fec <- d * (l + (size_bin_cm / 2))^(e) #   fecundity
      fec[is.na(fec)] <- 0
      fec <- round(fec, 2)
      mat <- 1 / (1 + exp(mat_B * (l + (size_bin_cm / 2) - l50))) #maturity ogive
      mat[is.na(mat)] <- 0
      mat <- round(mat, 4)
      mat[1] <- 0

      mat <- cbind(pa$index_pops[x], mat)
      write.table(
        mat[1:14, ],
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste("init_maturity_per_szgroup_biolsce", sce, ".dat", sep = '')
        ),
        append = TRUE,
        sep = " ",
        col.names = FALSE,
        row.names = FALSE
      )
      cat(paste("init_maturity_per_szgroup_biolsce.dat\n", sep = ""))

      #browser()
      # check SSB
      #sum(init_pops)
      #sum(number1)
      #print(sum(init_pops[2,1:14]*1000*weight[1:14]*mat[1:14, 2])/1000) # in tons

      if (!is.na(e)) {
        fec <- cbind(pa$index_pops[x], fec)
        write.table(
          fec[1:14, ],
          file = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ''),
            paste("init_fecundity_per_szgroup_biolsce", sce, ".dat", sep = '')
          ),
          append = TRUE,
          sep = " ",
          col.names = FALSE,
          row.names = FALSE
        )
      } else {
        # fill in with fake numbers for implicit pops i.e.
        #  the pops for which we do not have info on N because not assessed by ICES
        # in this  case the pop is not truly simulated in the IBM simulation but catches can still be done
        # using historic vessel and species-specific cpues...see Bastardie et al 2010
        options(scipen = 99)
        write.table(
          cbind(rep(pa$index_pops[x], 14), rep(100000, 14))[1:14, ],
          file = file.path(
            general$main.path.ibm,
            paste("popsspe_", general$application, sep = ''),
            paste("init_fecundity_per_szgroup_biolsce", sce, ".dat", sep = '')
          ),
          append = TRUE,
          row.names = FALSE,
          col.names = FALSE
        )
        cat(paste("init_fecundity_per_szgroup_biolsce.dat\n", sep = ""))
      }
    } # end if

    a_SSB <- replace(a_SSB, is.na(a_SSB), 0) # put 0 instead of NA because 'double' required by c++
    b_SSB <- replace(b_SSB, is.na(b_SSB), 0) # put 0 instead of NA because 'double' required by c++

    # Define recruitment using a Ricker, Beverton and Holt models, fixed method,
    # 0 for Ricker, 1 for B&H, and 2 for fixed
    recruit_method <- 1

    # SSB-R

    # Original code
    # write.table(c(a_SSB, b_SSB, CV_recru, recruit_method),
    #             file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
    #                            paste(pa$index_pops[x],"spe_SSB_R_parameters_biolsce",sce,".dat",sep='')),
    #             append=FALSE, sep=" ", col.names=FALSE, row.names=FALSE, quote=FALSE)
    # cat(paste(pa$index_pops[x],"spe_SSB_R_parameters_biolsce.dat\n", sep=""))

    # Update to include fixed recruitment consideration (UPDATED 26/12/2024)
    if (recruit_method == 2) {
      write.table(
        read.table(
          file = file.path(
            general$main_path_gis,
            "POPULATIONS",
            "SSB_R_parameters",
            paste(
              pa$index_pops[x],
              "spe_SSB_R_parameters_biolsce.dat",
              sep = ''
            )
          ),
          header = FALSE
        ) %>%
          pull(),
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste(
            pa$index_pops[x],
            "spe_SSB_R_parameters_biolsce",
            sce,
            ".dat",
            sep = ''
          )
        ),
        append = FALSE,
        sep = " ",
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE
      )
      cat(paste(
        pa$index_pops[x],
        "spe_SSB_R_parameters_biolsce.dat\n",
        sep = ""
      ))
    } else {
      write.table(
        c(a_SSB, b_SSB, CV_recru, recruit_method),
        file = file.path(
          general$main.path.ibm,
          paste("popsspe_", general$application, sep = ''),
          paste(
            pa$index_pops[x],
            "spe_SSB_R_parameters_biolsce",
            sce,
            ".dat",
            sep = ''
          )
        ),
        append = FALSE,
        sep = " ",
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE
      )
      cat(paste(
        pa$index_pops[x],
        "spe_SSB_R_parameters_biolsce.dat\n",
        sep = ""
      ))
    }

    # initial TAC
    write.table(
      as.numeric(as.character(pa[x, "tac_tons"])),
      file = file.path(
        general$main.path.ibm,
        paste("popsspe_", general$application, sep = ''),
        paste(pa$index_pops[x], "spe_initial_tac.dat", sep = '')
      ),
      append = FALSE,
      sep = " ",
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
    ) # pa$TAC is informed from
    cat(paste(pa$index_pops[x], "spe_initial_tac.dat\n", sep = ""))

    # fbar ages and LTMP F target and Fpercent e.g. f multiplier +/-10%  and TAC range e.g. +/-15%  and Btrigger and F-MSY
    some_params <- pa[
      x,
      c(
        'fbar_age_min',
        'fbar_age_max',
        'F_target',
        'F_percent',
        'TAC_percent',
        'B_trigger',
        FMSY = 'FMSY'
      )
    ]
    some_params$FMSY <- FMSY
    write.table(
      some_params,
      file = file.path(
        general$main.path.ibm,
        paste("popsspe_", general$application, sep = ''),
        paste(
          pa$index_pops[x],
          "spe_fbar_amin_amax_ftarget_Fpercent_TACpercent_biolsce",
          sce,
          ".dat",
          sep = ''
        )
      ),
      append = FALSE,
      sep = " ",
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
    )
    cat(paste(
      pa$index_pops[x],
      "spe_fbar_amin_amax_ftarget_Fpercent_TACpercent.dat\n",
      sep = ""
    ))
  } # end for
} # end loop over sce

cat(paste(".....stored in", general$main.path.ibm, "\n"))


# SPECIAL CASES-----------------
# overwrite when needed:
initialize_N_at_szgroup_from_an_external_file <- TRUE
if (initialize_N_at_szgroup_from_an_external_file) {
  sces <- 1:4
  for (sce in sces) {
    number <- read.csv(
      file = file.path(
        general$main_path_gis,
        "POPULATIONS",
        "Stock_abundances_at_szgroup.csv"
      ),
      sep = ",",
      header = TRUE
    )

    write(
      "stock  init_pops_per_szgroup",
      file = file.path(
        general$main.path.ibm,
        paste("popsspe_", general$application, sep = ''),
        paste("init_pops_per_szgroup_biolsce", sce, ".dat", sep = '')
      ),
      append = FALSE
    )

    numbers <- NULL
    for (st in 1:nrow(number)) {
      numbers <- unlist(c(numbers, as.vector(number[st, -1])))
    }
    numbers <- cbind(rep(0:(nrow(number) - 1), each = 14), numbers)

    if (length(number[1, -1]) != 14) {
      stop("we need 14 szgroups i.e. 0 to 13")
    }

    # fix NAs
    numbers[is.na(numbers)] <- 0

    write.table(
      numbers,
      file = file.path(
        general$main.path.ibm,
        paste("popsspe_", general$application, sep = ''),
        paste("init_pops_per_szgroup_biolsce", sce, ".dat", sep = '')
      ),
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE
    )
  }
}


cat(paste(".....done\n"))
