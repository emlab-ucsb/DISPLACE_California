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


nb_pops <- length(spp)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#species_interactions_mortality_proportion_matrix_biolsce1
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

mortality_interactions <- matrix(0, nrow = length(spp), ncol = length(spp))
diag(mortality_interactions) <- 1.0 # i.e. assuming no interaction by default


write.table(
  mortality_interactions, # the szgroup dimension is removed....
  file = file.path(
    general$main.path.ibm,
    paste0("popsspe_", general$application, sep = ''),
    paste(
      "species_interactions_mortality_proportion_matrix_biolsce1.dat",
      sep = ''
    )
  ),
  col.names = FALSE,
  row.names = FALSE,
  sep = ' ',
  quote = FALSE,
  append = FALSE
)

cat(paste(
  "Write species_interactions_mortality_proportion_matrix_biolsce1.dat....done \n"
))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#juveniles_diet_preference_per_stock.csv
#adults_diet_preference_per_stock.csv
#juveniles_diet_preference_per_stock_allstks_biolsce1.dat
#adults_diet_preference_per_stock_allstks_biolsce1.dat

if (
  !file.exists(file.path(
    general$main_path_gis,
    "POPULATIONS",
    "juveniles_diet_preference_per_stock.csv"
  ))
) {
  juveniles_diet_preference <- cbind.data.frame(
    stks = rep(0:(length(spp) - 1), each = length(spp)),
    diet_pref = 0
  )
  adults_diet_preference <- cbind.data.frame(
    stks = rep(0:(length(spp) - 1), each = length(spp)),
    diet_pref = 0
  )
  dim(juveniles_diet_preference)
  #=> all at 0 by default

  mat <- matrix(0, length(spp), length(spp))
  # pred in row, prey in column

  juveniles_diet_preference <- cbind(
    rep(0:(length(spp) - 1), each = length(spp)),
    as.vector(mat)
  )
  # note: predator first dim, prey second dim.

  mat <- matrix(0, length(spp), length(spp))
  # pred in row, prey in column

  #ADULTS DIET

  adults_diet_preference <- cbind(
    rep(0:(length(spp) - 1), each = length(spp)),
    as.vector(mat)
  )

  adults_diet_preference <- cbind(
    rep(0:(length(spp) - 1), each = length(spp)),
    as.vector(mat)
  )
} else {
  juveniles_diet_preference <- read.table(
    file.path(
      general$main_path_gis,
      "POPULATIONS",
      "juveniles_diet_preference_per_stock.csv"
    ),
    header = TRUE,
    sep = ";"
  )
  adults_diet_preference <- read.table(
    file.path(
      general$main_path_gis,
      "POPULATIONS",
      "adults_diet_preference_per_stock.csv"
    ),
    header = TRUE,
    sep = ";"
  )
}


#----------
#----------
#----------

# biolsce
biosce <- read.table(
  file = file.path(
    general$main.path.ibm,
    paste("multiplier_for_biolsce", general$application, ".dat", sep = '')
  ),
  header = TRUE
)

# repeat and export
for (sce in unique(biosce$sce)) {
  write.table(
    juveniles_diet_preference,
    file = file.path(
      general$main.path.ibm,
      paste0("popsspe_", general$application, sep = ''),
      paste0(
        "juveniles_diet_preference_per_stock_allstks_biolsce",
        sce,
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

  write.table(
    adults_diet_preference,
    file = file.path(
      general$main.path.ibm,
      paste0("popsspe_", general$application, sep = ''),
      paste0(
        "adults_diet_preference_per_stock_allstks_biolsce",
        sce,
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

cat(paste("Write _diet_preference_per_stock_allstks_biolsceXX.dat....done \n"))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# because used with the sizeSpectra Option:
# colnames required by the DISPLACE core c++ for sizeSpectra:
# 0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34
# stock	Winf	k	Linf	K	t0	a	b	L50	alpha	beta	r_age	tac_tons	fbar_age_min	fbar_age_max	F_target	F_percent	TAC_percent	B_trigger	FMSY	fbar_assessment	ssb_assessment	mls_cat	mls	size_bin_cm	unit_sizebin	CV_recru	mat	mat_cat	etha_m	kappa	q	n	fzeroest	species

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

cn <- c(
  'stock',
  'Winf',
  'k',
  'Linf',
  'K',
  't0',
  'a',
  'b',
  'L50',
  'alpha',
  'beta',
  'r_age',
  'tac_tons',
  'fbar_age_min',
  'fbar_age_max',
  'F_target',
  'F_percent',
  'TAC_percent',
  'B_trigger',
  'FMSY',
  'fbar_assessment',
  'ssb_assessment',
  'mls_cat',
  'mls',
  'sz_bin_cm',
  'unit_sizebin',
  'CV_recru',
  'mat',
  'mat_cat',
  'etha_m',
  'kappa',
  'q',
  'n',
  'fzeroest',
  'species'
)


# check
cn[!cn %in% colnames(params_pops)]


to_export <- params_pops[spp, cn]

to_export$species <- NA # because possibly weird characters

# remove NAs
to_export <- cbind.data.frame(
  stock = to_export[, 1],
  sapply(to_export[, -1], function(x) as.numeric(as.character(x)))
)
to_export[is.na(to_export) | to_export == "<NA>" | to_export == ""] <- 0

write.table(
  to_export,
  file = file.path(
    general$main.path.ibm,
    paste0("popsspe_", general$application, sep = ''),
    paste("Stock_biological_traits.csv", sep = '')
  ),
  sep = ";",
  col.names = TRUE,
  row.names = FALSE,
  quote = FALSE
)


# Not used in DISPLACE California

# background_mortality in the size spectra modelling.
# deduced from Andersen et al. szgroup specific.
# l<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,1000) *size_bin_cm  * unit_sizebin
#from surv<-round(exp(-(0.12*27*(l+(size_bin_cm/2))^(-1))),4)  # length dependent mortality vector using the lower bound length (+1 to ignore 0) to get survival
#mort<-round((1-surv),4)
#or should we use Audric?s way?:  values corresponds to (mu_0*Winf)^ (n-1) with n =2/3
#or mu_zero*((pa["SPR.nsea","a"]*(l+5/2)^pa["SPR.nsea","b"]))^((2/3)-1) with mu_zero at 2 or mu_zero=27*0.12 ??   (see Eq 3 in andersen et al 2009 How community ecology links natural mortality, growth, and production of fish populations Eq. 3)

background_M <- rep(
  c(
    0.7264,
    0.3508,
    0.2283,
    0.1690,
    0.1341,
    0.1111,
    0.0949,
    0.0828,
    0.0734,
    0.0659,
    0.0598,
    0.0548,
    0.0505,
    0.0469
  ),
  length = nb_pops * 14
) # i.e. 1-exp(-(0.12*27)*(l+(5/2))^(-1))
background_mortality <- cbind(
  stock = rep(0:(nb_pops - 1), each = 14),
  background_M = background_M
)

write.table(
  background_mortality,
  file = file.path(
    general$main.path.ibm,
    paste0("popsspe_", general$application),
    "background_mortality_biolsce1.dat"
  ),
  col.names = TRUE,
  row.names = FALSE,
  quote = FALSE
)

#  /vector<double> beta (simModel->config().nbpops, 100);   // prey size selection parameter # see Mizer params@species_params  // Predation/prey mass ratio
#// replace with logistic per 14 weight class
#// beta_end + (beta_begin - beta_end) *(1+ exp(1*(w0 -wend)))/(1+ exp(1*(w -wend)))  with beta_begin=100 and beta_end=300 so that larger fish eats on much smaller fish

beta_ssm <- rep(
  c(
    100.0001,
    100.0215,
    100.1079,
    100.3115,
    100.6974,
    101.3550,
    102.4202,
    104.1142,
    106.8150,
    111.1882,
    118.4140,
    130.5108,
    150.4701,
    180.9963
  ),
  length = nb_pops * 14
)

beta_ssm <- cbind(stock = rep(0:(nb_pops - 1), each = 14), beta_ssm = beta_ssm)

write.table(
  beta_ssm,
  file = file.path(
    general$main.path.ibm,
    paste0("popsspe_", general$application),
    "beta_ssm_biolsce1.dat"
  ),
  col.names = TRUE,
  row.names = FALSE,
  quote = FALSE
)


cat(paste("....done \n"))
