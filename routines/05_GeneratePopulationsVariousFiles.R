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


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# XXctrysspe_relative_stability_semesterXX.dat
# percent_landings_from_simulated_vessels.dat
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

for (pid in 0:(length(spp) - 1)) {
  for (a.semester in c(1, 2)) {
    #stop('Relative stability key needs to be defined for your own app')
    relative_stability_this_sp <- data.frame(Country = "DNK", Percent = 100)

    write.table(
      relative_stability_this_sp, # the szgroup dimension is removed....
      file = file.path(
        general$main.path.ibm,
        paste("popsspe_", general$application, sep = ''),
        paste(
          pid,
          "ctrysspe_relative_stability_semester",
          a.semester,
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
      pid,
      "ctrysspe_relative_stability_semester",
      a.semester,
      ".dat....done \n"
    ))
  }
}


#------------------------------------
percent_landings_from_simulated_vessels <- cbind(
  stock = 0:(length(spp) - 1),
  percent = 90
) # not used if no individual quotas...

write.table(
  percent_landings_from_simulated_vessels, # the szgroup dimension is removed....
  file = file.path(
    general$main.path.ibm,
    paste("popsspe_", general$application, sep = ''),
    paste("percent_landings_from_simulated_vessels.dat", sep = '')
  ),
  col.names = TRUE,
  row.names = FALSE,
  sep = ' ',
  quote = FALSE,
  append = FALSE
)

cat(paste("Write percent_landings_from_simulated_vessels.dat....done \n"))


cat(paste("....done \n"))
