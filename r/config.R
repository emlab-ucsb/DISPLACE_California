####
#### ENVIRONMENT ----
####

required_version <- "4.4.3" # minimum required R version

if (getRversion() < required_version) {
  stop(paste(
    "This script requires R version",
    required_version,
    "or higher. You are using",
    getRversion()
  ))
}

# Setup ----
# List of packages to install
packages <- c(
  "dplyr",
  "tictoc",
  "ggplot2",
  "tidyverse",
  "lubridate",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf",
  "raster",
  "janitor",
  "ggpubr",
  "lutz",
  "suncalc",
  "gridExtra",
  "readr",
  "adehabitatLT",
  "wildlifeDI",
  "units",
  "usmap",
  "RColorBrewer",
  "classInt",
  "janitor",
  "tidyverse",
  "mgcv",
  "concaveman",
  "bigrquery",
  "targets",
  "tarchetypes",
  "here",
  "quarto",
  "janitor",
  "tictoc",
  "knitr",
  "viridis",
  "kableExtra",
  "maps",
  "magick",
  "webshot2",
  "glue",
  "patchwork",
  "scales",
  "ggforce",
  "priceR",
  "lwgeom",
  "ggrepel",
  "purrr",
  "ggtext",
  "cowplot",
  "stringr",
  "broom",
  "ggnewscale",
  "htmltools",
  "officer",
  "flextable",
  "boot",
  "spatstat"
)


# Function to install packages if not available
install_if_not_available <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Install packages
sapply(packages, install_if_not_available)

# Special case for marmap which requires ncdf4
if (!requireNamespace("marmap", quietly = TRUE)) {
  # Check if ncdf4 is installed
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    # The ncdf4 package is required by marmap for reading and writing NetCDF files.
    # NetCDF is a set of software libraries and self-describing, machine-independent data formats
    # that support the creation, access, and sharing of array-oriented scientific data.
    # To install ncdf4, we first need to install the netcdf library using Homebrew (macOS).

    # Install netcdf using Homebrew (macOS)
    system("brew install netcdf")

    # Get the path to nc-config
    nc_config_path <- system("which nc-config", intern = TRUE)

    # Install ncdf4 package with the path to nc-config
    install.packages(
      "ncdf4",
      type = "source",
      configure.args = paste("--with-nc-config=", nc_config_path)
    )
  }
  install.packages("marmap")
}

# Load libraries
library(dplyr)
library(tictoc)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(marmap)
library(sf)
library(raster)
library(janitor)
library(ggpubr)
library(lutz)
library(suncalc)
library(gridExtra)
library(readr)
library(adehabitatLT) # for ltraj functions
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/wildlifeDI/wildlifeDI_0.5.1.tar.gz"
#  install.packages(packageurl, repos=NULL, type="source")
library(wildlifeDI) # for ltraj2sf. That function is deprecated so need to install wildlifeDI version < 1
library(units) # drop_units
library(usmap)
library(RColorBrewer)
library(classInt)
library(mgcv)
library(concaveman)
library(bigrquery)
library(here)
library(quarto)
library(janitor)
library(tictoc)
library(fs)
library(knitr)
library(viridis)
library(kableExtra)
library(maps)
library(magick)
library(webshot2)
library(glue)
library(patchwork)
library(scales)
library(readxl)
library(ggforce)
# remotes::install_github('rpkgs/gg.layers')
library(gg.layers)
library(priceR)
library(lwgeom)
library(ggrepel)
library(purrr)
library(ggtext)
library(cowplot)
library(stringr)
library(broom)
library(ggnewscale)
library(htmltools)
library(officer)
library(flextable)
library(boot)
library(spatstat)

####
#### GLOBAL OPTIONS ----
####

sf::sf_use_s2(FALSE) # Spherical geometry (s2) switched off
