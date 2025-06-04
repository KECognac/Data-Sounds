# Setup script for setting up workspace

#Install (if necessary) and load all required packages ----------------

packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }

# vector of packages to load
packages <- c("sonify",
              "tuneR",
              "dataRetrieval",
              "dplyr",
              "pastecs",
              "ggplot2",
              "tidyverse",
              "seewave",
              "signal",
              "soundgen",
              "purrr"
)

packageLoad(packages)

## GitHub package installs ----------------

# suggested install of github version for climateR
# remotes::install_github("mikejohnson51/AOI") # suggested! But I don't think we need it..
if (!"climateR" %in% installed.packages()) {
  remotes::install_github("mikejohnson51/climateR")
}
library(climateR)


if(!"fluidsynth" %in% installed.packages()) {
  install.packages('fluidsynth', repos = 'https://ropensci.r-universe.dev')
}

library(fluidsynth)


source("src/note_conversion_utils.R")
source("src/sonify_data.R")
source("src/sonify_data2.R")
source("src/load_psn.R")

# source all functions --------------------------

#purrr::map(list.files(
#  path = "XXX/",
#  pattern = "*.R",
#  full.names = TRUE,
#  recursive = TRUE
#),
#source)

