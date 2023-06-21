# Load Packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readr)
library(lmtest)
library(sandwich)
library(estimatr)
library(texreg)
library(glue)
library(purrr)
library(magrittr)
library(knitr)
library(kableExtra)

rm(list = ls()) # clear workspace



# IMPORT and CLEAN market data --------------------------------------------

source("r_scripts/data_prep.R")



# Prep phase-out data -----------------------------------------------------

weights <- FALSE # logical; set true if weights should be used

source("r_scripts/phase_out.R")



# Event Study -------------------------------------------------------------

# event study parameters
ev_length_pre <- 3
ev_length_post <- 3

est_length <- 255

reg_type <- "abate" # choose which model/specification to use
                        # "constant", "abate", "abate_diff"

# run event study
source("r_scripts/event_study.R")



# post-estimation (var, sd, p, t, ...) ------------------------------------

source("r_scripts/post_estimation.R")

# show results
marvel_at_results()