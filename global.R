# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinyBS)
library(shinydashboard)
library(bs4Dash)
library(shinycssloaders)
library(bslib)

# Packages for Data Cleaning/Processing
library(tidyverse)

# Packages for Chart Creation
library(psrcplot)
library(echarts4r)

# Packages for Map Creation
library(sf)
library(leaflet)

# Packages for Table Creation
library(DT)

# Package for Excel Data Creation
library(openxlsx)

library(DTedit)
library(psrc.travelsurvey)
library(shinyjs)


df <- data.frame(
  Buy = c('Tea', 'Biscuits', 'Apples'),
  Quantity = c(7, 2, 5),
  stringsAsFactors = FALSE
)

# Run Modules Files ---------------------------------------------------------------------------
# This section runs the modules and unless the folder name changes, it doesn't need to be changed
# It also loads in useful functions for dashboard creation
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")


# Page Information --------------------------------------------------------
# This section reads in the csv files that provide the text used on the relevant pages
# Unless the file names change, it doesn't need to be changed
left_panel_info <- read_csv("data/left_panel_information.csv", show_col_types = FALSE)
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)

# Inputs ---------------------------------------------------------------
# Section for any standard inputs like the crs for wgs84
wgs84 <- 4326
load_clr <- "#91268F"
latest_yr <- "2023"

# Data via RDS files ------------------------------------------------------
# Section reading in inputs from the data folder. RDS files are R objects and read quickly

ntd_data <- readRDS("data/ntd_data.rds")



# HTS Data ------------------------------------------------------
# trip_data_17_19 <- get_hhts("2017_2019", "t", vars=c("trip_id","driver","mode_1",'dest_purpose_cat', 'origin_purpose_cat',
#                                                      "google_duration", 'trip_path_distance'))
# trip_data <- trip_data_17_19 %>%
#   dplyr::filter(household_id == 17100024) %>%
#   dplyr::select(c(13,2:8,)) %>%
#   dplyr::mutate(person_id = as.character(person_id))
# trip_data$mode_1 <- factor(trip_data$mode_1, levels=unique(trip_data_17_19$mode_1))



# Values for Drop Downs ---------------------------------------------------
# Section for creating the values needed in any dropdowns, lists, etc.
ntd_metric_list <- as.character(unique(ntd_data$metric))
ntd_mode_list <- ntd_data |> select("variable") |> filter(variable != "All Transit Modes") |> distinct() |> pull()

# person_list <- unique(trip_data$person_id)
