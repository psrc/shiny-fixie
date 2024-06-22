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
library(psrcelmer)
library(shinyjs)

# Run Modules Files ---------------------------------------------------------------------------
# This section runs the modules and unless the folder name changes, it doesn't need to be changed
# It also loads in useful functions for dashboard creation
module_files <- c(
  list.files('modules', full.names = TRUE),
  list.files('modules_edit_components', full.names = TRUE))
sapply(module_files, source)
# source("functions.R")
source("db_connection.R")


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




# Values for Drop Downs ---------------------------------------------------
# Section for creating the values needed in any dropdowns, lists, etc.
ntd_metric_list <- as.character(unique(ntd_data$metric))
ntd_mode_list <- ntd_data |> select("variable") |> filter(variable != "All Transit Modes") |> distinct() |> pull()



# my lists ----
view.cols <- c("tripnum","modes_desc","daynum","depart_dhm","arrive_dhm","miles","mph","Error",
               "cotravelers","origin_purpose","dest_purpose","dest_name","duration_at_dest",
               "origin_coord","dest_coord","recid","rc","elevate_issue")



