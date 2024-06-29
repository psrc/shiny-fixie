# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinyBS)
library(shinydashboard)
library(bs4Dash)
library(shinycssloaders)
library(bslib)
library(shinyjs)

library(tidyverse)
library(DT)
library(psrcelmer)

# Run Modules Files ---------------------------------------------------------------------------
# This section runs the modules and unless the folder name changes, it doesn't need to be changed
# It also loads in useful functions for dashboard creation
module_files <- c(
  list.files('modules', full.names = TRUE),
  list.files('modules_edit_components', full.names = TRUE))
sapply(module_files, source)
# source("functions.R")
source("db_connection.R")


# list of columns showing in trip table ----
view.cols <- c("tripnum","modes_desc","daynum","depart_dhm","arrive_dhm","miles","mph","Error",
               "cotravelers","origin_purpose","dest_purpose","dest_name","duration_at_dest",
               "origin_coord","dest_coord","recid","rc","elevate_issue")

list_mode_choice <- get_query(sql = "select * from HHSurvey.trip_mode", db_name = "hhts_cleaning")$mode_desc

