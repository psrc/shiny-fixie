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
library(rclipboard)
library(shinyvalidate)

# Run Modules Files ---------------------------------------------------------------------------
# This section runs the modules and unless the folder name changes, it doesn't need to be changed
# It also loads in useful functions for dashboard creation
getwd()
module_files <- c( list.files('modules', full.names = TRUE) )
function_files <- c( list.files('functions', full.names = TRUE) )
sapply(module_files, source)
sapply(function_files, source)
source("configuration.R")

list_mode_choice <- get_query(sql = "select * from HHSurvey.trip_mode", db_name = "hhts_cleaning")$mode_desc


