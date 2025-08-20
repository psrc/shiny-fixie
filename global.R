# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinyBS)
library(shinydashboard)
library(bs4Dash)
library(shinycssloaders)
library(bslib)
library(shinyjs)
library(shinyTime)

library(tidyverse)
library(glue)
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

# codebook: get variable values
codebook <- read_csv("data/value_labels_2025.csv")

# database name
cleaning_database <- "hhts_cleaning"

# list of variables being edited in trip record editor (that would be shown in update preview comparison table)
tripeditor.cols <- c(#"depart_time_timestamp","arrival_time_timestamp", # commented out because they need additional processing
                     "mode_1","mode_2","mode_3","mode_4","mode_acc","mode_egr",
                     "origin_purpose","dest_purpose",
                     "distance_miles","origin_lat","origin_lng","dest_lat","dest_lng",
                     "driver","travelers_hh","travelers_nonhh",
                     # "dest_purpose_other","mode_other_specify",
                     # "hhmember1","hhmember2","hhmember3","hhmember4","hhmember5","hhmember6","hhmember7","hhmember8",
                     "psrc_comment"
                     )


