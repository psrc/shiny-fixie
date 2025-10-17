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
library(leaflet)

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
tripeditor.cols <- c("mode_1","mode_2","mode_3","mode_4","mode_acc","mode_egr",
                     "origin_purpose","dest_purpose",
                     "distance_miles","origin_lat","origin_lng","dest_lat","dest_lng",
                     "driver","travelers_hh","travelers_nonhh",
                     "psrc_comment"
                     )

# for validating driver field in trip editor: include any drive modes that would require "driver" entry
drive_modes <- c("100","101")


