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
codebook <- readxl::read_xlsx("J:/Projects/Surveys/HHTravel/Survey2023/Data/old_stuff/data_deliverable_81823/codebook_guide/PSRC_Combined_Codebook_2023_08162023.xlsx", 
                              sheet = 'value_labels_2023')

# database name
cleaning_database <- "hhts_cleaning"

# point of interest icons
poi_icons <- c("house", "briefcase", "school-flag")

# names of views for edits 
edit_persons_view_name <- list("All Persons" = "person_all_test",
                               "All Errors" = "person_all_error",
                               "Too Long at Dest." = "person_too_long_at_dest",
                               "Missing mode_1" = "person_mode_1_missing")

# columns showing in trip table
view.cols <- c("tripnum","modes_desc","daynum","depart_dhm","arrive_dhm","miles","mph","Error",
               "cotravelers","origin_purpose","dest_purpose","dest_name","duration_at_dest",
               "recid","rc","elevate_issue")

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


