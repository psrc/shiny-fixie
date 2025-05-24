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

# codebook: get variable values
codebook <- readxl::read_xlsx("J:/Projects/Surveys/HHTravel/Survey2023/Data/old_stuff/data_deliverable_81823/codebook_guide/PSRC_Combined_Codebook_2023_08162023.xlsx", 
                              sheet = 'value_labels_2023')


# names of views for edits 
# to add a tab: remember to also fix number of tabs in server/ui
edit_persons_view_name <- c("person_all_test","person_all_error","person_too_long_at_dest","person_mode_1_missing")

# columns showing in trip table
view.cols <- c("tripnum","modes_desc","daynum","depart_dhm","arrive_dhm","miles","mph","Error",
               "cotravelers","origin_purpose","dest_purpose","dest_name","duration_at_dest",
               # "origin_coord","dest_coord",
               "recid","rc","elevate_issue")

