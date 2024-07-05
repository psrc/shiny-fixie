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
source("functions.R")
source("db_connection.R")


# list of columns showing in trip table ----
view.cols <- c("tripnum","modes_desc","daynum","depart_dhm","arrive_dhm","miles","mph","Error",
               "cotravelers","origin_purpose","dest_purpose","dest_name","duration_at_dest",
               "origin_coord","dest_coord","recid","rc","elevate_issue")

list_mode_choice <- get_query(sql = "select * from HHSurvey.trip_mode", db_name = "hhts_cleaning")$mode_desc

# get variable values
codebook_2023 <- readxl::read_xlsx("J:/Projects/Surveys/HHTravel/Survey2023/Data/old_stuff/data_deliverable_81823/codebook_guide/PSRC_Combined_Codebook_2023_08162023.xlsx", 
                                   sheet = 'value_labels_2023')


# val_list_mode_1 <- value_labels %>% filter(variable =="mode_1") %>%
#   rowwise() %>%
#   mutate(value_label = paste0(value, ": ", label)) %>%
#   ungroup() %>%
#   select(value, value_label)
#   
# get_var_value_list(var_name="mode_1")
