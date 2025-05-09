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
module_files <- c(
  list.files('modules', full.names = TRUE))
sapply(module_files, source)
source("functions.R")
source("db_connection.R")
source("configuration.R")

list_mode_choice <- get_query(sql = "select * from HHSurvey.trip_mode", db_name = "hhts_cleaning")$mode_desc


# val_list_mode_1 <- value_labels %>% filter(variable =="mode_1") %>%
#   rowwise() %>%
#   mutate(value_label = paste0(value, ": ", label)) %>%
#   ungroup() %>%
#   select(value, value_label)
#   
# get_var_value_list(var_name="mode_1")
