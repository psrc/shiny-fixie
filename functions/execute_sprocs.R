# Procedures triggered or executed via pass-through queries in Shiny-Fixie

# ---- Trip deletion ----
sproc_delete_trip <- function(recid){
  
  execute_query(glue("EXECUTE HHSurvey.shifixy_delete_trip @target_recid = {recid};"))
  
  notification_confirm_action("Successfully deleted trip")
  
}

# ---- Dismiss error flag ----
sproc_dismiss_flag <- function(recid, person_id){
  
  execute_query(glue("EXECUTE HHSurvey.shifixy_dismiss_flag @target_recid = {recid};"))
  
  notification_confirm_action("Successfully dismissed error flag")
  
}

# ---- Add reverse trip ----
sproc_insert_reverse_trip <- function(recid, reverse_startdatetime){
  
  execute_query(glue("EXECUTE HHSurvey.shifixy_insert_reverse_trip @target_recid = {recid}, @startdatetime = '{reverse_startdatetime}';"))
  
  notification_confirm_action("Successfully inserted reverse trip")
  
}

# ---- Link trips ----
sproc_link_trips <- function(recid){
  all_recids <- paste(recid, collapse = ",")
  # browser()
  execute_query(glue("EXECUTE HHSurvey.link_trip_via_id @recid_list = '{all_recids}';"))
  
  notification_confirm_action("(Link trips function not completed) Successfully linked trips")
  
}

# ---- Unlink trip ----
sproc_unlink_trip <- function(recid){
  # browser()
  # execute_query(glue("EXECUTE HHSurvey.unlink_via_id @ref_recid = '{recid}';"))
  
  notification_confirm_action("(Link trips function not completed) Successfully unlinked trip")
  
}

# ---- Update data to database ----

# Helper function to evaluate whether a string can be evaluated as a number 
is_numeric_string <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

# Helper function to format SQL values based on data type
format_sql_value <- function(value) {
  if (is.null(value) || is.na(value)) {
    return("NULL")
  } else if (is_numeric_string(value)) {
    # For numeric values (including integers and doubles), don't use quotes
    return(as.character(value))
  } else if (is.logical(value)) {
    # For logical values, convert to 1/0 without quotes
    return(ifelse(value, "1", "0"))
  } else if (inherits(value, "Date")) {
    # For dates, use quotes and proper format
    return(paste0("'", format(value, "%Y-%m-%d"), "'"))
  } else if (inherits(value, "POSIXt")) {
    # For datetime, use quotes and proper format
    return(paste0("'", format(value, "%Y-%m-%d %H:%M:%S"), "'"))
  } else {
    # For character/text values, use quotes and escape single quotes
    escaped_value <- gsub("'", "''", as.character(value))
    return(paste0("'", escaped_value, "'"))
  }
}

# Function to build SET clause for UPDATE statements with proper data type handling
build_set_clause <- function(column_names, values) {
  # Format each value according to its data type
  formatted_values <- sapply(values, format_sql_value, USE.NAMES = FALSE)
  set_pairs <- paste0(column_names, " = ", formatted_values)
  return(paste(set_pairs, collapse = ", "))
}

sproc_update_data <- function(recid, person_id, edit_list){
  
  # build update query using proper data type formatting
  all_variable_edits <- build_set_clause(names(edit_list), edit_list)
  sql_query <- glue("UPDATE HHSurvey.trip SET {all_variable_edits} WHERE recid = {recid};")
  execute_query(sql_query)
  
  execute_query(glue("EXECUTE HHSurvey.shifixy_after_edits @target_person_id = {person_id};"))
  
  notification_confirm_action("Successfully updated trip")
  
}
