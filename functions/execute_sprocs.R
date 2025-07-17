# Procedures triggered or executed via pass-through queries in FixieUI

# Stored procedures maintained here:
# https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/fixie_sprocs.sql
# https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/rulesy_recalculate_after_edit.sql

# ---- Recalculation of derived fields ----
sproc_recalculate_after_edit <- function(person_id){
  
  execute_query(glue("EXECUTE HHSurvey.recalculate_after_edit @target_person_id = {person_id};"))
  
}

# ---- Trip deletion ----
sproc_remove_trip <- function(recid){
  
  # execute_query(paste0("EXECUTE HHSurvey.remove_trip @target_recid = '", recid, "';"))
  
  showModal( modal_confirm_action("Successfully deleted trip") )
  
}

# ---- Dismiss error flag ----
sproc_dismiss_flag <- function(recid, person_id){
  
  execute_query(glue("EXECUTE HHSurvey.dismiss_flag @target_recid = {recid}, @target_person_id = {person_id};"))
  
  showModal( modal_confirm_action("Successfully dismissed error flag") )
  
}

# ---- Update data to database ----
sproc_update_data <- function(recid, edit_list){
  
  # build update query
  # TODO: specify data type for each variable
  all_variable_edits <- paste(names(edit_list), edit_list, sep = " = ", collapse = ", ")

  execute_query(glue("UPDATE HHSurvey.trip SET {all_variable_edits} WHERE recid = {recid};"))
  
  showModal( modal_confirm_action("Successfully updated trip") )
  
}