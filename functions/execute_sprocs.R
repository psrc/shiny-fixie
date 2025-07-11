# Procedures triggered or executed via pass-through queries in FixieUI

# Stored procedures maintained here:
# https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/fixie_sprocs.sql
# https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/rulesy_recalculate_after_edit.sql

# ---- Recalculation of derived fields ----
sproc_recalculate_after_edit <- function(person_id){
  
  execute_query(paste0("EXECUTE HHSurvey.recalculate_after_edit @target_person_id = '", person_id, "';"))
  
}

# ---- Trip deletion ----
sproc_remove_trip <- function(recid){
  
  # execute_query(paste0("EXECUTE HHSurvey.remove_trip @target_recid = '", recid, "';"))
  
  showModal(
    modalDialog(title = "Successfully deleted trip",
                footer = div(
                  style = "display: flex; justify-content: space-between;",
                  modalButton('Ok')
                ),
                easyClose = TRUE,
                size = "l")
  )
  
}

# ---- Dismiss error flag ----
sproc_dismiss_flag <- function(recid){
  
  # execute_query(paste0("EXECUTE HHSurvey.remove_trip @target_recid = '", recid, "';"))
  
  showModal(
    modalDialog(title = "Successfully dismissed error flag",
                footer = div(
                  style = "display: flex; justify-content: space-between;",
                  modalButton('Ok')
                ),
                easyClose = TRUE,
                size = "l")
  )
  
}

# ---- Update data to database ----
sproc_update_data <- function(){
  
  # TODO: build update query
  
  # execute_query(paste0("EXECUTE HHSurvey.remove_trip @target_recid = '", recid, "';"))
  
  showModal(
    modalDialog(title = "Successfully updated trip",
                footer = div(
                  style = "display: flex; justify-content: space-between;",
                  modalButton('Ok')
                ),
                easyClose = TRUE,
                size = "l")
  )
  
}