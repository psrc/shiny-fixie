# Procedures triggered or executed via pass-through queries in FixieUI

# Stored procedures maintained here:
# https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/fixie_sprocs.sql
# https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/rulesy_recalculate_after_edit.sql

# ---- Recalculation of derived fields ----
sproc_recalculate_after_edit <- function(person_id){
  
  sql_execute(paste0("EXECUTE HHSurvey.recalculate_after_edit @target_person_id = '", person_id, "';"), db_name=hhts_cleaning)
  
}

# ---- Trip deletion ----
sproc_remove_trip <- function(recid){
  
  sql_execute(paste0("EXECUTE HHSurvey.remove_trip @target_recid = '", recid, "';"), db_name=hhts_cleaning)
  
}

# ---- Dismiss error flag ----
sproc_dismiss_flag <- function(recid){
  
  # sql_execute(paste0("EXECUTE HHSurvey.remove_trip @target_recid = '", recid, "';"), db_name=hhts_cleaning)
  
  showModal(
    modalDialog(title = "Successfully Dismissed Error Flag",
                footer = div(
                  style = "display: flex; justify-content: space-between;",
                  modalButton('Ok')
                ),
                easyClose = TRUE,
                size = "l")
  )
  
}