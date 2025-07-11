get_data <- function(view_name="data2fixie_test", person_id=NULL, recid=NULL){
  # get person-level data from database for edit platform
  
  if(!is.null(person_id)){
    # to get data for a person_id
    query <- paste0("select * from HHSurvey.",view_name, " where personid='", person_id,"'")
  }
  else if(!is.null(recid)){
    # to get data for a trip
    query <- paste0("select * from HHSurvey.",view_name, " where recid='", recid,"'")
  }
  else {
    # to get data for all persons
    query <- paste0("select * from HHSurvey.",view_name)
  }
  
  return(get_query(sql = query, db_name = "hhts_cleaning_temporal"))
}


# get_data_reactive <- function(view_name){
#   # get person-level data from database for edit platform
#   
#   rval <- reactiveValues(view_name = NULL)
#   observe({
#     rval$view_name <- view_name()
#   })
#   
#   data <- reactive({ 
#     get_query(
#       sql = paste0("select * from HHSurvey.",rval$view_name), 
#       db_name = "hhts_cleaning_temporal") })
# 
#   
#   return(data)
# }