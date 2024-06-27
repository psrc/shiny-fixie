get_data <- function(view_name="data2fixie", person_id=NULL){
  # get person-level data from database for edit platform
  
  if(is.null(person_id)){
    # to get data for all persons
    query <- paste0("select * from HHSurvey.",view_name)
  }
  else {
    # to get data for a person_id
    query <- paste0("select * from HHSurvey.",view_name, " where personid='", person_id,"'")
  }
  
  return(get_query(sql = query, db_name = "hhts_cleaning"))
}
