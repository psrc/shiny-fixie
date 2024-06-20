library(psrcelmer)


get_person_list <- function(){
  # get list of person_ids from database for edit platform dropdown
  
  query <- "select distinct personid from HHSurvey.data2fixie"
  return(get_query(sql = query, db_name = "hhts_cleaning"))
}

get_data2fixie <- function(person_id=NULL){
  # get person-level data from database for edit platform
  
  if(is.null(person_id)){
    # to get data for all persons
    query <- "select * from HHSurvey.data2fixie"
  }
  else {
    # to get data for a person_id
    query <- paste0("select * from HHSurvey.data2fixie where personid='", person_id,"'")
  }
  
  return(get_query(sql = query, db_name = "hhts_cleaning"))
}
