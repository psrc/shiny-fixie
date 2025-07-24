# --- get data from database ----
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
  
  return(get_query(sql = query, db_name = cleaning_database))
}

# --- get point of interest coordinates from person and houeshold data tables----

get_poi_geog <- function(poi_geog, person_id=NULL, hhid=NULL){
  # get person-level data from database for edit platform
  
  if(!is.null(person_id)){
    # to get data for a person_id
    query <- glue("select {poi_geog}.Long as lng, {poi_geog}.Lat as lat
                   from HHSurvey.Person
                   where person_id='{person_id}'")
  }
  else if(!is.null(hhid)){
    # to get data for a trip
    query <- glue("select {poi_geog}.Long as lng, {poi_geog}.Lat as lat
                   from HHSurvey.Household
                   where hhid='{hhid}'")
  }
  
  data <- get_query(sql = query, db_name = cleaning_database)
  poi_coord <- paste(data[['lat']], data[['lng']], sep = ", ")
  
  return(poi_coord)
}


# ---- execute SQL query ----
execute_query <- function(query){
  sql_execute(query, db_name=cleaning_database)
}