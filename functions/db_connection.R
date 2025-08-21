# ---- execute SQL query ----
execute_query <- function(query){
  
  sql_execute(query, db_name=cleaning_database)
}

# --- get data from database ----
get_data <- function(view_name="data2fixie", person_id=NULL, recid=NULL, order_by=NULL, custom_query = NULL){
  # browser()
  # person ID
  if(!is.null(person_id)){
    
    all_persons <- paste(person_id, collapse = ", ")
    
    if(is.null(order_by)){
      query <- glue("select * from HHSurvey.{view_name} where person_id in ({all_persons});")
    }
    else{
      # order table by columns
      order_by_list <- paste(order_by, collapse = ", ")
      query <- glue("select * from HHSurvey.{view_name} where person_id in ({all_persons}) order by {order_by_list};")
    }
    
  }
  # record ID
  else if(!is.null(recid)){
    
    all_recids <- paste(recid, collapse = ",")
    
    if(is.null(order_by)){
      query <- glue("select * from HHSurvey.{view_name} where recid in ({all_recids});")
    }
    else{
      # order table by columns
      order_by_list <- paste(order_by, collapse = ", ")
      query <- glue("select * from HHSurvey.{view_name} where recid in ({all_recids}) order by {order_by_list};")
    }
    
  }
  else if(!is.null(custom_query)){
    query <- glue(custom_query)
  }
  # return entire table
  else {
    query <- glue("select * from HHSurvey.{view_name};")
  }
  
  return(get_query(sql = query, db_name = cleaning_database))
}

# ---- get trip record ----
get_trip_record <- function(recid, order_by = NULL){
  # browser()
  trip_record <- get_data(view_name="Trip", recid=recid, order_by = order_by)
  
  return(trip_record)
  
}

# --- get list of all error flags for dropdown selection ----
get_all_error_flags <- function(){
  
  # get all error flags
  query <- glue("select error_flag from HHSurvey.trip_error_flags;")
  df <- get_query(sql = query, db_name = cleaning_database)
  error_names <- sort(unique(df[["error_flag"]]))
  error_list <- paste0("'",error_names,"'")
  
  # create named vector for dropdown selection that includes 
  # "all error flags" and "all persons"options
  all_errors <- paste(error_list, collapse = ", ")
  full_list <- append(c('all_person_placeholder', 'all_error_placeholder', 'all_elevated_placeholder'), error_list)
  names(full_list) <- append(c('all persons', 'all error flags', 'all elevated trips'), error_names)
  
  return(full_list)
}

# ---- get person list for person section panel ----
get_error_flag_person_list <- function(error_type){
  
  if(error_type == 'all_error_placeholder'){
    # show all persons with error flag
    query <- glue("select person_id from HHSurvey.trip_error_flags;")
    df <- get_query(sql = query, db_name = cleaning_database)
  }
  else if(error_type == 'all_person_placeholder'){
    # show all persons in trip table
    query <- glue("select person_id from HHSurvey.Trip;")
    df <- get_query(sql = query, db_name = cleaning_database)
    
  }
  else if(error_type == 'all_elevated_placeholder'){
    # show all persons with elevated comments
    query <- glue("SELECT DISTINCT person_id FROM HHSurvey.Trip WHERE psrc_comment IS NOT NULL AND psrc_comment<>'' ;")
    df <- get_query(sql = query, db_name = cleaning_database)
    
  }
  else{
    # show persons by individual error
    query <- glue("select person_id, error_flag from HHSurvey.trip_error_flags
                 where error_flag in ({error_type});")
    df <- get_query(sql = query, db_name = cleaning_database)
  }
  
  person_list <- unique(df[["person_id"]])
  
  return(person_list)
}

# --- get point of interest coordinates from person and household data tables----
get_poi_geog <- function(poi_geog, person_id=NULL, hhid=NULL){
  # get person-level data from database for edit platform
  
  if(!is.null(person_id)){
    # to get data for a person_id
    query <- glue("select {poi_geog}.Long as lng, {poi_geog}.Lat as lat
                   from HHSurvey.Person
                   where person_id={person_id};")
  }
  else if(!is.null(hhid)){
    # to get data for a trip
    query <- glue("select {poi_geog}.Long as lng, {poi_geog}.Lat as lat
                   from HHSurvey.Household
                   where hhid={hhid};")
  }
  
  data <- get_query(sql = query, db_name = cleaning_database)
  poi_coord <- paste(data[['lat']], data[['lng']], sep = ", ")
  
  return(poi_coord)
}

get_error_flag_stat <- function(){
  query <- glue("SELECT error_flag AS [Error Type], COUNT(*) AS [Flag Count]
                 FROM HHSurvey.trip_error_flags
                 GROUP BY error_flag
                 ORDER BY error_flag;")
  data <- get_query(sql = query, db_name = cleaning_database)
  
  return(data)
}
