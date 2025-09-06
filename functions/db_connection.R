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
  query <- glue("select assignment from HHSurvey.person_error_assignment;")
  df <- get_query(sql = query, db_name = cleaning_database)
  error_names <- sort(unique(df[["assignment"]]))
  error_list <- paste0("'",error_names,"'")
  
  # create named vector for dropdown selection that includes 
  # "all error flags" and "all persons"options
  all_errors <- paste(error_list, collapse = ", ")
  full_list <- append(c('all_person_placeholder', 'all_error_placeholder', 'all_elevated_placeholder'), error_list)
  
  # assign labels with count for dropdown
  append_names <- append(c('all persons', 'all error flags', 'all elevated trips'), error_names)
  count <- sapply(full_list, function(x) return (length(get_error_flag_person_list(x))))
  
  names(full_list) <- paste0(append_names, " (",count, ")")
  
  return(full_list)
}

# ---- get person list for person section panel ----
get_error_flag_person_list <- function(error_type){
  
  if(error_type == 'all_error_placeholder'){
    # show all persons with error flag
    query <- glue("select person_id from HHSurvey.person_error_assignment;")
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
    query <- glue("select person_id, assignment from HHSurvey.person_error_assignment
                 where assignment in ({error_type});")
    df <- get_query(sql = query, db_name = cleaning_database)
  }
  
  person_list <- unique(df[["person_id"]])
  
  return(person_list)
}

# --- get point of interest coordinates from person and household data tables----



get_poi_geog <- function(recid){
  
  # get all geogs: work_geog, school_geog, home_geog
  # must match column names with poi_ids!!!
  query <- glue("SELECT CONCAT(hh.home_lat, ' ,', hh.home_lng) as open_home_geog,
                        CONCAT(p.work_lat, ' ,', p.work_lng) as open_work_geog, 
                        CONCAT(p.school_loc_lat, ' ,', p.school_loc_lng) as open_school_geog
                 FROM HHSurvey.Trip AS t
                 LEFT JOIN HHSurvey.Person AS p ON t.person_id = p.person_id
                 LEFT JOIN HHSurvey.Household AS hh ON p.hhid = hh.hhid
                 WHERE t.recid={recid}")
  data <- get_query(sql = query, db_name = cleaning_database)
  
  named_list <- as.list(data)
  
  return(named_list)
  
}

get_error_flag_stat <- function(){
  query <- glue("SELECT error_flag AS [Error Type], COUNT(*) AS [Flag Count]
                 FROM HHSurvey.trip_error_flags
                 GROUP BY error_flag
                 ORDER BY error_flag;")
  data <- get_query(sql = query, db_name = cleaning_database) %>%
    bind_rows(summarise(., 
                        `Error Type` = "Total",
                        `Flag Count` = sum(`Flag Count`)))
  
  return(data)
}
