
# get trip record and trip summary table from recid ----

get_trip_record <- function(recid){
  print("inside function:")
  print(recid)
  # get trip data from database
  trip_record <- get_data(view_name="Trip", recid=recid)
  
  return(trip_record)
  
}

get_trip_summary <- function(trip_record){
  
  # get trip data from database
  trip_summary_table <- trip_record %>%
    select(hhid,pernum,person_id,tripnum,recid) %>%
    left_join(
      get_data(view_name = "trip_error_flags", recid = .[['recid']]) %>%
        select(recid, error_flag),
      by = "recid")
  
  return(trip_summary_table)
  
}

render_trip_summary <- function(trip_summary_table){
  
  DT::renderDT(
    
    trip_summary_table,
    
    rownames = FALSE,
    options =list(ordering = F,
                  dom = 't',
                  selection = 'single',
                  pageLength =-1)
    
  )
  
}

add_datavalidation <- function(input){
  
  rval <- reactiveValues(depart_time_timestamp_date = NULL,
                         depart_time_timestamp_time = NULL,
                         arrival_time_timestamp_date = NULL,
                         arrival_time_timestamp_time = NULL)
  
  observe({
    rval$depart_time_timestamp_date <- input[["data_edit-depart_time_timestamp_date"]]
    rval$depart_time_timestamp_time <- input[["data_edit-depart_time_timestamp_time"]]
    rval$arrival_time_timestamp_date <- input[["data_edit-arrival_time_timestamp_date"]]
    rval$arrival_time_timestamp_time <- input[["data_edit-arrival_time_timestamp_time"]]
  })
  
  # data validation: Create an InputValidator object
  iv <- InputValidator$new()
  # data validation: Add validation rules
  # origin
  iv$add_rule("data_edit-origin_lat", sv_lte(90))
  iv$add_rule("data_edit-origin_lat", sv_gte(-90))
  iv$add_rule("data_edit-origin_lng", sv_lte(180))
  iv$add_rule("data_edit-origin_lng", sv_gte(-180))
  # destination
  iv$add_rule("data_edit-dest_lat", sv_lte(90))
  iv$add_rule("data_edit-dest_lat", sv_gte(-90))
  iv$add_rule("data_edit-dest_lng", sv_lte(180))
  iv$add_rule("data_edit-dest_lng", sv_gte(-180))
  # distance
  iv$add_rule("data_edit-distance_miles", sv_gt(0))
  # arrival time later than departure time
  iv$add_rule("data_edit-arrival_time_timestamp_time", function(value) {
    
    depart_datetime_r <- as.POSIXct(paste(input[["data_edit-depart_time_timestamp_date"]],
                                          strftime(input[["data_edit-depart_time_timestamp_time"]], format="%H:%M:%S"))
    )
    arrival_datetime_r <- as.POSIXct(paste(input[["data_edit-arrival_time_timestamp_date"]],
                                           strftime(input[["data_edit-arrival_time_timestamp_time"]], format="%H:%M:%S"))
    )
    
    # if there are values in depart_datetime_r and arrival_datetime_r
    if (!identical(depart_datetime_r, as.POSIXct(numeric(0))) & !identical(arrival_datetime_r, as.POSIXct(numeric(0)))) {
      if(depart_datetime_r >= arrival_datetime_r){
        "Arrival time must be later than departure time"
      }
    }
  })
  
  # data validation: Start displaying errors in the UI
  iv$enable()
  
  return(iv)
}
