
# data validation object for trip editor ----
editdata_datavalidation <- function(input){
  
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
  
  # data validation: Don't proceed if any input is invalid
  observe({
    # grey out Preview Edits button if validation rules are not violated
    toggleState(id = "clickupdate", condition = iv$is_valid())
  })
  
  return(iv)
}

addtrip_datavalidation <- function(input, trip_record, depart_timestamp_id, pushbutton_id){
  
  # data validation: Create an InputValidator object
  iv <- InputValidator$new()

  # data validation: Add validation rules
  # iv$add_rule(depart_timestamp_id, function(value) {
  #   prev_arrival_datetime <- as.POSIXct(trip_record[["arrival_time_timestamp"]])
  #   browser()
  #   
  #   # if there are values in depart_datetime_r and arrival_datetime_r
  #   if (!is.null(input[[depart_timestamp_id]]) & length(input[[depart_timestamp_id]])>0){
  #     if(prev_arrival_datetime >= input[[depart_timestamp_id]]){
  #       "Departure time must be later than previous trip arrival time"
  #     }
  #   }
  #   
  # })
  
  # data validation: Start displaying errors in the UI
  iv$enable()
  
  # data validation: Don't proceed if any input is invalid
  observe({
    # grey out Preview Edits button if validation rules are not violated
    toggleState(id = pushbutton_id, condition = iv$is_valid())
  })
  
  return(iv)
}