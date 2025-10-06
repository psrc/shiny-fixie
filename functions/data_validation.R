
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
  # driver value must be entered for drive modes
  iv$add_rule("data_edit-driver", function(value) {
    
    
    # if any of the modes are drive
    if (any(c(input[["data_edit-mode_1"]], input[["data_edit-mode_2"]], 
              input[["data_edit-mode_3"]], input[["data_edit-mode_4"]])) %in% drive_modes) {
      if(input[["data_edit-driver"]]==995){
        "The \"driver\" field must be specified for drive trips"
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
