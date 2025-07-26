
# get trip record ----
get_trip_record <- function(recid){
  
  trip_record <- get_data(view_name="Trip", recid=recid)
  
  return(trip_record)
  
}

# get trip summary table ----
get_trip_summary <- function(trip_record){
  
  trip_summary_table <- trip_record %>%
    select(hhid,pernum,person_id,tripnum,recid) %>%
    left_join(
      get_data(view_name = "trip_error_flags", recid = .[['recid']]) %>%
        select(recid, error_flag),
      by = "recid")
  
  return(trip_summary_table)
  
}


# data validation object for trip editor ----
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
  
  # data validation: Don't proceed if any input is invalid
  observe({
    # grey out Preview Edits button if validation rules are not violated
    toggleState(id = "clickupdate", condition = iv$is_valid())
  })
  
  return(iv)
}

# generate compare table: detect if values are modified ----
generate_compare_table <- function(input, trip_record){
  
  # get all editable variables
  input_tripeditor.cols <- paste0("data_edit-", tripeditor.cols)
  
  ## ---- create compare table ----
  compare_table <- NULL
  for(i in 1:length(tripeditor.cols)){
    # variable name
    var_name <- tripeditor.cols[i]
    var_input_name <- input_tripeditor.cols[i]
    
    # updated value
    if(input[[var_input_name]] == ""){
      # if text inputs like dest_purpose_other, psrc_comment are kept empty, the output would be ""
      # change to NA to match DB value
      updated_value <- NA
    }
    else{
      updated_value <- input[[var_input_name]]
    }
    
    compare_var <- as.data.frame(
      cbind(Variable = var_name,
            # original value
            `Original Value` = trip_record[[var_name]],
            # updated value
            `Updated Value` = updated_value
      )
    )
    
    # create df with original and updated values
    compare_table <- rbind(compare_table,
                           compare_var)
  }
  ## ---- process datetime ----
  
  # updated timestamps
  depart_datetime <- paste(input[["data_edit-depart_time_timestamp_date"]],
                           strftime(input[["data_edit-depart_time_timestamp_time"]], format="%H:%M:%S"))
  arrival_datetime <- paste(input[["data_edit-arrival_time_timestamp_date"]],
                            strftime(input[["data_edit-arrival_time_timestamp_time"]], format="%H:%M:%S"))
  
  compare_var <- as.data.frame(
    cbind(Variable = c("depart_time_timestamp", "arrival_time_timestamp"),
          # original value
          `Original Value` = c(as.character(format(trip_record[["depart_time_timestamp"]], "%Y-%m-%d %H:%M:%S")),
                               as.character(format(trip_record[["arrival_time_timestamp"]], "%Y-%m-%d %H:%M:%S"))),
          # updated value
          `Updated Value` = c(depart_datetime, arrival_datetime)
    )
  )
  
  compare_table <- rbind(compare_var, compare_table)
  
  # detect if values are modified
  compare_table <- compare_table %>%
    mutate(mod=case_when(`Original Value`==`Updated Value`~0,
                         is.na(`Original Value`) & is.na(`Updated Value`)~0, # for empty text inputs
                         TRUE~1))
  
  return(compare_table)
  
}

# generate updated trip record ----
generate_updated_trip <- function(input, trip_record){
  
  trip <- NULL
  for(var_name in names(trip_record)){
    if(var_name %in% tripeditor.cols){
      row <- as.data.frame(input[[paste0("data_edit-",var_name)]])
    } else{
      row <- as.data.frame(trip_record[[var_name]])
    }
    
    if(is.null(trip)){
      trip <- row
    }
    else{
      trip <- cbind(trip, row)
    }
    
  }
  names(trip) <- names(trip_record)
  
  return(trip)
}