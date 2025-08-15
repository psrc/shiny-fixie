# combine date and time as timestamp ----
combine_datetime <- function(date, time){
  
  datetime <- paste(date, strftime(time, format="%H:%M:%S"))
  
  return(datetime)
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
  depart_datetime <- combine_datetime(input[["data_edit-depart_time_timestamp_date"]],input[["data_edit-depart_time_timestamp_time"]])
  arrival_datetime <- combine_datetime(input[["data_edit-arrival_time_timestamp_date"]],input[["data_edit-arrival_time_timestamp_time"]])
  
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

# generate inferred insert trip
generate_insert_trip <- function(.data){
  # TODO: replace this with smarter
  null_cols <- c("recid", "tripid", "tripnum",
                 "arrival_time_timestamp", "dest_lat", "dest_lng", "distance_miles",
                 "dest_purpose", "dest_purpose_other", "psrc_comment",
                 "travel_time",
                 "hhmember1","hhmember2","hhmember3","hhmember4","hhmember5",
                 "hhmember6","hhmember7","hhmember8","hhmember9","hhmember10",
                 "hhmember11","hhmember12","hhmember13",
                 "travelers_total",
                 "speed_mph",
                 "origin_geog","dest_geog",
                 "dest_county","dest_city","dest_zip","dest_is_home","dest_is_work","modes",
                 "psrc_inserted",
                 "revision_code",
                 "psrc_resolved")
  # browser()

  df <- .data %>%
    # columns that must copy directly from prev trip:
    #     hhid, person_id, pernum, traveldate, daynum, day_id   
    # inherent columns that could need editing by analysts: 
    #     travelers_hh,travelers_nonhh, mode_1, mode_2, mode_3, mode_4, driver, mode_acc, mode_egr, mode_other_specify
    mutate(depart_time_timestamp = arrival_time_timestamp,
           origin_lat = dest_lat,
           origin_lng = dest_lng,
           origin_purpose = dest_purpose) %>%
    # # make all columns that cannot be inferred NA
    mutate_at(vars(all_of(null_cols)), .funs = ~NA)
  
  return(df)
}

# generate updated trip record ----
# generate_updated_trip <- function(input, trip_record){
#   
#   trip <- NULL
#   for(var_name in names(trip_record)){
#     if(var_name %in% tripeditor.cols){
#       row <- as.data.frame(input[[paste0("data_edit-",var_name)]])
#     } else{
#       row <- as.data.frame(trip_record[[var_name]])
#     }
#     
#     if(is.null(trip)){
#       trip <- row
#     }
#     else{
#       trip <- cbind(trip, row)
#     }
#     
#   }
#   names(trip) <- names(trip_record)
#   
#   return(trip)
# }