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