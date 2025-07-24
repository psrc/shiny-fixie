
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

# datatable for trip summary ----
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
  
  return(iv)
}

prep_poi_buttons <- function(poi_ids, trip_record){
  
  # get point of interest locations
  # poi_ids <- c("open_home_geog", "open_work_geog", "open_school_geog")
  poi_latlong <-c(get_poi_geog("home_geog", hhid = trip_record['hhid']), 
                  get_poi_geog("work_geog", person_id = trip_record['person_id']), 
                  get_poi_geog("school_geog", person_id = trip_record['person_id']))
  
  observe({
    # grey out poi location buttons if this no valid location
    toggleState(id = "open_home_geog", condition = poi_latlong[1]!="NA, NA")
    toggleState(id = "open_work_geog", condition = poi_latlong[2]!="NA, NA")
    toggleState(id = "open_school_geog", condition = poi_latlong[3]!="NA, NA")
  })
  
  return(poi_latlong)
}
