tripeditor_top_panel <- function(trip_summary_id, poi_config = NULL){
  
  # only show trip summary
  if(is.null(poi_config)){
    
    div(
      class = "bottom-spacing",
      DT::DTOutput(trip_summary_id)
    )
    
  }
  # also show poi buttons
  else{
    
    fluidRow(
      column(10,
             # show trip table
             div(class = "bottom-spacing",
                 DT::DTOutput(trip_summary_id))
             ),
      column(2,
             # show point of interests: home, work and school locations
             pmap(poi_config, actionButton_google_poi)
             )
      ) # fluidRow
    
  }
  
}