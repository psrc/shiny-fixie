trip_summary_panel_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("tripsummarypanel"))
  )
}

# person control panel
trip_summary_panel_server <- function(id, trip_record, incl_poi = FALSE) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    trip_summary_table <- get_trip_summary(trip_record)
    output$trip_summary <-  DT::renderDT(
      
      trip_summary_table,
      
      rownames = FALSE,
      options =list(ordering = F,
                    dom = 't',
                    selection = 'single',
                    pageLength =-1)
      
    )
    
    # prep point of interest buttons ----
    poi_ids <- c("open_home_geog", "open_work_geog", "open_school_geog")
    
    # get poi locations
    poi_latlong <-c(get_poi_geog("home_geog", hhid = trip_record['hhid']), 
                    get_poi_geog("work_geog", person_id = trip_record['person_id']), 
                    get_poi_geog("school_geog", person_id = trip_record['person_id']))
    names(poi_latlong) <- poi_ids
    
    # poi button icons
    poi_icons <- c("house", "briefcase", "school-flag")
    names(poi_icons) <- poi_ids
    
    # generate poi button configuration
    poi_config <- tibble(
      inputId = character(),
      latlong = character(),
      icon = character()
    )
    
    for (x in names(poi_latlong)) {
      
      # add poi button if valid location exists
      if(poi_latlong[[x]] != "NA, NA"){
        
        poi_config <- poi_config %>% add_row(inputId = ns(x), 
                                             latlong = poi_latlong[[x]], 
                                             icon = poi_icons[[x]])
      }
      
    }
    
    # panel ----
    output$tripsummarypanel <- renderUI({
      
      # only show trip summary ----
      if(!incl_poi | nrow(poi_config)==0){
        
        div(
          class = "bottom-spacing",
          DT::DTOutput(ns("trip_summary"))
        )
        
      }
      # also show poi buttons ----
      else{
        
        fluidRow(
          column(10,
                 # show trip table
                 div(class = "bottom-spacing",
                     DT::DTOutput(ns("trip_summary")))
          ),
          column(2,
                 # show point of interests: home, work and school locations
                 pmap(poi_config, actionButton_google_poi)
          )
        ) # fluidRow
        }
      }) 
    
  })  # end moduleServer
}