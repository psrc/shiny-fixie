trip_summary_panel_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("tripsummarypanel"))
  )
}

# person control panel
trip_summary_panel_server <- function(id, selected_recid = NULL, incl_poi = FALSE) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # browser()
    output$trip_summary <- DT::renderDataTable(
      
      # get trip summary table from data2fixie
      get_data(recid = selected_recid) %>%
        select(recid,person_id,daynum,tripnum,OriginPurpose,DestPurpose,Error),
    
      rownames = FALSE,
      class = list('row-border order-column'),
      selection = 'none',
      options =list(ordering = F,
                    dom = 't',
                    pageLength =-1)
      
    )
    
    # prep point of interest buttons ----
    
    # generate poi button configuration
    poi_config <- tibble(
      inputId = character(),
      latlong = character(),
      icon = character()
    )
    
    if(incl_poi){
      
      poi_ids <- c("open_home_geog", "open_work_geog", "open_school_geog")
      # get poi locations: named list with matching poi_ids
      poi_latlong <- get_poi_geog(selected_recid)
      # poi button icons
      poi_icons <- c("house", "briefcase", "school-flag")
      names(poi_icons) <- poi_ids
      
      for (x in names(poi_latlong)) {
        
        # add poi button if valid location exists
        if(poi_latlong[[x]] != " ,"){
          
          poi_config <- poi_config %>% add_row(inputId = ns(x), 
                                               latlong = poi_latlong[[x]], 
                                               icon = poi_icons[[x]])
        }
      }
      
    }
    
    # panel ----
    output$tripsummarypanel <- renderUI({
      
      # only show trip summary ----
      if(nrow(poi_config)==0){
        
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
                 class = "google-buttons",
                 # show point of interests: home, work and school locations
                 pmap(poi_config, actionButton_google_poi)
          )
        ) # fluidRow
        }
      }) 
    
  })  # end moduleServer
}