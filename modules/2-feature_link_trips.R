modal_link_trips_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("linkbutton"))
  
}

modal_link_trips_server <- function(id, thedata, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
        
    # create objects ----
    trip_record <- reactive(get_trip_record(selected_recid(), order_by=c("tripnum")))
    
    # data cleaning tools ----
    observeEvent(input$clicklink, { 
      # browser()
      # test if selected recids are two or more consecutive records [all(diff(sort(selected_recid())) == 1)]
      if(length(selected_recid())>=2){
        
        # trip summary panel
        trip_summary_panel_server("trip_summary_panel", selected_recid())
        
        showModal(
          modalDialog(
            title = "Trip Linking Editor",
            
            "Are you sure you want to link these trips?",
            
            # editor top panel: trip summary table and point of interest buttons ----
            trip_summary_panel_ui(ns("trip_summary_panel")),
            
            
            footer = div(
              style = "display: flex; justify-content: space-between;",
              # push changes to database
              actionButton(ns("pushlink"), label = 'Yes'),
              modalButton('No'),
              width=12
            ),
            size = "l"
          )) 
      }
      else{
        notification_warning_select_linking()
      }
    })
    
    observeEvent(input$pushlink, { 
      sproc_link_trips(sort(selected_recid()))
    })

    output$linkbutton <- renderUI({ actionButton(ns("clicklink"), "Link Trips") })
    
  })  # end moduleServer
}