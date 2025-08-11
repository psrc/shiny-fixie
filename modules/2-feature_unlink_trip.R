modal_unlink_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_unlink_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { 
      
      
      if(length(selected_recid())==1){
        
        # trip data, trip summary and datatable widget
        trip_record <- get_trip_record(selected_recid())
        
        # trip summary panel
        trip_summary_panel_server("trip_summary_panel", trip_record)
        
        showModal(
          modalDialog(
            title = "Trip Unlinking Editor (still in testing)",
            
            div("Are you sure you want to unlink this trip?"),
            
            # editor top panel: trip summary table and point of interest buttons ----
            trip_summary_panel_ui(ns("trip_summary_panel")),
            
            div("The trip will be split into:"),
            
            footer = div(
              style = "display: flex; justify-content: space-between;",
              # push changes to database
              actionButton(ns("pushunlink"), label = 'Yes'),
              modalButton('No'),
              width=12
            ),
            size = "l")
        ) 
        
      }
      else{
        notification_warning_select_row()
      }
      
      
    })
    
    observeEvent(input$pushunlink, { 
      
      sproc_unlink_trip(selected_recid())
      
    })
    
    output$editbutton <- renderUI({ actionButton(ns("clickedit"), "(Unlink trip)") }) 
    
  })  # end moduleServer
}