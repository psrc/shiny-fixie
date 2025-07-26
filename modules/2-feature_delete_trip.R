modal_delete_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_delete_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(recid = NULL)
    
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { 
      
      # assign rval ----
      rval$recid <- selected_recid()

      
      if(!identical(rval$recid,integer(0))){
        
        # trip data, trip summary and datatable widget
        trip_record <- get_trip_record(rval$recid)
        
        # create trip summary panel ----
        trip_summary_panel_server("trip_summary_panel", trip_record)
        
        showModal(
          modalDialog(
            title = "Delete Trip",
            
            "Are you sure you want to delete this trip?",
                      
            # editor top panel: trip summary table and point of interest buttons ----
            trip_summary_panel_ui(ns("trip_summary_panel")),
            
            footer = div(
              style = "display: flex; justify-content: space-between;",
              # push changes to database
              actionButton(ns("clickdelete"), label = 'Yes'),
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
    
    observeEvent(input$clickdelete, { 
      sproc_remove_trip(rval$recid)
      })

    output$editbutton <- renderUI({ actionButton(ns("clickedit"), "Delete trip") }) 
                                             
  })  # end moduleServer
}