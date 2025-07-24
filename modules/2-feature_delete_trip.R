modal_delete_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_delete_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(recid = NULL, 
                           trip_record = NULL, 
                           trip_summary_table = NULL)
    
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { 
      
      # assign rval ----
      rval$recid <- selected_recid()
      # trip data, trip summary and datatable widget
      rval$trip_record <- get_trip_record(rval$recid)
      rval$trip_summary_table <- get_trip_summary(rval$trip_record)
      output$trip_summary <- render_trip_summary(rval$trip_summary_table)

      
      if(!identical(rval$recid,integer(0))){
        
        showModal(
          modalDialog(title = "Delete Trip",
                      
                      "Are you sure you want to delete this trip?",
                      
                      # show trip table
                      tripeditor_top_panel(ns("trip_summary")),
                      
                      footer = column(actionButton(ns("clickdelete"), label = 'Yes'), 
                                      modalButton('No'),
                                      width=12)
                      )) 
        
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