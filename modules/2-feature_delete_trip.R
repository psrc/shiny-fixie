modal_delete_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_delete_trip_server <- function(id, label_name, thedata, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(recid = NULL, 
                           trip_record = NULL, 
                           trip_summary_table = NULL)
    
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { 
      
      rval$recid <- selected_recid()
      rval$trip_record <- get_data(view_name="Trip", recid=rval$recid)

      
      if(!identical(rval$recid,integer(0))){
        
        
        rval$trip_summary_table <- rval$trip_record %>%
          select(hhid,pernum,person_id,tripnum,recid) %>%
          left_join(
            get_data(view_name = "trip_error_flags", recid = rval$recid) %>%
              select(recid, error_flag),
            by = "recid")
        
        output$trip_summary <- DT::renderDT(
          
          rval$trip_summary_table,
          
          rownames = FALSE,
          options =list(ordering = F,
                        dom = 't',
                        selection = 'single',
                        pageLength =-1)
          
        )
        
        showModal(
          modalDialog(title = "Delete Trip",
                      
                      "Are you sure you want to delete this trip?",
                      # show trip table
                      div(
                        class = "bottom-spacing",
                        DT::DTOutput(ns("trip_summary"))
                      ),
                      
                      footer = column(actionButton(ns("clickdelete"), label = 'Yes'), 
                                      modalButton('No'),
                                      width=12),
                      easyClose = TRUE)) 
        
      }
      else{
        notification_warning_select_row()
      }
      
      
      })
    
    observeEvent(input$clickdelete, { 
      sproc_remove_trip(selected_recid())
      })

    output$editbutton <- renderUI({ actionButton(ns("clickedit"), "Delete trip") }) 
                                             
  })  # end moduleServer
}