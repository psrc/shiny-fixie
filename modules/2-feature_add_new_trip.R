# NOTES: if no trip selected, insert new trip at very beginning of trip list

modal_new_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_new_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(recid = NULL, 
                           trip_record = NULL, 
                           trip_summary_table = NULL)
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { 
      
      rval$recid <- selected_recid()
      rval$trip_record <- get_data(view_name="Trip", recid=rval$recid) 
      
      
      # if a row is selected in table: show Trip Record Editor
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
        
        output$print_row <- renderPrint({
          cat('These rows were selected:\n\n')
          cat(rval$recid)
        })
        
        showModal(
          modalDialog(title = "Trip Record Generator",
                      
                      # show trip table
                      div(
                        class = "bottom-spacing",
                        DT::DTOutput(ns("trip_summary"))
                      ),
                      footer = column(12,
                                      class = "trip-buttons-panel",
                                      modalButton('Add blank trip'),
                                      modalButton('Add reverse trip'),
                                      modalButton('Add return home trip'),
                                      modalButton('Cancel')
                      ),
                      size = "l"
          )
        )
      }
      # if no row is selected
      else{
        notification_warning_select_row()
      }
      
      }) # end observeEvent

    output$editbutton <- renderUI({ actionButton(ns("clickedit"), "Add new trip") })
    
  })  # end moduleServer
}