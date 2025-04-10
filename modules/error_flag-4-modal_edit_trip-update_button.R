modal_update_trip_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("updatebutton"))
  )
}

modal_update_trip_server <- function(id, trip_record, label_name) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$trip_table <- DT::renderDT(
      trip_record, 
      rownames = FALSE,
      options =list(ordering = F, dom = 't',  selection = 'single'))
    
    observeEvent(input$clickupdate, { showModal(
      modalDialog(title = "Update Trip Record Preview",
                  
                  tagList(
                    # show trip table ----
                    DT::DTOutput(ns("trip_table"))
                    
                  ),
                  
                  footer = column(modalButton('Close'),
                                  width=12),
                  easyClose = TRUE,
                  size = "l"
      )
    ) })
    
    output$updatebutton <- renderUI({
      tagList(
        fluidRow(column(12, actionButton(ns("clickupdate"), label_name)))
      )
    }) 
    
  })  # end moduleServer
}