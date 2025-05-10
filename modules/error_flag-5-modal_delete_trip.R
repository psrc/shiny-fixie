modal_delete_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_delete_trip_server <- function(id, label_name, thedata, selected_row) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$print_row <- renderPrint({
      cat('These rows were selected:\n\n')
      cat(selected_row())
    })
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { showModal(
      modalDialog(title = "Delete Trip",
                  
                  "Are you sure you want to delete this trip?",
                  div(verbatimTextOutput(ns('print_row'))),
                  
                  footer = column(modalButton('Cancel'),
                                  modalButton('Delete Trip'), # TODO: replace delete button
                                  width=12),
                  easyClose = TRUE
      )
    ) })

    output$editbutton <- renderUI({ actionButton(ns("clickedit"), "(Delete trip)") }) 
                                             
  })  # end moduleServer
}