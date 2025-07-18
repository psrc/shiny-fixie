# NOTES: if no trip selected, insert new trip at very beginning of trip list

modal_new_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_new_trip_server <- function(id, thedata, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$print_row <- renderPrint({
      cat('These rows were selected:\n\n')
      cat(selected_recid())
    })
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { showModal(
      modalDialog(title = "New Trip Editor",
                  
                  "This is where the trip editing panel going to be!",
                  div(verbatimTextOutput(ns('print_row'))),
                  
                  footer = column(modalButton('Cancel'),
                                  modalButton('Add Trip'),
                                  width=12),
                  easyClose = TRUE
      )
    ) })

    output$editbutton <- renderUI({ actionButton(ns("clickedit"), "(Add new trip)") })
    
  })  # end moduleServer
}