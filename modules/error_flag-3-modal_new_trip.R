modal_new_trip_ui <- function(id) {
  ns <- NS(id)
  
  
}

modal_new_trip_server <- function(id, thedata, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$print_row <- renderPrint({
      cat('These rows were selected:\n\n')
      cat(selected_recid())
    })
    
    # data cleaning tools ----
    showModal(
      modalDialog(title = "New Trip Editor",
                  
                  "This is where the trip editing panel going to be!",
                  div(verbatimTextOutput(ns('print_row'))),
                  
                  footer = column(modalButton('Cancel'),
                                  modalButton('Add Trip'),
                                  width=12),
                  easyClose = TRUE
      )
    ) 

    
  })  # end moduleServer
}