modal_trip_linking_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("linkbutton"))
  
}

modal_trip_linking_server <- function(id, thedata, selected_row) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$print_row <- renderPrint({
      cat('These rows were selected:\n\n')
      cat(selected_row())
    })
    
    # data cleaning tools ----
    observeEvent(input$clicklink, { showModal(
      modalDialog(title = "Trip Linking Editor",
                  
                  "Placeholder for trip linking!",
                  div(verbatimTextOutput(ns('print_row'))),
                  
                  footer = column(modalButton('Cancel'),
                                  width=12),
                  easyClose = TRUE
      )
    ) })

    output$linkbutton <- renderUI({ actionButton(ns("clicklink"), "Link selected trips") })
    
  })  # end moduleServer
}