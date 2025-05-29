modal_dismiss_flag_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("dismissbutton"))
  
}

modal_dismiss_flag_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # output$print_row <- renderPrint({
    #   cat('These rows were selected:\n\n')
    #   cat(reactive(selected_recid))
    # })
    
    # data cleaning tools ----
    observeEvent(input$clickdismiss, { 
      showModal(
        modalDialog(title = "Dismiss flag",
                    
                    "Are you sure you dismiss this error flag?",
                    # div(verbatimTextOutput(ns('print_row'))),
                    
                    footer = column(modalButton('Yes'),
                                    modalButton('No'),
                                    width=12),
                    easyClose = TRUE
        )
    ) })
    
    output$dismissbutton <- renderUI({ actionButton(ns("clickdismiss"), "(Dismiss flag)") })
    
  })  # end moduleServer
}