modal_dismiss_flag_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("dismissbutton"))
  
}

modal_dismiss_flag_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(recid = NULL)
    
    output$print_row <- renderPrint({
      cat('These rows were selected:\n\n')
      cat(rval$recid)
    })
    
    # data cleaning tools ----
    observeEvent(input$clickdismiss, { 
      
      rval$recid <- selected_recid()
      
      showModal(
        modalDialog(title = "Dismiss flag",
                    
                    "Are you sure you dismiss this error flag?",
                    div(verbatimTextOutput(ns('print_row'))),
                    
                    footer = column(actionButton(ns("clickconfirm"), label = 'Yes'),
                                    modalButton('No'),
                                    width=12),
                    easyClose = TRUE
        )
    ) })
    
    # observeEvent(input$clickconfirm, {
    #   
    # })
    
    output$dismissbutton <- renderUI({ actionButton(ns("clickdismiss"), "(Dismiss flag)") })
    
  })  # end moduleServer
}