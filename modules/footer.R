# Display footer

footer_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('afooter'))
  )
  
}

footer_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$afooter <- renderUI({
      bs4Jumbotron(
        status = "info",
        btnName = NULL
      )
      
    })
    
  }) # end moduleServer
  
}