
overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("overview"))
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$overview_text <- renderText({"Welcome to Shiny Fixie!"})
    
    # Overview UI
    output$overview <- renderUI({
      tagList(
        br(),
        textOutput(ns("overview_text")),
        br()
      )
    })
  })  # end moduleServer
}
