
overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # uiOutput(ns("overview"))
    div("Welcome to Shiny Fixie!",
        style = 'margin-top:50px')
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    # output$overview_text <- renderText({"Welcome to Shiny Fixie!"})
    # 
    # # Overview UI
    # output$overview <- renderUI({
    #   tagList(
    #     br(),
    #     textOutput(ns("overview_text")),
    #     br()
    #   )
    # })
  })  # end moduleServer
}
