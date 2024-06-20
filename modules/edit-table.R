# feed in the data of a person already filtered

edit_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("edittable"))
  )
}

edit_table_server <- function(id, thedata) {
  moduleServer(id, function(input, output, session){
    
    output$thetable <- DT::renderDT({thedata}, selection='single', rownames=FALSE)
    
    output$edittable <- renderUI({
      tagList(
        DT::DTOutput(outputId = "thetable")
      )
    }) 
  })  # end moduleServer
}