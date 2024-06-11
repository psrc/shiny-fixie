transit_region_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("transitregion"))
  )
}

transit_region_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$edit_table <- dtedit(input, output,
                  # name = id,
                  name = "edit_table",
                  thedata = data.frame(
                    Buy = c('Tea', 'Biscuits', 'Apples'),
                    Quantity = c(7, 2, 5),
                    stringsAsFactors = FALSE
                  ))
  
    output$transitregion <- renderUI({
      tagList(
        fluidRow(column(12,uiOutput(ns("edit_table")))),
        hr(style = "border-top: 1px solid #000000;")
      )
    })
      
  })  # end moduleServer
}