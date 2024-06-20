transit_region_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    my_dtedit_ui(ns("inner_id"))
  )
}

transit_region_server <- function(id) {
  moduleServer(id, function(input, output, session){
    
    my_dtedit_server(
      id = "inner_id",
      thedata = data.frame(name = character(),
                           useR = factor(levels = c('Yes', 'No'))
                           )
      )
  
    
  })  # end moduleServer
}