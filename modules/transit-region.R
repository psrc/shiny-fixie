transit_region_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns(id))
  
}

transit_region_server <- function(id) {
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    return(
      dtedit(input = input,
             output = output,
             name = id,
             id = id,
             thedata = df)
    )
    
      
  })  # end moduleServer
}