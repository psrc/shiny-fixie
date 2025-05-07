modal_copy_latlong_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    rclipboard::rclipboardSetup(),
    uiOutput(ns("copybutton"))
  )
}

modal_copy_latlong_server <- function(id, lat_input, long_input) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    latlong <- reactive({ paste0(lat_input, ", ", long_input) })
    
    output$copybutton <- renderUI({
      rclipboard::rclipButton(
        inputId = ns("clickcopy"),
        label = "Copy to clipboard",
        clipText = latlong()
      )
    })
    
  })  # end moduleServer
}