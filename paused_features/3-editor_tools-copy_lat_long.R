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
    
    rval <- reactiveValues(latlng = NULL,
                           lat = NULL,
                           lng = NULL)
    
    observeEvent(input$clickcopy,{
      rval$lat <- lat_input
      rval$lng <- long_input
      browser()
      rval$latlng <- paste0(rval$lat, ", ", rval$lng)
      print(rval$latlng)
    })
    
    output$copybutton <- renderUI({
      rclipboard::rclipButton(
        inputId = ns("clickcopy"),
        label = icon("clipboard"),
        clipText = rval$latlng
      )
    })
    
  })  # end moduleServer
}