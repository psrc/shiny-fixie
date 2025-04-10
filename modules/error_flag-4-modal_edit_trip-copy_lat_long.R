modal_copy_latlong_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    rclipboard::rclipboardSetup(),
    uiOutput(ns("copybutton"))
  )
}

modal_copy_latlong_server <- function(id, df, lat_var_name, long_var_name) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    latlong <- reactive({ paste0(df[1,c(lat_var_name)], ", ", df[1,c(long_var_name)]) })
    
    output$copybutton <- renderUI({
      rclipboard::rclipButton(
        inputId = ns("clickcopy"),
        label = "Copy to clipboard",
        clipText = latlong()
      )
    })
    
  })  # end moduleServer
}