error_dropdown_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("errordropdown"))
  )
}

error_dropdown_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(all_error_types = NULL)
    observe({
      rval$all_error_types <- get_all_error_flags()
    })
    
    observeEvent(input$refresh_dropdown, {
      # clicking refresh button: update error type list
      
      rval$all_error_types <- get_all_error_flags()
      
      updateSelectInput(inputId = "dropdown_error_type",
                        label = "Select Error Type:", 
                        choices = rval$all_error_types)
    })
    
    output$errordropdown <- renderUI({
      # error type selection panel
      
      fluidRow(
        style = 'margin-top:50px; display: flex;',
        div(
          # dropdown: select error type
          selectInput("dropdown_error_type",
                      label = "Select Error Type:", 
                      choices = rval$all_error_types, 
                      selected = 'all_error_placeholder',
                      width = "480px")
          ),
        div(
          # refresh button: refresh error type dropdown
          style = 'margin-top: 30px;',
          actionButton_refresh(ns("refresh_dropdown"))
          )
        )
          
      
    }) 
    
  })  # end moduleServer
}