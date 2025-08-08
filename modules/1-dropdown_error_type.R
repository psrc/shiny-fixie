error_dropdown_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("errordropdown"))
  )
}

# person control panel
error_dropdown_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(all_error_types = NULL)
    observe({
      rval$all_error_types <- get_all_error_flags()
    })
    
    observeEvent(input$update_dropdown, {
      # browser()
      rval$all_error_types <- get_all_error_flags()
      
      updateSelectInput(inputId = "dropdown_error_type",
                        label = "Select Error Type:", 
                        choices = rval$all_error_types)
    })
    
    output$errordropdown <- renderUI({
      
      tagList(
        wellPanel(
          # style ='padding-left:25px; padding-right:25px; display: flex;',
          
          fluidRow(
            column(9,
              selectInput("dropdown_error_type",
                          label = "Select Error Type:", 
                          choices = rval$all_error_types, 
                          width = "480px"), 
              style = "padding-right: 0rem;"
              ),
            column(3,
              class = "google-buttons",
              actionButton(ns("update_dropdown"), 
                           label = icon("arrows-rotate"))
              )
            )
          
        ) # end wellPanel
      ) # end tagList
      
    }) 
    
  })  # end moduleServer
}