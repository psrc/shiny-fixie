overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("overview"))
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(df_error_flags = NULL)
    observe({
      # get error counts
      rval$df_error_flags <- get_error_flag_stat()
    })
    
    
    observeEvent(input$refresh_table, {
      # clicking refresh button: update error counts
      
      rval$df_error_flags <- get_error_flag_stat()
    })
    
    output$error_flags <- DT::renderDataTable(
      
      rval$df_error_flags,
      
      class = list('hover row-border order-column'),
      selection = 'none',
      options =list(dom = 't', pageLength = -1, 
                    autoWidth = TRUE),
      rownames = FALSE,
      server=TRUE
      
    )
    
    output$overview <- renderUI({
      
      tagList(
        
        fluidRow(
          h2(style = 'margin-top:50px',
             "Current Error Flag Count"),
          div(style = 'margin-top: 55px; margin-left: 0px;',
              # refresh button: refresh summary table
              actionButton_refresh(ns("refresh_table"))
              ),
          style = "display: flex;"
        ),
        
        fluidRow(
          column(6,DT::DTOutput(ns("error_flags")), class = "left-aligned-content")
        ))
      
    })
  })  # end moduleServer
}
