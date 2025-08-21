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
    
    # update error flag table
    observeEvent(input$update_table, {
      rval$df_error_flags <- get_error_flag_stat()
    })
    
    output$error_flags <- DT::renderDataTable(
      
      rval$df_error_flags,
      
      options =list(ordering = F, dom = 't'),
      selection = 'none',
      rownames = FALSE, 
      server=TRUE
      
    )
    
    output$overview <- renderUI({
      
      tagList(
        div("Welcome to Shiny Fixie!",
            style = 'margin-top:50px'),
        
        fluidRow(
          h2("Current Error Flag Count:"),
          div(class = "google-buttons",
              actionButton(ns("update_table"), 
                           label = icon("arrows-rotate"),
                           style = 'margin-top:20px; margin-left:20px;'
              )),
          style = "display: flex;"
        ),
        
        fluidRow(
          column(6,DT::DTOutput(ns("error_flags")))
        ))
      
    })
  })  # end moduleServer
}
