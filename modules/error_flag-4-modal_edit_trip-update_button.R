modal_update_trip_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("updatebutton"))
  )
}

modal_update_trip_server <- function(id, all_input, recid, label_name) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    trip_record <- reactive({ get_data(view_name="Trip", recid=recid()) })
    all_cols <- reactive({ str_remove(names(all_input)[grepl("data_edit-", names(all_input))],"data_edit-")})
    
    # result <- reactiveValues()
    # result$df <- trip_record
    # for(cols in edit.cols){
    #   
    #   result[1,cols] <- reactive({ all_input[[paste0("data_edit-",cols)]] })
      
      # result$og_value <- reactive({ trip_record()[1,c(cols)] })
      # result$new_value <- reactive({ all_input[[paste0("data_edit-",cols)]] })
      # if(result$og_value==result$new_value){
      #   result$df[1,cols] <- reactive({ all_input[[paste0("data_edit-",cols)]] })
      # }
    # }
    
    # print all input variables
    output$print_cols <- renderPrint({
      cat('These cols were edited:\n\n')
      cat(paste0(all_cols(),",\n"))
    })
    
    observeEvent(input$clickupdate, { showModal(
      modalDialog(title = "Update Trip Record Preview",
                  
                  # tagList(
                  #   # show trip table ----
                  #   DT::DTOutput(ns("trip_table"))
                  #   
                  # ),
                  div(verbatimTextOutput(ns('print_cols'))),
                  
                  footer = column(modalButton('Close'),
                                  width=12),
                  easyClose = TRUE,
                  size = "l"
      )
    ) })
    
    output$updatebutton <- renderUI({
      tagList(
        fluidRow(column(12, actionButton(ns("clickupdate"), label_name)))
      )
    }) 
    
  })  # end moduleServer
}