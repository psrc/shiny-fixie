modal_update_trip_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("updatebutton"))
  )
}

modal_update_trip_server <- function(id, all_input, recid, label_name) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    trip_record <- reactive({ get_data(view_name="Trip", recid=recid) })
    
    test_all_inputs <- reactive({
      
      # get all editable variables
      all_vars_input_names <- names(all_input)[grepl("data_edit-", names(all_input))]
      all_vars <- str_remove(all_vars_input_names,"data_edit-")

      # create df with original and updated values
      ## original values
      compare_values <- trip_record() %>%
        select(all_of(all_vars)) %>%
        mutate(type="original")
      ## updated values
      new_values <- list()
      for(var_input_name in all_vars_input_names){
        new_values <- append(new_values, all_input[[var_input_name]])
      }
      ## concat
      compare_values[2,] <- append(new_values,"updated")
      compare_values
      
    })
    
    # print all input variables
    output$print_cols <- renderDT({
      datatable(test_all_inputs())
    })
    
    observeEvent(input$clickupdate, { showModal(
      modalDialog(title = "Update Trip Record Preview",
                  
                  div(
                    DTOutput(ns('print_cols'))
                  ),
                  
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