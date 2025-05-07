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
    table_colnames <- c("Variable","Original Value","Updated Value")
    
    test_all_inputs <- reactive({
      
      # get all editable variables
      all_vars_input_names <- names(all_input)[grepl("data_edit-", names(all_input))]
      all_vars <- str_remove(all_vars_input_names,"data_edit-")
      
      # create df with original and updated values
      myvalues <- NULL
      for(i in 1:length(all_vars_input_names)){
        # variable name
        var_name <- all_vars[i]
        var_input_name <- all_vars_input_names[i]
        myvalues <- as.data.frame(rbind(myvalues,
                                        cbind(var_name,
                                              # original value
                                              trip_record()[[var_name]],
                                              # updated value
                                              all_input[[var_input_name]]
                                              )
                                        )
                                  )
      }
      names(myvalues) <- table_colnames
      
      # detect if values are modified
      myvalues <- myvalues %>% 
        # rowwise() %>%
        mutate(mod=case_when(`Original Value`==`Updated Value`~0,
                             TRUE~1))
      
      myvalues
      })
    
 
    # print all input variables
    output$print_cols <- renderDataTable(
      datatable(test_all_inputs(),
                options =list(ordering = F, dom = 't', pageLength =-1,
                              # hide mod column
                              columnDefs = list(list(targets = 4,visible = FALSE)))
                ) %>% 
        formatStyle(
        'mod',
        target = 'row',
        backgroundColor = styleEqual(c(0, 1), c('white', '#00A7A0'))
      )
     )
    
    observeEvent(input$clickupdate, { showModal(
      modalDialog(title = "Update Trip Record Preview",
                  #TODO: add trip summary
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