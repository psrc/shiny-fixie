modal_update_trip_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("updatebutton"))
  )
}

modal_update_trip_server <- function(id, all_input, recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    trip_record <- reactive({ get_data(view_name="Trip", recid=recid) })
    
    
    # featured buttons ----
    # modal_edit_trip_server("button_edit_again", selected_row = recid(), updated_trip = reactive(update_trip()))
    
    # create comparison table showing edited data
    compare_table <- reactive({
      
      # get all editable variables
      all_vars_input_names <- names(all_input)[grepl("data_edit-", names(all_input))]
      all_vars <- str_remove(all_vars_input_names,"data_edit-")
      
      compare_table <- NULL
      for(i in 1:length(all_vars_input_names)){
        # variable name
        var_name <- all_vars[i]
        var_input_name <- all_vars_input_names[i]
        # create df with original and updated values
        compare_table <- as.data.frame(rbind(compare_table,
                                             cbind(var_name,
                                                   # original value
                                                   trip_record()[[var_name]],
                                                   # updated value
                                                   all_input[[var_input_name]]
                                                   ))
                                       )
      }
      names(compare_table) <- c("Variable","Original Value","Updated Value")
      
      # detect if values are modified
      compare_table <- compare_table %>% 
        mutate(mod=case_when(`Original Value`==`Updated Value`~0,
                             TRUE~1))
      
      compare_table
      })
    
    # create updated trip record
    update_trip <- reactive({
      
      # get all editable variables
      all_vars_input_names <- names(all_input)[grepl("data_edit-", names(all_input))]
      all_vars <- str_remove(all_vars_input_names,"data_edit-")
      
      trip <- NULL
      for(var_name in names(trip_record())){
        if(var_name %in% all_vars){
          trip <- as.data.frame(cbind(trip,
                                      all_input[[paste0("data_edit-",var_name)]]
                                      ))
        } else{
          trip <- as.data.frame(cbind(trip,
                                      trip_record()[[var_name]]
                                      ))
          
        }
        
      }
      names(trip) <- names(trip_record())
      
      trip
    })
    
 
    # print all comparison table
    output$print_cols <- renderDataTable(
      datatable(compare_table(),
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
                                  # go back to edit trip modal
                                  # modal_edit_trip_ui(ns('button_edit_again')),
                                  width=12),
                  easyClose = TRUE,
                  size = "l"
      )
    ) })
    
    output$updatebutton <- renderUI({
      tagList(
        fluidRow(column(12, actionButton(ns("clickupdate"), "Apply")))
      )
    }) 
    
  })  # end moduleServer
}