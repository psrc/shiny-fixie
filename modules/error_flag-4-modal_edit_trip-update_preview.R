modal_update_trip_ui <- function(id) {
  ns <- NS(id)

    uiOutput(ns("updatebutton"))

}

modal_update_trip_server <- function(id, all_input, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(recid = NULL, 
                           orig_trip_record = NULL, 
                           compare_table = NULL, 
                           update_trip = NULL
                           )
    observeEvent(input$clickupdate, {
      rval$recid <- selected_recid()
      rval$orig_trip_record <- get_data(view_name = "Trip", recid = rval$recid)
    })
    
    observeEvent(input$clickupdate, {
      # get all editable variables
      all_vars_input_names <- names(all_input)[grepl("data_edit-", names(all_input))]
      all_vars <- str_remove(all_vars_input_names,"data_edit-")
      
      # print("start making compare table")
      compare_table <- NULL
      for(i in 1:length(all_vars_input_names)){
        # variable name
        var_name <- all_vars[i]
        var_input_name <- all_vars_input_names[i]
        # print(var_input_name)
        compare_var <- as.data.frame(
          cbind(var_name,
                # original value
                rval$orig_trip_record[[var_name]],
                # updated value
                all_input[[var_input_name]]
                )
        )
        
        # create df with original and updated values
        compare_table <- rbind(compare_table,
                               compare_var)
      }
      
      names(compare_table) <- c("Variable","Original Value","Updated Value")
      
      # detect if values are modified
      rval$compare_table <- compare_table %>%
        mutate(mod=case_when(`Original Value`==`Updated Value`~0,
                             TRUE~1))
      # print("complete compare table")
      
      trip <- NULL
      for(var_name in names(rval$orig_trip_record)){
        if(var_name %in% all_vars){
          row <- as.data.frame(all_input[[paste0("data_edit-",var_name)]])
        } else{
          row <- as.data.frame(rval$orig_trip_record[[var_name]])
        }
        
        if(is.null(trip)){
          trip <- row
        }
        else{
          trip <- cbind(trip, row)
        }
        
      }
      names(trip) <- names(rval$orig_trip_record)
      rval$update_trip <- trip
      # print("complete update trip")
    })
    
    

    observeEvent(input$clickupdate, {
      removeModal()

      # print all comparison table
      output$print_cols <- renderDT({

          datatable(rval$compare_table,
                    options =list(ordering = F,
                                  dom = 't',
                                  pageLength = -1,
                                  # hide mod column
                                  columnDefs = list(list(targets = 4,visible = FALSE)))
          ) %>%
            formatStyle(
              'mod',
              target = 'row',
              backgroundColor = styleEqual(c(0, 1), c('white', '#00A7A0'))
            )

      })

      showModal(
        modalDialog(title = "Update Trip Record Preview",
                    #TODO: add trip summary
                    div(
                      DTOutput(ns('print_cols'))
                    ),

                    footer = div(
                      style = "display: flex; justify-content: space-between;",

                      actionButton(ns('button_edit_again'),
                                   label = "Back to Editor"),

                      modalButton('Close')
                    ),
                    easyClose = TRUE,
                    size = "l"
        )
      )
    })

    observeEvent(input$button_edit_again, {
      removeModal()
      modal_revise_trip_server("revise-trip",
                               selected_recid_revise = reactive(rval$recid),
                               updated_trip = reactive(rval$update_trip))
    })

    output$updatebutton <- renderUI({
      actionButton(ns("clickupdate"),
                   "Apply Changes")
      })

  })  # end moduleServer
}