modal_update_trip_ui <- function(id) {
  ns <- NS(id)

    uiOutput(ns("updatebutton"))

}

modal_update_trip_server <- function(id, trip_editor_input, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    rval <- reactiveValues(recid = NULL,
                           orig_trip_record = NULL,
                           compare_table = NULL,
                           updated_trip = NULL
    )
    observeEvent(input$clickupdate, {
      
      # browser()
      rval$recid <- selected_recid()
      rval$orig_trip_record <- get_data(view_name = "Trip", recid = rval$recid)
      
      # get all editable variables
      input_tripeditor.cols <- paste0("data_edit-", tripeditor.cols)

      modal_revise_trip_server("button_revise",
                               selected_recid_revise = reactive(rval$recid),
                               updated_trip = reactive(rval$updated_trip))

      # ---- create compare table ----
      compare_table <- NULL
      for(i in 1:length(tripeditor.cols)){
        # variable name
        var_name <- tripeditor.cols[i]
        var_input_name <- input_tripeditor.cols[i]

        compare_var <- as.data.frame(
          cbind(var_name,
                # original value
                rval$orig_trip_record[[var_name]],
                # updated value
                trip_editor_input[[var_input_name]]
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

      # ---- generate updated trip record ----
      trip <- NULL
      for(var_name in names(rval$orig_trip_record)){
        if(var_name %in% tripeditor.cols){
          row <- as.data.frame(trip_editor_input[[paste0("data_edit-",var_name)]])
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
      rval$updated_trip <- trip


      # ---- print all comparison table ----
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
      
      # ---- show update preview pane ----
      showModal(
        modalDialog(title = "Update Trip Record Preview",
                    #TODO: add trip summary
                    div(
                      DTOutput(ns('print_cols'))
                    ),

                    footer = div(
                      style = "display: flex; justify-content: space-between;",

                      modal_revise_trip_ui(ns('button_revise')),

                      modalButton('Close')
                    ),
                    easyClose = TRUE,
                    size = "l"
        )
      )
    })

    # observeEvent(input$button_edit_again, {
    #   removeModal()
    #   modal_revise_trip_server("revise-trip",
    #                            selected_recid_revise = reactive(rval$recid),
    #                            updated_trip = reactive(rval$updated_trip))
    # })

    output$updatebutton <- renderUI({
      actionButton(ns("clickupdate"),
                   "Apply Changes")
      })

  })  # end moduleServer
}