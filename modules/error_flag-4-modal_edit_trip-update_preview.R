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

      rval$recid <- selected_recid()
      rval$orig_trip_record <- get_data(view_name = "Trip", recid = rval$recid)

    })

    observeEvent(input$clickupdate, {

      modal_revise_trip_server("button_revise",
                               selected_recid_revise = reactive(rval$recid),
                               updated_trip = reactive(rval$updated_trip))
      
      # get all editable variables
      input_tripeditor.cols <- paste0("data_edit-", tripeditor.cols)

      # compare table
      compare_table <- NULL
      for(i in 1:length(tripeditor.cols)){
        # variable name
        var_name <- tripeditor.cols[i]
        var_input_name <- input_tripeditor.cols[i]

        # Get original and updated values
        orig_val <- rval$orig_trip_record[[var_name]]
        updated_val <- trip_editor_input[[var_input_name]]
        
        # Handle datetime columns specially
        if(var_name %in% c("depart_time_timestamp", "arrival_time_timestamp")) {
          # Convert original datetime to character for display
          orig_val_display <- if(is.null(orig_val) || is.na(orig_val)) {
            ""
          } else {
            format(as.POSIXct(orig_val), "%Y-%m-%d %H:%M:%S")
          }
          
          # For the new split date/time inputs, combine date and time parts
          date_input <- trip_editor_input[[paste0(var_input_name, "_date")]]
          time_input <- trip_editor_input[[paste0(var_input_name, "_time")]]
          
          # Combine date and time inputs
          if(is.null(date_input) || is.null(time_input) || time_input == "") {
            updated_val_display <- ""
            combined_datetime <- NA
          } else {
            # Combine date and time
            datetime_string <- paste(date_input, time_input)
            updated_val_display <- datetime_string
            combined_datetime <- tryCatch({
              as.POSIXct(datetime_string)
            }, error = function(e) {
              NA
            })
          }
          
          # Compare original datetime with combined updated value
          is_modified <- if(is.null(orig_val) || is.na(orig_val)) {
            !is.na(combined_datetime)
          } else if(is.na(combined_datetime)) {
            TRUE
          } else {
            tryCatch({
              orig_datetime <- as.POSIXct(orig_val)
              !identical(orig_datetime, combined_datetime)
            }, error = function(e) {
              TRUE  # If parsing fails, assume modified
            })
          }
          
        } else {
          # For non-datetime columns, use original logic
          orig_val_display <- as.character(orig_val)
          updated_val_display <- as.character(updated_val)
          is_modified <- !identical(as.character(orig_val), as.character(updated_val))
        }

        compare_var <- data.frame(
          Variable = var_name,
          `Original Value` = orig_val_display,
          `Updated Value` = updated_val_display,
          mod = ifelse(is_modified, 1, 0),
          stringsAsFactors = FALSE
        )

        # create df with original and updated values
        compare_table <- rbind(compare_table, compare_var)
      }

      names(compare_table) <- c("Variable","Original Value","Updated Value", "mod")
      rval$compare_table <- compare_table

      # update trip
      trip <- NULL
      for(var_name in names(rval$orig_trip_record)){
        if(var_name %in% tripeditor.cols){
          # Get the input value
          input_val <- trip_editor_input[[paste0("data_edit-",var_name)]]
          
          # Handle datetime columns specially
          if(var_name %in% c("depart_time_timestamp", "arrival_time_timestamp")) {
            # For split date/time inputs, combine them
            date_input <- trip_editor_input[[paste0("data_edit-", var_name, "_date")]]
            time_input <- trip_editor_input[[paste0("data_edit-", var_name, "_time")]]
            
            if(is.null(date_input) || is.null(time_input) || time_input == "") {
              processed_val <- NA
            } else {
              # Combine date and time
              datetime_string <- paste(date_input, time_input)
              processed_val <- tryCatch({
                as.POSIXct(datetime_string)
              }, error = function(e) {
                # If parsing fails, keep original value
                rval$orig_trip_record[[var_name]]
              })
            }
            row <- as.data.frame(processed_val)
          } else {
            row <- as.data.frame(input_val)
          }
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
