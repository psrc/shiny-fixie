# buttons lead to sql sprocs: https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/fixie_sprocs.sql

modal_edit_trip_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("editbutton"))

}

modal_edit_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    rval <- reactiveValues(recid = NULL, 
                           trip_record = NULL, 
                           trip_summary_table = NULL,
                           compare_table = NULL,
                           updated_trip = NULL)

    # data validation: Create an InputValidator object
    iv <- InputValidator$new()

    # Trip Record Editor ----
    observeEvent(input$clickedit, {
      # browser()   
      
      rval$recid <- selected_recid()
      rval$trip_record <- get_data(view_name="Trip", recid=rval$recid) 
      
      
      # if a row is selected in table: show Trip Record Editor
      if(!identical(rval$recid,integer(0))){
        
        # browser()
        
        # data validation: Add validation rules
        # origin
        iv$add_rule("data_edit-origin_lat", sv_lte(90))
        iv$add_rule("data_edit-origin_lat", sv_gte(-90))
        iv$add_rule("data_edit-origin_lng", sv_lte(180))
        iv$add_rule("data_edit-origin_lng", sv_gte(-180))
        # destination
        iv$add_rule("data_edit-dest_lat", sv_lte(90))
        iv$add_rule("data_edit-dest_lat", sv_gte(-90))
        iv$add_rule("data_edit-dest_lng", sv_lte(180))
        iv$add_rule("data_edit-dest_lng", sv_gte(-180))
        # distance
        iv$add_rule("data_edit-distance_miles", sv_gt(0))
        # arrival time later than departure time
        iv$add_rule("data_edit-arrival_time_timestamp_time", function(value) {
          
          depart_datetime_r <- as.POSIXct(paste(input[["data_edit-depart_time_timestamp_date"]],
                                                strftime(input[["data_edit-depart_time_timestamp_time"]], format="%H:%M:%S"))
          )
          arrival_datetime_r <- as.POSIXct(paste(input[["data_edit-arrival_time_timestamp_date"]],
                                                 strftime(input[["data_edit-arrival_time_timestamp_time"]], format="%H:%M:%S"))
          )
          
          # if there are values in depart_datetime_r and arrival_datetime_r
          if (!identical(depart_datetime_r, as.POSIXct(numeric(0))) & !identical(arrival_datetime_r, as.POSIXct(numeric(0)))) {
            if(depart_datetime_r >= arrival_datetime_r){
              "Arrival time must be later than departure time"
            }
          }
        })
        
        # data validation: Start displaying errors in the UI
        iv$enable()
        
        rval$trip_summary_table <- rval$trip_record %>%
          select(hhid,pernum,person_id,tripnum,recid) %>%
          left_join(
            get_data(view_name = "trip_error_flags", recid = rval$recid) %>%
              select(recid, error_flag),
            by = "recid")
            
        
        output$trip_summary <- DT::renderDT(
          
          rval$trip_summary_table,
          
          rownames = FALSE,
          options =list(ordering = F,
                        dom = 't',
                        selection = 'single',
                        pageLength =-1)
          
        )

        # featured buttons
        modal_copy_latlong_server("button-copy_origin",
                                  lat_input=input$`data_edit-origin_lat`, long_input=input$`data_edit-origin_lng`)
        modal_copy_latlong_server("button-copy_dest",
                                  lat_input=input$`data_edit-dest_lat`, long_input=input$`data_edit-dest_lng`)
        
        observe({
          toggleState(id = "clickdissmissflag", condition = !is.na(rval$trip_summary_table[['error_flag']]))
        })
        
        showModal(
          modalDialog(title = "Trip Record Editor",

                      tagList(

                        # show trip table
                        div(
                          class = "bottom-spacing",
                          DT::DTOutput(ns("trip_summary"))
                        ),

                        ## time and distance ----

                        div(class = "modal-header",
                            "time and distance"),

                        fluidRow(
                          class = "bottom-spacing",
                          # timestamps
                          # TODO: find better way to select date and time
                          column(7, 
                                 dateTimeInput(ns("data_edit-depart_time_timestamp"), df = rval$trip_record),
                                 dateTimeInput(ns("data_edit-arrival_time_timestamp"), df = rval$trip_record)),
                          # trip distance
                          column(5, numericInputSimple(ns("data_edit-distance_miles"), df = rval$trip_record, min = 0))),


                        ## trip origin and destination ----

                        div(class = "modal-header",
                            "trip origin and destination"),
                        
                        # buttons for mapping in google maps
                        fluidRow(class = "bottom-spacing",
                          column(12,
                                 actionButton_google_direction("get_directions", df = rval$trip_record)
                                 )),
                        
                        fluidRow(
                          column(12,
                                 actionButton_google_place("open_origin",
                                                           label = "Open origin location in Google Maps",
                                                           df = rval$trip_record,
                                                           lat_var_name = "origin_lat",
                                                           long_var_name = "origin_lng")
                                 )),
                        
                        fluidRow(class = "section-padding",
                                 # origin purpose
                                 column(5, selectInputSingle(ns("data_edit-origin_purpose"), df = rval$trip_record)),
                                 # leave space
                                 column(3, ),
                                 # origin lat/long
                                 column(4,
                                        fluidRow(
                                          column(6, numericInputSimple(ns("data_edit-origin_lat"), df = rval$trip_record)),
                                          column(6, numericInputSimple(ns("data_edit-origin_lng"), df = rval$trip_record))
                                        ),
                                        # button for copying origin lat/long to clipboard
                                        fluidRow( column(12,modal_copy_latlong_ui(ns('button-copy_origin'))) )
                                        ) # end column
                                 ),

                        fluidRow(
                          # button for mapping destination
                          column(12,
                                 actionButton_google_place("open_dest",
                                                           label = "Open destination location in Google Maps",
                                                           df = rval$trip_record,
                                                           lat_var_name = "dest_lat",
                                                           long_var_name = "dest_lng")
                                 )
                          ),
                        fluidRow(class = "section-padding",
                          # destination purpose
                          column(5, selectInputSingle(ns("data_edit-dest_purpose"), df = rval$trip_record)),
                          # destination label
                          column(3, textInputSimple(ns("data_edit-dest_purpose_other"), df = rval$trip_record)),
                          # destination lat/long
                          column(4,
                                 fluidRow(column(6, numericInputSimple(ns("data_edit-dest_lat"), df = rval$trip_record)),
                                          column(6, numericInputSimple(ns("data_edit-dest_lng"), df = rval$trip_record))),
                                 # button for copying destination lat/long to clipboard
                                 fluidRow(
                                   column(12,
                                          modal_copy_latlong_ui(ns('button-copy_dest'))
                                          )
                                   )
                                 ) # end column
                        ),

                        ## mode type ----

                        fluidRow(column(4,
                                        div(class = "modal-header", "mode type"),
                                           selectInputSingle(ns("data_edit-mode_1"), df = rval$trip_record),
                                           selectInputSingle(ns("data_edit-mode_2"), df = rval$trip_record),
                                           selectInputSingle(ns("data_edit-mode_3"), df = rval$trip_record),
                                           selectInputSingle(ns("data_edit-mode_4"), df = rval$trip_record),
                                           textInputSimple(ns("data_edit-mode_other_specify"), df = rval$trip_record),
                                           selectInputSingle(ns("data_edit-mode_acc"), df = rval$trip_record),
                                           selectInputSingle(ns("data_edit-mode_egr"), df = rval$trip_record)
                                        ), # end column
                                 column(8,
                                        div(class = "modal-header", "travelers"),
                                        column(6,
                                               selectInputSingle(ns("data_edit-driver"), df = rval$trip_record),
                                               selectInputSingle(ns("data_edit-travelers_hh"), df = rval$trip_record),
                                               selectInputSingle(ns("data_edit-travelers_nonhh"), df = rval$trip_record)),
                                        column(6,
                                           div("hhmembers"),
                                           selectInputSingle(ns("data_edit-hhmember1"), df = rval$trip_record, label_name = "1"),
                                           selectInputSingle(ns("data_edit-hhmember2"), df = rval$trip_record, label_name = "2"),
                                           selectInputSingle(ns("data_edit-hhmember3"), df = rval$trip_record, label_name = "3"),
                                           selectInputSingle(ns("data_edit-hhmember4"), df = rval$trip_record, label_name = "4"),
                                           selectInputSingle(ns("data_edit-hhmember5"), df = rval$trip_record, label_name = "5"),
                                           selectInputSingle(ns("data_edit-hhmember6"), df = rval$trip_record, label_name = "6"),
                                           selectInputSingle(ns("data_edit-hhmember7"), df = rval$trip_record, label_name = "7"),
                                           selectInputSingle(ns("data_edit-hhmember8"), df = rval$trip_record, label_name = "8"))
                                        ) # end column
                                 ), # fluidRow

                        ## elevate comment ----

                        fluidRow(
                          column(5,
                                 textInputSimple(ns("data_edit-psrc_comment"),
                                                 df = rval$trip_record,
                                                 label_name = "Add comment:")
                                 )
                        )

                      ), # end tagList
                      br(),
                      fluidRow(column(12,
                                      div(
                                        class = "trip-buttons-panel",
                                        actionButton(ns("clickupdate"), label = "Preview Edits")
                                          )
                                      )),
                      footer = column(12,
                                      class = "trip-buttons-panel",
                                      actionButton(ns("clickdissmissflag"), label = "Dismiss Flag"),
                                      modalButton('(Delete trip)'),
                                      modalButton('(Split from traces)'),
                                      modalButton('Cancel')
                                      ),
                      # easyClose = TRUE,
                      size = "l"
          ))}
      # if no row is selected
        else{
          notification_warning_select_row()
        }
      })
    
    
    # ---- Show Preview Pane & Apply Changes ----
    observeEvent(input$clickupdate, {
      
      # data validation: Don't proceed if any input is invalid
      if(!iv$is_valid()) {
        showNotification(
          "Please fix the errors in the form before continuing",
          type = "warning"
        )
      }
      else{
        # get all editable variables
        input_tripeditor.cols <- paste0("data_edit-", tripeditor.cols)
        
        ## ---- create compare table ----
        compare_table <- NULL
        for(i in 1:length(tripeditor.cols)){
          # variable name
          var_name <- tripeditor.cols[i]
          var_input_name <- input_tripeditor.cols[i]
          
          # updated value
          if(input[[var_input_name]] == ""){
            # if text inputs like dest_purpose_other, psrc_comment are kept empty, the output would be ""
            # change to NA to match DB value
            updated_value <- NA
          }
          else{
            updated_value <- input[[var_input_name]]
          }
          
          compare_var <- as.data.frame(
            cbind(Variable = var_name,
                  # original value
                  `Original Value` = rval$trip_record[[var_name]],
                  # updated value
                  `Updated Value` = updated_value
            )
          )
          
          # create df with original and updated values
          compare_table <- rbind(compare_table,
                                 compare_var)
        }
        ## ---- process datetime ----
        
        # updated timestamps
        depart_datetime <- paste(input[["data_edit-depart_time_timestamp_date"]],
                                 strftime(input[["data_edit-depart_time_timestamp_time"]], format="%H:%M:%S"))
        arrival_datetime <- paste(input[["data_edit-arrival_time_timestamp_date"]],
                                  strftime(input[["data_edit-arrival_time_timestamp_time"]], format="%H:%M:%S"))
        
        compare_var <- as.data.frame(
          cbind(Variable = c("depart_time_timestamp", "arrival_time_timestamp"),
                # original value
                `Original Value` = c(as.character(format(rval$trip_record[["depart_time_timestamp"]], "%Y-%m-%d %H:%M:%S")),
                                     as.character(format(rval$trip_record[["arrival_time_timestamp"]], "%Y-%m-%d %H:%M:%S"))),
                # updated value
                `Updated Value` = c(depart_datetime, arrival_datetime)
          )
        )
        
        compare_table <- rbind(compare_var, compare_table)
        
        # detect if values are modified
        rval$compare_table <- compare_table %>%
          mutate(mod=case_when(`Original Value`==`Updated Value`~0,
                               is.na(`Original Value`) & is.na(`Updated Value`)~0, # for empty text inputs
                               TRUE~1))
        
        ## ---- generate updated trip record ----
        trip <- NULL
        for(var_name in names(rval$trip_record)){
          if(var_name %in% tripeditor.cols){
            row <- as.data.frame(input[[paste0("data_edit-",var_name)]])
          } else{
            row <- as.data.frame(rval$trip_record[[var_name]])
          }
          
          if(is.null(trip)){
            trip <- row
          }
          else{
            trip <- cbind(trip, row)
          }
          
        }
        names(trip) <- names(rval$trip_record)
        rval$updated_trip <- trip
        
        
        ## ---- print all comparison table ----
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
        
        ## ---- modal dialog: show update preview pane ----
        showModal(
          modalDialog(title = "Update Trip Record Preview",
                      
                      # show trip table
                      div(
                        class = "bottom-spacing",
                        DT::DTOutput(ns("trip_summary"))
                      ),
                      #TODO: add trip summary
                      div(
                        DTOutput(ns('print_cols'))
                      ),
                      
                      footer = div(
                        style = "display: flex; justify-content: space-between;",
                        # push changes to database
                        actionButton(ns("clickpush"), label = "Apply Changes"),
                        modalButton('Close')
                      ),
                      easyClose = TRUE,
                      size = "l"
          )
        )
      }
      
    })
    
    # ---- Update Data in Database ----
    observeEvent(input$clickpush, {
      
      # create a named list of all edits
      df <- rval$compare_table
      
      edit_list <- df[df$mod == 1, "Updated Value"]
      names(edit_list) <- df[df$mod == 1, "Variable"]
      
      # write update query
      sproc_update_data(rval$recid, edit_list)
      
    })
    
    # ---- Dismiss Flag ----
    observeEvent(input$clickdissmissflag, {
      
      showModal(
        modalDialog(title = "Are you sure you want to dismiss this error flag?",
                    
                    # show trip table
                    div(
                      class = "bottom-spacing",
                      DT::DTOutput(ns("trip_summary"))
                    ),
                    footer = div(
                      style = "display: flex; justify-content: space-between;",
                      # push changes to database
                      actionButton(ns("clickdissmissflag_action"), label = 'Yes'),
                      modalButton('No')
                    ),
                    easyClose = TRUE,
                    size = "l") 
        )
      
    })
    
    ## ---- Confirm Dismiss Flag ----
    observeEvent(input$clickdissmissflag_action, {
      
      # executes dismiss flag and show success message
      sproc_dismiss_flag(rval$recid, rval$trip_record[["person_id"]])
      
    })
    


    output$editbutton <- renderUI({  actionButton(ns("clickedit"), "Edit trip") })

  })  # end moduleServer
}